using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.Schema;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public class XmlDocumentLineInfo : XmlDocument
    {
        private static readonly ICollection<string> MustFormat = new HashSet<string>();
        private static readonly ICollection<string> MustNotFormat = new HashSet<string>();

        private static readonly string[][] PredefinedNamespaces =
            {
                new[] {"aiml", "http://alicebot.org/2001/AIML-1.0.1"},
                new[] {"html", "http://www.w3.org/1999/xhtml"},
                new[] {"xsl", "http://www.w3.org/1999/XSL/Transform"},
                new[] {"xsi", "http://www.w3.org/2001/XMLSchema-instance"},
                new[] {"msxml", "urn:schemas-microsoft-com:xslt"},
                new[] {"js", "JavaScript"}
            };

        private static readonly List<String> RemoveNamepaceURIs = new List<string>()
                                                             {
                                                                 "http://alicebot.org/2001/AIML-1.0.1",
                                                             };

        private static readonly List<String> RemovePrefixes = new List<string>()
                                                                  {
                                                                      "html",
                                                                      "aiml",
                                                                  };
        public static OutputDelegate errorOutput;

        public static bool SkipXmlns = true;
        public static Func<string, string> TextFormatter = StaticXMLUtils.CleanWhitepaces;
        public static Func<string, string> TextWhitespaceCleaner = StaticXMLUtils.CleanWhitepaces;
        public readonly XmlNamespaceManager Manager;

        public bool RemoveXmlns = SkipXmlns;
        private string currentAttributeName;
        private string currentNodeType;
        private XmlReader CurrentReader;
        public string docEncoding;
        public XmlReaderSettings docSettings = DefaultSettings;
        public string docStandalone;

        public bool DocumentHasNormalized = true;
        public bool DocumentInNormalize = true;
        public string docVersion;
        public string InfoString;
        public IXmlLineInfo LineTracker;
        public bool MustSpaceAttributeValues;
        public bool MustSpaceWildcards;
        public bool NeedCleanAttribs = true;
        private string preXml = "";
        public bool SuspendLineInfo;

        static XmlDocumentLineInfo()
        {
            lock (MustFormat)
            {
                MustFormat.Add("pre");
                MustFormat.Add("pattern");
                MustFormat.Add("that");
                MustNotFormat.Add("category");
                MustNotFormat.Add("template");
            }
        }

        public XmlDocumentLineInfo()
            : this(null, true)
        {
        }

        public XmlDocumentLineInfo(string toString, bool presrveWhite)
        {
            InfoString = toString;
            PreserveWhitespace = presrveWhite;
            base.Prefix = "";
            Schemas = new XmlSchemaSet();
            Manager = new XmlNamespaceManager(NameTable);
            SetupNamespaces(Manager, Schemas);
            //SetupDoc();
        }

        public override XmlDocument OwnerDocument
        {
            get { return base.OwnerDocument ?? this; }
        }

        public override bool IsReadOnly
        {
            get
            {
                bool was = base.IsReadOnly;
                if (DocumentInNormalize) return false;
                return DocumentHasNormalized || was;
            }
        }

        public static XmlReaderSettings DefaultSettings
        {
            get
            {
                XmlReaderSettings settings = new XmlReaderSettings();
                settings.ConformanceLevel = ConformanceLevel.Fragment;
                settings.ValidationType = ValidationType.None;
                settings.IgnoreWhitespace = true;
                settings.IgnoreComments = true;
                //settings.ProhibitDtd = true;
                settings.ProhibitDtd = false;
                settings.CheckCharacters = false;
                settings.ValidationFlags = XmlSchemaValidationFlags.AllowXmlAttributes |
                                           XmlSchemaValidationFlags.ReportValidationWarnings |
                                           XmlSchemaValidationFlags.ProcessIdentityConstraints;
                XmlNameTable settingsNameTable = settings.NameTable;
                if (settingsNameTable != null)
                {
                    XmlNamespaceManager manager = new XmlNamespaceManager(settingsNameTable);
                    SetupNamespaces(manager, settings.Schemas);
                }
                return settings;
            }
        }

        private static void SetupNamespaces(XmlNamespaceManager manager, XmlSchemaSet schemas)
        {
            foreach (string[] ns in PredefinedNamespaces)
            {
                string prefix = ns[0];
                string url = ns[1];
                manager.AddNamespace(prefix, url);
                // schemas.Add(prefix, url);
            }
        }

        private void SetupDoc()
        {
            foreach (string[] ns in PredefinedNamespaces)
            {
                AddNameSpace(ns[0], ns[1]);
            }
            preXml += " exclude-result-prefixes=\"msxsl js\" ";
            //LoadXml("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            //LoadXml("<?xml-stylesheet type=\"text/xsl\" ?>");
        }

        private string AddNameSpace(string prefix, string url)
        {
            Schemas.Add(prefix, url);
            Manager.AddNamespace(prefix, url);
            string str = string.Format("xmlns:{0}=\"{1}\" s", prefix, url);
            preXml += str;
            return str;
        }

        public override void Normalize()
        {
            if (NeedCleanAttribs)
            {
                NeedCleanAttribs = false;
                if (DocumentElement != null)
                {
                    XmlAttributeCollection attrs = DocumentElement.Attributes;
                    if (attrs != null)
                    {
                        var list = new List<XmlAttribute>();
                        foreach (XmlAttribute s in attrs)
                        {
                            if (s.LocalName == "xmlns")
                            {
                                list.Add(s);
                            }
                        }
                        foreach (XmlAttribute s in list)
                        {
                            DocumentElement.RemoveAttributeNode(s.LocalName, s.NamespaceURI);
                        }
                    }
                }
            }
            SuspendLineInfo = true;
            LineTracker = null;
            if (DocumentHasNormalized || DocumentInNormalize)
            {
                // writeToLog("In normalization already");
                return;
            }
            DocumentInNormalize = true;
            try
            {
                base.Normalize();
                DocumentHasNormalized = true;
            }
            finally
            {
                DocumentInNormalize = false;
            }
        }

        public override void Load(string filename)
        {
            var s = XmlReader.Create(filename, DefaultSettings);
            Load(s);
        }
        public override void Load(Stream reader)
        {
            IXmlLineInfo prev = LineTracker;
            try
            {
                try
                {
                    if (reader is IXmlLineInfo)
                    {
                        LineTracker = (IXmlLineInfo) reader;
                    }
                    else
                    {
                        Load(CreateXmlTextReader(reader));
                        return;
                    }
                    base.Load(reader);
                    Normalize();
                }
                catch (SystemException e)
                {
                    string errmsg = GetErrorMsg(reader, e);
                    writeToLog("Load(Stream) " + errmsg);
                    throw;
                }
            }
            finally
            {
                LineTracker = prev;
            }
        }

        public void CheckNode(XmlNode node)
        {
            return;
            if (node.NodeType == XmlNodeType.Text) return;
            if (node.NodeType == XmlNodeType.Comment) return;
            if (node.NodeType == XmlNodeType.Element) return;
            if (node.NodeType == XmlNodeType.Attribute) return;
            if (node.NodeType == XmlNodeType.XmlDeclaration) return;
            string s = node.OuterXml;
            writeToLog("checknode: " + s);
            //if (node.NodeType == XmlNodeType.XmlDeclaration) return;
            if (s.Contains("l:") || s.Contains("nls"))
            {
                return;
            }
        }

        // ReSharper disable RedundantOverridenMember
        public override XmlNode CreateNode(XmlNodeType type, string prefix, string localName, string namespaceURI)
        {
            if (type == XmlNodeType.Element) return CreateElement(prefix, localName, namespaceURI);
            string nodeTypeString = type.ToString();
            string name = null;
            TransformElement(ref nodeTypeString, ref prefix, ref localName, ref name, ref namespaceURI);
            XmlNode v = base.CreateNode(type, prefix, localName, namespaceURI);
            CheckNode(v);
            return v;
        }

        public override XmlCDataSection CreateCDataSection(string data)
        {
            XmlCDataSection v = base.CreateCDataSection(data);
            CheckNode(v);
            return v;
        }

        public override XmlNode CreateNode(string nodeTypeString, string name, string namespaceURI)
        {
            string prefix = null;
            string localName = null;
            TransformElement(ref nodeTypeString, ref prefix, ref localName, ref name, ref namespaceURI);
            XmlNode v = base.CreateNode(nodeTypeString, name, namespaceURI);
            CheckNode(v);
            return v;
        }

        public override XmlNode CreateNode(XmlNodeType type, string name, string namespaceURI)
        {
            string prefix = null;
            string nodeTypeString = type.ToString();
            string localName = null;
            TransformElement(ref nodeTypeString, ref prefix, ref localName, ref name, ref namespaceURI);
            XmlNode v = base.CreateNode(type, name, namespaceURI);
            CheckNode(v);
            return v;
        }

        protected override XmlAttribute CreateDefaultAttribute(string prefix, string localName, string namespaceURI)
        {
            string nodeTypeString = null;
            string name = null;
            TransformElement(ref nodeTypeString, ref prefix, ref localName, ref name, ref namespaceURI);
            XmlAttribute v = base.CreateDefaultAttribute(prefix, localName, namespaceURI);
            CheckNode(v);
            StepTrace();
            return v;
        }

        public override XmlDocumentFragment CreateDocumentFragment()
        {
            XmlDocumentFragment v = base.CreateDocumentFragment();
            CheckNode(v);
            StepTrace();
            return v;
        }

        public override XmlDocumentType CreateDocumentType(string name, string publicId, string systemId,
                                                           string internalSubset)
        {
            XmlDocumentType v = base.CreateDocumentType(name, publicId, systemId, internalSubset);
            CheckNode(v);
            StepTrace();
            return v;
        }

        // ReSharper restore RedundantOverridenMember

        public override XmlDeclaration CreateXmlDeclaration(string version, string encoding, string standalone)
        {
            docVersion = version;
            if (encoding == null)
            {
                encoding = "ISO-8859-1";
            }
            if (docEncoding == null) docEncoding = encoding;
            docStandalone = standalone;
            XmlDeclaration v = base.CreateXmlDeclaration(version, encoding, standalone);
            CheckNode(v);
            return v;
        }

        public override string GetPrefixOfNamespace(string prefix)
        {
            string namespaceURI = base.GetPrefixOfNamespace(prefix);
            StepTrace();
            return namespaceURI;
        }

        public override string GetNamespaceOfPrefix(string namespaceURI)
        {
            string prefix = base.GetNamespaceOfPrefix(NamespaceURI);
            StepTrace();
            return prefix;
        }


        private static void StepTrace()
        {
        }
        void TransformElement(ref string nodeTypeString, ref string prefix, ref string localName, ref string name,
                              ref string namespaceURI)
        {
            bool prefixIsNullable = localName == null;
            string nodeName = String.Format("{0}:{1}", (prefix ?? "_"), (prefixIsNullable ? name : localName));

            if (prefix == null)
            {
                if (!prefixIsNullable)
                {
                    writeToLog("RemoveXmlns: null prefix in " + nodeName);
                    prefix = "";
                }
            }
            else if (prefix == "")
            {
                if (prefixIsNullable)
                {
                    writeToLog("RemoveXmlns: null prefix in " + nodeName);
                    prefix = null;
                }
            }
            else
            {
                if (RemovePrefixes.Contains(prefix))
                {
                    prefix = prefixIsNullable ? null : "";
                }
            }

            if (namespaceURI == null)
            {
                writeToLog("RemoveXmlns: null namespaceURI in " + nodeName);
                namespaceURI = "";
            }
            if (RemoveNamepaceURIs.Contains(namespaceURI))
            {
                namespaceURI = "";
            }

            if (RemoveXmlns)
            {
                if (!string.IsNullOrEmpty(prefix))
                {
                  //  writeToLog("RemoveXmlns: prefix=" + prefix + " in " + nodeName);
                }
                prefix = prefixIsNullable ? null : "";

                if (!string.IsNullOrEmpty(namespaceURI))
                {
                   // writeToLog("RemoveXmlns: namespaceURI=" + namespaceURI + " in " + nodeName);
                }
                namespaceURI = "";

                if (localName == "xmlns")
                {
                    localName = "legacy_xmlns";
                    //writeToLog("RemoveXmlns: localName=xmlns in " + nodeName);
                }
                if (name == "xmlns")
                {
                    name = "legacy_xmlns";
                    writeToLog("RemoveXmlns: name=xmlns in " + nodeName);
                }
            }
        }

        public override void Load(XmlReader reader)
        {
            IXmlLineInfo prev = LineTracker;
            XmlReader prereader = CurrentReader;
            try
            {
                if (reader is IXmlLineInfo)
                {
                    LineTracker = (IXmlLineInfo) reader;
                }
                if (!reader.EOF)
                {
                    try
                    {
                        LoadFromReader(reader);
                    }
                    catch (SystemException e)
                    {
                        string errmsg = GetErrorMsg(reader, e);
                        writeToLog(errmsg);
                        throw;
                    }
                }
                else
                {
                    try
                    {
                        LoadFromReader(reader);
                    }
                    catch (SystemException e)
                    {
                        string errmsg = GetErrorMsg(reader, e);
                        writeToLog("EOF " + errmsg);
                        throw;
                    }
                }
            }
            finally
            {
                CurrentReader = prereader;
                LineTracker = prev;
            }
        }

        public string GetErrorMsg(Object aka, Exception e)
        {
            var reader = (aka as XmlReader) ?? (LineTracker as XmlReader);
            var li = (aka as IXmlLineInfo) ?? LineTracker ?? (reader as IXmlLineInfo);
            bool notEOF = reader != null && !reader.EOF;
            return "ERROR " + e.Message + " " + LineInfoStr(li) + " notEOF="
                   + notEOF + " on " + (reader ?? aka) + " " + e + " @ " + this;
        }

        public static string LineInfoStr(IXmlLineInfo li)
        {
            if (li == null) return "";
            return li+ ":(" + li.LineNumber + "," + li.LinePosition + ")";
        }

        private void LoadFromReader(XmlReader reader)
        {
            XmlReader reader0 = CheckReader(reader);
            base.Load(reader0);
            Normalize();
        }

        private XmlReader CheckReader(XmlReader reader)
        {
            CurrentReader = reader;
            CheckSettings(reader.Settings);
            return reader;
        }

        private void CheckSettings(XmlReaderSettings settings)
        {
            if (settings == null) return;
            XmlReaderSettings xmlReaderSettings = DefaultSettings;
            if (settings.ConformanceLevel != xmlReaderSettings.ConformanceLevel)
                writeToLog("ConformanceLevel settings odd");
            if (settings.ProhibitDtd != xmlReaderSettings.ProhibitDtd)
                writeToLog("ProhibitDtd settings odd");
            if (settings.ValidationType != xmlReaderSettings.ValidationType) writeToLog("ValidationType settings odd");
            if (settings.IgnoreWhitespace != xmlReaderSettings.IgnoreWhitespace) writeToLog("whitespace settings odd");
            if (settings.IgnoreComments != xmlReaderSettings.IgnoreComments) writeToLog("IgnoreComments settings odd");
            if (settings.CheckCharacters != xmlReaderSettings.CheckCharacters)
                writeToLog("CheckCharacters settings odd");
            //  if (settings.ValidationFlags != DefaultSettings.ValidationFlags) writeToLog("ValidationFlags settings odd");
        }

        private void writeToLog(string s)
        {
            DLRConsole.DebugWriteLine("{0} ({1})", s, ToString());
        }

        public override void Load(TextReader reader)
        {
            IXmlLineInfo prev = LineTracker;
            try
            {
                if (reader is IXmlLineInfo)
                {
                    LineTracker = (IXmlLineInfo) reader;
                }
                base.Load(reader);
                Normalize();
            }
            finally
            {
                LineTracker = prev;
            }
        }

        public override string ToString()
        {
            return InfoString ?? base.ToString();
        }

        /// <summary>
        /// Creates an <see cref="T:System.Xml.XmlNode"/> object based on the information in the <see cref="T:System.Xml.XmlReader"/>. The reader must be positioned on a node or attribute.
        /// </summary>
        /// <returns>
        /// The new XmlNode or null if no more nodes exist.
        /// </returns>
        /// <param name="reader">The XML source 
        ///                 </param><exception cref="T:System.NullReferenceException">The reader is positioned on a node type that does not translate to a valid DOM node (for example, EndElement or EndEntity). 
        ///                 </exception>
        public override XmlNode ReadNode(XmlReader reader)
        {
            IXmlLineInfo prev = LineTracker;
            try
            {
                if (reader is IXmlLineInfo)
                {
                    LineTracker = (IXmlLineInfo) reader;
                }
                try
                {
                    CheckReader(reader);
                    return base.ReadNode(reader);
                }
                catch (SystemException e)
                {
                    string errmsg = GetErrorMsg(reader, e);
                    writeToLog("ReadNode " + errmsg);
                    throw;
                }
            }
            finally
            {
                LineTracker = prev;
            }
        }

        public override void LoadXml(string xml)
        {
            //base.Load(new XmlTextReader(new StringReader(xml)));
            base.Load(CreateXmlTextReader(new StringReader(xml)));
            //Load(CreateXmlTextReader(new StringReader(xml)));
            Normalize();
        }

        public override XmlText CreateTextNode(string text)
        {
            XmlTextLineInfo node;
            string clean = text;
            if (MustSpaceWildcards)
            {
                clean = FormatTextNode(text);
            }
            else
            {
                clean = CleanWhitepaces(text);
            }
            if (clean != text)
            {
                node = new XmlTextLineInfo(clean, this);
            }
            else
            {
                node = new XmlTextLineInfo(text, this);
            }
            setLineInfo(node);
            return node;
        }

        public void setLineInfo(XmlNode node)
        {
            CheckNode(node);
            if (LineTracker != null)
            {
                XmlSourceLineInfo nodeL = node as XmlSourceLineInfo;
                if (!SuspendLineInfo && nodeL != null && LineTracker != null)
                    nodeL.SetLineInfo(LineTracker.LineNumber, LineTracker.LinePosition);
            }
            if (node is XmlSourceInfo) (node as XmlSourceInfo).ReadOnly = true;
        }


        public static void SuggestLineNo(IXmlLineInfo lie, XmlSourceLineInfo target)
        {
            int atline = lie.LineNumber;
            if (atline > target.LineNumber)
            {
                target.SetLineInfo(lie.LineNumber, lie.LinePosition);
            }
        }

        public override XmlAttribute CreateAttribute(string prefix, string localName, string namespaceURI)
        {
            XmlAttributeLineInfo elem;
            string nodeTypeString = null;
            string name = null;
            TransformElement(ref nodeTypeString, ref prefix, ref localName, ref name, ref namespaceURI);
            string prev = currentAttributeName;
            try
            {
                currentAttributeName = localName;
                if (false)
                {
                    if (localName == "xmlns")
                    {
                        localName = "xxxxx";
                    }
                    prefix = "";
                    namespaceURI = "";
                }
                elem = new XmlAttributeLineInfo(prefix, localName, namespaceURI, this);
            }
            finally
            {
                // currentAttributeName = prev;
            }
            setLineInfo(elem);
            return elem;
        }

        public override XmlWhitespace CreateWhitespace(string text)
        {
            //return new XmlWhitespace(text, this);
            if (text != " ")
            {
                writeToLog("WHITE='" + text + "'");
            }
            return base.CreateWhitespace(" ");
        }

        public override XmlElement CreateElement(string prefix, string localName, string namespaceURI)
        {
            LineInfoElementImpl elem;
            string nodeTypeString = "Element";
            string name = null;
            TransformElement(ref nodeTypeString, ref prefix, ref localName, ref name, ref namespaceURI);
            bool prev = MustSpaceWildcards;
            try
            {
                if (MustFormat.Contains(localName))
                    MustSpaceWildcards = true;
                if (MustNotFormat.Contains(localName))
                    MustSpaceWildcards = false;

                // prevNodeType = currentNodeType;
                currentNodeType = localName;
                elem = new LineInfoElementImpl(prefix, localName, namespaceURI, this);
            }
            finally
            {
                // currentNodeType = prev;
            }
            setLineInfo(elem);
            return elem;
        }

        public static XmlReader CreateXmlTextReader(XmlNodeReader nodeReader)
        {
            XmlDocumentLineInfo doc = new XmlDocumentLineInfo("CreateXmlTextReader for NODE", false);
            doc.Load("books.xml");

            // Set the validation settings.
            XmlReaderSettings settings = DefaultSettings; //new XmlReaderSettings();
            if (false)
            {
                settings.ValidationType = ValidationType.Auto;
                settings.Schemas.Add("urn:bookstore-schema", "books.xsd");
                settings.ValidationEventHandler += ValidationCallBack;

                // Create a validating reader that wraps the XmlNodeReader object.
            }
            XmlReader reader = XmlReader.Create(nodeReader, settings);
            // Parse the XML file.
            return reader;
            //while (reader.Read()) ;
        }

        private static void ValidationCallBack(object sender, ValidationEventArgs e)
        {
        }

        public static XmlReader CreateXmlTextReader(Stream stream)
        {
            return XmlReader.Create(stream, DefaultSettings);
        }

        public static XmlReader CreateXmlTextReader(XmlTextReader xmlTextReader)
        {
            return XmlReader.Create(xmlTextReader, DefaultSettings);
        }

        public static XmlReader CreateXmlTextReader(TextReader tr)
        {
            //XmlTextReader xmlTextReader = new XmlTextReader(tr);
            return XmlReader.Create(tr, DefaultSettings);
        }


        /***
         
         This document already has a 'DocumentElement' node.
          at System.Xml.XmlDocument.IsValidChildType(XmlNodeType type)
          at System.Xml.XmlDocument.AppendChildForLoad(XmlNode newChild, XmlDocument doc)
          at System.Xml.XmlLoader.LoadDocSequence(XmlDocument parentDoc)
          at System.Xml.XmlLoader.Load(XmlDocument doc, XmlReader reader, Boolean preserveWhitespace)
          at System.Xml.XmlDocument.Load(XmlReader reader)
                 
         */

        public string TextAndSourceInfo(XmlNode node)
        {
            string s = StaticXMLUtils.TextInfo(node);
            return s + " " + StaticXMLUtils.LocationEscapedInfo(node);
        }

        internal string FormatTextNode(string text)
        {
            if (TextFormatter != null && FormatTextNode != TextFormatter) return TextFormatter(text);
            return CleanWhitepaces(text);
        }

        internal string CleanWhitepaces(string text)
        {
            if (TextWhitespaceCleaner != null && CleanWhitepaces != TextWhitespaceCleaner)
                return TextWhitespaceCleaner(text);
            return text;
        }

        public static void DebugWriteLine(string format, params object[] args)
        {
            if (errorOutput != DebugWriteLine && errorOutput != null) errorOutput(format, args);
            DLRConsole.DebugWriteLine(format, args);
        }

        public static XmlTextReader CreateXmlTextFileReader(string xcmd)
        {
            return new XmlTextReader(xcmd); //XmlReader.Create(, DefaultSettings);
        }
    }
}