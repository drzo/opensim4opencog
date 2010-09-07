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
                new[] {"html", "http://www.w3.org/1999/xhtml"},
                new[] {"xsl", "http://www.w3.org/1999/XSL/Transform"},
                new[] {"xsi", "http://www.w3.org/2001/XMLSchema-instance"},
                new[] {"msxml", "urn:schemas-microsoft-com:xslt"},
                new[] {"js", "JavaScript"}
            };

        public static OutputDelegate errorOutput;

        public static bool SkipXmlns = true;
        public static Func<string, string> TextFormatter = StaticXMLUtils.CleanWhitepaces;
        public static Func<string, string> TextWhitespaceCleaner = StaticXMLUtils.CleanWhitepaces;
        public readonly XmlNamespaceManager Manager;

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
                var settings = new XmlReaderSettings();
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
                    var manager = new XmlNamespaceManager(settingsNameTable);
                    SetupNamespaces(manager, settings.Schemas);
                }
                return settings;
            }
        }

        private static void SetupNamespaces(XmlNamespaceManager manager, XmlSchemaSet schemas)
        {
            foreach (var ns in PredefinedNamespaces)
            {
                string prefix = ns[0];
                string url = ns[1];
                manager.AddNamespace(prefix, url);
                // schemas.Add(prefix, url);
            }
        }

        private void SetupDoc()
        {
            foreach (var ns in PredefinedNamespaces)
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
                catch (Exception e)
                {
                    writeToLog("ERROR " + e);
                }
            }
            finally
            {
                LineTracker = prev;
            }
        }

        public static void CheckNode(XmlNode node)
        {
            return;
            if (node.OuterXml.Contains("l:") || node.OuterXml.Contains("nls"))
            {
                return;
            }
        }

        // ReSharper disable RedundantOverridenMember
        public override XmlNode CreateNode(XmlNodeType type, string prefix, string name, string namespaceURI)
        {
            XmlNode v = base.CreateNode(type, prefix, name, namespaceURI);
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
            XmlNode v = base.CreateNode(nodeTypeString, name, namespaceURI);
            CheckNode(v);
            return v;
        }

        public override XmlNode CreateNode(XmlNodeType type, string name, string namespaceURI)
        {
            XmlNode v = base.CreateNode(type, name, namespaceURI);
            CheckNode(v);
            return v;
        }

        protected override XmlAttribute CreateDefaultAttribute(string prefix, string localName, string namespaceURI)
        {
            XmlAttribute v = base.CreateDefaultAttribute(prefix, localName, namespaceURI);
            CheckNode(v);
            return v;
        }

        public override XmlDocumentFragment CreateDocumentFragment()
        {
            XmlDocumentFragment v = base.CreateDocumentFragment();
            CheckNode(v);
            return v;
        }

        public override XmlDocumentType CreateDocumentType(string name, string publicId, string systemId,
                                                           string internalSubset)
        {
            XmlDocumentType v = base.CreateDocumentType(name, publicId, systemId, internalSubset);
            CheckNode(v);
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
            StepTrace();
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
                    catch (Exception e)
                    {
                        writeToLog("ERROR " + e);
                        throw e;
                    }
                }
            }
            finally
            {
                CurrentReader = prereader;
                LineTracker = prev;
            }
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

        private static void writeToLog(string s)
        {
            DLRConsole.DebugWriteLine("{0}", s);
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
                catch (Exception e)
                {
                    writeToLog("ERROR: " + e);
                    throw e;
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
                var nodeL = node as XmlSourceLineInfo;
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
            try
            {
                if (MustFormat.Contains(localName))
                    MustSpaceWildcards = true;
                if (MustNotFormat.Contains(localName))
                    MustSpaceWildcards = true;

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
            var doc = new XmlDocumentLineInfo("CreateXmlTextReader for NODE", false);
            doc.Load("books.xml");

            // Set the validation settings.
            var settings = new XmlReaderSettings();
            settings.ValidationType = ValidationType.Schema;
            settings.Schemas.Add("urn:bookstore-schema", "books.xsd");
            settings.ValidationEventHandler += ValidationCallBack;

            // Create a validating reader that wraps the XmlNodeReader object.
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

        internal static XmlReader CreateXmlTextReader(TextReader tr)
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