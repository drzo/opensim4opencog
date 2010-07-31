using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.IO;
using System.Xml;
using System.Xml.Schema;
using System.Text;
using MushDLR223.ScriptEngines;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using MushDLR223.Virtualization;

namespace RTParser.Utils
{

    public class XmlDocumentLineInfo : XmlDocument
    {

        public static bool SkipXmlns = true;

        public override bool IsReadOnly
        {
            get
            {
                bool was = base.IsReadOnly;
                if (DocumentInNormalize) return false;
                return DocumentHasNormalized || was;
            }
        }

        public override void Normalize()
        {
            if (DocumentHasNormalized || DocumentInNormalize) 
            {
               // writeToLog("In normalization already");
                return;
            }

            if (DocumentElement != null)
            {
                var attrs = DocumentElement.Attributes;
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

            SuspendLineInfo = true;
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

        public bool DocumentHasNormalized = false;
        public bool DocumentInNormalize = false;
        public override void Load(Stream reader)
        {
            try
            {
                if (reader is IXmlLineInfo)
                {
                    LineTracker = (IXmlLineInfo)reader;
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
            var v = base.CreateNode(type, prefix, name, namespaceURI);
            CheckNode(v);
            return v;
        }
        public override XmlCDataSection CreateCDataSection(string data)
        {
            var v = base.CreateCDataSection(data);
            CheckNode(v);
            return v;
        }
        public override XmlNode CreateNode(string nodeTypeString, string name, string namespaceURI)
        {
            var v = base.CreateNode(nodeTypeString, name, namespaceURI);
            CheckNode(v);
            return v;
        }
        public override XmlNode CreateNode(XmlNodeType type, string name, string namespaceURI)
        {
            var v = base.CreateNode(type, name, namespaceURI);
            CheckNode(v);
            return v;
        }
        protected override XmlAttribute CreateDefaultAttribute(string prefix, string localName, string namespaceURI)
        {            
            var v = base.CreateDefaultAttribute(prefix, localName, namespaceURI);
            CheckNode(v);
            return v;
        }

        public override XmlDocumentFragment CreateDocumentFragment()
        {
            var v = base.CreateDocumentFragment();
            CheckNode(v);
            return v;
        }

        public override XmlDocumentType CreateDocumentType(string name, string publicId, string systemId, string internalSubset)
        {
            var v = base.CreateDocumentType(name, publicId, systemId, internalSubset);
            CheckNode(v);
            return v;
        }
        // ReSharper restore RedundantOverridenMember

        string version, encoding, standalone;
        public override XmlDeclaration CreateXmlDeclaration(string version, string encoding, string standalone)
        {
            this.version = version;
            if (encoding == null)
            {
                encoding = "ISO-8859-1";
            }
            this.encoding = encoding;
            this.standalone = standalone;
            var v = base.CreateXmlDeclaration(version, encoding, standalone);
            CheckNode(v);
            return v;
        }
        public override void Load(XmlReader reader)
        {
            CheckReader(reader.Settings);
            if (reader is IXmlLineInfo)
            {
                LineTracker = (IXmlLineInfo)reader;
            }
            if (!reader.EOF)
            {
                try
                {
                    base.Load(reader);
                    Normalize();
                }
                catch (Exception e)
                {
                    writeToLog("ERROR " + e);
                    throw e;
                }
            }
        }

        static void CheckReader(XmlReaderSettings settings)
        {
            if (settings == null) return;
            var xmlReaderSettings = DefaultSettings;
            if (settings.ConformanceLevel != xmlReaderSettings.ConformanceLevel) writeToLog("ConformanceLevel settings odd");
            if (settings.ValidationType != xmlReaderSettings.ValidationType) writeToLog("ValidationType settings odd");
            if (settings.IgnoreWhitespace != xmlReaderSettings.IgnoreWhitespace) writeToLog("whitespace settings odd");
            if (settings.IgnoreComments != xmlReaderSettings.IgnoreComments) writeToLog("IgnoreComments settings odd");
            if (settings.CheckCharacters != xmlReaderSettings.CheckCharacters) writeToLog("CheckCharacters settings odd");
            //  if (settings.ValidationFlags != DefaultSettings.ValidationFlags) writeToLog("ValidationFlags settings odd");
        }

        private static void writeToLog(string s)
        {
            Console.WriteLine("{0}", s);
        }

        public override void Load(TextReader reader)
        {
            if (reader is IXmlLineInfo)
            {
                LineTracker = (IXmlLineInfo)reader;
            }
            base.Load(reader);
            Normalize();
        }

        public override string ToString()
        {
            return InfoString ?? base.ToString();
        }
        private Stream LineInfoReader;
        public IXmlLineInfo LineTracker;
        public string InfoString;
        private bool PresrveWhitespace = false;
        public bool MustSpaceWildcards = false;
        public bool MustSpaceAttributeValues = false;
        private string currentNodeType;
        private string prevNodeType;
        private string currentAttributeName;

        public XmlDocumentLineInfo(Stream lineInfoReader, string toString)
        {
            InfoString = toString;
            LineInfoReader = lineInfoReader;
        }
        public XmlDocumentLineInfo(string toString, bool presrveWhite)
        {
            PresrveWhitespace = true;
            InfoString = toString;
        }

        public XmlDocumentLineInfo(IXmlLineInfo lineInfoReader, string toString)
        {
            InfoString = toString;
            LineTracker = lineInfoReader;
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
            CheckReader(reader.Settings);
            if (reader is IXmlLineInfo)
            {
                LineTracker = (IXmlLineInfo)reader;
            }
            try
            {
                return base.ReadNode(reader);
            }
            catch (Exception e)
            {

                writeToLog("ERROR: " + e);
                throw e;
            }
        }

        public override void LoadXml(string xml)
        {
            base.Load(new XmlTextReader(new StringReader(xml)));
            Normalize();
        }

        public override XmlText CreateTextNode(string text)
        {
            XmlTextLineInfo node;
            string clean = text;
            if (MustSpaceWildcards)
            {
                clean = AIMLLoader.CleanWildcards(text);
            }
            else
            {
                clean = AIMLLoader.CleanWhitepaces(text);
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
                AIMLLineInfo nodeL = node as AIMLLineInfo;
                if (!SuspendLineInfo && nodeL != null)
                    nodeL.SetLineInfo(LineTracker.LineNumber, LineTracker.LinePosition);
            }
            if (node is AIMLXmlInfo) (node as AIMLXmlInfo).ReadOnly = true;
        }


        static public void SuggestLineNo(IXmlLineInfo lie, AIMLLineInfo target)
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
            var prev = currentAttributeName;
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
            return base.CreateWhitespace(text);
        }
        public override XmlElement CreateElement(string prefix, string localName, string namespaceURI)
        {
            LineInfoElement elem;
            try
            {

                switch (localName)
                {
                    case "pattern":
                    case "that":
                        MustSpaceWildcards = true;
                        break;
                    case "category":
                    case "template":
                        MustSpaceWildcards = false;
                        break;
                    default:
                        break;
                }

                prevNodeType = currentNodeType;
                currentNodeType = localName;
                elem = new LineInfoElement(prefix, localName, namespaceURI, this);
            }
            finally
            {
                // currentNodeType = prev;
            }
            setLineInfo(elem);
            return elem;
        }

        public static XmlReader CreateXmlTextReader(Stream stream)
        {
            return XmlTextReader.Create(stream, DefaultSettings);
        }
        internal static XmlReader CreateXmlTextReader(TextReader tr)
        {
            XmlTextReader xmlTextReader = new XmlTextReader(tr);
            return XmlTextReader.Create(tr, DefaultSettings);
        }


        /***
         
         This document already has a 'DocumentElement' node.
          at System.Xml.XmlDocument.IsValidChildType(XmlNodeType type)
          at System.Xml.XmlDocument.AppendChildForLoad(XmlNode newChild, XmlDocument doc)
          at System.Xml.XmlLoader.LoadDocSequence(XmlDocument parentDoc)
          at System.Xml.XmlLoader.Load(XmlDocument doc, XmlReader reader, Boolean preserveWhitespace)
          at System.Xml.XmlDocument.Load(XmlReader reader)
                 
         */

        public static XmlReaderSettings DefaultSettings
        {
            get
            {
                XmlReaderSettings settings = new XmlReaderSettings();
                settings.ConformanceLevel = ConformanceLevel.Fragment;
                settings.ValidationType = ValidationType.None;
                settings.IgnoreWhitespace = true;
                settings.IgnoreComments = true;
                settings.ProhibitDtd = true;
                settings.CheckCharacters = false;
                settings.ValidationFlags = XmlSchemaValidationFlags.AllowXmlAttributes | XmlSchemaValidationFlags.ReportValidationWarnings | XmlSchemaValidationFlags.ProcessIdentityConstraints;
                return settings;
            }
        }

        public bool SuspendLineInfo;
    }

    public class XmlAttributeLineInfo : XmlAttribute, AIMLLineInfo
    {
        public override XmlNode CloneNode(bool deep)
        {
            var v = new XmlAttributeLineInfo(base.Prefix, base.LocalName, base.NamespaceURI,
                                             (XmlDocumentLineInfo) OwnerDocument);
            v.CloneOf = CloneOf ?? this;
            v.SetLineInfo(LineNumber, LinePosition);
            v.Value = Value;
            v.ReadOnly = ReadOnly;
            return v;
        }
        public override bool IsReadOnly
        {
            get
            {
                if (!OwnerDocument.IsReadOnly) return false;
                if (!base.IsReadOnly)
                {
                    if (CloneOf == null) return true;
                    return false;
                }
                return true;
            }
        }
        public bool ReadOnly { get; set; }
        public override string Value
        {
            get
            {
                return base.Value;
            }
            set
            {
                value = AIMLLoader.CleanWildcards(value);
                base.Value = value;
            }
        }
        public XmlAttributeLineInfo(string prefix, string name, string uri, XmlDocumentLineInfo doc)
            : base(prefix, name, uri, doc)
        {

        }

        public override string ToString()
        {
            return AIMLLoader.TextAndSourceInfo(this);
        }
        public int lineNumber = 0;
        public int linePosition = 0;
        public long charPos = 0;
        public LineInfoElement lParent;
        public XmlNode CloneOf { get; set; }

        public void SetLineInfo(int linenum, int linepos)
        {
            lineNumber = linenum;
            linePosition = linepos;
        }
        public int LineNumber
        {
            get
            {
                return lineNumber;
            }
        }
        public int LinePosition
        {
            get
            {
                return linePosition;
            }
        }
        public bool HasLineInfo()
        {
            return true;
        }

        public void SetPos(long position)
        {
            charPos = position;
        }


        internal void SetParentFromNode(XmlNode xmlNode)
        {

            var pn = xmlNode.ParentNode;
            if (pn is LineInfoElement)
            {
                lParent = (LineInfoElement)pn;
            }
            if (!(xmlNode is LineInfoElement))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElement)
            {
                LineInfoElement lie = (LineInfoElement)xmlNode;
                lineNumber = lie.LineNumber;
                linePosition = lie.linePosition;
                charPos = lie.charPos;
            }
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent;
            }
        }
    }

    public interface AIMLLineInfo : IXmlLineInfo, AIMLXmlInfo
    {
        void SetLineInfo(int number, int position);
        void SetPos(long pos);
    }

    public interface AIMLXmlInfo
    {
        bool ReadOnly { get; set; }
    }

    //#define HOLD_PARENT
    public class XmlTextLineInfo : XmlText, AIMLLineInfo
    {
        public bool ReadOnly { get; set; }
        public override bool IsReadOnly
        {
            get
            {
                if (!OwnerDocument.IsReadOnly) return false;
                if (!base.IsReadOnly)
                {
                    if (base.ParentNode==null) return false;
                    if (CloneOf == null)
                    {
                        return true;
                    }
                    return false;
                }
                return true;
            }
        }
        public XmlNode CloneOf { get; set; }
        public override XmlNode CloneNode(bool deep)
        {
            var v = new XmlTextLineInfo(base.Data, (XmlDocumentLineInfo)OwnerDocument);
            v.SetLineInfo(LineNumber, LinePosition);
            v.ReadOnly = ReadOnly;
            v.CloneOf = CloneOf ?? this;
            return v;
        }

        public override string InnerXml
        {
            get
            {
                return base.InnerXml;
            }
            set
            {
                base.InnerText = value;
            }
        }
        public XmlTextLineInfo(string text, XmlDocumentLineInfo info)
            : base(text, info)
        {
        }
        public override string ToString()
        {
            return AIMLLoader.TextAndSourceInfo(this);
        }
        public int lineNumber = 0;
        public int linePosition = 0;
        public long charPos = 0;
        public LineInfoElement lParent;
        public void SetLineInfo(int linenum, int linepos)
        {
            lineNumber = linenum;
            linePosition = linepos;
        }
        public int LineNumber
        {
            get
            {
                return lineNumber;
            }
        }
        public int LinePosition
        {
            get
            {
                return linePosition;
            }
        }
        public bool HasLineInfo()
        {
            return true;
        }

        public void SetPos(long position)
        {
            charPos = position;
        }


        internal void SetParentFromNode(XmlNode xmlNode)
        {

            var pn = xmlNode.ParentNode;
            if (pn is LineInfoElement)
            {
                lParent = (LineInfoElement)pn;
            }
            if (!(xmlNode is LineInfoElement))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElement)
            {
                LineInfoElement lie = (LineInfoElement)xmlNode;
                lineNumber = lie.LineNumber;
                linePosition = lie.linePosition;
                charPos = lie.charPos;
            }
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent;
            }
        }
    }


    public class LineInfoElement : XmlElement, AIMLLineInfo
    {
        internal LineInfoElement(string prefix, string localname, string nsURI, XmlDocument doc)
            : base(prefix, localname, nsURI, doc)
        {
            XmlDocumentLineInfo.CheckNode(this);
            //((XmlDocumentLineInfo)doc).IncrementElementCount();
        }

        public override void Normalize()
        {
            if (OwnerDocument.IsReadOnly) OwnerDocument.Normalize();
            else
            {
                base.Normalize();
            }
        }

        public override string ToString()
        {
            return AIMLLoader.TextAndSourceInfo(this);
        }
        public int lineNumber = 0;
        public int linePosition = 0;
        public long charPos = 0;
        public LineInfoElement lParent;
        public void SetLineInfo(int linenum, int linepos)
        {
            if (linenum < lineNumber)
            {
                writeToLog("too small");
                return;
            } 

            lineNumber = linenum;
            linePosition = linepos;
        }
        public int LineNumber
        {
            get
            {
                return lineNumber;
            }
        }
        public int LinePosition
        {
            get
            {
                return linePosition;
            }
        }
        public bool HasLineInfo()
        {
            return true;
        }

        public void SetPos(long position)
        {
            charPos = position;
        }


        internal void SetParentFromNode(XmlNode xmlNode)
        {

            var pn = xmlNode.ParentNode;
            if (pn is LineInfoElement)
            {
                lParent = (LineInfoElement)pn;
                XmlDocumentLineInfo.SuggestLineNo(lParent,this);
            }
            if (!(xmlNode is LineInfoElement))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElement)
            {
                LineInfoElement lie = (LineInfoElement)xmlNode;
                XmlDocumentLineInfo.SuggestLineNo(lie, this);
            }
        }
        
        internal void SetParent(LineInfoElement pn)
        {
            lParent = (LineInfoElement) pn;
        }


        public int IndexInParent
        {
            get
            {
                XmlNode sib = lParent;
                if (sib == null)
                {
                    return IndexInBaseParent;
                }
                int idx = 0;
                sib = sib.FirstChild;
                while (sib != null)
                {
                    if (ReferenceEquals(sib, this)) return idx;
                    sib = sib.NextSibling;
                    idx++;
                }
                return IndexInBaseParent;
            }
        }


        public int IndexInBaseParent
        {
            get
            {
                return IndexIn(base.ParentNode);
            }
        }

        public int IndexIn(XmlNode parent)
        {
            if (parent == null) return -2;
            int idx = 0;
            XmlNode sib = parent.FirstChild;
            while (sib != null)
            {
                if (ReferenceEquals(sib, this)) return idx;
                sib = sib.NextSibling;
                idx++;
            }
            return -1;
        }

        public override XmlNode CloneNode(bool deep)
        {
            XmlDocumentLineInfo od = (XmlDocumentLineInfo) OwnerDocument;
            //od.Normalize();
            var newnode = new LineInfoElement(Prefix, LocalName, NamespaceURI, od);
            newnode.CloneOf = CloneOf ?? this;
            newnode.SetLineInfo(LineNumber, LinePosition);
            if (deep)
            {
                bool newnodeWas = newnode.protect;
                newnode.protect = false;
                if (HasChildNodes)
                    foreach (XmlNode a in ChildNodes)
                    {
                        try
                        {
                            var a2 = a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly;
                            var a22 = a2 as AIMLXmlInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.AppendChild(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            writeToLog("newnode.AppendChild " + e);
                        }
                    }
                var ats = Attributes;
                if (ats != null)
                    foreach (XmlAttribute a in ats)
                    {
                        try
                        {
                            if (XmlDocumentLineInfo.SkipXmlns && a.Name == "xmlns") continue;
                            var a2 = (XmlAttribute)a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly;
                            var a22 = a2 as AIMLXmlInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.Attributes.Append(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            newnode.writeToLog("newnode.AppendChild " + e);
                        }
                    }
                newnode.protect = newnodeWas;
            }
            else
            {
                newnode.protect = false;
            }
            return newnode;
        }

        public XmlNode CloneOf
        {
            get;
            set;
        }

        public override XmlNode RemoveChild(XmlNode newChild)
        {
            try
            {
                return base.RemoveChild(newChild);
            }
            catch (Exception e)
            {
                writeToLog("newnode.AppendChild " + e);
                return null;
            }
        }

        public override XmlNode AppendChild(XmlNode newChild)
        {
            try
            {            
                return base.AppendChild(newChild);
            }
            catch (Exception e)
            {
                writeToLog("newnode.AppendChild " + e);
                return null;
            }
        }

        public static IXmlLineInfo ToLineInfoElement(XmlNode pattern)
        {
            if (pattern == null) return null;
            if (pattern is IXmlLineInfo)
            {
                return (IXmlLineInfo)pattern;
            }
            return null;// CopyNode(pattern, true);
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent;
            }
        }

        public virtual XmlNode ParentNodeBase
        {
            get
            {
                return base.ParentNode;
            }
        }

        public bool protect = true;


        public override string InnerXml
        {
            get
            {
                return base.InnerXml;
            }
            set
            {
                if (InnerXml == value) return;
                if (protect)
                {
                    writeToLog("InnerXml Should not be changed to " + value);
                }
                base.InnerXml = value;
            }
        }

        public override string InnerText
        {
            get
            {
                return base.InnerText;
            }
            set
            {
                if (protect)
                {
                    writeToLog("InnerText Should not be changed to " + value);
                }
                base.InnerText = value;
            }
        }

        private void writeToLog(string s)
        {
            RTPBot.writeDebugLine("ERROR " + s + " in " + this);
        }

        public override bool IsReadOnly
        {
            get
            {
                if (!OwnerDocument.IsReadOnly) return false;
                if (!base.IsReadOnly)
                {
                    if (!protect) return false;

                    if (CloneOf == null) return true;
                    return false;
                }
                return true;
            }
        }
        public bool ReadOnly
        {
            get
            {
                if (base.IsReadOnly)
                {
                    if (!protect)
                    {
                        protect = IsReadOnly;
                    }
                }
                return protect;
            }
            set
            {
                protect = value;

                foreach (object node in ChildNodes)
                {
                    if (node is AIMLXmlInfo)
                    {
                        ((AIMLXmlInfo)node).ReadOnly = value;
                    }
                    else
                    {
                        writeToLog("Child is not a AIMLXmlInfo: " + node);
                    }
                }
                foreach (object node in Attributes)
                {
                    if (node is AIMLXmlInfo)
                    {
                        ((AIMLXmlInfo)node).ReadOnly = value;
                    }
                    else
                    {
                        writeToLog("Attribute is not a AIMLXmlInfo: " + node);
                    }
                }
            }

        }
    } // End LineInfoElement class.

}
