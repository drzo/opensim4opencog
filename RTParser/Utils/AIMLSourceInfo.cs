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
            }
            catch (Exception e)
            {
                writeToLog("ERROR " + e);
            }
        }

        public override XmlDocumentFragment CreateDocumentFragment()
        {
            return base.CreateDocumentFragment();
        }

        public override XmlDocumentType CreateDocumentType(string name, string publicId, string systemId, string internalSubset)
        {
            return base.CreateDocumentType(name, publicId, systemId, internalSubset);
        }

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
            return base.CreateXmlDeclaration(version, encoding, standalone);
        }
        public override void Load(XmlReader reader)
        {
            SetupReader(reader.Settings);
            if (reader is IXmlLineInfo)
            {
                LineTracker = (IXmlLineInfo)reader;
            }
            if (!reader.EOF)
            {
                try
                {
                    base.Load(reader);
                }
                catch (Exception e)
                {
                    writeToLog("ERROR " + e);
                    throw e;
                }
            }
        }

        static void SetupReader(XmlReaderSettings settings)
        {
            if (settings == null) return;
            if (settings.ConformanceLevel != DefaultSettings.ConformanceLevel) writeToLog("ConformanceLevel settings odd");
            if (settings.ValidationType != DefaultSettings.ValidationType) writeToLog("ValidationType settings odd");
            if (settings.IgnoreWhitespace != DefaultSettings.IgnoreWhitespace) writeToLog("whitespace settings odd");
            if (settings.IgnoreComments != DefaultSettings.IgnoreComments) writeToLog("IgnoreComments settings odd");
            if (settings.CheckCharacters != DefaultSettings.CheckCharacters) writeToLog("CheckCharacters settings odd");
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
            SetupReader(reader.Settings);
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

        public void setLineInfo(AIMLLineInfo node)
        {
            if (LineTracker != null)
            {
                node.SetLineInfo(LineTracker.LineNumber, LineTracker.LinePosition);
            }
        }

        public override XmlAttribute CreateAttribute(string prefix, string localName, string namespaceURI)
        {
            XmlAttributeLineInfo elem;
            var prev = currentAttributeName;
            try
            {
                currentAttributeName = localName;
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
                settings.CheckCharacters = false;
                settings.ValidationFlags = XmlSchemaValidationFlags.AllowXmlAttributes | XmlSchemaValidationFlags.ReportValidationWarnings | XmlSchemaValidationFlags.ProcessIdentityConstraints;
                return settings;
            }
        }

    }

    public class XmlAttributeLineInfo : XmlAttribute, AIMLLineInfo
    {
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

    public interface AIMLLineInfo : IXmlLineInfo
    {
        void SetLineInfo(int number, int position);
    }

    //#define HOLD_PARENT
    public class XmlTextLineInfo : XmlText, AIMLLineInfo
    {
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
            : base(prefix, localname.ToLower(), nsURI, doc)
        {
            //((XmlDocumentLineInfo)doc).IncrementElementCount();
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
    } // End LineInfoElement class.

}
