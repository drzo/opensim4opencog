using System;
using System.Xml;

namespace MushDLR223.Utilities
{
    public interface XmlSourceLineInfo : IXmlLineInfo, XmlSourceInfo
    {
        void SetLineInfo(int number, int position);
        void SetPos(long pos);
    }

    public interface XmlSourceInfo
    {
        bool ReadOnly { get; set; }
    }

    public class LineInfoElementImpl : XmlElement, XmlSourceLineInfo
    {
        private static bool _whenReadOnly = true;
        private readonly XmlDocumentLineInfo docLineInfo;
        public long charPos;
        public int lineNumber;
        public int linePosition;
        public LineInfoElementImpl lParent;
        public bool protect = true;

        internal LineInfoElementImpl(string prefix, string localname, string nsURI, XmlDocument doc)
            : base(prefix, localname, nsURI, doc)
        {
            XmlDocumentLineInfo.CheckNode(this);
            docLineInfo = doc as XmlDocumentLineInfo;
            //((XmlDocumentLineInfo)doc).IncrementElementCount();
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
            get { return IndexIn(base.ParentNode); }
        }

        public XmlNode CloneOf { get; set; }

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
            get { return base.ParentNode; }
        }

        public override string InnerXml
        {
            get { return base.InnerXml; }
            set
            {
                if (InnerXml == value) return;
                if (protect)
                {
                    writeToLog("WARNING: InnerXml Should not be changed to \"" + value + "\"");
                }
                base.InnerXml = value;
            }
        }

        public override string InnerText
        {
            get { return base.InnerText; }
            set
            {
                if (protect)
                {
                    writeToLog("WARNING: InnerText Should not be changed to \"" + value + "\"");
                }
                base.InnerText = value;
            }
        }

        public override bool IsReadOnly
        {
            get
            {
                if (!OwnerDocument.IsReadOnly) return false;
                if (!base.IsReadOnly)
                {
                    if (!protect) return false;

                    if (CloneOf == null)
                    {
                        return _whenReadOnly;
                    }
                    return false;
                }
                return _whenReadOnly;
            }
        }

        #region XmlSourceLineInfo Members

        public void SetLineInfo(int linenum, int linepos)
        {
            if (linenum < lineNumber)
            {
                writeToLog("Line number too small");
                return;
            }

            lineNumber = linenum;
            linePosition = linepos;
        }

        public int LineNumber
        {
            get { return lineNumber; }
        }

        public int LinePosition
        {
            get { return linePosition; }
        }

        public bool HasLineInfo()
        {
            return true;
        }

        public void SetPos(long position)
        {
            charPos = position;
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
                bool vitalChange = false;
                if (protect != IsReadOnly)
                {
                    vitalChange = true;
                }
                if (protect != value)
                {
                    protect = value;
                    if (!vitalChange)
                    {
                        vitalChange = true;
                    }
                }
                foreach (XmlNode node in ChildNodes)
                {
                    if (node is XmlSourceInfo)
                    {
                        ((XmlSourceInfo) node).ReadOnly = value;
                    }
                    else
                    {
                        if (!node.IsReadOnly && !value)
                        {
                            writeToLog("Non Readonly Child is not a AIMLXmlInfo: " + node);
                        }
                        if (vitalChange)
                        {
                            writeToLog("ERROR: Child is not a AIMLXmlInfo: " + node);
                        }
                        else
                        {
                            writeToLog("WARNING: Child is not a AIMLXmlInfo: " + node);
                        }
                    }
                }
                foreach (object node in Attributes)
                {
                    if (node is XmlSourceInfo)
                    {
                        ((XmlSourceInfo) node).ReadOnly = value;
                    }
                    else
                    {
                        writeToLog("Attribute is not a AIMLXmlInfo: " + node);
                    }
                }
            }
        }

        #endregion

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
            return docLineInfo.TextAndSourceInfo(this);
        }


        public void SetParentFromNode(XmlNode xmlNode)
        {
            XmlNode pn = xmlNode.ParentNode;
            if (pn is LineInfoElementImpl)
            {
                lParent = (LineInfoElementImpl) pn;
                XmlDocumentLineInfo.SuggestLineNo(lParent, this);
            }
            if (!(xmlNode is LineInfoElementImpl))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElementImpl)
            {
                var lie = (LineInfoElementImpl) xmlNode;
                XmlDocumentLineInfo.SuggestLineNo(lie, this);
            }
        }

        internal void SetParent(LineInfoElementImpl pn)
        {
            lParent = pn;
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
            var od = (XmlDocumentLineInfo) OwnerDocument;
            //od.Normalize();
            var newnode = new LineInfoElementImpl(Prefix, LocalName, NamespaceURI, od);
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
                            XmlNode a2 = a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly;
                            var a22 = a2 as XmlSourceInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.AppendChild(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            writeToLog("ERROR: newnode.AppendChild " + e);
                        }
                    }
                XmlAttributeCollection ats = Attributes;
                if (ats != null)
                    foreach (XmlAttribute a in ats)
                    {
                        try
                        {
                            if (XmlDocumentLineInfo.SkipXmlns && a.Name == "xmlns") continue;
                            var a2 = (XmlAttribute) a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly;
                            var a22 = a2 as XmlSourceInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.Attributes.Append(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            newnode.writeToLog("ERROR: newnode.AppendChild " + e);
                        }
                    }
                newnode.protect = newnodeWas;
            }
            else
            {
                XmlAttributeCollection ats = Attributes;
                if (ats != null)
                    foreach (XmlAttribute a in ats)
                    {
                        try
                        {
                            if (XmlDocumentLineInfo.SkipXmlns && a.Name == "xmlns") continue;
                            var a2 = (XmlAttribute) a.CloneNode(deep);
                            bool a2ro = a2.IsReadOnly && false;
                            var a22 = a2 as XmlSourceInfo;
                            if (a22 != null) a22.ReadOnly = false;
                            newnode.Attributes.Append(a2);
                            if (a22 != null) a22.ReadOnly = a2ro;
                        }
                        catch (Exception e)
                        {
                            newnode.writeToLog("ERROR: newnode.AppendChild " + e);
                        }
                    }
                newnode.protect = false;
            }
            return newnode;
        }

        public override XmlNode RemoveChild(XmlNode newChild)
        {
            bool before = _whenReadOnly;
            try
            {
                if (ReadOnly)
                {
                    writeToLog("ERROR RemoveChild on ReadOnly ");
                }
                _whenReadOnly = false;
                return base.RemoveChild(newChild);
            }
            catch (Exception e)
            {
                writeToLog("ERROR: newnode.AppendChild " + e);
                return null;
            }
            finally
            {
                _whenReadOnly = before;
            }
        }

        public override XmlNode AppendChild(XmlNode newChild)
        {
            try
            {
                if (newChild.OwnerDocument != OwnerDocument)
                {
                    if (newChild.NodeType == XmlNodeType.Text)
                    {
                        newChild = OwnerDocument.CreateTextNode(newChild.InnerText);
                    }
                }
                return base.AppendChild(newChild);
            }
            catch (Exception e)
            {
                writeToLog("ERROR: newnode.AppendChild " + e);
                return null;
            }
        }

        public static IXmlLineInfo ToLineInfoElement(XmlNode pattern)
        {
            if (pattern == null) return null;
            if (pattern is IXmlLineInfo)
            {
                return (IXmlLineInfo) pattern;
            }
            return null; // CopyNode(pattern, true);
        }

        internal void writeToLog(string s)
        {
            XmlDocumentLineInfo.DebugWriteLine(s + " on XML node: '" + this + "'");
        }

        public static void unsetReadonly(XmlSourceLineInfo node)
        {
            if (node.ReadOnly)
            {
                node.ReadOnly = false;
            }
        }

        public static void unsetReadonly(LineInfoElementImpl node)
        {
            if (node.ReadOnly)
            {
                node.ReadOnly = false;
            }
        }

        public static void unsetReadonly(XmlNode node)
        {
            var lie = node as XmlSourceLineInfo;
            if (node.IsReadOnly)
            {
                unsetReadonly(lie);
            }
        }

        public static void notReadonly(XmlNode node)
        {
            var lie = node as XmlSourceLineInfo;
            if (node.IsReadOnly)
            {
                unsetReadonly(lie);
            }
        }

        public static void chopParent(XmlNode chilz)
        {
            XmlNode p = chilz.ParentNode;
            p.RemoveChild(chilz);
        }

        public static void SetReadOnly(XmlNode node)
        {
            var lie = node as XmlSourceLineInfo;
            if (node.IsReadOnly)
            {
                lie.ReadOnly = true;
            }
        }

        public static void SetParentFromNode(XmlNode newLineInfoPattern, XmlNode patternNode)
        {
            ((LineInfoElementImpl) newLineInfoPattern).SetParentFromNode(patternNode);
        }
    }

    // End LineInfoElement class.
}