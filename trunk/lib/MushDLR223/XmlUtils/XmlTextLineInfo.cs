using System.IO;
using System.Xml;
using System.Xml.XPath;

namespace MushDLR223.Utilities
{
    public class XmlTextLineInfo : XmlText, XmlSourceLineInfo
    {
        private XmlDocumentLineInfo _docLineInfo;

        public long charPos;

        public int lineNumber;
        public int linePosition;
        public LineInfoElementImpl lParent;

        public XmlTextLineInfo(string text, XmlDocumentLineInfo info)
            : base(XmlDocumentLineInfo.Intern(text), info)
        {
            _docLineInfo = info;
        }

        internal XmlDocumentLineInfo docLineInfo
        {
            get { return _docLineInfo ?? OwnerDocument as XmlDocumentLineInfo; }
            set { _docLineInfo = value; }
        }

        public override XmlDocument OwnerDocument
        {
            get { return _docLineInfo ?? base.OwnerDocument; }
        }

        public override bool IsReadOnly
        {
            get
            {
                if (!OwnerDocument.IsReadOnly) return false;
                if (!base.IsReadOnly)
                {
                    if (base.ParentNode == null) return false;
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

        public override string InnerXml
        {
            get { return base.InnerXml; }
            set { base.InnerText = value; }
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent;
            }
        }

        #region XmlSourceLineInfo Members

        public bool ReadOnly { get; set; }

        public void SetLineInfo(int linenum, int linepos)
        {
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

        public void SetParentFromNode(XmlNode xmlNode)
        {
            XmlNode pn = xmlNode.ParentNode;
            if (pn is LineInfoElementImpl)
            {
                lParent = (LineInfoElementImpl) pn;
            }
            if (!(xmlNode is LineInfoElementImpl))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElementImpl)
            {
                LineInfoElementImpl lie = (LineInfoElementImpl) xmlNode;
                lineNumber = lie.LineNumber;
                linePosition = lie.LinePosition;
                charPos = lie.charPos;
            }
        }

        #endregion

        public override XmlNode CloneNode(bool deep)
        {
            XmlTextLineInfo v = new XmlTextLineInfo(base.Data, (XmlDocumentLineInfo) OwnerDocument);
            v.SetLineInfo(LineNumber, LinePosition);
            v.ReadOnly = ReadOnly;
            v.CloneOf = CloneOf ?? this;
            return v;
        }

        public override string ToString()
        {
            if (docLineInfo == null) return StaticXMLUtils.TextAndSourceInfo(this);
            return docLineInfo.TextAndSourceInfo(this);
        }
    }
}