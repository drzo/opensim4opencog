using System.Xml;

namespace MushDLR223.Utilities
{
    public class XmlTextLineInfo : XmlText, XmlSourceLineInfo
    {
        private readonly XmlDocumentLineInfo docLineInfo;
        public long charPos;

        public int lineNumber;
        public int linePosition;
        public LineInfoElementImpl lParent;

        public XmlTextLineInfo(string text, XmlDocumentLineInfo info)
            : base(text, info)
        {
            docLineInfo = info;
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

        #endregion

        public override XmlNode CloneNode(bool deep)
        {
            var v = new XmlTextLineInfo(base.Data, (XmlDocumentLineInfo) OwnerDocument);
            v.SetLineInfo(LineNumber, LinePosition);
            v.ReadOnly = ReadOnly;
            v.CloneOf = CloneOf ?? this;
            return v;
        }

        public override string ToString()
        {
            return docLineInfo.TextAndSourceInfo(this);
        }


        internal void SetParentFromNode(XmlNode xmlNode)
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
                var lie = (LineInfoElementImpl) xmlNode;
                lineNumber = lie.LineNumber;
                linePosition = lie.LinePosition;
                charPos = lie.charPos;
            }
        }
    }
}