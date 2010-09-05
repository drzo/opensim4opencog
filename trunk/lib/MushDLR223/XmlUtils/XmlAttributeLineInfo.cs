using System.Xml;

namespace MushDLR223.Utilities
{
    public class XmlAttributeLineInfo : XmlAttribute, XmlSourceLineInfo
    {
        private readonly XmlDocumentLineInfo docLineInfo;
        public long charPos;
        public int lineNumber;
        public int linePosition;
        public LineInfoElementImpl lParent;

        public XmlAttributeLineInfo(string prefix, string name, string uri, XmlDocumentLineInfo doc)
            : base(prefix, name, uri, doc)
        {
            docLineInfo = doc;
        }

        public override XmlDocument OwnerDocument
        {
            get { return docLineInfo ?? base.OwnerDocument; }
        }

        public override bool IsReadOnly
        {
            get
            {
                if (!OwnerDocument.IsReadOnly) return false;
                if (!base.IsReadOnly)
                {
                    if (!ReadOnly)
                    {
                        return false;
                    }
                    if (CloneOf == null) return true;
                    return false;
                }
                return ReadOnly;
            }
        }

        public override string Value
        {
            get { return base.Value; }
            set
            {
                value = docLineInfo.FormatTextNode(value);
                bool wasReadOnly = ReadOnly;
                try
                {
                    if (IsReadOnly)
                    {
                        if (ReadOnly)
                        {
                            ReadOnly = false;
                        }
                    }
                    base.Value = value;
                }
                finally
                {
                    ReadOnly = wasReadOnly;
                }
            }
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
            var v = new XmlAttributeLineInfo(base.Prefix, base.LocalName, base.NamespaceURI,
                                             (XmlDocumentLineInfo) OwnerDocument);
            v.CloneOf = CloneOf ?? this;
            v.SetLineInfo(LineNumber, LinePosition);
            v.Value = Value;
            v.ReadOnly = ReadOnly;
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
                linePosition = lie.linePosition;
                charPos = lie.charPos;
            }
        }
    }
}