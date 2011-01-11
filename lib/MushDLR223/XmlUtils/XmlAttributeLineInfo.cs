using System;
using System.Xml;

namespace MushDLR223.Utilities
{
    public class XmlAttributeLineInfo : XmlAttribute, XmlSourceLineInfo
    {
        public void SetOwnerDocument(XmlDocumentLineInfo newDoc)
        {
            var prev = OwnerDocument as XmlDocumentLineInfo;
            if (prev != newDoc)
            {
                _docLineInfo = newDoc;
            }
        }

        private XmlDocumentLineInfo _docLineInfo
        {
            get { return null; }
            // ReSharper disable ValueParameterNotUsed
            set { }
            // ReSharper restore ValueParameterNotUsed
        }
        public XmlSourceLineInfo lParent
        {
            get { return base.ParentNode as XmlSourceLineInfo; }
            set { throw new NotImplementedException(); }
        }

        public XmlAttributeLineInfo(string prefix, string name, string uri, XmlDocumentLineInfo doc)
            : base(XmlDocumentLineInfo.Intern(prefix), XmlDocumentLineInfo.Intern(name), XmlDocumentLineInfo.Intern(uri), doc.FileDoc)
        {
            docLineInfo = doc;
        }

        internal XmlDocumentLineInfo docLineInfo
        {
            get { return _docLineInfo ?? OwnerDocument as XmlDocumentLineInfo; }
            set { _docLineInfo = value; }
        }

        public override XmlDocument OwnerDocument
        {
            get { return XmlDocumentLineInfo.DefaultDoc ?? _docLineInfo ?? base.OwnerDocument; }
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
                if (lParent == null)
                {
                    XmlNode pn = lParent as XmlNode;
                    if (pn != null) return pn;
                }
                return base.ParentNode;
            }
        }

        #region XmlSourceLineInfo Members

        public bool ReadOnly { get; set; }

        public void SetLineInfo(int linenum, int linepos)
        {
            if (lParent != null)
            {
                lParent.SetLineInfo(linenum, linepos);
            }
        }

        public int LineNumber
        {
            get { return StaticXMLUtils.ToLineInfo(this).LineNumber; }
        }

        public int LinePosition
        {
            get { return StaticXMLUtils.ToLineInfo(this).LinePosition; }
        }

        public bool HasLineInfo()
        {
            return false;
        }

        public void SetPos(long position)
        {
            if (lParent != null)
            {
                lParent.SetPos(position);
            }
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
                xmlNode = lParent as XmlNode;
            }
            if (xmlNode is LineInfoElementImpl)
            {
                LineInfoElementImpl lie = (LineInfoElementImpl) xmlNode;
            }
        }

        #endregion

        public override XmlNode CloneNode(bool deep)
        {
            XmlAttributeLineInfo v = new XmlAttributeLineInfo(base.Prefix, base.LocalName, base.NamespaceURI,
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
    }
}