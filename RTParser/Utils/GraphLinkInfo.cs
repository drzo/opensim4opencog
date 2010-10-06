using System;
using System.Xml;
using MushDLR223.Utilities;

namespace RTParser.Utils
{
    public abstract class GraphLinkInfo
    {
        public static bool NoInfo;
        internal XmlNode srcNode;
        public Unifiable FullPath { get; set; }

        protected GraphLinkInfo(XmlNode template)
        {
            srcNode = template;

            if (XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null)
                this.srcNode.Attributes.RemoveNamedItem("xmlns");

            if (NoInfo)
            {
                throw new InvalidOperationException("now Inof");
            }
        }

        public string InnerXml
        {
            get
            {
                if (!XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null)
                    this.srcNode.Attributes.RemoveNamedItem("xmlns");
                return srcNode.InnerXml;
            }
        }

        public string OuterXml
        {
            get
            {
                if (!XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null)
                    this.srcNode.Attributes.RemoveNamedItem("xmlns");
                return srcNode.OuterXml;
            }
        }

        public override string ToString()
        {
            if (!XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null)
                this.srcNode.Attributes.RemoveNamedItem("xmlns");
            return srcNode.OuterXml + " " + StaticXMLUtils.LocationEscapedInfo(srcNode);
        }
    }
}