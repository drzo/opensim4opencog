using System;
using System.Xml;

namespace RTParser.Utils
{
    public class GraphLinkInfo 
    {
        readonly protected XmlNode srcNode;
        public static bool NoInfo = false;

        protected GraphLinkInfo(XmlNode template)
        {
            srcNode = template;

            if (XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null) this.srcNode.Attributes.RemoveNamedItem("xmlns");

            if (NoInfo)
            {
                throw new InvalidOperationException("now Inof");
            }

        }
        public override string ToString()
        {
            if (XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null) this.srcNode.Attributes.RemoveNamedItem("xmlns");
            return srcNode.OuterXml + " " + AIMLLoader.LocationEscapedInfo(srcNode);
        }
        public string InnerXml
        {
            get
            {
                if (XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null) this.srcNode.Attributes.RemoveNamedItem("xmlns"); 
                return srcNode.InnerXml;
            }
        }

        public string OuterXml
        {
            get
            {
                if (XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null) this.srcNode.Attributes.RemoveNamedItem("xmlns");
                return srcNode.OuterXml;
            }
        }
    }
}