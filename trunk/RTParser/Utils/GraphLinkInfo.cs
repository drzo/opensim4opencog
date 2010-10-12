using System;
using System.Xml;
using MushDLR223.Utilities;

namespace RTParser.Utils
{
    public abstract class GraphLinkInfo: StaticAIMLUtils
    {
        public static bool NoInfo;

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


        internal abstract XmlNode srcNode { get; set; }
        abstract public override string ToString();
        abstract public Unifiable FullPath { get; set; }

    }
}