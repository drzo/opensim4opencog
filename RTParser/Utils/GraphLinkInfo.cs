using System;
using System.Xml;

namespace RTParser.Utils
{
    public class GraphLinkInfo 
    {
        readonly protected XmlNode srcNode;

        protected GraphLinkInfo(XmlNode template)
        {
            srcNode = template;
        }
        public override string ToString()
        {
            return srcNode.OuterXml;
        }
        public string InnerXml
        {
            get
            {
                return srcNode.InnerXml;
            }
        }

        public string OuterXml
        {
            get
            {
                return srcNode.OuterXml;
            }
        }
    }
}