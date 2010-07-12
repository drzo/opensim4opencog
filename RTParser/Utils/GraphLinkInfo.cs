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

            if (NoInfo)
            {
                throw new InvalidOperationException("now Inof");
            }

        }
        public override string ToString()
        {
            return srcNode.OuterXml + " " + AIMLLoader.LineNumberInfo(srcNode);
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