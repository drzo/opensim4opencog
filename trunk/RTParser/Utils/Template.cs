using System.Xml;

namespace RTParser.Utils
{
    public class Template
    {
        public XmlNode Output;
        public XmlNode Guard;

        public Template(XmlNode template, XmlNode guard)
        {
            Output = template;
            Guard = guard;
        }
    }
}