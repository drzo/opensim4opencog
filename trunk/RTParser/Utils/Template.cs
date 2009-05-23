using System.Xml;

namespace RTParser.Utils
{
    public class Template
    {
        public string Output;
        public XmlNode Guard;

        public Template(string template, XmlNode guard)
        {
            Output = template;
            Guard = guard;
        }
    }
}