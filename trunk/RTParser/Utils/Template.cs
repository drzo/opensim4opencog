using System.Xml;

namespace RTParser.Utils
{
    public class Template
    {
        readonly private XmlNode _output;
        public XmlNode Guard;
        public Node Node;

        public Template(XmlNode template, XmlNode guard, Node node)
        {
            _output = template;
            Guard = guard;
            Node = node;
        }

        public XmlNode Output
        {
            get { return _output; }
        }
    }
}