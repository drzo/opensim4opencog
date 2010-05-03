using System;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class OutputInfo : GraphLinkInfo
    {

        public OutputInfo(XmlNode template):base(template)
        {
        }

        virtual public XmlNode Output
        {
            get { return srcNode; }
        }


    }
}