using System;
using System.Collections.Generic;
using LAIR.XML;

namespace LAIR.CommonPort
{
    public class CommonXmlParser //: XmlParser
    {
        private XmlParser innerParser;
        public CommonXmlParser(string roleSetXml)//:base(roleSetXml)
        {
            innerParser = new XmlParser(roleSetXml);
        }

        public string AttributeValue(string roleset, string p1)
        {
            return innerParser.AttributeValue(roleset, p1);
        }

        public string OuterXML(string roleset)
        {
            return innerParser.OuterXML(roleset);
        }

        public bool SkipToElement(string layer, Dictionary<string, string> targetXmlConstraints)
        {
            return innerParser.SkipToElement(layer, targetXmlConstraints);
        }

        public bool SkipToElement(string param1)
        {
            return innerParser.SkipToElement(param1);
        }
        public string ElementText(string text)
        {
            return innerParser.ElementText(text);
        }
    }
}