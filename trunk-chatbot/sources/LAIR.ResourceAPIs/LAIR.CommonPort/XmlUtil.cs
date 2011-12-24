using System;
using LAIR.XML;

namespace LAIR.CommonPort
{
    public static class XmlUtil
    {
        public static string OuterXML(XmlParser frameP, string fes)
        {
            string found = frameP.OuterXML(fes);
            if (found != null) return found;
            return found;
        }
    }
}