using System.Collections.Generic;
using System.Xml;
using MushDLR223.ScriptEngines;

namespace RTParser.Utils
{
    public delegate IEnumerable<XmlNode> XmlNodeEval(XmlNode src, Request request, OutputDelegate outputDelegate);
}