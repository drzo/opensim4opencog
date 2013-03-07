using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using MushDLR223.ScriptEngines;

namespace AltAIMLbot.Utils
{
    public interface AIMLLoader
    {
        string generateCPath(string graphName, string pattern, string that, string flag, string topicName,
                             string stateNamePre,
                             string stateNamePost, bool isUserInput, Func<Unifiable, bool, Unifiable> innerFormater,
                             AltBot bot);

        Dictionary<XmlNode, StringBuilder> DumpErrors(OutputDelegate debugWriteLine, bool clearAfterDump);
        long loadAIMLURI(string path);
        long loadAIMLNode(XmlNode documentElement);
        AltBot bot { get; }
        LoaderOptions loadOpts { get; }
        long loadAIMLFromXML(XmlNode newAiml, string filename);

        string generatePath(string graphName, string pattern, string that, string topicName,
                            string stateNamePre,
                            string stateNamePost, bool isUserInput);

        long processCategory(XmlNode templateNode, string currentFilename);
        long loadAIMLString(string aimlSrc);

    }


}
