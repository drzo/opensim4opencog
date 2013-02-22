using System;
using System.Text;
using System.Xml;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace AltAIMLbot.AIMLTagHandlers
{
    internal class lazyClosure : AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public lazyClosure(AltBot bot,
                           User user,
                           SubQuery query,
                           Request request,
                           Result result,
                           XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        #region Overrides of TextTransformer

        /// <summary>
        /// The method that does the actual Processing of the text.
        /// </summary>
        /// <returns>The resulting Processed text</returns>
        protected override Unifiable ProcessChangeU()
        {
            if (templateNode.NodeType == XmlNodeType.Comment) return Unifiable.Empty;
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                string s = Trim(templateNode.InnerText);
                if (String.IsNullOrEmpty(s))
                {
                    return Unifiable.Empty;
                }
                return s;
            }
            string currentNodeName = templateNode.Name.ToLower();
            if (currentNodeName == "substitutions")
            {
                var prevDict = request.TargetSettings;
                // Process each of these child "settings"? nodes
                try
                {
                    request.TargetSettings = request.TargetBot.InputSubstitutions;
                    SettingsDictionaryReal.loadSettingNode(request.TargetSettings, templateNode, SettingsPolicy.Default, request);
                }
                finally
                {
                    request.TargetSettings = prevDict;
                }
                return ProcessSucceed();
            }
            if (currentNodeName == "genlmt")
            {
                string name = GetAttribValue(templateNode, "name,mt,to,super,into", null);
                string removeTo = GetAttribValue(templateNode, "remove", null);
                string from = GetAttribValue(templateNode, "graph,from", null);
                bool deleteLink = false;
                if (name == null)
                {
                    name = Trim(templateNode.InnerText);
                }
                if (removeTo != null)
                {
                    deleteLink = true;
                    name = removeTo;
                }
                GraphMaster FROM = request.GetGraph(from);
                GraphMaster TO = request.GetGraph(name);
                if (FROM != null && TO != null)
                {
                    if (deleteLink)
                    {
                        FROM.RemoveGenlMT(TO, writeToLog);
                    }
                    else
                    {
                        FROM.AddGenlMT(TO, writeToLog);
                    }
                    return Succeed("GENLMT: " + FROM + " => " + name + " => " + TO);
                }
            }
            if (currentNodeName == "sraigraph")
            {
                string name = GetAttribValue(templateNode, "name,mt,to,super,into", null);
                string from = GetAttribValue(templateNode, "graph,from", null);
                if (name == null)
                {
                    name = Trim(templateNode.InnerText);
                }
                GraphMaster FROM = request.GetGraph(from);
                GraphMaster TO = request.GetGraph(name);
                if (FROM != null && TO != null)
                {
                    FROM.Srai = name;
                    return Succeed("SRAI: " + FROM + " => " + name + " => right now " + TO);
                }
                return Failure("FROM '" + from + "'='" + FROM + "'" + " TO '" + name + "'='" + TO + "'");
            }
            if (currentNodeName == "meta")
            {
                return Succeed("UNUSED: " + templateNode.OuterXml);
            }
            if (currentNodeName == "#comment")
            {
                return Succeed("UNUSED: " + templateNode.OuterXml);

            }
            if (currentNodeName == "item")
            {
                SettingsDictionaryReal.loadSettingNode(request.TargetSettings, templateNode, SettingsPolicy.Default, request);
                return ProcessSucceed();

            }
            if (currentNodeName == "bot")
            {
                SettingsDictionaryReal.loadSettingNode(request.TargetBot.Settings, templateNode, SettingsPolicy.Default, request);
                return ProcessSucceed();
            }
            string currentNodeOuterXml = templateNode.OuterXml;
            if (currentNodeOuterXml.Length > 280) currentNodeOuterXml = TextFilter.ClipString(currentNodeOuterXml, 280);
            writeToLog("ImmediateAiml: " + currentNodeOuterXml);
            /*
               <TestCase name="connect">
                    <Input>CONNECT</Input>
                    <ExpectedAnswer>Connected to test case AIML set.</ExpectedAnswer>
               </TestCase>
            */

            if (templateNode.NodeType == XmlNodeType.Comment) return ProcessSucceed();

            // pull from late bound sustituion dictionaries
            var sd = request.GetSubstitutions(currentNodeName, false);
            if (sd != null)
            {
                if (FinalResultValid) return FinalResult;
                if (!Unifiable.IsIncomplete(FinalResult))
                {
                    return FinalResult;
                }
                Func<Unifiable, Unifiable> Format = (v) => ApplySubstitutions.Substitute(sd, templateNodeInnerText);
                if (base.isRecursive && !ReadOnly)
                {
                    FinalResult = Format(TransformAtomically(null, true));
                    return finalResult.Value;
                }
                return FinalResult = TransformAtomically(Format, false);
            }

            if (AltBot.UnknownTagsAreBotVars)
            {
                var v = Proc.GlobalSettings.grabSetting(currentNodeName);
                if (!Unifiable.IsIncomplete(v)) return v;
            }
            var vs = Proc.EvalAiml(templateNode, request, request.writeToLog);
            StringBuilder sb = new StringBuilder();
            int writeThrus = 0;
            int total = 0;
            OutputDelegate WriteLine = DLRConsole.SystemWriteLine;
            foreach (var node in vs)
            {
                total++;
                string nodeOuterXml = ToXmlValue(node);
                WriteLine(nodeOuterXml);
                string p = GetAttribValue(node,"PASSED","FALSE");
                if (p=="False")
                {
                    writeThrus++;
                    sb.Append("\n" + nodeOuterXml.Replace("\" ","\"\n ") + "\n");
                }
                WriteLine("");
            }

            WriteLine("");
            WriteLine("");
            WriteLine("" + writeThrus);
            WriteLine("");
            string ss = sb.ToString();
            WriteLine(ss);
            WriteLine("");
            WriteLine("");
            WriteLine("");
            return Succeed("total is " + total);
        }

        #endregion
    }
}