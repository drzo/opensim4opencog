using System;
using System.Text;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser.AIMLTagHandlers
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
        /// <param name="templateNode">The node to be processed</param>
        public lazyClosure(RTParser.RTPBot bot,
                           RTParser.User user,
                           RTParser.Utils.SubQuery query,
                           RTParser.Request request,
                           RTParser.Result result,
                           XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        #region Overrides of TextTransformer

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        public override Unifiable RecurseProcess()
        {
            if (templateNode.NodeType == XmlNodeType.Comment) return Unifiable.Empty;
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                string s = templateNode.InnerText.Trim();
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
                // process each of these child "settings"? nodes
                try
                {
                    request.TargetSettings = request.TargetBot.InputSubstitutions;
                    SettingsDictionary.loadSettingNode(request.TargetSettings, templateNode, true, false, request);
                }
                finally
                {
                    request.TargetSettings = prevDict;
                }
                return Succeed();
            }
            if (currentNodeName == "genlmt")
            {
                string name = RTPBot.GetAttribValue(templateNode, "name,mt,to,super,into", null);
                string removeTo = RTPBot.GetAttribValue(templateNode, "remove", null);
                string from = RTPBot.GetAttribValue(templateNode, "graph,from", null);
                bool deleteLink = false;
                if (name == null)
                {
                    name = templateNode.InnerText.Trim();
                }
                if (removeTo != null)
                {
                    deleteLink = true;
                    name = removeTo;
                }
                GraphMaster FROM = request.TargetBot.GetGraph(from, request.Graph);
                GraphMaster TO = request.TargetBot.GetGraph(name, request.Graph);
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
                string name = RTPBot.GetAttribValue(templateNode, "name,mt,to,super,into", null);
                string from = RTPBot.GetAttribValue(templateNode, "graph,from", null);
                if (name == null)
                {
                    name = templateNode.InnerText.Trim();
                }
                GraphMaster FROM = request.TargetBot.GetGraph(from, request.Graph);
                GraphMaster TO = request.TargetBot.GetGraph(name, request.Graph);
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
                SettingsDictionary.loadSettingNode(request.TargetSettings, templateNode, true, false, request);
                return Succeed();

            }
            if (currentNodeName == "bot")
            {
                SettingsDictionary.loadSettingNode(request.TargetBot.Settings, templateNode, true, false, request);
                return Succeed();
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

            if (templateNode.NodeType == XmlNodeType.Comment) return Succeed();

            // pull from late bound sustituion dictionaries
            var sd = request.GetSubstitutions(currentNodeName, false);
            if (sd != null)
            {
                if (RecurseResultValid) return RecurseResult;
                if (!Unifiable.IsNull(RecurseResult))
                {
                    return RecurseResult;
                }
                Func<Unifiable, Unifiable> Format = (v) => RTParser.Normalize.ApplySubstitutions.Substitute(sd, templateNodeInnerText);
                if (isRecursive && !ReadOnly)
                {
                    return RecurseResult = Format(TransformAtomically(null, true));
                }
                return RecurseResult = TransformAtomically(Format, false);
            }

            if (RTPBot.UnknownTagsAreBotVars)
            {
                var v = Proc.GlobalSettings.grabSetting(currentNodeName);
                if (!Unifiable.IsNull(v)) return v;
            }
            var vs = Proc.EvalAiml(templateNode, request, request.writeToLog);
            StringBuilder sb = new StringBuilder();
            int writeThrus = 0;
            int total = 0;
            OutputDelegate WriteLine = DLRConsole.SystemWriteLine;
            foreach (var node in vs)
            {
                total++;
                string nodeOuterXml = node.InnerXml;
                WriteLine(nodeOuterXml);
                string p = RTPBot.GetAttribValue(node,"PASSED","FALSE");
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

        #region Overrides of TextTransformer

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected override Unifiable ProcessChange()
        {
            return Unifiable.STAR;
        }

        #endregion
    }
}