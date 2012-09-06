using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using Unifiable = System.String;

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
        /// <param name="templateNode">The node to be processed</param>
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
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        public Unifiable RecurseProcess()
        {
            if (templateNode.NodeType == XmlNodeType.Comment) return Unifiable.Empty;
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                string s = StaticXMLUtils.Trim(templateNode.InnerText);
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
                return ProcessSucceed();
            }/*
            if (currentNodeName == "genlmt")
            {
                string name = StaticXMLUtils.GetAttribValue(templateNode, "name,mt,to,super,into", null);
                string removeTo = StaticXMLUtils.GetAttribValue(templateNode, "remove", null);
                string from = StaticXMLUtils.GetAttribValue(templateNode, "graph,from", null);
                bool deleteLink = false;
                if (name == null)
                {
                    name = StaticXMLUtils.Trim(templateNode.InnerText);
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
                string name = StaticXMLUtils.GetAttribValue(templateNode, "name,mt,to,super,into", null);
                string from = StaticXMLUtils.GetAttribValue(templateNode, "graph,from", null);
                if (name == null)
                {
                    name = StaticXMLUtils.Trim(templateNode.InnerText);
                }
                GraphMaster FROM = request.GetGraph(from);
                GraphMaster TO = request.GetGraph(name);
                if (FROM != null && TO != null)
                {
                    FROM.Srai = name;
                    return Succeed("SRAI: " + FROM + " => " + name + " => right now " + TO);
                }
                return Failure("FROM '" + from + "'='" + FROM + "'" + " TO '" + name + "'='" + TO + "'");
            }*/
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
                return ProcessSucceed();

            }
            if (currentNodeName == "bot")
            {
                SettingsDictionary.loadSettingNode(request.TargetBot.GlobalSettings, templateNode, true, false, request);
                return ProcessSucceed();
            }
            string currentNodeOuterXml = templateNode.OuterXml;
            if (currentNodeOuterXml.Length > 280) currentNodeOuterXml = TextFilter.ClipString(currentNodeOuterXml, 280);
            bot.writeToLog("ImmediateAiml: " + currentNodeOuterXml);
            /*
               <TestCase name="connect">
                    <Input>CONNECT</Input>
                    <ExpectedAnswer>Connected to test case AIML set.</ExpectedAnswer>
               </TestCase>
            */

            if (templateNode.NodeType == XmlNodeType.Comment) return ProcessSucceed();
            /*
            // pull from late bound sustituion dictionaries
            var sd = request.GetSubstitutions(currentNodeName, false);
            if (sd != null)
            {
                if (RecurseResultValid) return RecurseResult;
                if (!Unifiable.IsIncomplete(RecurseResult))
                {
                    return RecurseResult;
                }
                Func<Unifiable, Unifiable> Format = (v) => Normalize.ApplySubstitutions.Substitute(bot, sd, TemplateNodeInnerText);
                if (isRecursive && !ReadOnly)
                {
                    RecurseResult = Format(TransformAtomically(null, true));
                    return finalResult.Value;
                }
                return RecurseResult = TransformAtomically(Format, false);
            }
            */
            if (AltBot.UnknownTagsAreBotVars)
            {
                var v = bot.GlobalSettings.grabSetting(currentNodeName);
                if (!string.IsNullOrEmpty(v)) return v;
            }
            // @todo var vs = bot.EvalAiml(templateNode, request, request.writeToLog);
            var vs = EvalAiml(templateNode, request, bot.writeToLog);
            StringBuilder sb = new StringBuilder();
            int writeThrus = 0;
            int total = 0;
            OutputDelegate WriteLine = DLRConsole.SystemWriteLine;
            foreach (var node in vs)
            {
                total++;
                string nodeOuterXml = StaticXMLUtils.ToXmlValue(node);
                WriteLine(nodeOuterXml);
                string p = GetAttribValue<string>(node, "PASSED", "FALSE");
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

        private string Failure(string s)
        {
            return string.Empty;
        }

        private string Succeed(string s)
        {
            return s;
        }

        private string ProcessSucceed()
        {
            return TemplateNodeInnerText;
        }

        #endregion

        #region Overrides of TextTransformer


        public IEnumerable<XmlNode> EvalAiml(XmlNode currentNode, Request request, OutputDelegate del)
        {
            var nodes = new HashSet<XmlNode>();
            bool evaledNode = false;
            del = del ?? DLRConsole.SystemWriteLine;
            var getEvaluators = XmlNodeEvaluatorImpl.Get1Evaluators(currentNode);
            foreach (XmlNodeEval funct in getEvaluators)
            {
                evaledNode = true;
                var newNode = funct(currentNode, request, del);
                if (newNode != null)
                {
                    evaledNode = true;
                    foreach (XmlNode node in newNode)
                    {
                        nodes.Add(node);
                    }
                }
            }
            if (evaledNode)
            {
                del("evaledNode=" + evaledNode);
                del("nodes.Count=" + nodes.Count);
                int nc = 1;
                foreach (XmlNode n in nodes)
                {
                    del("node {0}:{1}", nc, n);
                    nc++;
                }
                return nodes;
            }
            return XmlNodeEvaluatorImpl.NO_XmlNode;
        }

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected override Unifiable ProcessChange()
        {
            var vv = RecurseProcess();
            return vv;
            // return Unifiable.STAR;
        }

        #endregion
    }
}