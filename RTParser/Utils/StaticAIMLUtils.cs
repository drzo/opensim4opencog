using System;
using System.Collections;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;
using System.IO;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using System.Collections.Generic;
using RTParser.Database;
using RTParser.Variables;
using System.Text;

namespace RTParser.Utils
{
    public class StaticAIMLUtils: StaticXMLUtil 
    {
        public static bool DebugSRAIs = true;
        public static bool NoRuntimeErrors = false;
        public static readonly XmlNode TheTemplateOverwrite = StaticXMLUtil.getNode("<template></template>");
        public static Dictionary<XmlNode, StringBuilder> ErrorList = new Dictionary<XmlNode, StringBuilder>();
        public static Func<string> EmptyFunct = (() => String.Empty);
        public static Func<string> NullStringFunct = (() => null);
        public static OutputDelegate DEVNULL = TextFilter.DEVNULL;
        public static bool ThatWideStar = false;
        public static bool useInexactMatching = false;
        public static OutputDelegate userTraceRedir;
        protected static XmlNode PatternStar
        {
            get
            {
                var ps = StaticXMLUtil.getNode("<pattern name=\"*\">*</pattern>");
                LineInfoElementImpl.SetReadOnly(ps);

                return ps;
            }
        }


        protected static List<string> skip = new List<string>()
                                               {
                                                   "#comment",
                                               //    "debug",
                                               };

        protected static List<string> flatten = new List<string>()
                                               {
                                                   "template",
                                                   "pattern",
                                               };

        /// <summary>
        /// Attributes that we use from AIML not intended to be stacked into user dictionary
        /// </summary>
        public static ICollection<string> ReservedAttributes =
            new HashSet<string>
                {
                    "name",
                    "var",
                    "index",
                    "default",
                    "defaultValue",
                    "match",
                    "user",
                    "bot",
                    "value",
                    "type",
                    "value",
                    "id",
                    "graph",
                    "size",
                    "evidence",
                    "prop",
                    "min",
                    "max",
                    "threshold",
                    "to",
                    "from",
                    "max",
                    "wordnet",
                    "whword",
                    "pos",
                    "constant",
                    "id",
                };

        public static ICollection<string> PushableAttributes = new HashSet<string>()
        {
        };


        protected static R FromLoaderOper<R>(Func<R> action, GraphMaster gm)
        {
            OutputDelegate prev = userTraceRedir;
            try
            {
                userTraceRedir = gm.writeToLog;
                try
                {
                    lock (ErrorList)
                    {
                        lock (gm.LockerObject)
                        {
                            return action();
                        }
                    }
                }
                catch (Exception e)
                {
                    RTPBot.writeDebugLine("ERROR: LoaderOper {0}", e);
                    if (NoRuntimeErrors) return default(R);
                    throw;
                    //return default(R);
                }
            }
            finally
            {
                userTraceRedir = prev;
            }
        }

        static public ThreadStart EnterTag(Request request, XmlNode templateNode, SubQuery query)
        {
            bool needsUnwind = false;
            object thiz = (object)query ?? request;
            ISettingsDictionary dict = query ?? request.TargetSettings;
            XmlAttributeCollection collection = templateNode.Attributes;
            if (collection != null && collection.Count > 0)
            {
                // graphmaster
                GraphMaster oldGraph = request.Graph;
                GraphMaster newGraph = null;
                // topic
                Unifiable oldTopic = request.Topic;
                Unifiable newTopic = null;

                // that
                Unifiable oldThat = request.That;
                Unifiable newThat = null;

                UndoStack savedValues = null;

                foreach (XmlAttribute node in collection)
                {
                    switch (node.Name.ToLower())
                    {
                        case "graph":
                            {
                                string graphName = ReduceStar(node.Value, query, dict);
                                if (graphName != null)
                                {
                                    GraphMaster innerGraph = request.TargetBot.GetGraph(graphName, oldGraph);
                                    needsUnwind = true;
                                    if (innerGraph != null)
                                    {
                                        if (innerGraph != oldGraph)
                                        {
                                            request.Graph = innerGraph;
                                            newGraph = innerGraph;
                                            request.writeToLog("ENTERING: {0} as {1} from {2}",
                                                               graphName, innerGraph, oldGraph);
                                        }
                                        else
                                        {
                                            newGraph = innerGraph;
                                        }
                                    }
                                    else
                                    {
                                        oldGraph = null; //?
                                    }
                                }
                            }
                            break;
                        case "topic":
                            {
                                newTopic = ReduceStar(node.Value, query, dict);
                                if (newTopic != null)
                                {
                                    if (newTopic.IsEmpty) newTopic = "Nothing";
                                    needsUnwind = true;
                                    request.Topic = newTopic;
                                }

                            }
                            break;
                        case "that":
                            {
                                newThat = ReduceStar(node.Value, query, dict);
                                if (newThat != null)
                                {
                                    if (newThat.IsEmpty) newThat = "Nothing";
                                    needsUnwind = true;
                                    request.That = newThat;
                                }

                            }
                            break;

                        default:
                            {

                                string n = node.Name;
                                lock (ReservedAttributes)
                                {
                                    if (ReservedAttributes.Contains(n))
                                        continue;
                                    bool prev = NamedValuesFromSettings.UseLuceneForGet;
                                    try
                                    {
                                        NamedValuesFromSettings.UseLuceneForGet = false;
                                        if (!dict.containsSettingCalled(n))
                                        {
                                            ReservedAttributes.Add(n);
                                            request.writeToLog("ReservedAttributes: {0}", n);
                                        }
                                        else
                                        {
                                            if (!PushableAttributes.Contains(n))
                                            {
                                                PushableAttributes.Add(n);
                                                request.writeToLog("PushableAttributes: {0}", n);
                                            }
                                        }
                                    }
                                    finally
                                    {
                                        NamedValuesFromSettings.UseLuceneForGet = prev;
                                    }
                                }
                                Unifiable v = (Unifiable)ReduceStar(node.Value, query, dict);
                                UndoStack.FindUndoAll(thiz);
                                savedValues = savedValues ?? UndoStack.GetStackFor(thiz);
                                //savedValues = savedValues ?? query.GetFreshUndoStack();
                                savedValues.pushValues(dict, n, v);
                                needsUnwind = true;
                            }
                            break;
                    }
                }

                // unwind
                if (needsUnwind)
                {
                    return () =>
                    {
                        try
                        {

                            if (savedValues != null)
                            {
                                savedValues.UndoAll();
                            }
                            if (newGraph != null)
                            {
                                var cg = request.Graph;
                                if (cg == newGraph)
                                {
                                    request.writeToLog("LEAVING: {0}  back to {1}", request.Graph, oldGraph);
                                    request.Graph = oldGraph;
                                }
                                else
                                {
                                    request.writeToLog(
                                        "WARNING: UNWIND GRAPH UNEXPECTED CHANGE {0} FROM {1} SETTING TO {2}",
                                        cg, newGraph, oldGraph);
                                    request.Graph = oldGraph;
                                }
                            }
                            if (newTopic != null)
                            {
                                var ct = request.Topic;
                                if (newTopic == ct)
                                {
                                    request.Topic = oldTopic;
                                }
                                else
                                {
                                    request.writeToLog(
                                        "WARNING: UNWIND TOPIC UNEXPECTED CHANGE {0} FROM {1} SETTING TO {2}",
                                        ct, newTopic, oldTopic);
                                    request.Topic = oldTopic;
                                }
                            }
                            if (newThat != null)
                            {
                                var ct = request.That;
                                if (newThat == ct)
                                {
                                    request.That = oldThat;
                                }
                                else
                                {
                                    request.writeToLog(
                                        "WARNING: UNWIND THAT UNEXPECTED CHANGE {0} FROM {1} SETTING TO {2}",
                                        ct, newThat, oldThat);
                                    request.That = oldThat;
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            request.writeToLog("ERROR " + ex);
                        }
                    };
                }
            }
            return () => { };
        }

        static public Unifiable ReduceStar(string name, SubQuery query, ISettingsDictionary dict)
        {
            string[] nameSplit = name.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
            foreach (var nameS in nameSplit)
            {
                var r = AltStar(nameS, query, dict);
                if (!Unifiable.IsNullOrEmpty(r))
                {
                    return r;
                }
            }
            return name;
        }
        static public Unifiable AltStar(string name, SubQuery query, ISettingsDictionary dict)
        {
            try
            {
                if (name.StartsWith("star_"))
                {
                    return GetDictData(query.InputStar, name, 5);
                }
                else if (name.StartsWith("inputstar_"))
                {
                    return GetDictData(query.InputStar, name, 10);
                }
                else if (name.StartsWith("input_"))
                {
                    return GetDictData(query.InputStar, name, 6);
                }
                else if (name.StartsWith("thatstar_"))
                {
                    return GetDictData(query.ThatStar, name, 9);
                }
                else if (name.StartsWith("that_"))
                {
                    return GetDictData(query.ThatStar, name, 5);
                }
                else if (name.StartsWith("topicstar_"))
                {
                    return GetDictData(query.TopicStar, name, 10);
                }
                else if (name.StartsWith("topic_"))
                {
                    return GetDictData(query.TopicStar, name, 6);
                }
                else if (name.StartsWith("guardstar_"))
                {
                    return GetDictData(query.GuardStar, name, 10);
                }
                else if (name.StartsWith("guard_"))
                {
                    return GetDictData(query.GuardStar, name, 6);
                }
                else if (name.StartsWith("@"))
                {
                    Unifiable value = query.Request.TargetBot.SystemExecute(name, null, query.Request);
                    if (!Unifiable.IsNullOrEmpty(value)) return value;
                }
                else if (name.StartsWith("%dictvar_"))
                {
                    Unifiable value = value = GetValue(query, dict, name.Substring(8));
                    if (!Unifiable.IsNullOrEmpty(value)) return value;
                }
                else if (name.StartsWith("%"))
                {
                    Unifiable value = null;
                    string str = name.Substring(1);
                    if (str.StartsWith("bot."))
                    {
                        var dict2 = query.Request.TargetBot.GlobalSettings;
                        str = str.Substring(4);
                        value = GetValue(query, dict2, str);
                        if (!Unifiable.IsNullOrEmpty(value)) return value;
                    }
                    else if (str.StartsWith("user."))
                    {
                        ISettingsDictionary dict2 = query.Request.user;
                        str = str.Substring(5);
                        value = GetValue(query, dict2, str);
                        if (!Unifiable.IsNullOrEmpty(value)) return value;
                    }
                    if (dict != null)
                    {
                        value = GetValue(query, dict, str);
                        if (!Unifiable.IsNullOrEmpty(value)) return value;
                    }
                }
            }
            catch (Exception e)
            {
                RTPBot.writeDebugLine("" + e);
            }
            return null;
        }

        private static Unifiable GetValue(SubQuery query, ISettingsDictionary dict2, string str)
        {
            Unifiable value;
            value = dict2.grabSetting(str);
            return value;
        }

        private static Unifiable GetDictData(IList<Unifiable> unifiables, string name, int startChars)
        {
            var u = GetDictData0(unifiables, name, startChars);
            string toup = u.ToUpper();
            if (string.IsNullOrEmpty(toup)) return u;
            if (char.IsLetterOrDigit(toup[0])) return u;
            return u;
        }

        private static Unifiable GetDictData0(IList<Unifiable> unifiables, string name, int startChars)
        {
            string s = name.Substring(startChars);

            if (s == "*" || s == "ALL" || s == "0")
            {
                var result = Unifiable.CreateAppendable();
                foreach (Unifiable u in unifiables)
                {
                    result.Append(u);
                }
                return result;
            }

            int uc = unifiables.Count;

            bool fromend = false;
            if (s.StartsWith("-"))
            {
                fromend = true;
                s = s.Substring(1);
            }

            int i = Int32.Parse(s);

            if (i == 0)
            {
                if (uc == 0) return "";
            }
            int ii = i - 1;
            if (fromend) ii = uc - i;
            if (uc == 0)
            {
                RTPBot.writeDebugLine(" !ERROR -star underflow! " + i + " in " + name);
                return String.Empty;
            }
            if (ii >= uc || ii < 0)
            {

                RTPBot.writeDebugLine(" !ERROR -star badindexed 0 < " + i + " < " + uc + " in " + name);
                return unifiables[ii];
            }
            return unifiables[ii];
        }


        public static bool IsPredMatch(Unifiable required, Unifiable actualValue, SubQuery subquery)
        {
            if (Unifiable.IsNull(required))
            {
                return Unifiable.IsNullOrEmpty(actualValue);
            }
            if (Unifiable.IsNull(actualValue))
            {
                return Unifiable.IsNullOrEmpty(required);
            }
            required = required.Trim();
            if (required.IsAnySingleUnit())
            {
                return !Unifiable.IsNullOrEmpty(actualValue);
            }

            actualValue = actualValue.Trim();
            if (actualValue.WillUnify(required, subquery))
            {
                return true;
            }
            string requiredAsStringReplaceReplace = required.AsString().Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+");
            Regex matcher = new Regex("^" + requiredAsStringReplaceReplace + "$",
                                      RegexOptions.IgnoreCase);
            if (matcher.IsMatch(actualValue))
            {
                return true;
            }
            if (required.ToUpper() == "UNKNOWN" && (Unifiable.IsUnknown(actualValue)))
            {
                return true;
            }
            return false;
        }


        protected static string PadStars(string pattern)
        {
            pattern = pattern.Trim();
            int pl = pattern.Length;
            if (pl == 0) return "~*";
            if (pl == 1) return pattern;
            if (pl == 2) return pattern;
            if (char.IsLetterOrDigit(pattern[pl - 1])) pattern = pattern + " ~*";
            if (char.IsLetterOrDigit(pattern[0])) pattern = "~* " + pattern;
            return pattern;
        }

        public static void PrintResult(Result result, OutputDelegate console, PrintOptions printOptions)
        {
            console("-----------------------------------------------------------------");
            console("Result: " + result.Graph + " Request: " + result.request);
            foreach (var s in result.InputSentences)
            {
                console("input: \"" + s + "\"");
            }
            PrintTemplates(result.UsedTemplates, console, printOptions);
            foreach (var s in result.SubQueries)
            {
                console("\n" + s);
            }
            console("-");
            foreach (var s in result.OutputSentences)
            {
                console("outputsentence: " + s);
            }
            console("-----------------------------------------------------------------");
        }

        public static string GetTemplateSource(IEnumerable CI, PrintOptions printOptions)
        {
            if (CI == null) return "";
            var fs = new StringWriter();
            GraphMaster.PrintToWriter(CI, printOptions, fs, null);
            return fs.ToString();
        }

        public static void PrintTemplates(IEnumerable CI, OutputDelegate console, PrintOptions printOptions)
        {
            GraphMaster.PrintToWriter(CI, printOptions, new OutputDelegateWriter(console), null);
        }

        public static bool IsSilentTag(XmlNode node)
        {
            // if (true) return false;
            if (node.Name == "think") return true;
            if (node.NodeType == XmlNodeType.Text)
            {
                string innerText = node.InnerText;
                if (innerText.Trim().Length == 0)
                {
                    return true;
                }
                return false;
            }
            if (node.Name == "template")
            {
                foreach (XmlNode xmlNode in node.ChildNodes)
                {
                    if (!IsSilentTag(xmlNode)) return false;
                }
                if (node.ChildNodes.Count != 1)
                {
                    return true;
                }
                return true;
            }
            return false;
        }


        protected static string ToNonSilentTags(string sentenceIn)
        {
            var nodeO = StaticXMLUtil.getNode("<node>" + sentenceIn + "</node>");
            LineInfoElementImpl.notReadonly(nodeO);
            return VisibleRendering(nodeO.ChildNodes, skip, flatten);
        }


        public static string VisibleRendering(XmlNodeList nodeS)
        {
            return VisibleRendering(nodeS, skip, flatten);
        }

        private static string VisibleRendering(XmlNodeList nodeS, List<string> skip, List<string> flatten)
        {
            var sentenceIn = "";
            foreach (XmlNode nodeO in nodeS)
            {
                sentenceIn = sentenceIn + " " + VisibleRendering(nodeO, skip, flatten);
            }
            return sentenceIn.Trim().Replace("  ", " ");
        }

        private static string VisibleRendering(XmlNode nodeO, List<string> skip, List<string> flatten)
        {
            if (nodeO.NodeType == XmlNodeType.Comment) return "";
            string nodeName = nodeO.Name.ToLower();
            if (skip.Contains(nodeName)) return "";
            if (flatten.Contains(nodeName))
            {
                return VisibleRendering(nodeO.ChildNodes, skip, flatten);
            }
            if (nodeO.NodeType == XmlNodeType.Element) return nodeO.OuterXml;
            if (nodeO.NodeType == XmlNodeType.Text) return nodeO.InnerText;
            return nodeO.OuterXml;
        }

        public static string RenderInner(XmlNode nodeO)
        {
            return VisibleRendering(nodeO.ChildNodes, skip, flatten);
        }
    }
}