using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;
using AltAIMLbot;
using AltAIMLbot.Database;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;
using AltAIMLParser;
using AltAIMLbot.Variables;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace AltAIMLbot.Utils
{
    [Serializable]
    public class StaticAIMLUtils : TextPatternUtils
    {
        public static readonly Func<string> NullStringFunct = (() => null);
        public static readonly Func<Unifiable> NullUnifyFunct = (() => null /*Unifiable.NULL*/);

        public static readonly ICollection<string> PushableAttributes = new HashSet<string>
                                                                            {
                                                                            };

        /// <summary>
        /// Attributes that we use from AIML not intended to be stacked into user dictionary
        /// </summary>
        public static readonly ICollection<string> ReservedAttributes =
            new HashSet<string>
                {
                    "name",
                    "var",
                    "index",
                    "default",
                    "defaultValue",
                    "match",
                    "matches",
                    "existing",
                    "ifUnknown",
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

                    "state",
                    "flag",
                    "graph",
                    "topic",
                    "that",
                };

        public static readonly List<String> LoaderTags = new List<string>()
                                                             {
                                                                 "aiml",
                                                                 "topic",
                                                                 "category",
                                                                 "genlMt",
                                                             };


        public static readonly List<String> SilentTags = new List<string>(LoaderTags)
                                          {
                                "#comment",
                                "silence",
                                "bookmark",
                                "src",
                                "think",
                                "that",
                                 "genlMt",
                                //    "debug",
                                          };

        public static readonly List<string> TagsRecurseToFlatten = new List<string>
                                                                       {
                                                                           "template",
                                                                           "pattern",                                                                         
                                                                           "sapi",
                                                                           "node",
                                                                           "pre",
                                                                           "bold",
                                                                       };

        public static readonly List<string> TagsWithNoOutput = new List<string>
                                                                   {
                                                                       "#comment",
                                                                       //    "debug",                                                                                                                        
                                                                       "that",
                                                                       "br",
                                                                       "p",
                                                                       "flags",
                                                                   };


        public static readonly RenderOptions TemplateSideRendering =
            new RenderOptions()
                {
                    flatten =
                        new List<string>(TagsRecurseToFlatten)
                            {
                                "node",
                            },
                    skip = new List<string>(TagsWithNoOutput)
                               {
                                   "#comment",
                                   "silence",
                                   "bookmark",
                                   "src",
                                   "that",
                                   //    "debug",
                               }
                };
        public static readonly RenderOptions PatternSideRendering =
    new RenderOptions()
    {
        flatten =
            new List<string>(TagsRecurseToFlatten) { "a", },
        skip =
            new List<string>(TagsWithNoOutput)
                            {
                                "#comment",
                                "silence",
                                "bookmark",
                                "src",
                                "think",
                                //    "debug",
                            }
    };

        public static bool DebugSRAIs = true;
        public static Dictionary<XmlNode, StringBuilder> ErrorList = new Dictionary<XmlNode, StringBuilder>();
        public static bool NoRuntimeErrors = false;
        public static readonly FirstUse<XmlNode> PatternStar = InitOnce(() => StaticXMLUtils.getNode(true, "<pattern>*</pattern>"));
        public static readonly FirstUse<XmlNode> ThatStar = new Func<XmlNode>(() => StaticXMLUtils.getNode(true, "<that>*</that>"));

        public static readonly FirstUse<XmlNode> TheTemplateOverwrite = (Func<XmlNode>) (() => getNode("<template></template>"));

        public static readonly FirstUse<XmlNode> TopicStar = (Func<XmlNode>) (() => StaticXMLUtils.getNode(true, "<topic name=\"*\"/>"));
        public static readonly FirstUse<XmlNode> XmlStar = FirstUse<XmlNode>.F((() => PatternStar.Value.FirstChild));
        public static bool ThatWideStar;
        public static bool useInexactMatching;
        public static OutputDelegate userTraceRedir;
        public static bool TrackTemplates = true; // to save mememory

        public static int CompareXmlNodes(XmlNode node1, XmlNode node2)
        {
            return ReferenceCompare(node1, node2);
        }

        public static string ToTemplateXML(XmlNode templateNode)
        {
            string requestName = ToXmlValue(templateNode);
            if (templateNode.NodeType != XmlNodeType.Element)
            {
                string sentence = VisibleRendering(StaticAIMLUtils.getTemplateNode(requestName).ChildNodes,
                                                   PatternSideRendering);
                requestName = "<template>" + sentence + "</template>";
            }
            return requestName;
        }

        public static XmlNode getTemplateNode(string sentence)
        {
            if (!sentence.Trim().StartsWith("<template"))
            {
                sentence = "<template>" + sentence + "</template>";
            }
            var vv = getDocNode(false, sentence, false, false, StringOnlyDocPreserve);
            return vv;
        }

        public static R FromLoaderOper<R>(Func<R> action, GraphMaster gm, LoaderOptions loadOpts)
        {
            OutputDelegate prev = userTraceRedir;
            try
            {
                userTraceRedir = gm.writeToLog;
                try
                {
                    if (!loadOpts.NeedsLoaderLock) return action();
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
                    AltBot.writeDebugLine("ERROR: LoaderOper {0}", e);
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

        public static Action EnterTag(Request request, XmlNode templateNode, SubQuery query)
        {
            if (templateNode.NodeType != XmlNodeType.Element)
            {
                return DoNothing;
                throw new NotImplementedException("EnterTag: " + templateNode.NodeType);
            }
            bool needsUnwind = false;
            UndoStackHolder thiz = (UndoStackHolder) query ?? request;
            ISettingsDictionary dict = query ?? request.TargetSettings;
            XmlAttributeCollection collection = templateNode.Attributes;
            EnterContext(request, query);
            if (collection == null || collection.Count <= 0)
            {
                return () =>
                           {
                               ExitContext(request, query);
                           };
            }
            else
            {
                var used = new List<XmlAttribute>();
                string defaultElement = "";
                Action gmrerstore;
                gmrerstore = request.WithAttributesForUnwind(templateNode, ref defaultElement, used);
                int uc = used.Count;
                UndoStack savedValues = null;

                foreach (XmlAttribute node in collection)
                {
                    if (used.Contains(node))
                    {
                        continue;
                    }
                    bool found;
                    string n = node.Name.ToLower();
                    switch (n)
                    {
                        case "state":
                        case "flag":
                        case "graph":
                        case "topic":
                        case "that":
                            break;

                        default:
                            {
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

                                // now require temp vars to s   ay  with_id="tempId"
                                // to set the id="tempid" teporarily while evalig tags
                                if (!n.StartsWith("with_"))
                                {
                                    continue;
                                }
                                else
                                {
                                    n = n.Substring(5);
                                }

                                Unifiable v = ReduceStar<Unifiable>(node.Value, query, dict, out found);
                                UndoStack.FindUndoAll(thiz, true);
                                savedValues = savedValues ?? UndoStack.GetStackFor(thiz);
                                //savedValues = savedValues ?? query.GetFreshUndoStack();
                                savedValues.pushValues(dict, n, v);
                                needsUnwind = true;
                            }
                            break;
                    }
                }

                // unwind                

                return () =>
                           {
                               if (needsUnwind)
                               {
                                   try
                                   {
                                       EnterContext(request, query);
                                       if (savedValues != null)
                                       {
                                           savedValues.UndoAll();
                                       }
                                   }
                                   catch (Exception ex)
                                   {
                                       request.writeToLog("ERROR " + ex);
                                   }
                                   finally
                                   {
                                       ExitContext(request, query);
                                   }
                               }
                               ExitContext(request, query);
                               if (uc > 0) gmrerstore();
                           };
            }
        }


        public static void DoNothing()
        {
             
        }

        private static void EnterContext(Request request, SubQuery query)
        {
            request.Enter(query);
        }
        private static void ExitContext(Request request, SubQuery query)
        {
            request.Exit(query);
        }


        public static bool ContainsAiml(Unifiable unifiable)
        {
            String s = unifiable.AsString();
            return StaticXMLUtils.ContainsXml(s);
        }

        internal static bool AimlSame(XmlNode info, XmlNode Output)
        {
            return info.Name == Output.Name && info.OuterXml == Output.OuterXml;
        }

        public static bool AimlSame(string xml1, string xml2)
        {
            if (xml1 == xml2) return true;
            if (xml1 == null) return String.IsNullOrEmpty(xml2);
            if (xml2 == null) return String.IsNullOrEmpty(xml1);
            xml1 = MakeAimlMatchable(xml1);
            xml2 = MakeAimlMatchable(xml2);
            if (xml1 == xml2) return true;
            return XmlSame(xml1, xml2);
        }

        public static string MakeAimlMatchable(string xml1)
        {
            if (xml1 == null) return xml1;
            xml1 = MakeMatchable(xml1);
            xml1 = ReplaceMap(xml1, new[]
                                     {
                                         new string[] {" index=\"1\"", " "},
                                         new string[] {" index=\"1,1\"", " "},
                                         new string[] {" var=", " name="},
                                         new string[] {"yours", "your"},
                                     });
            //    ).Replace().Replace();
            //xml1 = CleanWhitepaces(xml1);
            // t = t.Replace("<star index=\"1\"/>", " * ");
            // t = t.Replace("<star/>", " * ");
            //t = t.Replace("<sr/>", " * ");
            // t = t.Replace("  ", " ").Trim();
            return xml1;
        }

        public static int FromInsideLoaderContext(XmlNode currentNode, Request request, SubQuery query, Func<int> doit)
        {
            int total = 0;
            query = query ?? request.CurrentQuery;
            //Result result = query.Result;
            AltBot RProcessor = request.TargetBot;
            AIMLLoader prev = RProcessor.Loader;
            try
            {
                // RProcessor.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();

                var ts = EnterTag(request, currentNode, query);
                try
                {
                    total += doit();
                }
                finally
                {
                    ts();
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
            return total;
        }

        /*
        public static int NonAlphaCount(string input)
        {
            input = CleanWhitepaces(input);
            int na = 0;
            foreach (char s in input)
            {
                if (char.IsLetterOrDigit(s)) continue;
                na++;
            }
            return na;
        }

        public static string NodeInfo(XmlNode templateNode, Func<string, XmlNode, string> funct)
        {
            string s = null;
            XmlNode nxt = templateNode;
            s = funct("same", nxt);
            if (s != null) return s;
            nxt = templateNode.NextSibling;
            s = funct("next", nxt);
            if (s != null) return s;
            nxt = templateNode.PreviousSibling;
            s = funct("prev", nxt);
            if (s != null) return s;
            nxt = templateNode.ParentNode;
            s = funct("prnt", nxt);
            if (s != null) return s;
            return s;
        }
        */

        public static T ReduceStar<T>(IConvertible name, SubQuery query, ISettingsDictionary dict, out bool rfound)
            where T : IConvertible
        {
            var nameSplit = name.ToString().Split(new[] {','}, StringSplitOptions.RemoveEmptyEntries);
            foreach (string nameS in nameSplit)
            {                
                Unifiable r = AltStar(nameS, query, dict, out rfound);
                if (!IsNullOrEmpty(r) || rfound)
                {
                    PASSTHRU<T>(r);
                }
                continue;
            }
            rfound = false;
            return PASSTHRU<T>(name);
        }

        public static Unifiable AltStar(string name, SubQuery query, ISettingsDictionary dict, out bool rfound)
        {
            try
            {
                if (name.Contains(","))
                {
                    foreach (string subname in NamesStrings(name))
                    {
                        var val = AltStar(name, query, dict, out rfound);
                        if (rfound)
                        {
                            rfound = true;
                            return val;
                        }
                    }
                }
                if (name.StartsWith("star_"))
                {
                    return GetDictData(query.InputStars, name, 5, out rfound);
                }
                else if (name.StartsWith("inputstar_"))
                {
                    return GetDictData(query.InputStars, name, 10, out rfound);
                }
                else if (name.StartsWith("input_"))
                {
                    return GetDictData(query.InputStars, name, 6, out rfound);
                }
                else if (name.StartsWith("thatstar_"))
                {
                    return GetDictData(query.ThatStars, name, 9, out rfound);
                }
                else if (name.StartsWith("that_"))
                {
                    return GetDictData(query.ThatStars, name, 5, out rfound);
                }
                else if (name.StartsWith("topicstar_"))
                {
                    return GetDictData(query.TopicStar, name, 10, out rfound);
                }
                else if (name.StartsWith("topic_"))
                {
                    return GetDictData(query.TopicStar, name, 6, out rfound);
                }
                else if (name.StartsWith("guardstar_"))
                {
                    return GetDictData(query.GuardStar, name, 10, out rfound);
                }
                else if (name.StartsWith("guard_"))
                {
                    return GetDictData(query.GuardStar, name, 6, out rfound);
                }
                else if (name.StartsWith("@"))
                {
                    Unifiable value = query.Request.TargetBot.SystemExecute(name, null, query.Request);
                    rfound = true;
                    if (!IsNullOrEmpty(value)) return value;
                    return value;
                }
                else if (name.StartsWith("%dictvar_"))
                {
                    Unifiable value = value = GetValue(query, dict, name.Substring(8), out rfound);
                    if (rfound) return value;
                    return value;
                }
                else
                {
                    if (name.StartsWith("%") || name.StartsWith("$"))
                    {
                        string str = name.Substring(1);
                        var vv = ResolveVariableValue(str, query, dict, out rfound);
                        if (rfound)
                        {
                            return vv;
                        }
                        return vv;
                    }
                    else if (name.Contains("."))
                    {
                        var vv = ResolveVariableValue(name, query, dict, out rfound);
                        if (rfound)
                        {
                            return vv;
                        }
                        return vv;
                    }
                    rfound = false;
                    return name;
                }
            }
            catch (Exception e)
            {
                AltBot.writeDebugLine("" + e);
                rfound = false;
                return null;
            }
        }

        private static Unifiable ResolveVariableValue(string str, SubQuery query, ISettingsDictionary dict, out bool found)
        {
            Unifiable value = null;
            bool rfound;
            if (str.Contains("{"))
            {
                str = str.Replace("{", "").Replace("}", "");
            }
            if (str.StartsWith("query."))
            {
                ISettingsDictionary dict2 = query;
                str = str.Substring(4);
                value = GetValue(query, dict2, str, out rfound);
                if (!IsNullOrEmpty(value))
                {
                    found = true;
                    return value;
                }
            }
            if (str.StartsWith("bot."))
            {
                ISettingsDictionary dict2 = query.Request.Responder;
                str = str.Substring(4);
                value = GetValue(query, dict2, str, out found);
                if (!IsNullOrEmpty(value))
                {
                    found = true;
                    return value;
                }
            }
            else if (str.StartsWith("user."))
            {
                ISettingsDictionary dict2 = query.Request.Requester;
                str = str.Substring(5);
                value = GetValue(query, dict2, str, out found);
                if (!IsNullOrEmpty(value))
                {
                    found = true;
                    return value;
                }
            }
            if (dict != null)
            {
                value = GetValue(query, dict, str, out found);
                if (!IsNullOrEmpty(value))
                {
                    found = true;
                    return value;
                }
            }
            found = false;
            return null;
        }

        private static Unifiable GetValue(SubQuery query, ISettingsDictionary dict2, string str, out bool rfound)
        {
            Unifiable value;
            value = dict2.grabSetting(str);
            rfound = !IsNull(value);
            return value;
        }

        private static Unifiable GetDictData<T>(IList<T> unifiables, string name, int startChars, out bool found) where T : IConvertible
        {
            T u = GetDictData0<T>(unifiables, name, startChars, out found);
            string toup = ToUpper(u.ToString(FormatProvider));
            if (string.IsNullOrEmpty(toup)) return PASSTHRU<Unifiable>(u);
            if (char.IsLetterOrDigit(toup[0])) return PASSTHRU<Unifiable>("" + u);
            return PASSTHRU<Unifiable>(u);
        }

        private static T GetDictData0<T>(IList<T> unifiables, string name, int startChars, out bool found) where T : IConvertible
        {
            string s = name.Substring(startChars);

            if (s == "*" || s == "ALL" || s == "0")
            {
                StringAppendableUnifiableImpl result = Unifiable.CreateAppendable();
                foreach (T u in unifiables)
                {
                    result.Append(u.ToString());
                }
                found = true;
                return PASSTHRU<T>(result);
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
                if (uc == 0)
                {
                    found = true;
                    return PASSTHRU<T>("");
                }
            }
            int ii = i - 1;
            if (fromend) ii = uc - i;
            if (uc == 0)
            {
                AltBot.writeDebugLine(" !ERROR -star underflow! " + i + " in " + name);
                found = false;
                return PASSTHRU<T>(String.Empty);
            }
            if (ii >= uc || ii < 0)
            {
                AltBot.writeDebugLine(" !ERROR -star badindexed 0 < " + i + " < " + uc + " in " + name);
                found = false;
                return unifiables[ii];
            }
            found = true;
            return unifiables[ii];
        }


        public static bool IsPredMatch(Unifiable required, Unifiable actualValue, SubQuery subquery)
        {
            if (IsNull(required))
            {
                return IsNullOrEmpty(actualValue);
            }
            required = required.Trim();
            if (required.IsAnyText)
            {
                return !IsNullOrEmpty(actualValue);
            }

            string requiredToUpper = required.ToUpper();
            if (requiredToUpper == "*")
            {
                return !IsUnknown(actualValue);
            }

            if (requiredToUpper == "OM" || IsNullOrEmpty(required) || requiredToUpper == "$MISSING")
            {
                return IsNullOrEmpty(actualValue) || actualValue == "OM";
            }
            if (IsIncomplete(required))
            {
                return IsIncomplete(actualValue);
            }
            if (IsNull(actualValue))
            {
                return IsNullOrEmpty(required);
            }
            actualValue = actualValue.Trim();
            if (actualValue.WillUnify(required, subquery))
            {
                return true;
            }
            string requiredAsStringReplaceReplace = required.AsString().Replace(" ", "\\s")
                .Replace("*", "[\\sA-Z0-9]+").Replace("_", "[A-Z0-9]+");
            Regex matcher = new Regex("^" + requiredAsStringReplaceReplace + "$",
                                      RegexOptions.IgnoreCase);
            if (matcher.IsMatch(actualValue))
            {
                return true;
            }
            if (requiredToUpper == "UNKNOWN" && (IsUnknown(actualValue)))
            {
                return true;
            }
            return false;
        }


        public static string PadStars(string pattern)
        {
            if (!AIMLLoader.SeekOutAndRepair) return pattern;
            pattern = Trim(pattern);
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
            foreach (Unifiable s in result.InputSentences)
            {
                console("input: \"" + s + "\"");
            }
            PrintTemplates(result.ResultTemplates, console, printOptions, TimeSpan.Zero);
            foreach (SubQuery s in result.SubQueries)
            {
                console("\n" + s);
            }
            console("-");
            var OutputSentences = result.OutputSentences;
            lock (OutputSentences)
            {
                foreach (string s in OutputSentences)
                {
                    console("outputsentence: " + s);
                }
            }
            console("-----------------------------------------------------------------");
        }

        public static string GetTemplateSource(IEnumerable CI, PrintOptions printOptions)
        {
            if (CI == null) return "";
            StringWriter fs = new StringWriter();
            GraphMaster.PrintToWriter(CI, printOptions, fs, null, TimeSpan.Zero);
            return fs.ToString();
        }

        public static void PrintTemplates(IEnumerable CI, OutputDelegate console, PrintOptions printOptions, TimeSpan sleepBetween)
        {
            GraphMaster.PrintToWriter(CI, printOptions, new OutputDelegateWriter(console), null, sleepBetween);
        }

        public static bool IsEmptyPattern(XmlNode node)
        {
            if (node.NodeType == XmlNodeType.Comment || IsEmptyText(node)) return true;
            string patternSide = VisibleRendering(node.ChildNodes, PatternSideRendering);
            return Trim(patternSide).Length == 0;
        }
        public static bool IsEmptyTemplate(XmlNode node)
        {
            if (node == null) return true;
            if (node.NodeType == XmlNodeType.Comment) return true;
            return (!node.HasChildNodes && node.LocalName == "template");
        }
        public static bool IsSilentTag(XmlNode node)
        {
            // if (true) return false;
            if (SilentTags.Contains(node.Name.ToLower()))
            {
                if (node.Attributes != null)
                    if (node.Attributes.Count == 0)
                        return true;
            }
            if (IsEmptyText(node))
            {
                return true;
            }
            if (TemplateSideRendering.flatten.Contains(node.Name))
            {
                if (!IsSilentTag(node.ChildNodes)) return false;
                return true;
            }
            return false;
        }

        public static bool IsSilentTag(XmlNodeList childNodes)
        {
            foreach (XmlNode xmlNode in childNodes)
            {
                if (xmlNode.NodeType == XmlNodeType.Comment) continue;
                if (!IsSilentTag(xmlNode))
                {
                    return false;
                }
            }
            return true;
        }

        private static bool IsEmptyText(XmlNode node)
        {
            if (node.NodeType == XmlNodeType.Comment) return true;
            if (node.NodeType == XmlNodeType.Text)
            {
                string innerText = TextNodeValue(node);
                if (Trim(innerText).Length == 0)
                {
                    return true;
                }
                return false;
            }
            return false;
        }

        internal static string ForOutputTemplate(string sentenceIn)
        {
            return VisibleRendering(StaticAIMLUtils.getTemplateNode(sentenceIn).ChildNodes, TemplateSideRendering);
        }

        internal static string ForInputTemplate(string sentenceIn)
        {
            string patternSide =
                VisibleRendering(getNode(true, "<pattern>" + sentenceIn + "</pattern>").ChildNodes, PatternSideRendering);
            return ForOutputTemplate(patternSide);
        }

        const string SYMS = "\\/!@#$*()<>{}[]|;'!.?\t\n\r";
        static private char[] symc = SYMS.ToCharArray();
        static private readonly char[] symcs = (" "+SYMS).ToCharArray();
        
        public static List<string> SentenceBreaker(string sentence, Func<string, string> post)
        {
            var OutputSentences = new List<string>();
            if (string.IsNullOrEmpty(sentence)) return OutputSentences;
            sentence = StaticAIMLUtils.ReplacePairs(sentence, " />", "/>", " >", ">",
                                                    "<br>", "<br/>", "<p>", "<p/>", "</p>", "<p/>",
                                                    "<br/>", "\n", "<p/>", "\n",
                                                    "\r", "\n").Replace("\n", " \n ");
            if (sentence.Trim(symcs).Length == 0)
            {
                return OutputSentences;
            }
            bool dontBreak = false;
            if (sentence.StartsWith("<!--"))
            {
                string s1 = sentence;
                int endof = sentence.IndexOf("-->");
                if (endof == sentence.Length - 3)
                {
                    dontBreak = true;
                }
            }
            if (sentence.StartsWith("@"))
            {
                dontBreak = true;
            }
            else if (sentence.IndexOfAny(symc) == -1)
            {
                dontBreak = true;
            }
            if (dontBreak)
            {
                string s1 = sentence;
                if (post != null)
                {
                    s1 = post(s1);
                }
                if (!string.IsNullOrEmpty(s1))
                {
                    OutputSentences.Add(s1);
                }
                return OutputSentences;
            }        
            var sp = new HashSet<string>();
            sp.Add(".");
            sp.Add("!");
            sp.Add("?");
            foreach (var splitter in AltBot.Splitters)
            {
                sp.Add(splitter);
            }
            foreach (var s00 in sentence.Split('\n', '\r'))
            {
                var s0 = s00;
                while (s0.Trim().Length > 0)
                {
                    s0= StaticAIMLUtils.Trim(s0).Trim(' ', ',', '-', ' ', '.');
                    if (string.IsNullOrEmpty(s0)) continue;
                    bool usedBreaker = false;
                    foreach (string c in sp)
                    {
                        String NeedAfter = " ";
                        int ia = (s0 + NeedAfter).IndexOf(c + NeedAfter);
                        if (ia == -1) continue;
                        if (s0.Length > ia + 3)
                        {
                            string twochars = s0.Substring(ia, 2);
                            if (twochars == ".x")
                            {
                                continue;
                            }
                            if (twochars == "!-")
                            {
                                continue;
                            }
                            if (twochars == ": ")
                            {
                                continue;
                            }
                        }                        
                        if (ia > 1)
                        {
                            string s = s0.Substring(0, ia + 1);
                            s = CleanToEnglish(s);
                            if (!string.IsNullOrEmpty(s))
                            {
                                if (post != null)
                                {
                                    s = post(s);
                                }
                                if (!string.IsNullOrEmpty(s)) OutputSentences.Add(s);                                
                            }
                            usedBreaker = true;
                            s0 = s0.Substring(ia + 1);
                        }
                        //if (usedBreaker) break;
                    }
                    if (usedBreaker)
                    {
                        continue;
                    }
                    string s1 = CleanToEnglish(s0);
                    //if (string.IsNullOrEmpty(s1) && !s1.Contains("<")) continue;
                    if (post != null)
                    {
                        s1 = post(s1);
                    }
                    if (!string.IsNullOrEmpty(s1)) OutputSentences.Add(s1);
                    s0 = "";
                    continue;
                }
            }
            return OutputSentences;
        }

        private static string CleanToEnglish(string s0)
        {
            string sent2 = StaticAIMLUtils.ForOutputTemplate(s0);
            var s = StaticAIMLUtils.ToEnglish(sent2, null);
            s = StaticAIMLUtils.Trim(s).Trim(' ', ',', '-', ' ', ' ');
            return s;
        }


        static public string ToEnglish(string sentenceIn, ISettingsDictionary OutputSubstitutions)
        {
            var writeToLog = (OutputDelegate)null;
            if (sentenceIn == null)
            {
                return null;
            }
            sentenceIn = sentenceIn.Trim();
            if (sentenceIn == "")
            {
                return "";
            }
            var sentence = "";
            string xmlsentenceIn = ToEnglishT(sentenceIn);
            if (xmlsentenceIn == "")
            {
                return "";
            }
            sentence = ApplySubstitutions.Substitute(OutputSubstitutions, xmlsentenceIn);
            if (sentenceIn != sentence)
            {
                if (writeToLog!=null) writeToLog("SUBTS: " + sentenceIn + " -> " + sentence);
            }
            sentence = AltBot.CleanupCyc(sentence);
            sentence = ApplySubstitutions.Substitute(OutputSubstitutions, sentence);
            return sentence.Trim();
        }


        internal static string ToEnglishT(string sentenceIn)
        {
            string patternSide =
                VisibleRendering(getNode("<template>" + sentenceIn + "</template>").ChildNodes, PatternSideRendering);
            return ForOutputTemplate(patternSide);
        }

        public static int ReferenceCompare(Object thiz, Object other)
        {
            if (ReferenceEquals(thiz, other)) return 0;
            int cmpthis = RuntimeHelpers.GetHashCode(thiz);
            int cmpthat = RuntimeHelpers.GetHashCode(other);
            if (cmpthis == cmpthat) throw new InvalidCastException(thiz + " == " + other);
            return cmpthis.CompareTo(cmpthat);
        }

        public static int CollectionCompare<T>(IEnumerable thispath, IEnumerable thatpath, Func<T, T, int> comparer)
        {
            if (ReferenceEquals(thispath, thatpath)) return 0;
            if (ReferenceEquals(thispath, null))
            {
                if (ReferenceEquals(null, thatpath)) return 0;
                return -1;
            }
            if (ReferenceEquals(null, thatpath)) return 1;
            var thisE = thatpath.GetEnumerator();
            var thatE = thatpath.GetEnumerator();
            while (true)
            {
                bool nz = thisE.MoveNext();
                bool nt = thatE.MoveNext();
                if (!nz) return nt ? 0 : -1;
                if (!nt) return 1;
                T thispath1 = (T) thisE.Current;
                T thatpath1 = (T) thatE.Current;
                int diff = comparer(thispath1, thatpath1);
                if (diff != 0) return diff;
            }
        }

        public static int CollectionCompare<T>(IList<T> thispath, IList<T> thatpath, Func<T, T, int> comparer) //where T : IComparable<T>
        {
            if (ReferenceEquals(thispath, thatpath)) return 0;
            if (ReferenceEquals(thispath, null))
            {
                if (ReferenceEquals(null, thatpath)) return 0;
                return -1;
            }
            if (ReferenceEquals(null, thatpath)) return 1;
            int cmpthis = thispath.Count;
            int cmpthat = thatpath.Count;

            if (cmpthis == cmpthat)
            {
                double detailThis = 0;
                double detailThat = 0;
                for (int i = 0; i < cmpthis; i++)
                {
                    T thatpath1 = thatpath[i];
                    T thispath1 = thispath[i];
                    int diff = comparer(thispath1,thatpath1);
                    if (diff != 0) return diff;
                    detailThat += thatpath1.GetHashCode();
                    detailThis += thispath1.GetHashCode();
                }
                if (detailThat == detailThis)
                {
                    return ReferenceCompare(thispath, thispath);
                }
                return detailThat.CompareTo(detailThat);
            }
            return cmpthis.CompareTo(cmpthat);
        }

        public static bool CollectionEquals<T>(List<T> left, List<T> right)
        {
            int rightCount = right.Count;
            if (left.Count != rightCount) return false;
            for (int index = 0; index < rightCount; index++)
            {
                if (!Equals(left[index], right[index])) return false;
            }
            return true;
        }

        public static bool SetsEquals<T>(List<T> left, List<T> right)
        {
            return left.Count == right.Count && right.TrueForAll(left.Contains);
        }

        public static bool UnifyWithTextNode(XmlNode xmlNode, string srch)
        {
            string toUpper = ToUpper(" " + TextNodeValue(xmlNode, false) + " ");
            return toUpper.Contains(ToUpper(" " + srch + " "));
        }

    }
}