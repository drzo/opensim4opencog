using System;
using System.Collections;
using System.Collections.Generic;
using System.Configuration;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text.RegularExpressions;
using System.Xml;
using AIMLbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.AIMLTagHandlers;
using UPath = RTParser.Unifiable;
using StringAppendableUnifiable = RTParser.StringAppendableUnifiableImpl;
//using CategoryInfo = RTParser.Utils.TemplateInfo;

//using StringAppendableUnifiable = System.Text.StringBuilder;
using ThatInfo = System.Xml.XmlNode;
using ResponseInfo = System.Xml.XmlNode;

namespace RTParser.Utils
{
    public class GraphMaster // : QuerySettings
    {
        private static string _STAR_PATH;

        /// <summary>
        /// Should tags that make no output be placed in parallel Graphmaster
        /// </summary>
        [ApplicationScopedSetting]
        static public bool DefaultSilentTagsInPutParallel { get; set; }

        /// <summary>
        /// Should template Objects be stored in graphmasters
        /// </summary>
        [UserScopedSettingAttribute]
        public bool TrackTemplates { get { return StaticAIMLUtils.TrackTemplates; } }

        public static bool NoIndexing = false;
        private readonly List<GraphMaster> FallBacksGraphs = new List<GraphMaster>();
        private readonly String graphName;
        QuerySettingsImpl _forcedSettings;
        public QuerySettings ForcedSettings
        {
            get
            {
                if (_forcedSettings == null)
                {
                    _forcedSettings = new QuerySettingsImpl(QuerySettings.CogbotDefaults);
                }
                return _forcedSettings;
            }
        }

        /// <summary>
        /// All the &lt;guard&gt;s (if any) associated with this database
        /// </summary>
        private readonly List<GuardInfo> Guards = new List<GuardInfo>();

        private readonly Dictionary<string, DateTime> LoadedFiles = new Dictionary<string, DateTime>();

        private readonly List<GraphMaster> Parallels = new List<GraphMaster>();

        /// <summary>
        /// All the &lt;pattern&gt;s (if any) associated with this database
        /// </summary>
        private readonly Dictionary<String, PatternInfo> Patterns = new Dictionary<string, PatternInfo>();

        /// <summary>
        /// All the &lt;templates&gt;s (if any) associated with this database
        /// </summary>
        internal readonly Dictionary<string, ResponseInfo> ResponseInfos = new Dictionary<string, ResponseInfo>();

        /// <summary>
        /// All the &lt;that&gt;s (if any) associated with this database
        /// </summary>
        internal readonly Dictionary<String, ThatInfo> Thats = new Dictionary<string, ThatInfo>();

        /// <summary>
        /// All the &lt;topic&gt;s (if any) associated with this database
        /// </summary>
        internal readonly Dictionary<String, TopicInfo> Topics = new Dictionary<string, TopicInfo>();

        private GraphMaster _parallel;

        /// <summary>
        /// All the &lt;category&gt;s (if any) associated with this database
        /// </summary>
        private List<CategoryInfo> CategoryInfos;

        /// <summary>
        /// All the &lt;templates&gt;s (if any) associated with this database
        /// </summary>
        static internal readonly Dictionary<string, List<CategoryInfo>> FileCategories = new Dictionary<string, List<CategoryInfo>>();

        public bool DoParallels = true;
        private bool FullDepth = true;
        public bool IsBusy;
        public bool CannotHaveParallel = false;    

        private int parallel0;
        private Node PostParallelRootNode = new Node(null);
        private Node RootNode = new Node(null);
        [UserScopedSetting]
        public bool SilentTagsInPutParallel { get; set; }
        public int Size;
        public String Srai;
        public bool UnTraced;
        // ReSharper disable FieldCanBeMadeReadOnly.Local
        private List<TemplateInfo> UnusedTemplates;
        public static readonly Dictionary<string, XmlNode> PatternNodes = new Dictionary<string, XmlNode>();

        /// <summary>
        /// Search and try to elimentate duplicate Templates
        /// slows it down but maybe important to do
        /// </summary>
        public bool RemoveDupicateTemplatesFromNodes = true; 
        /// <summary>
        /// This is nomal AIML default (false = we we might rotate templates fopr a more interesting robot (false = cogbot normally))
        /// </summary>
        public bool RemovePreviousTemplatesFromNodes = false;  
        /// <summary>
        /// True = Normal Cogbot defualt (false would mean AIML templates vetted to make the robot respond)
        /// </summary>        
        public bool DistinguishSilenetTags = true;

        public bool IsParallel = false;

        static GraphMaster()
        {
            DefaultSilentTagsInPutParallel = false;
        }
        public GraphMaster(string gn)
            : this(gn, null, false)
        {
        }
        public GraphMaster(string gn, GraphMaster child, bool isParallel)
        //: base(bot)
        {
            IsParallel = isParallel;
            SilentTagsInPutParallel = DefaultSilentTagsInPutParallel;
            SilentTagsInPutParallel = false;
            CategoryInfos = TrackTemplates ? new List<CategoryInfo>() : null;
            //Templates = TrackTemplates ? new List<TemplateInfo>() : null;
            graphName = gn;

            // most graphs try to recuse on themselves until otehrwise stated (like in make-parallel)
            Srai = gn;
            RootNode.Graph = this;
            PostParallelRootNode.Graph = this;
            if (!TrackTemplates)
            {
                UnusedTemplates = null;
                //Templates = null;
                CategoryInfos = null;
            }
            //UnusedTemplates = new List<TemplateInfo>();
            if (isParallel)
            {
                IsParallel = true;
                // useless for parallel to have parallels
                CannotHaveParallel = true;
                // parallels are already silent
                SilentTagsInPutParallel = false;
                // parallels are "mutli-templated"
                RemovePreviousTemplatesFromNodes = false;
                // parallels only max out in "timeout"
                // = false;
            }
            else
            {
               // CanMaxOutStage1 = false;
                if (gn.Contains("parallel") || gn.Contains("parent"))
                {
                    throw new Exception("Parallel should use other constructor! " + gn);
                }
            }
        }

        // ReSharper restore FieldCanBeMadeReadOnly.Local

        public GraphMaster Parallel
        {
            get
            {
                if (ScriptingName.Contains("parallel") || ScriptingName.Contains("parent"))
                {
                    return this;
                }
                if (CannotHaveParallel)
                {
                    writeToLog("CantHaveParallels!");
                    return this;
                }
                if (_parallel == null)
                {
                    if (Parallels.Count > 0)
                    {
                        _parallel = Parallels[0];
                    }
                    else
                    {
                        _parallel = makeParallel();
                    }
                }
                return _parallel;
            }
        }

        public String ScriptingName
        {
            get { return graphName; }
        }

        public bool GraphsAcceptingUserInput
        {
            get
            {
                return true;
                //if (!theBot.isAcceptingUserInput)
                //{
                //    return false;
                //}
                //if (IsBusy)
                //{
                //    return false;
                //}
                //return true;
            }
            set
            {
                //if (!theBot.isAcceptingUserInput)
                //{
                //    theBot.isAcceptingUserInput = true;
                //}
                IsBusy = !value;
            }
        }

        public object LockerObject
        {
            get { return LoadedFiles; }
        }

        public static string STAR_PATH
        {
            get
            {
                if (_STAR_PATH == null)
                {
                    _STAR_PATH = "TAG-INPUT * TAG-THAT * TAG-TOPIC * TAG-FLAG *";
                    // ((RTPBot)null).Loader.generatePath("*", "*", "*", "*", false);
                }
                return _STAR_PATH;
            }
        }

        public PatternInfo FindPattern(XmlNode pattern, Unifiable unifiable)
        {
            if (NoIndexing) return null;
            LineInfoElementImpl lineInfoE = StaticXMLUtils.ToLineInfoElement(pattern);
            if (lineInfoE == null) lineInfoE = null;
            string pats = MakeMatchKey(unifiable);
            int skip = pats.IndexOf("TAG-THAT");
            if (skip > 0) pats = pats.Substring(0, skip - 1);
            else
            {
                skip = pats.IndexOf("TAG-FLAG");
                if (skip > 0) pats = pats.Substring(0, skip - 1);
            }
            PatternInfo pi;
            if (Patterns == null)
            {
                pi = new PatternInfo(lineInfoE, pats);
                return pi;
            }
            lock (LockerObject)
            {
                lock (Patterns)
                {
                    if (!Patterns.TryGetValue(pats, out pi))
                    {
                        Patterns[pats] = pi = new PatternInfo(lineInfoE, pats);
                    }
                    else
                    {
                        CheckMismatch(pi, pats);
                        return pi;
                    }
                }
            }
            return pi;
        }

        public ResponseInfo FindResponse(XmlNode responseNode, Unifiable responseText)
        {            
            if (NoIndexing) return null;
           // var responseNode = GetMatchableXMLNode("template", responseText);
            if (ResponseInfos == null) return new ResponseInfo(responseNode, responseText);
            string pats = MakeMatchKey(responseText);
            ResponseInfo pi;
            lock (LockerObject)
                lock (ResponseInfos)
                {
                    if (!ResponseInfos.TryGetValue(pats, out pi))
                    {
                        ResponseInfos[pats] =
                            pi = new ResponseInfo(responseNode, responseText);
                    }
                    else
                    {
                        CheckMismatch(pi, pats);
                        return pi;
                    }
                }
            return pi;
        }

        public ThatInfo FindThat(XmlNode thatNode,  Unifiable topicName)
        {
            if (NoIndexing) return null;
            thatNode = thatNode ?? GetMatchableXMLNode("that", topicName);
            if (Thats == null) return new ThatInfo(thatNode, topicName);
            string pats = MakeMatchKey(topicName);
            ThatInfo pi;
            lock (LockerObject)
                lock (Thats)
                {
                    if (!Thats.TryGetValue(pats, out pi))
                    {
                        Thats[pats] =
                            pi = new ThatInfo(thatNode, topicName);
                    }
                    else
                    {
                        CheckMismatch(pi, pats);
                        return pi;
                    }
                }
            return pi;
        }

        private string MakeMatchKey(Unifiable pattern)
        {
            string v = TextPatternUtils.MatchKeyClean(pattern.AsString()).ToUpper();
            if (v.Length < 1)
            {
                return "*";
            }
            return v;
        }

        private void CheckMismatch(GraphLinkInfo info, string pats)
        {
            if (info.FullPath.AsNodeXML().ToString().ToUpper() != pats.ToUpper())
            {
                string s = "CheckMismatch " + info.FullPath.AsNodeXML() + "!=" + pats;
                writeToLog(s);
                throw new Exception(s);
            }
        }
        public XmlNode GetMatchableXMLNode(string nodeName, Unifiable topicName)
        {
            var node = GetMatchableXMLNode0(nodeName, topicName);
            if (node != null) return node;
            return GetMatchableXMLNode0(nodeName, topicName);
        }
        public XmlNode GetMatchableXMLNode0(string nodeName, string topicName)
        {
            if (NoIndexing) return null;
            if (string.IsNullOrEmpty(topicName))
            {
                topicName = "*";
            }
            if (PatternNodes == null)
            {
                return StaticXMLUtils.getNode(String.Format("<{0}>{1}</{0}>", nodeName, topicName));
            }
            lock (PatternNodes)
            {
                string pats = MakeMatchKey(topicName + " " + nodeName);
                XmlNode pi;
                if (!PatternNodes.TryGetValue(pats, out pi))
                {
                if (pi != null) return pi;
                pi = PatternNodes[pats] = StaticXMLUtils.getNode(String.Format("<{0}>{1}</{0}>", nodeName, topicName));
                }
                return pi;
            }
        }

        public TopicInfo FindTopic(Unifiable topicName)
        {
            if (NoIndexing) return null;
            string pats = MakeMatchKey(topicName);
            TopicInfo pi;
            if (Topics == null)
            {
                return new TopicInfo(GetMatchableXMLNode("pattern", topicName), topicName);
            }
            lock (LockerObject)
                lock (Topics)
                {
                    if (!Topics.TryGetValue(pats, out pi))
                    {
                        Topics[pats] = pi = new TopicInfo(GetMatchableXMLNode("pattern", topicName), topicName);
                    }
                    else
                    {
                        CheckMismatch(pi, topicName.AsNodeXML().ToString());
                        return pi;
                    }
                }
            return pi;
        }

        public CategoryInfo FindCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename, 
            ResponseInfo template, GuardInfo guard, Node patternNode, object categoryInfo)
        {
            return TemplateInfo.MakeCategoryInfo(info, node, filename, template, guard, patternNode, categoryInfo);
        }

        private GraphMaster makeParallel()
        {
            if (CannotHaveParallel)
            {
                writeToLog("CantHaveParallels!");
                return this;
            }
            GraphMaster p = new GraphMaster("" + graphName + ".parallel" + (parallel0 == 0 ? "" : "" + parallel0), this, true);
            p.Srai = graphName;
            parallel0++;
            p.UnTraced = true;
            Parallels.Add(p);
            return p;
        }

        /// <summary>
        /// Saves the graphmaster node (and children) to a binary file to avoid processing the AIML each time the 
        /// Proccessor starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(Unifiable path)
        {
            // check to delete an existing version of the file
            FileInfo fi = new FileInfo(path);
            if (fi.Exists)
            {
                fi.Delete();
            }

            FileStream saveFile = HostSystem.Create(path);
            BinaryFormatter bf = new BinaryFormatter();
            bf.Serialize(saveFile, this.RootNode);
            bf.Serialize(saveFile, this.PostParallelRootNode);
            saveFile.Close();
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(Unifiable path)
        {
            Stream loadFile = HostSystem.OpenRead(path);
            BinaryFormatter bf = new BinaryFormatter();
            this.RootNode = (Node)bf.Deserialize(loadFile);
            this.PostParallelRootNode = (Node)bf.Deserialize(loadFile);
            loadFile.Close();
        }

        public List<CategoryInfo> addCategoryTag(Unifiable generatedPath, PatternInfo patternInfo, // out CategoryInfo category,
                                   XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo,
                                   List<ConversationCondition> additionalRules, out bool wouldBeRemoval, LoaderOptions loaderOptions)
        {
            lock (LockerObject)
            {
               return addCategoryTag0(generatedPath, patternInfo, /*category,*/ outerNode, templateNode, guard, thatInfo,
                                additionalRules, out wouldBeRemoval, loaderOptions);
            }
        }

        private List<CategoryInfo> addCategoryTag0(Unifiable generatedPath, PatternInfo patternInfo,// CategoryInfo category,
                                   XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo,
                                   List<ConversationCondition> additionalRules, out bool wouldBeRemoval, LoaderOptions loaderOptions)
        {
            if (SilentTagsInPutParallel && !StaticAIMLUtils.IsEmptyTemplate(templateNode) && StaticAIMLUtils.IsSilentTag(templateNode))
            {
                GraphMaster parallel1 = makeParallel();
                this.Parallels.Add(parallel1);
                parallel1.SilentTagsInPutParallel = false;
                //writeToLog("Adding to Parallel " + category);
                return parallel1.addCategoryTag(generatedPath, patternInfo, /*category,*/ outerNode, templateNode, guard, thatInfo,
                                       additionalRules, out wouldBeRemoval, loaderOptions);
            }

            Node rootNode = this.RootNode;
            if (IsStarStarStar(generatedPath))
            {
                rootNode = this.PostParallelRootNode;
            }
            else if (generatedPath.AsString().Contains("TAG-THAT * TAG-TOPIC * TAG-FLAG"))
            {
                rootNode = this.PostParallelRootNode;
            }
            else if (IsAnyStar(generatedPath) >= 2)
            {
                //rootNode = this.PostParallelRootNode;
                //writeToLog("Putting at end of queue " + generatedPath);
            }
            NodeAdder nodeAdder = null;
            Node thiz = rootNode.addPathNodeChilds(generatedPath, nodeAdder);

            int countBefore = thiz.TemplateInfoCount;

            XmlNode cateNode = StaticXMLUtils.FindNode("category", outerNode, null);
            var info0 = thiz.addTerminal(templateNode, cateNode, guard, thatInfo, loaderOptions, patternInfo,
                                                 additionalRules, out wouldBeRemoval);
            if (wouldBeRemoval)
            {
                Node other = rootNode == this.RootNode ? this.PostParallelRootNode : this.RootNode;
                Node thatz = other.addPathNodeChilds(generatedPath, nodeAdder);
                //writeToLog("Doing other removal: " + generatedPath);
                info0 = thatz.addTerminal(templateNode, cateNode, guard, thatInfo, loaderOptions, patternInfo,
                                          additionalRules, out wouldBeRemoval);
            }
            if (info0 != null)
            {
                foreach (var VARIABLE in info0)
                {
                    VARIABLE.GraphmasterNode = rootNode;
                }
            }
            int countAfter = thiz.TemplateInfoCount;
            /*
             * Node created = Node.addCategoryTag(node, generatedPath, patternInfo,
                                category, outerNode, templateNode, guard, thatInfo, this);*/
            int changed = countAfter - countBefore;
            if (changed == 0)
            {
                return info0;
            }
            if (changed < 0 || info0 == null)
            {
                return info0;
            }
            this.Size += changed;
            // keep count of the number of categories that have been processed
            return info0;
        }

        public override string ToString()
        {
            return "[Graph: " + graphName + ":" + Size + "]";
        }

        //query.Templates = 

        public GraphQuery gatherQueriesFromGraph(Unifiable path, Request request, MatchState state)
        {
            if (TextPatternUtils.IsNullOrEmpty(path))
            {
                string s = "ERROR! path.IsEmpty  returned no results for " + state + " in " + this;
                writeToLog(s);
                throw new Exception(s);
            }
            string ss = (string)path;
            if (ss.Contains("*"))
            {
                string s = "ERROR! path.HasWildCard!  returned no results for " + state + " in " + this;
                writeToLog(s);
                throw new Exception(s);
            }
            GraphQuery ql = new GraphQuery(path, request, this, state);
            ql.matchState = state;
            QuerySettings.ApplySettings(request, ql);
            request.TopLevelQuery = ql;
            lock (LockerObject)
                lock (request.Requester.AllQueries)
                {
                    if (!request.Requester.AllQueries.Contains(ql)) request.Requester.AllQueries.Add(ql);
                }
            return ql;
        }

        public void RunGraphQuery(GraphQuery ql)
        {
            MatchState state = ql.matchState;
            Unifiable path = ql.InputPath;
            Request request = ql.TheRequest;
            if (DoParallels) DoParallelEval(Parallels, request, request.rawInput);
            evaluateQL(path, request, state, ql, true);
            if (ql.TemplateCount == 0)
            {
                bool trace = request.IsTraced && !UnTraced;
                if (trace)
                    writeToLog(this + " returned no results for " + path);
                return;
            }
        }

        private void evaluateQL(Unifiable path, Request request, MatchState matchState, GraphQuery ql, bool locallyDoFallbacks)
        {
            bool trace = request.IsTraced && !UnTraced;
            while (getQueries(RootNode, path, request, matchState, 0, Unifiable.CreateAppendable(), ql))
            {
                if (ql.IsMaxedOut)
                {
                    break;
                }
                if (!((QuerySettingsReadOnly)request).ProcessMultiplePatterns)
                {
                    break;
                }
            }
            if (locallyDoFallbacks && FallBacksGraphs != null && !ql.IsMaxedOut)
            {
                foreach (GraphMaster graphMaster in CopyOf(FallBacksGraphs))
                {
                    graphMaster.evaluateQL(path, request, matchState, ql, !IsParallel);
                    if (ql.IsMaxedOutOrOverBudget)
                    {
                        if (trace)
                            writeToLog("using parallel templates from " + ql);
                        return;
                    }
                }
            }
            if (!ql.IsMaxedOut)
            {
                while (getQueries(PostParallelRootNode, path, request, matchState, 0, Unifiable.CreateAppendable(),
                                  ql))
                {
                    if (ql.IsMaxedOutOrOverBudget)
                    {
                        break;
                    }
                    if (!((QuerySettingsReadOnly)request).ProcessMultiplePatterns)
                    {
                        break;
                    }
                }
            }
            return;
        }


        public static bool IsStarStarStar(String bubble)
        {
            if (bubble == null) return false;
            string s = bubble;

            bool b = s.Trim().StartsWith(STAR_PATH);
            if (!b) return false;
            return b;
        }

        public static int IsAnyStar(String bubble)
        {
            int wideStar = 0;
            if (bubble == null) return wideStar;
            if (bubble.Contains("TAG-THAT * TAG")) wideStar++;
            if (bubble.Contains("TAG-INPUT * TAG")) wideStar++;
            if (bubble.Contains("TAG-TOPIC * TAG")) wideStar++;
            return wideStar;
        }
        /// <summary>
        // If we get a result from the branch process the wildcard matches and return 
        // the result
        /// </summary>
        /// <param name="result"></param>
        /// <param name="newWildcard"></param>
        /// <param name="mtchList"></param>
        /// <param name="query"></param>
        /// <returns></returns>
        private bool getQueries(Node rootNode, Unifiable upath, Request request, MatchState matchstate, int index,
                                StringAppendableUnifiableImpl wildcard, GraphQuery toplevel)
        {
            if (toplevel.DisallowedGraphs.Contains(this))
            {
                return false;
            }
            //  lock (LockerObject)
            {
                bool b = getQueries000(rootNode, upath, request, matchstate, index, wildcard, toplevel);
                if (toplevel.IsMaxedOut && toplevel.TemplateCount == 0)
                {
                    return false;
                }
                if (b) return true;
                return false;
            }
        }

        private bool getQueries000(Node rootNode, Unifiable upath, Request request, MatchState matchstate, int index,
                                   StringAppendableUnifiableImpl wildcard, GraphQuery toplevel)
        {
            int resin = toplevel.TemplateCount;
            int patternCountChanged = 0;
            int tried = 0;
            bool doIt = !toplevel.IsComplete(request);
            if (!doIt)
            {
                writeToLog("AIMLTRACE DOIT: " + tried + " pc=" + patternCountChanged + ": " + false + "  " + request);
                //   return false;
            }
            Proof Prf = new Proof();
            request.Proof = Prf;

            Node toplevelBubble;
            while (!toplevel.NoMoreResults)
            {
                int patternCount = toplevel.PatternCount;
                toplevelBubble = null;
                SubQuery query = new SubQuery(upath, request.CurrentResult, request);
                query.TopLevel = toplevel;
                Node pattern = rootNode.evaluate(Unifiable.ToVMString(upath), query, request, matchstate, wildcard);
                if (pattern != null && pattern.IsSatisfied(query))
                {
                    var tmplateInfos = pattern.TemplateInfoCopy;
                    if (toplevel.ContainsPattern(pattern))
                    {
                        toplevelBubble = pattern;
                        writeToLog("p=" + pattern);
                        toplevel.NoMoreResults = true;
                    }
                    else if (!pattern.disabled)
                    {
                        if (IsStarStarStar(ToString()))
                        {
                            toplevelBubble = pattern;
                            writeToLog("p=" + pattern);
                            toplevel.NoMoreResults = true;
                            //pattern.disabled = true;
                            break;
                        }
                        toplevelBubble = pattern;
                        pattern.disabled = true;
                        if (tmplateInfos != null && tmplateInfos.Count != 0)
                        {
                            toplevel.AddPattern(pattern);
                            query.Pattern = pattern;
                            toplevel.AddBindingSet(query);
                            foreach (TemplateInfo sol in tmplateInfos)
                            {
                                if (!sol.IsSatisfied(query))
                                {
                                    continue;
                                }
                                if (!request.CanUseRequestTemplate(sol))
                                {
                                    continue;
                                }
                                sol.Query = query;
                                query.CurrentTemplate = sol;
                                query.Templates.Add(sol);
                                toplevel.AddTemplate(sol);
                            }
                            if (query.Templates.Count==0)
                            {
                                
                            }
                        }
                    }
                    else
                    {
                        pattern.disabled = true;
                    }
                }

                if (toplevelBubble != null)
                {
                    toplevelBubble.disabled = true;
                    Prf.Add(toplevelBubble);
                }
                if (toplevel.PatternCount != patternCount)
                {
                    patternCountChanged++;
                }
                else
                {
                    tried++;
                }
                if (tried > 100)
                {
                    break;
                }
                bool toplevelIsComplete = toplevel.IsComplete(request);
                if (toplevelIsComplete)
                {
                    break;
                }
                if (toplevelBubble != null && IsStarStarStar(toplevelBubble.ToString()))
                {
                    toplevel.NoMoreResults = true;
                    break;
                }
            }
            {
                bool f = toplevel.TemplateCount > resin;
                bool sc = patternCountChanged > 0;
                if (f != sc)
                {
                    writeToLog("AIMLNODE: " + tried + " pc=" + patternCountChanged + ": " + f + "  " + request);
                }
                var PU = toplevel.PatternsUsed;
                if (PU != null)
                    foreach (Node list in PU)
                    {
                        list.disabled = false;
                    }
                return f;
            }
        }

        internal void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine("GRAPH: " + message + " in " + ToString(), args);
        }


        public void RemoveGenlMT(GraphMaster fallback, OutputDelegate writeToLog)
        {
            lock (LockerObject)
            {
                if (fallback == this)
                {
                    writeToLog("Trying to genlMt reversed to " + this);
                    FallBacksGraphs.Remove(fallback);
                    return;
                }
                lock (FallBacksGraphs)
                {
                    if (FallBacksGraphs.Contains(fallback))
                    {
                        FallBacksGraphs.Remove(fallback);
                        writeToLog("GENLMT REMOVING " + fallback + " FROM " + this);
                    }
                }
            }
        }

        internal void AddGenlMT(GraphMaster fallback, OutputDelegate writeToLog)
        {
            lock (LockerObject)
            {
                if (fallback == this)
                {
                    writeToLog("Trying to genlMt reversed to " + this);
                    return;
                }
                lock (FallBacksGraphs)
                {
                    if (!FallBacksGraphs.Contains(fallback))
                    {
                        FallBacksGraphs.Add(fallback);
                        writeToLog("GENLMT ADDING " + fallback + " TO " + this);
                    }
                    fallback.RemoveGenlMT(this, writeToLog);
                }
            }
        }

        public void AddParallelMT(GraphMaster doall, OutputDelegate writeToLog)
        {
            lock (LockerObject)
            {
                var parallels = Parallels;
                if (doall == this)
                {
                    writeToLog("Trying to PARALLEL reversed to " + this);
                    return;
                }
                lock (parallels)
                {
                    if (!parallels.Contains(doall))
                    {
                        parallels.Add(doall);
                        writeToLog("PARALLEL ADDING " + doall + " TO " + this);
                    }
                    doall.RemoveGenlMT(this, writeToLog);
                }
            }
        }


        public static IList<T> CopyOf<T>(List<T> list)
        {
            if (list == null) return new List<T>();
            lock (list)
            {
                return list.ToArray();
            }
        }

        public static IList<T> CopyOf<T>(IEnumerable<T> list)
        {
            var copy = new List<T>();
            if (list == null) return copy;
            lock (list)
            {
                copy.AddRange(list);
            }
            return copy;
        }

        private List<Result> DoParallelEval(List<GraphMaster> totry, Request request, Unifiable unifiable)
        {
            var pl = new List<Result>();
            RTPBot proc = request.TargetBot;
            foreach (GraphMaster p in CopyOf(totry))
            {
                if (request.IsTimedOutOrOverBudget) return pl;
                if (request.TopLevelQuery.DisallowedGraphs.Contains(p)) continue;
                if (p != null)
                {
                    GraphMaster g = request.Graph;
                    Result resBack = request.CurrentResult;
                    bool wasTopLevel = request.IsToplevelRequest;
                    bool wasUntraced = p.UnTraced;
                    bool userTracing = request.IsTraced;
                    try
                    {
                        if (wasUntraced) request.IsTraced = false;
                        if (wasTopLevel) request.IsToplevelRequest = false;

                        p.UnTraced = Size > 0;
                        
                        var req = new AIMLbot.MasterRequest(request.rawInput, request.Requester, request.TargetBot, null,
                                                          request.Responder);
                        req.OriginalSalientRequest = request.OriginalSalientRequest;
                        req.Graph = p;
                        req.IsToplevelRequest = false;
                        //req.CurrentResult = null;

                        AIMLbot.MasterResult r = (MasterResult) proc.ChatWithRequest(req);
                        
                        if (!r.IsEmpty) pl.Add(r);
                    }
                    finally
                    {
                        request.IsToplevelRequest = wasTopLevel;
                        p.UnTraced = wasUntraced;
                        request.Graph = g;
                        request.IsTraced = userTracing;
                        //request.CurrentResult = resBack;
                    }
                }
            }
            return pl;
        }

        public void AddTemplate(TemplateInfo templateInfo)
        {
            lock (LockerObject)
            {
                //if (Templates != null) lock (Templates) Templates.Add(templateInfo);
                if (CategoryInfos != null) lock (CategoryInfos)
                        CategoryInfos.Add(templateInfo.CategoryInfo);
                if (templateInfo.IsTraced || templateInfo.IsDisabled)
                {
                    if (UnusedTemplates != null) UnusedTemplates.Remove(templateInfo);
                }
            }
        }

        public void RemoveTemplate(TemplateInfo templateInfo)
        {
            if (templateInfo == null) return;
            //System.writeToLog("removing " + templateInfo.CategoryInfo.ToString());
            lock (LockerObject)
            {
                CategoryInfo categoryInfo = templateInfo.CategoryInfo;
                templateInfo.IsTraced = true;
                categoryInfo.IsDisabled = true;
                //if (Templates != null) lock (Templates) Templates.Remove(templateInfo);
                bool found = false;
                if (CategoryInfos != null)
                {
                    lock (CategoryInfos)
                    {
                        found = CategoryInfos.Remove(categoryInfo);
                    }
                }
                if (!found)
                {
                    if (UnusedTemplates != null)
                    {
                        lock (UnusedTemplates) UnusedTemplates.Remove(templateInfo);
                        categoryInfo.IsDisabled = false;
                    }
                }
            }
        }


        public void EnableTemplate(TemplateInfo templateInfo)
        {
            if (templateInfo == null) return;
            lock (LockerObject)
            {
                templateInfo.CategoryInfo.IsDisabled = false;
                bool found = true;
                if (UnusedTemplates != null)
                {
                    found = UnusedTemplates.Remove(templateInfo);
                }
                //if (Templates != null) lock (Templates) Templates.Remove(templateInfo);
                if (CategoryInfos != null) lock (CategoryInfos)
                        CategoryInfos.Add(templateInfo.CategoryInfo);
                templateInfo.IsTraced = !found;
            }
        }

        public void DisableTemplate(TemplateInfo templateInfo)
        {
            if (templateInfo == null) return;
            lock (LockerObject)
            {
                templateInfo.IsTraced = true;
                templateInfo.CategoryInfo.IsDisabled = true;
                //if (Templates != null) lock (Templates) Templates.Remove(templateInfo);
                if (CategoryInfos != null) lock (CategoryInfos)
                        CategoryInfos.Remove(templateInfo.CategoryInfo);
                if (UnusedTemplates != null) UnusedTemplates.Add(templateInfo);
            }
        }

        internal void WriteConfig()
        {
            lock (LockerObject)
            {
                lock (Topics)
                    foreach (KeyValuePair<string, TopicInfo> info in Topics)
                    {
                        writeToLog("topic = " + info.Key);
                    }
                lock (Thats)
                    foreach (KeyValuePair<string, ThatInfo> info in Thats)
                    {
                        writeToLog("that = " + info.Key);
                    }
            }
        }

        public void WriteToFile(string name, string filename, PrintOptions printOptions)
        {
            WriteToFile(name, filename, printOptions, writeToLog);
        }

        public void WriteToFile(string name, string filename, PrintOptions printOptions, OutputDelegate logger)
        {
            lock (LockerObject)
            {
                FileInfo fi = new FileInfo(filename);
                string di = fi.DirectoryName;
                HostSystem.CreateDirectory(di);
                HostSystem.BackupFile(filename);
                StreamWriter fs = new StreamWriter(filename, false);
                if (!printOptions.XMLWriterSettings.OmitXmlDeclaration)
                {
                    fs.WriteLine("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>");
                }

                fs.WriteLine("<aiml graph=\"{0}\">", name);
                printOptions.CurrentGraphName = name;
                var skipped = new List<CategoryInfo>(1000);
                var written = new List<CategoryInfo>(10000);
                try
                {
                    PrintToWriter(CopyOf(CategoryInfos), printOptions, fs, written);
                }
                catch (Exception e)
                {
                    writeToLog("ERROR {0}", e);
                    logger("ERROR {0}", e);
                    throw;
                }
                finally
                {
                    fs.WriteLine("</aiml>");
                    fs.Flush();
                    fs.Close();
                }
                logger("COMPLETE WRITTING " + this + " to " + filename + " written=" + CountOF(written) +
                       " skipped=" + CountOF(skipped) + " original=" + CountOF(CategoryInfos));
                this.CategoryInfos = written;
                Size = CategoryInfos.Count;
            }
        }

        public static void PrintToWriter(IEnumerable items, PrintOptions printOptions, TextWriter fs, IList written)
        {
            //string hide = "";
            if (items == null) return;
            foreach (IAIMLInfo ci in items)
            {
                string graphName = ci.Graph.graphName;
                if (printOptions.DontPrint(ci)) continue;
                string c = ci.ToFileString(printOptions);
                string cws = TextPatternUtils.CleanWhitepaces(c);
                if (printOptions.DontPrint(cws)) continue;

                if (printOptions.RemoveDuplicates)
                {
                    printOptions.Writting(ci);
                    printOptions.Writting(cws);
                }
                if (written != null) written.Add(ci);
                string ss = c.TrimEnd();
                if (printOptions.CleanWhitepaces)
                {
                    // ss = cws;
                }
                fs.Write(ss);
                if (printOptions.IncludeLineno || printOptions.IncludeGraphName)
                {
                    if (!printOptions.CategoryPerLine) if (ss.Length > 50) fs.WriteLine();
                    fs.Write("   <!-- ");
                    if (printOptions.IncludeGraphName)
                    {
                        fs.Write(graphName);
                    }
                    if (printOptions.IncludeLineno)
                    {
                        c = ci.SourceInfo();
                        if (!c.Contains("(0,0)"))
                        {
                            fs.Write(" " + c);
                        }
                    }
                    fs.WriteLine("-->");
                }
            }
        }

        public void WriteMetaHeaders(OutputDelegate fs, PrintOptions printOptions)
        {
            foreach (GraphMaster list in CopyOf(FallBacksGraphs))
            {
                fs(" <genlMt name=\"{0}\"/>", list.ScriptingName);
            }
            foreach (GraphMaster list in CopyOf(Parallels))
            {
                fs(" <!-- parallel name=\"{0}\" -->", list.ScriptingName);
            }
            string srai = Srai;
            if (srai != null)
                fs(" <sraiGraph name=\"{0}\" />", srai);
            if (printOptions.WriteStatistics)
            {
                fs(" <!-- categories={0} disabled={1} thats={2} patterns={3} topics={4} preparent={5} postparent={6}  -->",
                   CountOF(CategoryInfos), CountOF(UnusedTemplates), CountOF(Thats), CountOF(Patterns), CountOF(Topics), RootNode.ChildCount,
                   PostParallelRootNode.ChildCount);
            }
        }

        private static int CountOF(ICollection col)
        {
            if (col == null) return -1;
            return col.Count;
        }

        public void AddRedundantTemplate(TemplateInfo redundant, TemplateInfo info)
        {
            lock (LockerObject)
            {
                if (UnusedTemplates == null) return;
                lock (UnusedTemplates)
                {
                    //  Templates.Remove(redundant);
                    UnusedTemplates.Add(info);
                }
            }
        }

        internal void AddRedundantCate(CategoryInfo category, TemplateInfo temp)
        {
            //  throw new NotImplementedException();
        }

        public IList<CategoryInfo> GetCategoriesMatching(string match)
        {
            lock (LockerObject)
            {
                if (match == null || match == "*" || match == "" || match == ".*")
                {
                    return CopyOf(CategoryInfos);
                }
                var cats = new List<CategoryInfo>();
                foreach (CategoryInfo ci in CopyOf(CategoryInfos))
                {
                    if (ci.Matches(match))
                    {
                        cats.Add(ci);
                    }
                }
                return cats;
            }
        }

        public bool AddFileLoaded(string filename)
        {
            FileInfo fi = new FileInfo(filename);
            string fullName = fi.FullName;
            DateTime dt;
            lock (LockerObject)
            {
                lock (LoadedFiles)
                {
                    if (!LoadedFiles.TryGetValue(fullName, out dt))
                    {
                        LoadedFiles[fullName] = fi.LastWriteTime;
                        return true;
                    }
                    if (fi.LastWriteTime > dt)
                    {
                        LoadedFiles[fi.FullName] = fi.LastWriteTime;
                        return true;
                    }
                    return false;
                }
            }
        }

        public bool RemoveFileLoaded(string filename)
        {
            FileInfo fi = new FileInfo(filename);
            string fullName = fi.FullName;
            if (FileCategories!=null)
            {
                lock (FileCategories)
                {
                    List<CategoryInfo> categoryInfos;
                    if (FileCategories.TryGetValue(fullName, out categoryInfos))
                    {
                        if (categoryInfos!=null)
                        {
                            lock (categoryInfos)
                            {
                                foreach (CategoryInfo categoryInfo in categoryInfos)
                                {
                                    RemoveTemplate(categoryInfo.Template);                                    
                                }
                            }
                        }
                    }
                }
            }
            DateTime dt;
            lock (LockerObject)
            {
                lock (LoadedFiles)
                {
                    return LoadedFiles.Remove(fullName);
                }
            }
        }

        public bool IsFileLoaded(string filename)
        {
            FileInfo fi = new FileInfo(filename);
            string fullName = fi.FullName;
            DateTime dt;
            lock (LockerObject)
            {
                lock (LoadedFiles)
                {
                    if (!LoadedFiles.TryGetValue(fullName, out dt))
                    {
                        return false;
                    }
                    if (fi.LastWriteTime > dt)
                    {
                        return false;
                    }
                    return true;
                }
            }
        }

        public void Listing(OutputDelegate console, string match, PrintOptions printOptions)
        {
            lock (LockerObject)
            {
                GraphMaster G = this;
                var Cats = G.GetCategoriesMatching(match);
                console("-----------------------------------------------------------------");
                PrintToWriter(Cats, printOptions, new OutputDelegateWriter(console), null);
                console("-----------------------------------------------------------------");
                console("Shown " + Cats.Count + " from " + G);
                OutputDelegate When = (s, a) => { console(s, a); };
                G.WriteMetaHeaders(When, printOptions);
            }
        }


        public static bool Matches(string pattern, string target)
        {
            if (pattern == null || pattern == "*" || pattern == "") return true;
            ;
            if (target.Contains(pattern)) return true;
            return Regex.Matches(target, pattern).Count > 0;
        }

        #region Overrides of QuerySettings

        ///// <summary>
        ///// The Graph to start the query on
        ///// </summary>
        //public override string GraphName
        //{
        //    get { return Srai; }
        //    set
        //    {
        //        writeToLog("WARNING SETTING SRAI on " + this + " to " + value);
        //        Srai = value;
        //    }
        //}

        #endregion

        public bool DoGraphCommand(string cmd, OutputDelegate console, bool showHelp, string args, Request request)
        {
            PrintOptions printOptions = request.WriterOptions ?? PrintOptions.CONSOLE_LISTING;
            printOptions.ClearHistory();

            string match;
            string graphname;
            if (!TextPatternUtils.SplitOff(args, "-", out graphname, out match))
            {
                graphname = "current";
                match = ".*";
            }

            if (showHelp)
            {
                console("\n Example of <tmatch>s: ");
                console("\n  " + @"matches only default patterns:  ^\<category\>\<pattern\>\*\</pattern\>\<te");
                console("\n  matches all: .*");
            }

            if (showHelp) console("@ls <graph> - <tmatch>   --  lists all graph elements matching some elements");
            int foundResults = 0;
            if (cmd == "ls")
            {
                var matchingGraphs = request.GetMatchingGraphs(graphname, this);
                foreach (KeyValuePair<string, GraphMaster> ggg in matchingGraphs)
                {
                    string n = ggg.Key;
                    GraphMaster G = ggg.Value;
                    var cis = G.GetCategoriesMatching(match);
                    if (cis.Count == 0) continue;
                    foundResults += cis.Count;
                    console("-----------------------------------------------------------------");
                    console("LISTING: count=" + cis.Count + " local=" + G + " key='" + n + "'");
                    G.Listing(console, match, printOptions);
                    console("-----------------------------------------------------------------");
                }
                console(cmd + ": foundResults=" + foundResults);
                return true;
            }

            if (showHelp) console("@disable <graph> - <tmatch>   --  disables all graph elements matching <tmatch>");
            if (cmd == "disable")
            {
                var matchingGraphs = request.GetMatchingGraphs(graphname, this);
                foreach (var ggg in matchingGraphs)
                {
                    string n = ggg.Key;
                    GraphMaster G = ggg.Value;
                    var cis = G.GetCategoriesMatching(match);
                    if (cis.Count == 0) continue;
                    foundResults += cis.Count;
                    console("-----------------------------------------------------------------");
                    console("DISABLE: count=" + cis.Count + " local=" + G + " key='" + n + "'");
                    foreach (var ci in cis) G.DisableTemplate(ci.Template);
                }
                console(cmd + ": foundResults=" + foundResults);
                return true;
            }

            if (showHelp) console("@clear <graph> - <tmatch>   --  clears all graph elements matching <tmatch>");
            if (cmd == "clear")
            {
                var matchingGraphs = request.GetMatchingGraphs(graphname, this);
                foreach (var ggg in matchingGraphs)
                {
                    string n = ggg.Key;
                    GraphMaster G = ggg.Value;
                    var cis = G.GetCategoriesMatching(match);
                    if (cis.Count == 0) continue;
                    foundResults += cis.Count;
                    console("-----------------------------------------------------------------");
                    console("CLEAR: count=" + cis.Count + " local=" + G + " key='" + n + "'");
                    foreach (var ci in cis) G.RemoveTemplate(ci.Template);
                }
                console(cmd + ": foundResults=" + foundResults);
                return true;
            }

            if (showHelp) console("@tmatch <graph> - <tmatch> @eval <code>  --  runs code on all graph elements matching <tmatch>");
            if (cmd == "tmatch")
            {
                var matchingGraphs = request.GetMatchingGraphs(graphname, this);
                string newMatch;
                string newCmd;
                if (!TextPatternUtils.SplitOff(match, "@", out newMatch, out newCmd))
                {
                    newMatch = match;
                    newCmd = "@eval System.Out.WriteLine ";
                }
                foreach (var ggg in matchingGraphs)
                {
                    string n = ggg.Key;
                    GraphMaster G = ggg.Value;
                    var cis = G.GetCategoriesMatching(newMatch);
                    if (cis.Count == 0) continue;
                    foundResults += cis.Count;
                    console("-----------------------------------------------------------------");
                    console("TMATCH: count=" + cis.Count + " local=" + G + " key='" + n + "' cmd=" + newCmd);
                    foreach (var ci in cis)
                    {
                        ci.IsTraced = true;
                    }
                }
                console(cmd + ": foundResults=" + foundResults);
                return true;
            }
            return false;
        }

    }
}