using System;
using System.Collections;
using System.Collections.Generic;
using System.Configuration;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using System.Xml;
using AIMLbot;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.AIMLTagHandlers;
using UPath = RTParser.Unifiable;
using PatternInfo = RTParser.Unifiable;
using ThatInfo = RTParser.Unifiable;
using TopicInfo = RTParser.Unifiable;
using GuardInfo = RTParser.Unifiable;
using ResponseInfo = RTParser.Unifiable;
using System.Threading;
using System.Text;
using UNode = AltAIMLbot.Utils.Node;

//using StringAppendableUnifiable = System.Text.StringBuilder;

namespace RTParser.Utils
{
    [Serializable]
    public class GraphMaster : ParentChild
    {
        private static string _STAR_PATH;

        public ExternDB chatDB = null;
        /// <summary>
        /// Should tags that make no output be placed in parallel Graphmaster
        /// </summary>
        [ApplicationScopedSetting]
        static public bool DefaultSilentTagsInPutParallel { get; set; }

        /// <summary>
        /// Should template Objects be stored in graphmasters
        /// </summary>
        [UserScopedSetting]
        public bool TrackTemplates { get { return StaticAIMLUtils.TrackTemplates; } }

        public List<string> GraphNames
        {
            get
            {
                var names = new List<string>();
                //lock (AltBot.GraphsByName)
                {
                    foreach (KeyValuePair<string, GraphMaster> pair in LockInfo.CopyOf(AltBot.GraphsByName))
                    {
                        if (pair.Value == this)
                        {
                            names.Add(pair.Key);
                        }
                    }
                }
                return names;
            }
        }
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
        private readonly List<Unifiable> Guards = new List<Unifiable>();

        private readonly Dictionary<string, DateTime> LoadedFiles = new Dictionary<string, DateTime>();

        private readonly List<GraphMaster> Parallels = new List<GraphMaster>();

        /// <summary>
        /// All the &lt;pattern&gt;s (if any) associated with this database
        /// </summary>
        private readonly Dictionary<String, Unifiable> Patterns = new Dictionary<string, Unifiable>();

        /// <summary>
        /// All the &lt;templates&gt;s (if any) associated with this database
        /// </summary>
        internal readonly Dictionary<string, Unifiable> ResponseInfos = new Dictionary<string, Unifiable>();

        /// <summary>
        /// All the &lt;that&gt;s (if any) associated with this database
        /// </summary>
        internal readonly Dictionary<String, Unifiable> Thats = new Dictionary<string, Unifiable>();

        /// <summary>
        /// All the &lt;topic&gt;s (if any) associated with this database
        /// </summary>
        internal readonly Dictionary<String, Unifiable> Topics = new Dictionary<string, Unifiable>();

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
        //private Node PostParallelRootNode;

        private UNode RootNode
        {
            get { throw new NotImplementedException("use ExternDB"); }
        }
        [UserScopedSetting]
        public bool SilentTagsInPutParallel { get; set; }
        public int Size;
        public String Srai;
        public bool UnTraced;
        // ReSharper disable FieldCanBeMadeReadOnly.Local
        private List<TemplateInfo> UnusedTemplates;
        [NonSerialized]
        public static Dictionary<string, XmlNode> PatternNodes = new Dictionary<string, XmlNode>();

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
        public bool CompareTemplatesForReferenceIdentity = true;

        static GraphMaster()
        {
            DefaultSilentTagsInPutParallel = false;
        }

        private AltBot theBot = null;
        public GraphMaster(string gn, AltBot _bot)
            : this(gn, null, false)
        {
            theBot = _bot;
        }
        private GraphMaster(string gn, GraphMaster child, bool isParallel)
        //: base(bot)
        {
            AltBot.GraphsByName[gn] = this;
            IsParallel = isParallel;
            SilentTagsInPutParallel = DefaultSilentTagsInPutParallel;
            SilentTagsInPutParallel = false;
            CategoryInfos = TrackTemplates ? new List<CategoryInfo>() : null;
            //Templates = TrackTemplates ? new List<TemplateInfo>() : null;
            graphName = gn;

            // most graphs try to recuse on themselves until otehrwise stated (like in make-parallel)
            Srai = gn;
            //RootNode = new Node(null, Unifiable.Empty);
            //RootNode.Graph = this;
            //PostParallelRootNode = new Node(this, Unifiable.Empty);
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
                    throw new NotImplementedException("Parallel should use other constructor! " + gn);
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
                    // ((AltBot)null).Loader.generatePath("*", "*", "*", "*", false);
                }
                return _STAR_PATH;
            }
        }

        public Unifiable FindPattern(XmlNode pattern, Unifiable unifiable)
        {
            if (NoIndexing) return null;
            string pats = unifiable;
            int skip = pats.IndexOf("TAG-THAT");
            if (skip > 0) pats = pats.Substring(0, skip);
            else
            {
                skip = pats.IndexOf("TAG-FLAG");
                if (skip > 0) pats = pats.Substring(0, skip);
            }
            if (pats.StartsWith("TAG-INPUT ")) pats = pats.Substring(10);
            pats = MakeMatchKey(pats);
            return pats;
#if false
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
#endif
        }

        public Unifiable FindResponse(XmlNode responseNode, Unifiable responseText)
        {
#if false
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
#endif
            return responseText;
        }

        public Unifiable FindThat(XmlNode thatNode, Unifiable topicName)
        {
            if (NoIndexing) return null;
            return topicName;
#if false
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
#endif
        }

        private string MakeMatchKey(string pattern)
        {
            string v = StaticXMLUtils.ToUpper(TextPatternUtils.MatchKeyClean(pattern).ToUpper());
            if (v.Length < 1)
            {
                return "*";
            }
            return v;
        }

        private void CheckMismatch(GraphLinkInfo info, string pats)
        {
            throw new NotImplementedException();
/*
            if (info.FullPath.AsNodeXML().ToString().ToUpper() != pats.ToUpper())
            {
                string s = "CheckMismatch " + info.FullPath.AsNodeXML() + "!=" + pats;
                writeToLog(s);
                throw new Exception(s);
            }
        */
        }

        private XmlNode GetMatchableXMLNode(string nodeName, Unifiable topicName)
        {
            var node = GetMatchableXMLNode0(nodeName, topicName);
            if (node != null) return node;
            return GetMatchableXMLNode0(nodeName, topicName);
        }
        private XmlNode GetMatchableXMLNode0(string nodeName, string topicName)
        {
            if (NoIndexing) return null;
            if (String.IsNullOrEmpty(topicName))
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
                    pi =
                        PatternNodes[pats] =
                        StaticXMLUtils.getNode(String.Format("<{0}>{1}</{0}>", nodeName, topicName));
                }
                return pi;
            }
        }

        public Unifiable FindTopic(Unifiable topicName)
        {
            if (NoIndexing) return null;
            return topicName;
#if false
            string pats = MakeMatchKey(topicName);
            TopicInfo pi;
            if (Topics == null)
            {
                return new TopicInfo(null, topicName);
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
#endif
        }

        public CategoryInfo FindCategoryInfo(Unifiable info, XmlNode node, LoaderOptions filename, XmlNode templateNode,
            Unifiable template, Unifiable guard, Unifiable topicInfo, Node patternNode, Unifiable thatInfo, IEnumerable<ConversationCondition> conds)
        {
            return CategoryInfoImpl1.MakeCategoryInfo(info, node, filename, templateNode, template, guard, topicInfo, patternNode,
                                                 thatInfo, conds);
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
        public void saveToBinaryFile(string path, BinaryFormatter bf)
        {
            // check to delete an existing version of the file
            FileInfo fi = new FileInfo(path);
            if (fi.Exists)
            {
                fi.Delete();
            }

            FileStream saveFile = HostSystem.Create(path);
            
            try
            {
                //var testO = this.RootNode.AllDecendantTemplates[0];
                //bf.Serialize(saveFile, testO);
                bf.Serialize(saveFile, this.RootNode);
                //bf.Serialize(saveFile, this.PostParallelRootNode);

            }
            finally
            {
                saveFile.Close();                
            }
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(string path, BinaryFormatter bf)
        {
            var r = this.RootNode; // throw the not impl
            Stream loadFile = HostSystem.OpenRead(path);
            //this.RootNode = (Node)bf.Deserialize(loadFile);
            ////this.PostParallelRootNode = (Node)bf.Deserialize(loadFile);
            loadFile.Close();
        }

        public List<CategoryInfo> addCategoryTag(Unifiable generatedPath, Unifiable patternInfo, // out CategoryInfo category,
                                   XmlNode categoryNode, XmlNode templateNode, Unifiable guard, Unifiable topicInfo, Unifiable thatInfo,
                                   List<ConversationCondition> additionalRules, out bool wouldBeRemoval, LoaderOptions loaderOptions)
        {
            lock (LockerObject)
            {
                return addCategoryTag0(generatedPath, patternInfo, /*category,*/ categoryNode, templateNode, guard, topicInfo, thatInfo,
                                 additionalRules, out wouldBeRemoval, loaderOptions);
            }
        }

        private List<CategoryInfo> addCategoryTag0(Unifiable generatedPath, Unifiable patternInfo,// CategoryInfo category,
                                   XmlNode categoryNode, XmlNode templateNode, Unifiable guard, Unifiable topicInfo, Unifiable thatInfo,
                                   List<ConversationCondition> additionalRules, out bool wouldBeRemoval, LoaderOptions loaderOptions)
        {
            if (SilentTagsInPutParallel && !StaticAIMLUtils.IsEmptyTemplate(templateNode) && StaticAIMLUtils.IsSilentTag(templateNode))
            {
                GraphMaster parallel1 = makeParallel();
                this.Parallels.Add(parallel1);
                parallel1.SilentTagsInPutParallel = false;
                //writeToLog("Adding to Parallel " + category);
                return parallel1.addCategoryTag(generatedPath, patternInfo, /*category,*/ categoryNode, templateNode, guard,
                                                topicInfo, thatInfo,
                                                additionalRules, out wouldBeRemoval, loaderOptions);
            }

            UNode rootNode = this.RootNode;
            if (IsStarStarStar(generatedPath))
            {
              //  rootNode = this.PostParallelRootNode;
            }
            else if (generatedPath.AsString().Contains("TAG-THAT * TAG-TOPIC * TAG-FLAG"))
            {
              //  rootNode = this.PostParallelRootNode;
            }
            else if (IsAnyStar(generatedPath) >= 2)
            {
                //rootNode = this.PostParallelRootNode;
                //writeToLog("Putting at end of queue " + generatedPath);
            }
            NodeAdder nodeAdder = null;
            UNode thiz = rootNode.addPathNodeChilds(generatedPath, nodeAdder);

            int countBefore = thiz.TemplateInfoCount;

            XmlNode cateNode = categoryNode ?? StaticXMLUtils.FindNodeOrHigher("category", templateNode, null);
            var info0 = thiz.addTerminal(templateNode, cateNode, guard, topicInfo, thatInfo, loaderOptions, patternInfo,
                                                 additionalRules, out wouldBeRemoval);
            if (wouldBeRemoval)
            {
                UNode other = rootNode;// == this.RootNode ? this.PostParallelRootNode : this.RootNode;
                UNode thatz = other.addPathNodeChilds(generatedPath, nodeAdder);
                //writeToLog("Doing other removal: " + generatedPath);
                info0 = thatz.addTerminal(templateNode, cateNode, guard, topicInfo, thatInfo, loaderOptions, patternInfo,
                                          additionalRules, out wouldBeRemoval);
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
            //QuerySettings.ApplySettings(request, ql);
            request.TopLevelQuery = ql;
#if DEBUG_ALLQUERIES

            lock (LockerObject)
                lock (request.Requester.DuringProcessing.AllQueries)
                {
                    if (!request.Requester.AllQueries.Contains(ql)) request.Requester.AllQueries.Add(ql);
                }
#endif
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
            int templatesStart = ql.TemplateCount;
            while (getQueries(RootNode, path, request, matchState, 0, Unifiable.CreateAppendable(), ql))
            {
                if (ql.IsMaxedOut)
                {
                    break;
                }
                if (!((QuerySettingsReadOnly) request).ProcessMultiplePatterns)
                {
                    break;
                }
            }
            int newtemplates0 = ql.TemplateCount - templatesStart;
            if (ql.IsMaxedOut)
            {
                if (trace)
                    writeToLog("Maxed out " + ql);
                return;
            }
            /*while (getQueries(PostParallelRootNode, path, request, matchState, 0, Unifiable.CreateAppendable(),
                              ql))
            {
                if (ql.IsMaxedOutOrOverBudget)
                {
                    break;
                }
                if (!((QuerySettingsReadOnly) request).ProcessMultiplePatterns)
                {
                    break;
                }
            }*/
            int newtemplates1 = ql.TemplateCount - templatesStart;
            if (ql.IsMaxedOut)
            {
                if (trace)
                    writeToLog("Maxed out " + ql);
                return;
            }
            if (locallyDoFallbacks && FallBacksGraphs != null && FallBacksGraphs.Count > 0)
            {
                foreach (GraphMaster graphMaster in CopyOf(FallBacksGraphs))
                {
                    graphMaster.evaluateQL(path, request, matchState, ql, !IsParallel);
                    if (ql.IsMaxedOutOrOverBudget)
                    {
                        if (trace)
                            writeToLog("only using parallel templates from " + ql);
                        return;
                    }
                }
            }

            return;
        }


        public static bool IsStarStarStar(String bubble)
        {
            if (bubble == null) return false;
            string s = bubble;

            bool b = StaticXMLUtils.Trim(s).StartsWith(STAR_PATH);
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
            if (!toplevel.IsAllowedGraph(this))
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
            request.TimeOutFromNow = TimeSpan.FromSeconds(3);
            request.Requester.ProofTemplates.Clear();
            writeToLog("GETQUERIES: " + request);
            //clearDiabled(rootNode);
            //request.ResetValues(false);
            //request.ParentMostRequest.ResetValues(false);
            if (!doIt)
            {
                writeToLog("AIMLTRACE DOIT: " + tried + " pc=" + patternCountChanged + ": " + false + "  " + request);
                //   return false;
            }
            var Prf = request.Proof = request.Proof ?? new Proof();

            UNode toplevelBubble;
            try
            {
                while (!toplevel.NoMoreResults)
                {
                    int patternCount = toplevel.PatternCount;
                    toplevelBubble = null;
                    SubQuery query = new SubQuery(upath, request.CurrentResult, request);
                    query.TopLevel = toplevel;
                    var wildcardsb = new StringBuilder();
                    wildcardsb.Append(wildcard.AsString());
                    UNode pattern = rootNode.evaluateU(Unifiable.ToVMString(upath), query, request, matchstate, wildcardsb);
                    wildcard = new StringAppendableUnifiableImpl();
                    wildcard.Append(wildcardsb.ToString());
                    if (pattern != null)
                    {
                        pattern.disabled = true;
                        if (pattern.IsSatisfied(query) && !toplevel.ContainsPattern(pattern))
                        {
                            var tmplateInfos = pattern.TemplateInfoCopy;
                            toplevelBubble = pattern;
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
                            }
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
                    if (tried > 10)
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
                       // toplevel.NoMoreResults = true;
                       // break;
                    }
                }

                bool f = toplevel.TemplateCount > resin;
                bool sc = patternCountChanged > 0;
                if (f != sc)
                {
                    writeToLog("AIMLNODE: " + tried + " pc=" + patternCountChanged + ": " + f + "  " + request);
                }
                return f;
            } catch(Exception e)
            {
                writeToLog("GATHER " + e);
                throw e;
            }

            finally
            {
                var PU = toplevel.PatternsUsed;
                if (PU != null)
                    foreach (UNode list in PU)
                    {
                        list.disabled = false;
                    }
               /* foreach (Node list in disabledPatterns)
                {
                    list.disabled = false;
                }
                */
            }
        }

        private void clearDiabled(UNode rootNode)
        {
            foreach (UNode list in rootNode.AllDecendants)
            {
                if (list.disabled)
                {
                    list.disabled = false;
                }
            }
            foreach (var list in rootNode.AllDecendantTemplates)
            {
                if (list.IsDisabled)
                {
                    list.IsDisabled = false;
                }
            }
        }

        internal void writeToLog(string message, params object[] args)
        {
            AltBot.writeDebugLine("GRAPH: " + message + " in " + ToString(), args);
        }


        public void RemoveGenlMT(GraphMaster fallback, OutputDelegate writeToLog)
        {
            lock (LockerObject)
            {
                if (fallback == this)
                {
                    writeToLog("Trying to RemoveGenlMT self: " + this);
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
                    writeToLog("Trying to AddGenlMT self: " + this);
                    return;
                }
                lock (FallBacksGraphs)
                {
                    if (!FallBacksGraphs.Contains(fallback))
                    {
                        writeToLog("GENLMT NOT ADDING " + fallback + " TO " + this);
                        return;
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
                        writeToLog("PARALLEL NOT ADDING " + doall + " TO " + this);
                        return;
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

        public static IDictionary<K, V> CopyOf<K, V>(IDictionary<K, V> list)
        {
            var copy = new Dictionary<K, V>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var kv in list)
                {
                    copy.Add(kv.Key, kv.Value);
                }
            }
            return copy;
        }

        private List<Result> DoParallelEval(List<GraphMaster> totry, Request request, Unifiable unifiable)
        {
            var pl = new List<Result>();
            AltBot proc = request.TargetBot;
            foreach (GraphMaster p in CopyOf(totry))
            {
                if (request.IsTimedOutOrOverBudget) return pl;
                if (!request.IsAllowedGraph(p)) continue;
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
                        var req = request.CreateSubRequest(request.rawInput, p);
                        req.OriginalSalientRequest = request.OriginalSalientRequest;
                        req.Graph = p;
                        req.IsToplevelRequest = false;
                        //req.CurrentResult = null;

                        AIMLbot.MasterResult r = (MasterResult)proc.ChatWithRequest(req);

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
                SetDisabled(templateInfo, false);
            }
        }

        public void RemoveTemplate(TemplateInfo templateInfo)
        {
            if (templateInfo == null) return;           
            //System.writeToLog("removing " + templateInfo.CategoryInfo.ToString());
            lock (LockerObject)
            {
                bool found = true;
                CategoryInfo categoryInfo = templateInfo.CategoryInfo;
                templateInfo.IsTraced = true;
                if (templateInfo.InGraph == this)
                {
                    found = false;
                    templateInfo.InGraph = null;
                }
                categoryInfo.IsDisabled = true;
                //if (Templates != null) lock (Templates) Templates.Remove(templateInfo);
                if (CategoryInfos != null)
                {
                    lock (CategoryInfos)
                    {
                        found = CategoryInfos.Remove(categoryInfo);
                        Size = CategoryInfos.Count;
                    }
                }
                if (!found)
                {
                    if (UnusedTemplates != null)
                    {
                        lock (UnusedTemplates)
                        {
                            found = UnusedTemplates.Remove(templateInfo);
                        }
                        categoryInfo.IsSearchDisabled = false;
                    }
                }
                if (found)
                {
                    writeToLog("removed " + templateInfo);
                }
            }
        }


        internal void SetDisabled(TemplateInfo templateInfo, bool value)
        {
            lock (LockerObject)
            {
                if (value)
                {
                    bool found = false;
                    if (this.CategoryInfos != null)
                    {
                        if (templateInfo.InGraph == this)
                        {
                            found = this.CategoryInfos.Remove(templateInfo);
                        }
                        //if (found && this.CategoryInfos.Count == 0) this.CategoryInfos = null;
                    }
                    if (this.UnusedTemplates == null) return;
                    this.UnusedTemplates = this.UnusedTemplates ?? new List<TemplateInfo>();
                    if (!found && this.UnusedTemplates.Contains(templateInfo)) return;
                    this.UnusedTemplates.Add(templateInfo);
                }
                else
                {
                    bool found = false;
                    if (this.UnusedTemplates != null)
                    {
                        found = this.UnusedTemplates.Remove(templateInfo);
                        //if (found && this.UnusedTemplates.Count == 0) this.UnusedTemplates = null;
                    }
                    if (this.CategoryInfos == null) return;
                    this.CategoryInfos = this.CategoryInfos ?? new List<CategoryInfo>();
                    bool inGraph = templateInfo.InGraph == this;
                    if (!found && inGraph && this.CategoryInfos.Contains(templateInfo)) return;
                    if (!inGraph)
                    {
                        templateInfo.InGraph = this;
                        this.CategoryInfos.Add(templateInfo);
                    }
                }
            }
        }

        public void EnableTemplate(TemplateInfo templateInfo)
        {
            if (templateInfo == null) return;
            lock (LockerObject)
            {
                templateInfo.IsDisabled = false;
            }
        }

        public void DisableTemplate(TemplateInfo templateInfo)
        {
            if (templateInfo == null) return;
            lock (LockerObject)
            {
                templateInfo.IsTraced = true;
                templateInfo.IsDisabled = true;
            }
        }

        internal void WriteConfig()
        {
            lock (LockerObject)
            {
                lock (Topics)
                    foreach (KeyValuePair<string, Unifiable> info in Topics)
                    {
                        writeToLog("topic = " + info.Key);
                    }
                lock (Thats)
                    foreach (KeyValuePair<string, Unifiable> info in Thats)
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
                    PrintToWriter(CopyOf(CategoryInfos), printOptions, fs, written, TimeSpan.Zero);
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

        public static void PrintToWriter(IEnumerable items, PrintOptions printOptions, TextWriter fs, IList written, TimeSpan sleepBetween)
        {
            //string hide = "";
            if (items == null) return;
            foreach (var cio in items)
            {
                if (sleepBetween > TimeSpan.Zero) Thread.Sleep(sleepBetween);
                IAIMLInfo ci = cio as IAIMLInfo;
                if (ci == null)
                {
                    fs.WriteLine("" + cio);
                    continue;
                }
                string graphName = ci.Graph.graphName;
                if (printOptions.DontPrint(ci)) continue;
                string c = ci.ToFileString(printOptions);
                string cws = StaticXMLUtils.CleanWhitepaces(c);
                if (printOptions.DontPrint(cws)) continue;

                if (printOptions.RemoveDuplicates)
                {
                    printOptions.Writting(c);
                    if (cws != c) printOptions.Writting(cws);
                }
                if (written != null) written.Add(ci);
                string ss = c.TrimEnd();
                if (printOptions.CleanWhitepaces)
                {
                    // ss = cws;
                }
                fs.Write(ss);
                if (printOptions.IncludeLineInfoExternal || printOptions.IncludeGraphName)
                {
                    if (!printOptions.CategoryPerLine) if (ss.Length > 50) fs.WriteLine();
                    fs.Write("   <!-- ");
                    if (printOptions.IncludeGraphName)
                    {
                        fs.Write(graphName);
                    }
                    if (printOptions.IncludeLineInfoExternal)
                    {
                        c = ci.SourceInfo();
                        if (!c.Contains("(0,0)"))
                        {
                            fs.Write(" " + c);
                        }
                    }
                    fs.WriteLine("-->");
                }
                Application.DoEvents();
            }
        }

        public void WriteMetaHeaders(OutputDelegate fs, PrintOptions printOptions)
        {
            foreach (GraphMaster list in CopyOf(FallBacksGraphs))
            {
                fs(" <genlMt name=\"{0}\" />", list.ScriptingName);
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
                /*fs(" <!-- categories={0} disabled={1} thats={2} patterns={3} topics={4} preparent={5} postparent={6}  -->",
                   CountOF(CategoryInfos), CountOF(UnusedTemplates), CountOF(Thats), CountOF(Patterns), CountOF(Topics), RootNode.ChildCount,
                   PostParallelRootNode.ChildCount);*/
                fs(" <!-- categories={0} disabled={1} -->",
                   CountOF(CategoryInfos), CountOF(UnusedTemplates)
                    // CountOF(Thats), CountOF(Patterns), CountOF(Topics), RootNode.ChildCount,
                    //PostParallelRootNode.ChildCount
                    );
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
            if (FileCategories != null)
            {
                lock (FileCategories)
                {
                    List<CategoryInfo> categoryInfos;
                    if (FileCategories.TryGetValue(fullName, out categoryInfos))
                    {
                        if (categoryInfos != null)
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
                PrintToWriter(Cats, printOptions, new OutputDelegateWriter(console), null, printOptions.SleepBetween);
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
            PrintOptions printOptions = PrintOptions.CONSOLE_LISTING;
            if (request != null && request.WriterOptions != null) printOptions = request.WriterOptions;
            printOptions.ClearHistory();

            string match0;
            string graphname;
            if (!TextPatternUtils.SplitOff(args, "-", out graphname, out match0))
            {
                graphname = "current";
                match0 = ".*";
            }
            string match;
            string filename;
            if (!TextPatternUtils.SplitOff(match0, "-", out match, out filename))
            {
                filename = null;
                match = match0;
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
                    var console1 = console;
                    TextWriter tw = null;
                    if (filename != null)
                    {
                        tw = new StreamWriter(filename);
                        console1 = tw.WriteLine;
                    }
                    G.Listing(console1, match, printOptions);
                    if (tw != null) tw.Close();
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

            if (showHelp) console("@enable <graph> - <tmatch>   --  enables all graph elements matching <tmatch>");
            if (cmd == "enable")
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
                    console("ENABLE: count=" + cis.Count + " local=" + G + " key='" + n + "'");
                    foreach (var ci in cis) G.EnableTemplate(ci.Template);
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

        public Unifiable GetGuardInfo(XmlNode guardnode)
        {
            return guardnode.InnerXml;
        }

        public ParentChild ParentObject { get; set; }

        public static GraphMaster FindOrCreate(string dgn, AltBot theBot)
        {
            dgn = DeAliasGraphName(dgn);
            var gbn = AltBot.GraphsByName;
            lock (gbn)
            {
                GraphMaster v;
                if (!gbn.TryGetValue(dgn, out v))
                {
                    v = gbn[dgn] = new GraphMaster(dgn, theBot);
                }
                return v;
            }
            //throw new NotImplementedException();
        }

        internal bool AlsoKnownAs(string p)
        {
            return GraphNames.Contains(p);
        }

        public bool InRunLowMemHooks = false;
        public long RunLowMemHooks()
        {
            return RootNode.RunLowMemHooks();
        }

        public static string[] StarTypes = new[] { "state", "topic", "pattern", "that", };

        /// <summary>
        /// 
        /// </summary>
        public AltAIMLbot.Utils.Node root
        {
            get
            {
                lock (ExternDB.mylock)
                {
                    if (_root == null)
                    {
                        _root = ensureEdb().fetchNode("", true);
                    }
                    return _root;
                }
            }
        }
        public bool useChatDB = true;
        private Node _root;

        public string evaluate(string path, SubQuery query, Request request, MatchState state, StringBuilder builder)
        {
            ensureEdb();
            if (!useChatDB)
            {
                //root = root ?? RootNode;
                return root.evaluate(path, query, request, state, builder); 
            }
            else
            {
                return root.evaluateDB(path, query, request, state, builder, "", chatDB);
            }
        }
        public bool IsMicrosoftCLR()
        {
            return (Type.GetType("Mono.Runtime") == null);
        }
        public bool wasloaded(string filename)
        {
            lock (ExternDB.mylock)
            {

                if (chatDB == null)
                {

                    string rapStoreDirectory = "";
                    if (IsMicrosoftCLR())
                    {
                        rapStoreDirectory = theBot.rapStoreDirectory.TrimEnd("/\\".ToCharArray()) + "_" + graphName + "/";
                    }
                    else
                    {
                        rapStoreDirectory = theBot.rapStoreDirectory.TrimEnd(Path.DirectorySeparatorChar) + "_" + graphName + Path.DirectorySeparatorChar;
                    }

                    chatDB = new ExternDB(rapStoreDirectory);
                    chatDB.bot = this.theBot;
                    chatDB._dbdir = rapStoreDirectory;
                    if (theBot.rapStoreSlices > 0) chatDB.slices = theBot.rapStoreSlices;
                    if (theBot.rapStoreTrunkLevel > 0) chatDB.trunkLevel = theBot.rapStoreTrunkLevel;
                    //chatDB.OpenAll();
                }
                return chatDB.wasLoaded(filename);

            }

        }
        public ExternDB ensureEdb()
        {
            lock (ExternDB.mylock)
            {
                if (chatDB == null)
                {

                    string rapStoreDirectory = "";
                    if (IsMicrosoftCLR())
                    {
                        rapStoreDirectory = theBot.rapStoreDirectory.TrimEnd("/\\".ToCharArray()) + "_" + graphName + "/";
                    }
                    else
                    {
                        rapStoreDirectory = theBot.rapStoreDirectory.TrimEnd(Path.DirectorySeparatorChar) + "_" + graphName + Path.DirectorySeparatorChar;
                    }

                    chatDB = new ExternDB(rapStoreDirectory);
                    chatDB.bot = this.theBot;
                    chatDB._dbdir = rapStoreDirectory;
                    if (theBot.rapStoreSlices > 0) chatDB.slices = theBot.rapStoreSlices;
                    if (theBot.rapStoreTrunkLevel > 0) chatDB.trunkLevel = theBot.rapStoreTrunkLevel;
                    if (!chatDB.allLoaded) chatDB.OpenAll();
                }
            }
            return chatDB;
        }

        public double getPathScore(string path)
        {
            return root.getPathScore(path);
        }

        public void collectFullPaths(string inpath, List<string> collector)
        {
            root.collectFullPaths(inpath, collector, ensureEdb());
        }

        public void searchFullPaths(string targetPath, string inpath, List<string> collector)
        {
            root.searchFullPaths(targetPath, inpath, collector);
        }

        public void addCategory(string path, string template, string filename, double score, double scale)
        {
            root.addCategory(path, template, filename, score, scale);
        }

        public void DisableFilename(string filename)
        {
            root.WithFilename(filename, false, false);
        }

        public void EnableFilename(string filename)
        {
            root.WithFilename(filename, false, true);
        }

        public void UnloadFilename(string filename)
        {
            root.WithFilename(filename, true, false);
        }

        public void Close()
        {
            lock (ExternDB.mylock)
            {
                if (chatDB != null)
                {
                    chatDB.Close();
                    chatDB = null;
                    _root = null;
                }
            }
        }

        internal void AddName(string alias)
        {
            theBot.Graphs[alias] = this;
        }

        public static string DeAliasGraphName(string graphPath)
        {
            if (String.IsNullOrEmpty(graphPath) || graphPath == "*")
            {
                throw new NullReferenceException("graphPath=" + graphPath);
            }
            graphPath = AltBot.ToScriptableName(graphPath);
            if (graphPath == "default")
            {
                graphPath = "base";
            }
            return graphPath;
        }
    }
}