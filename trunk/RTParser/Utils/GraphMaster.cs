using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using Console=System.Console;
using File=System.IO.File;
using UPath = RTParser.Unifiable;

namespace RTParser.Utils
{
    public class GraphMaster : QuerySettings
    {
        public static bool DefaultSilentTagsInPutParent = false;

        public bool SilentTagsInPutParent = DefaultSilentTagsInPutParent;
        public bool DoParents = true;

        private String graphName;
        private RTPBot theBot;
        public GraphMaster Srai;
        private bool NoIndexing = true;
        private bool FullDepth = true;
        readonly private List<GraphMaster> Parents = new List<GraphMaster>();

        /// <summary>
        /// All the &lt;category&gt;s (if any) associated with this database
        /// </summary>
        readonly private List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();

        /// <summary>
        /// All the &lt;pattern&gt;s (if any) associated with this database
        /// </summary>
        readonly private Dictionary<String, PatternInfo> Patterns = new Dictionary<string, PatternInfo>();


        /// <summary>
        /// All the &lt;that&gt;s (if any) associated with this database
        /// </summary>
        readonly private Dictionary<String, ThatInfo> Thats = new Dictionary<string, ThatInfo>();

        /// <summary>
        /// All the &lt;topic&gt;s (if any) associated with this database
        /// </summary>
        readonly private Dictionary<String, TopicInfo> Topics = new Dictionary<string, TopicInfo>();

        /// <summary>
        /// All the &lt;templates&gt;s (if any) associated with this database
        /// </summary>
        readonly private List<TemplateInfo> Templates = new List<TemplateInfo>();

        /// <summary>
        /// All the &lt;guard&gt;s (if any) associated with this database
        /// </summary>
        readonly private List<GuardInfo> Guards = new List<GuardInfo>();

        private Node RootNode = new RTParser.Utils.Node(null);
        public int Size = 0;
        private GraphMaster _parent = null;
        private int parent0 = 0;
        private bool UnTraced = false;

        public GraphMaster Parent
        {
            get
            {
                if (ScriptingName.Contains("parent"))
                {
                    return this;
                }
                if (_parent == null)
                {
                    if (Parents.Count > 0)
                    {
                        _parent = Parents[0];
                    }
                    else
                    {
                        _parent = makeParent();
                    }
                }
                return _parent;
            }
        }

        public String ScriptingName
        {
            get { return graphName; }
        }

        public GraphMaster(string gn, RTPBot bot)
            : base(bot)
        {
            graphName = gn;
            theBot = bot;
            Srai = this;
        }

        public PatternInfo FindPattern(XmlNode pattern, Unifiable unifiable)
        {
            if (NoIndexing) return null;
            string pats = MakeMatchKey(unifiable);
            int skip = pats.IndexOf("TAG-THAT");
            if (skip > 0) pats = pats.Substring(0, skip - 1);
            else
            {
                skip = pats.IndexOf("TAG-FLAG");
                if (skip > 0) pats = pats.Substring(0, skip - 1);
            }
            PatternInfo pi;
            lock (Patterns)
            {
                if (!Patterns.TryGetValue(pats, out pi))
                {
                    Patterns[pats] = pi = new PatternInfo(pattern, pats);
                }
                else
                {
                    CheckMismatch(pi, pats);
                    return pi;
                }
            }
            return pi;
        }

        public ThatInfo FindThat(Unifiable topicName)
        {
            if (NoIndexing) return null;
            string pats = MakeMatchKey(topicName);
            ThatInfo pi;
            lock (Thats)
            {
                if (!Thats.TryGetValue(pats, out pi))
                {
                    Thats[pats] = pi = new ThatInfo(Utils.AIMLTagHandler.getNode("<that>" + topicName + "</that>"), topicName);
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
            var v = AIMLLoader.MatchKeyClean(pattern.AsString()).ToUpper();
            if (v.Length < 1)
            {
                return "*";
            }
            return v;
        }

        private void CheckMismatch(MatchInfo info, string pats)
        {
            if (info.FullPath.AsNodeXML().ToString().ToUpper() != pats.ToUpper())
            {
                string s = "CheckMismatch " + info.FullPath.AsNodeXML().ToString() + "!=" + pats;
                writeToLog(s);
                throw new Exception(s);

            }
        }

        public TopicInfo FindTopic(Unifiable topicName)
        {
            if (NoIndexing) return null;
            string pats = MakeMatchKey(topicName);
            TopicInfo pi;
            lock (Topics)
            {
                if (!Topics.TryGetValue(pats, out pi))
                {
                    Topics[pats] = pi = new TopicInfo(Utils.AIMLTagHandler.getNode("<pattern>" + topicName + "</pattern>"), topicName);
                }
                else
                {
                    CheckMismatch(pi, topicName.AsNodeXML().ToString());
                    return pi;
                }
            }
            return pi;
        }

        public CategoryInfo FindCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename)
        {
            return CategoryInfo.MakeCategoryInfo(info, node, filename);
        }

        private GraphMaster makeParent()
        {
            var p = new GraphMaster("" + graphName + ".parent" + (parent0 == 0 ? "" : "" + parent0), theBot);
            p.Srai = this;
            parent0++;
            p.UnTraced = true;
            Parents.Add(p);
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

            FileStream saveFile = File.Create(path);
            BinaryFormatter bf = new BinaryFormatter();
            bf.Serialize(saveFile, this.RootNode);
            saveFile.Close();
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(Unifiable path)
        {
            FileStream loadFile = File.OpenRead(path);
            BinaryFormatter bf = new BinaryFormatter();
            this.RootNode = (Node)bf.Deserialize(loadFile);
            loadFile.Close();

        }

        public void addCategoryTag(Unifiable generatedPath, PatternInfo patternInfo, CategoryInfo category, XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo)
        {
            var node = this.RootNode;
            if (SilentTagsInPutParent && AIMLLoader.IsSilentTag(templateNode))
            {
                GraphMaster parent1 = makeParent();
                this.Parents.Add(parent1);
                parent1.Size++;
                node = parent1.RootNode;
                writeToLog("Adding to Parent " + category);
            }
            Node created = Node.addCategoryTag(node, generatedPath, patternInfo,
                                category, outerNode, templateNode, guard, thatInfo, this);

            this.Size++;
            // keep count of the number of categories that have been processed
        }


        public override string ToString()
        {
            return "[Graph: " + graphName + ":" + Size + "]";
        }
        //query.Templates = 

        public QueryList gatherQueriesFromGraph(Unifiable path, Request request, MatchState state)
        {
            if (path.IsEmpty)
            {
                string s = "ERROR! path.IsEmpty  returned no results for " + state + " in " + this;
                writeToLog(s);
                throw new Exception(s);
            }
            QueryList ql = new QueryList(request);
            ApplySettings(request, ql);
            request.TopLevel = ql;
            evaluateQL(path, request, state, ql);
            if (ql.TemplateCount == 0)
            {
                bool trace = request.IsTraced && !UnTraced;
                if (trace)
                    writeToLog(this + " returned no results for " + path);
                return ql;
            }
            lock (request.user.AllQueries)
            {
                request.user.AllQueries.Add(ql);
            }
            return ql;
        }

        private void evaluateQL(Unifiable unifiable, Request request, MatchState matchState, QueryList ql)
        {
            if (DoParents) DoParentEval(Parents, request, unifiable);
            bool trace = request.IsTraced && !UnTraced;
            while (getQueries(unifiable, request, matchState, 0, Unifiable.CreateAppendable(), ql))
            {
                if (ql.IsMaxedOut)
                {
                    break;
                }
                if (!request.ProcessMultiplePatterns)
                {
                    break;
                }
            }
            if (ql.TemplateCount == 0)
            {
                if (trace) writeToLog("no templates for " + this);
                var fallbacks = FallBacks(request.user);
                if (fallbacks == null) return;
                foreach (GraphMaster graphMaster in fallbacks)
                {
                    graphMaster.evaluateQL(unifiable, request, matchState, ql);
                    if (ql.TemplateCount > 0)
                    {
                        if (trace)
                            writeToLog("using parent templates from " + ql);
                        return;
                    }
                }
            }
            return;
        }


        static bool IsStartStarStar(Node bubble)
        {
            if (bubble == null) return false;
            string s = bubble.ToString();
            bool b = s.Trim().StartsWith("* TAG-THAT * TAG-FLAG * TAG-TOPIC *");
            if (!b) return false;
            return b;
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

        private bool getQueries(UPath upath, Request request, MatchState matchstate, int index, StringAppendableUnifiable wildcard, QueryList toplevel)
        {
            int resin = toplevel.TemplateCount;
            int patternCountChanged = 0;
            int tried = 0;
            bool doIt = !request.IsComplete(request.CurrentResult);
            if (!doIt)
            {
                writeToLog("AIMLTRACE DOIT: " + tried + " pc=" + patternCountChanged + ": " + false + "  " + request);
                //   return false;
            }
            var Prf = new Proof();
            request.Proof = Prf;

            Node toplevelBubble;
            while (!toplevel.NoMoreResults)
            {
                int patternCount = toplevel.PatternCount;
                toplevelBubble = null;
                SubQuery query = new SubQuery(upath, request.CurrentResult, request);
                query.TopLevel = toplevel;
                var pattern = RootNode.evaluate00(upath.ToString(), query, request, matchstate, wildcard);
                if (pattern != null)
                {
                    var tmplateInfos = pattern.TemplateInfos;
                    if (toplevel.ContainsPattern(pattern))
                    {
                        toplevelBubble = pattern;
                        writeToLog("p=" + pattern);
                        toplevel.NoMoreResults = true;
                    }
                    else if (!pattern.disabled)
                    {
                        toplevelBubble = pattern;
                        toplevel.AddPattern(pattern);
                        pattern.disabled = true;
                        if (tmplateInfos != null && tmplateInfos.Count != 0)
                        {
                            query.Pattern = pattern;
                            toplevel.AddBindingSet(query);
                            foreach (TemplateInfo sol in tmplateInfos)
                            {
                                sol.Query = query;
                                query.CurrentTemplate = sol;
                                query.Templates.Add(sol);
                                toplevel.AddTemplate(sol);
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
                if (toplevel.PatternCount >= toplevel.MaxPatterns || toplevel.IsMaxedOut || tried > 100 ||
                    request.hasTimedOut)
                {
                    break;
                }
                if (IsStartStarStar(toplevelBubble))
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
                    foreach (var list in PU)
                    {
                        list.disabled = false;
                    }
                return f;
            }
        }

        private void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine("GRAPH: " + message + " in " + ToString(), args);
        }

        private IEnumerable<GraphMaster> FallBacks(User user)
        {
            return null;
        }

        private List<Result> DoParentEval(List<GraphMaster> totry, Request request, Unifiable unifiable)
        {
            List<Result> pl = new List<Result>();
            RTPBot proc = request.Proccessor;
            GraphMaster g = request.Graph;
            bool userTracing = request.IsTraced;
            foreach (var p in totry)
            {
                if (p != null)
                {
                    bool wasUntraced = p.UnTraced;
                    try
                    {
                        p.UnTraced = true;
                        if (wasUntraced)
                            request.IsTraced = false;
                        request.Graph = p;
                        request.CurrentResult = null;
                        var r = proc.Chat0(request, p);
                        if (!r.IsEmpty) pl.Add(r);
                    }
                    finally
                    {
                        p.UnTraced = wasUntraced;
                        request.Graph = g;
                        request.IsTraced = userTracing;
                    }
                }
            }
            return pl;
        }

        public void AddTemplate(TemplateInfo templateInfo)
        {
            Templates.Add(templateInfo);
            CategoryInfos.Add(templateInfo.CategoryInfo);
        }

        private void AddCategory(CategoryInfo categoryInfo)
        {
            CategoryInfos.Add(categoryInfo);
        }

        public void RemoveTemplate(TemplateInfo templateInfo)
        {
            //System.writeToLog("removing " + templateInfo.CategoryInfo.ToString());
            Templates.Remove(templateInfo);
        }

        internal void WriteConfig()
        {
            lock (Topics)
            {
                foreach (KeyValuePair<string, TopicInfo> info in Topics)
                {
                    writeToLog("topic = " + info.Key);
                }
                foreach (KeyValuePair<string, ThatInfo> info in Thats)
                {
                    writeToLog("that = " + info.Key);
                }
            }
        }

        #region Overrides of QuerySettings

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public override GraphMaster Graph
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        #endregion
    }
}