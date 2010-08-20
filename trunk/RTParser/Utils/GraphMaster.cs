using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;
using System.Text.RegularExpressions;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using RTParser.Variables;
using Console=System.Console;
using File=System.IO.File;
using UPath = RTParser.Unifiable;

namespace RTParser.Utils
{
    public class GraphMaster// : QuerySettings
    {
        public static bool DefaultSilentTagsInPutParent = false;

        public bool SilentTagsInPutParent = DefaultSilentTagsInPutParent;
        public bool DoParents = true;

        private String graphName;
        //private RTPBot theBot;
        public String Srai;
        static public bool NoIndexing = false;
        private bool FullDepth = true;
        readonly private List<GraphMaster> Parents = new List<GraphMaster>();

        /// <summary>
        /// All the &lt;category&gt;s (if any) associated with this database
        /// </summary>
        private List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();

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


        //public GraphMaster GetGraph(string value)
        //{
        //    return theBot.GetGraph(value, this);
        //}
        private Node RootNode = new RTParser.Utils.Node(null);
        private Node PostParentRootNode = new RTParser.Utils.Node(null);
        public int Size = 0;
        private GraphMaster _parent = null;
        private int parent0 = 0;
        private bool UnTraced = false;
        private readonly List<GraphMaster> FallBacksGraphs = new List<GraphMaster>();
        public bool IsBusy;
        private List<TemplateInfo> UnusedTemplates = new List<TemplateInfo>();

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

        public GraphMaster(string gn)
        //: base(bot)
        {
            graphName = gn;
            //theBot = bot;
            // most graphs try to recuse on themselves until otehrwise stated (like in make-parent)
            Srai = gn;
            RootNode.Graph = this;
            PostParentRootNode.Graph = this;
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
            lock (LockerObject) lock (Patterns)
                {
                    if (!Patterns.TryGetValue(pats, out pi))
                    {
                        Patterns[pats] = pi = new PatternInfo(AIMLLoader.ToLineInfoElement(pattern), pats);
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
            lock (LockerObject) lock (Thats)
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
            lock (LockerObject) lock (Topics)
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
            var p = new GraphMaster("" + graphName + ".parent" + (parent0 == 0 ? "" : "" + parent0));
            p.Srai = graphName;
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

            FileStream saveFile = HostSystem.Create(path);
            BinaryFormatter bf = new BinaryFormatter();
            bf.Serialize(saveFile, this.RootNode);
            bf.Serialize(saveFile, this.PostParentRootNode);
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
            this.PostParentRootNode = (Node)bf.Deserialize(loadFile);
            loadFile.Close();

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

        public void addCategoryTag(Unifiable generatedPath, PatternInfo patternInfo, CategoryInfo category, XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo)
        {
            if (SilentTagsInPutParent && AIMLLoader.IsSilentTag(templateNode))
            {
                GraphMaster parent1 = makeParent();
                this.Parents.Add(parent1);
                parent1.SilentTagsInPutParent = false;
                writeToLog("Adding to Parent " + category);
                parent1.addCategoryTag(generatedPath, patternInfo, category, outerNode, templateNode, guard, thatInfo);
                return;
            }

            Node rootNode = this.RootNode;
            if (IsStartStarStar(generatedPath))
            {
                rootNode = this.PostParentRootNode;
            }
            Node thiz = rootNode.addPathNodeChilds(generatedPath);
            int countBefore = thiz.TemplateInfoCount;
            TemplateInfo info = thiz.addTerminal(templateNode, category, guard, thatInfo, this, patternInfo);
            int countAfter = thiz.TemplateInfoCount;
            /*
             * Node created = Node.addCategoryTag(node, generatedPath, patternInfo,
                                category, outerNode, templateNode, guard, thatInfo, this);*/
            int changed = countAfter - countBefore;
            if (changed == 0)
            {
                return;
            }
            if (changed < 0 || info == null)
            {
                return;
            }
            this.Size += changed;
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
            QuerySettings.ApplySettings(request, ql);
            request.TopLevel = ql;
            evaluateQL(path, request, state, ql);
            if (ql.TemplateCount == 0)
            {
                bool trace = request.IsTraced && !UnTraced;
                if (trace)
                    writeToLog(this + " returned no results for " + path);
                return ql;
            }
            lock (LockerObject) lock (request.user.AllQueries)
                {
                    if (!request.user.AllQueries.Contains(ql)) request.user.AllQueries.Add(ql);
                }
            return ql;
        }

        private void evaluateQL(Unifiable unifiable, Request request, MatchState matchState, QueryList ql)
        {
            if (DoParents) DoParentEval(Parents, request, unifiable);
            bool trace = request.IsTraced && !UnTraced;
            while (getQueries(RootNode, unifiable, request, matchState, 0, Unifiable.CreateAppendable(), ql))
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
            if (!ql.IsMaxedOut)
            {
                if (FallBacksGraphs == null) return;
                foreach (GraphMaster graphMaster in CopyOf(FallBacksGraphs))
                {
                    graphMaster.evaluateQL(unifiable, request, matchState, ql);
                    if (ql.IsMaxedOut)
                    {
                        if (trace)
                            writeToLog("using parent templates from " + ql);
                        return;
                    }
                }
            }
            if (!ql.IsMaxedOut)
            {
                while (getQueries(PostParentRootNode, unifiable, request, matchState, 0, Unifiable.CreateAppendable(), ql))
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
            }
            return;
        }


        static public bool IsStartStarStar(String bubble)
        {
            if (bubble == null) return false;
            string s = bubble.ToString();

            bool b = s.Trim().StartsWith(STAR_PATH);
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

        private bool getQueries(Node rootNode, UPath upath, Request request, MatchState matchstate, int index, StringAppendableUnifiable wildcard, QueryList toplevel)
        {
            lock (LockerObject)
            {
                return getQueries000(rootNode, upath, request, matchstate, index, wildcard, toplevel);
            }
        }

        private bool getQueries000(Node rootNode, UPath upath, Request request, MatchState matchstate, int index, StringAppendableUnifiable wildcard, QueryList toplevel)
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
                var pattern = rootNode.evaluate(upath.ToString(), query, request, matchstate, wildcard);
                if (pattern != null)
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
                        toplevelBubble = pattern;
                        pattern.disabled = true;
                        if (tmplateInfos != null && tmplateInfos.Count != 0)
                        {
                            toplevel.AddPattern(pattern);
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
                if (toplevelBubble != null && IsStartStarStar(toplevelBubble.ToString()))
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


        public void RemoveGenlMT(GraphMaster fallback)
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

        internal void AddGenlMT(GraphMaster fallback)
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
                    fallback.RemoveGenlMT(this);
                }
            }
        }

        public static IList<T> CopyOf<T>(List<T> list)
        {
            lock (list)
            {
                return list.ToArray();
            }
        }
        public static IList<T> CopyOf<T>(IEnumerable<T> list)
        {
            var copy = new List<T>();
            lock (list)
            {
                copy.AddRange(list);
            }
            return copy;
        }

        private List<Result> DoParentEval(List<GraphMaster> totry, Request request, Unifiable unifiable)
        {
            List<Result> pl = new List<Result>();
            RTPBot proc = request.TargetBot;
            GraphMaster g = request.Graph;
            bool userTracing = request.IsTraced;
            foreach (var p in CopyOf(totry))
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
            lock (LockerObject)
            {
                lock (Templates)
                    Templates.Add(templateInfo);
                lock (CategoryInfos)
                    CategoryInfos.Add(templateInfo.CategoryInfo);
            }
        }

        private void AddCategory(CategoryInfo categoryInfo)
        {
            lock (LockerObject) { lock (CategoryInfos) { CategoryInfos.Add(categoryInfo); } }
        }

        public void RemoveTemplate(TemplateInfo templateInfo)
        {
            //System.writeToLog("removing " + templateInfo.CategoryInfo.ToString());
            lock (LockerObject)
            {
                lock (Templates)
                    Templates.Remove(templateInfo);

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
                lock (Thats) foreach (KeyValuePair<string, ThatInfo> info in Thats)
                    {
                        writeToLog("that = " + info.Key);
                    }
            }
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

        public void WriteToFile(string name, string filename, PrintOptions printOptions)
        {
            lock (LockerObject)
            {
                var fi = new FileInfo(filename);
                var di = fi.DirectoryName;
                HostSystem.CreateDirectory(di);
                HostSystem.BackupFile(filename);
                var fs = new StreamWriter(filename, false);
                if (!printOptions.XMLWriterSettings.OmitXmlDeclaration)
                {
                    fs.WriteLine("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>");
                }

                fs.WriteLine("<aiml graph=\"{0}\">", name);
                printOptions.CurrentGraphName = name;
                List<CategoryInfo> skipped = new List<CategoryInfo>(1000);
                List<CategoryInfo> written = new List<CategoryInfo>(10000);
                try
                {
                    PrintToWriter(CopyOf(CategoryInfos), printOptions, fs, written);
                }
                finally
                {
                    fs.WriteLine("</aiml>");
                    fs.Flush();
                    fs.Close();
                    writeToLog("COMPLETE WRITTING " + this + " to " + filename + " written=" + CountOF(written) +
                               " skipped=" + CountOF(skipped) + " original=" + CountOF(CategoryInfos));
                    this.CategoryInfos = written;
                    Size = CategoryInfos.Count;
                }
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
                string cws = AIMLLoader.CleanWhitepaces(c);
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
            foreach (var list in CopyOf(FallBacksGraphs))
            {
                fs(" <genlMt name=\"{0}\"/>", list.ScriptingName);
            }
            foreach (var list in CopyOf(Parents))
            {
                fs(" <!-- parent name=\"{0}\" -->", list.ScriptingName);
            }
            var srai = Srai;
            if (srai != null)
                fs(" <sraiGraph name=\"{0}\" />", srai);
            if (printOptions.WriteStatistics)
            {
                fs(" <!-- templates={0} thats={1} patterns={2} topics={3} nodes1={4} nodes2={5}  -->",
                   CountOF(Templates), CountOF(Thats), CountOF(Patterns), CountOF(Topics), RootNode.ChildCount,
                   PostParentRootNode.ChildCount);
            }
        }

        static int CountOF(ICollection col)
        {
            if (col == null) return -1;
            return col.Count;
        }

        public void AddRedundantTemplate(TemplateInfo redundant, TemplateInfo info)
        {
            lock (LockerObject)
            {
                lock (Templates)
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
                List<CategoryInfo> cats = new List<CategoryInfo>();
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

        private Dictionary<string, DateTime> LoadedFiles = new Dictionary<string, DateTime>();
        static string _STAR_PATH;

        public object LockerObject
        {
            get { return LoadedFiles; }
        }

        static public string STAR_PATH
        {
            get
            {

                if (_STAR_PATH == null)
                {
                    _STAR_PATH = "TAG-INPUT * TAG-THAT * TAG-TOPIC * TAG-FLAG *"; // ((RTPBot)null).Loader.generatePath("*", "*", "*", "*", false);
                }
                return _STAR_PATH;
            }
        }

        public bool AddFileLoaded(string filename)
        {
            var fi = new FileInfo(filename);
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
            var fi = new FileInfo(filename);
            string fullName = fi.FullName;
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
            var fi = new FileInfo(filename);
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
                OutputDelegate When = new OutputDelegate((s, a) =>
                                                             {
                                                                 console(s, a);
                                                             });
                G.WriteMetaHeaders(When, printOptions);
            }
        }


        static public bool Matches(string pattern, string target)
        {
            if (pattern == null || pattern == "*" || pattern == "") return true; ;
            if (target.Contains(pattern)) return true;
            return Regex.Matches(target, pattern).Count > 0;
        }

    }
}