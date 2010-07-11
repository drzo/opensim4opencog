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
    public class GraphMaster
    {
        public static bool DefaultSilentTagsInPutParent = false;

        private String graphName;
        private RTPBot theBot;
        public GraphMaster Srai;
        public bool NoIndexing = true;
        public bool FullDepth = true;
        public bool SilentTagsInPutParent = DefaultSilentTagsInPutParent;
        readonly public List<GraphMaster> Parents = new List<GraphMaster>();

        /// <summary>
        /// All the &lt;category&gt;s (if any) associated with this database
        /// </summary>
        readonly public List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();

        /// <summary>
        /// All the &lt;pattern&gt;s (if any) associated with this database
        /// </summary>
        readonly public Dictionary<String, PatternInfo> Patterns = new Dictionary<string, PatternInfo>();


        /// <summary>
        /// All the &lt;that&gt;s (if any) associated with this database
        /// </summary>
        readonly public Dictionary<String, ThatInfo> Thats = new Dictionary<string, ThatInfo>();

        /// <summary>
        /// All the &lt;topic&gt;s (if any) associated with this database
        /// </summary>
        readonly public Dictionary<String, TopicInfo> Topics = new Dictionary<string, TopicInfo>();

        /// <summary>
        /// All the &lt;templates&gt;s (if any) associated with this database
        /// </summary>
        readonly public List<TemplateInfo> Templates = new List<TemplateInfo>();

        /// <summary>
        /// All the &lt;guard&gt;s (if any) associated with this database
        /// </summary>
        readonly public List<GuardInfo> Guards = new List<GuardInfo>();

        public Node RootNode = new RTParser.Utils.Node(null);
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
                RTPBot.writeDebugLine(s);
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
            var RootNode = this.RootNode;
            if (SilentTagsInPutParent && SilentTag(templateNode))
            {
                GraphMaster Parent = makeParent();
                this.Parents.Add(Parent);
                Parent.Size++;
                RootNode = Parent.RootNode;
                RTPBot.writeDebugLine("Adding to parent" + category);
                return;
            }
            Node.addCategoryTag(RootNode, generatedPath, patternInfo,
                                category, outerNode, templateNode, guard, thatInfo, this);
            this.Size++;
            // keep count of the number of categories that have been processed
        }

        public static bool SilentTag(XmlNode node)
        {
           // if (true) return false;
            if (node.ChildNodes.Count != 1) return false;
            string s = node.InnerXml;
            if (s.StartsWith("<think>"))
            {
                if (s.EndsWith("</think>"))
                {
                    if (node.ChildNodes.Count != 1)
                    {
                        return false;
                    }
                    return true;
                }
            }
            return false;
        }


        public override string ToString()
        {
            return "[Graph: " + graphName + ":" + Size + "]";
        }
        //query.Templates = 

        public QueryList gatherQueries(Unifiable path, Request request, MatchState state)
        {
            QueryList ql = new QueryList(request);
            var templs2 = evaluateQL(path, request, state, ql);
            if (templs2.TemplateCount == 0)
            {
                bool trace = request.IsTraced && !UnTraced;
                if (trace)
                    RTPBot.writeDebugLine(this + " returned no results for " + path);
                return templs2;
            }
            return templs2;
        }

        private QueryList evaluateQL(UPath unifiable, Request request, MatchState matchState, QueryList ql)
        {
            DoParentEval(Parents, request, unifiable);
            bool trace = request.IsTraced && !UnTraced;
            var templs = RootNode.getQueries(unifiable, request, matchState, 0, Unifiable.CreateAppendable(), ql);
            if (ql.TemplateCount == 0)
            {
                if (trace) RTPBot.writeDebugLine("no templates for " + this);
                var fallbacks = FallBacks(request.user);
                if (fallbacks == null) return ql;
                foreach (GraphMaster graphMaster in fallbacks)
                {
                    var templs2 = graphMaster.evaluateQL(unifiable, request, matchState, ql);
                    if (templs2.TemplateCount > 0)
                    {
                        if (trace)
                            RTPBot.writeDebugLine("using parent templates from " + templs2);
                        return templs2;
                    }
                }
            }
            return ql;
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
                        request.result = null;
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

        public void AddCategory(CategoryInfo categoryInfo)
        {
            CategoryInfos.Add(categoryInfo);
        }

        public void RemoveTemplate(TemplateInfo templateInfo)
        {
            //System.RTPBot.writeDebugLine("removing " + templateInfo.CategoryInfo.ToString());
            Templates.Remove(templateInfo);
        }

        internal void WriteConfig()
        {
            lock (Topics)
            {
                foreach (KeyValuePair<string, TopicInfo> info in Topics)
                {
                    RTPBot.writeDebugLine("topic = " + info.Key);
                }
                foreach (KeyValuePair<string, ThatInfo> info in Thats)
                {
                    RTPBot.writeDebugLine("that = " + info.Key);
                }
            }
        }

    }
}