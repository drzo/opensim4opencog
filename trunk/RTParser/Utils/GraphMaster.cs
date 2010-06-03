using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;
using System.Xml;
using java.io;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using Console=System.Console;
using File=System.IO.File;
using UPath = RTParser.Unifiable;

namespace RTParser.Utils
{
    public class GraphMaster
    {
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


        public PatternInfo FindPattern(XmlNode pattern, Unifiable unifiable)
        {
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
                throw new InvalidObjectException(s);

            }
        }

        public TopicInfo FindTopic(Unifiable topicName)
        {
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


        public Node RootNode = new RTParser.Utils.Node(null);
        public int Size;
        private GraphMaster _parent = null;
        public GraphMaster Parent 
        {
            get
            {
                if (_parent == null)
                {
                    if (Parents.Count > 0)
                    {
                        _parent = Parents[0];
                    }
                    else
                    {
                        _parent = new GraphMaster();
                    }
                }
                return _parent;
            }
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
            if(SilentTag(templateNode))
            {
                GraphMaster Parent = new GraphMaster();
                this.Parents.Add(Parent);
                RootNode = Parent.RootNode;
                RTPBot.writeDebugLine("" + category);
            }
            Node.addCategoryTag(RootNode, generatedPath, patternInfo, category, outerNode, templateNode, guard, thatInfo,
                                this);
            // keep count of the number of categories that have been processed
            this.Size++;
        }

        public static bool SilentTag(XmlNode node)
        {
            string s = node.InnerXml;
            if (s.StartsWith("<think>"))
            {
                if (s.EndsWith("</think>"))
                {
                    return true;
                }
            }
            return false;
        }

        public UList evaluate(UPath unifiable, SubQuery query, Request request, List<Unifiable> state, MatchState matchState, int index, Unifiable appendable)
        {
            DoParentEval(request, unifiable, query, state);
            QueryList ql = new QueryList();
            var templs = RootNode.evaluate(unifiable, query, request, state, matchState,index, appendable, ql);
            var qr = ql.ToUList();
            if (qr.Count==0)
            {
                RTPBot.writeDebugLine("no templates for " + this);
                foreach (GraphMaster graphMaster in Parents)
                {
                    var templs2 = graphMaster.evaluate(unifiable, query, request, state, matchState, index, appendable);
                    if (templs2.Count>0)
                    {
                        RTPBot.writeDebugLine("using parent templates from " + templs);
                        return templs2;
                    }
                }
            }
            return qr;
        }

        private void DoParentEval(Request request, Unifiable unifiable, SubQuery query, List<Unifiable> state)
        {
            RTPBot proc = request.Proccessor;
            GraphMaster g = request.Graph;
            foreach (var p in Parents)
            {
                if (p != null)
                {
                    try
                    {
                        if (false)
                        {
                            request.Graph = p;
                            proc.Chat(request, p);
                        }
                        else
                        {
                            QueryList ql = new QueryList();
                            AIMLbot.Result result = new AIMLbot.Result(request.user, proc, request);
                            var subquery = query; // new SubQuery(request.rawInput, result, request);
                            request.Graph = p;
                            var silent = p.RootNode.evaluate(unifiable, subquery, request, subquery.InputStar,
                                                             MatchState.UserInput, 0, Unifiable.CreateAppendable(), ql);
                            if (ql.Count > 0)
                            {
                                bool found0 = false;
                                UList v = ql.ToUList();
                                foreach (TemplateInfo s in v)
                                {
                                    subquery = s.Query ?? subquery;
                                    string st = "" + s;
                                    // Start each the same
                                    s.Rating = 1.0;
                                    try
                                    {
                                        subquery.CurrentTemplate = s;
                                        if (proc.proccessResponse(subquery, request, result, s.Output, s.Guard,
                                                                  out found0,
                                                                  null))
                                        {
                                            found0 = true;
                                        }
                                    }
                                    catch (Exception e)
                                    {
                                        proc.writeToLog("" + e);
                                        proc.writeToLog(
                                            "WARNING! A problem was encountered when trying to process the input: " +
                                            request.rawInput + " with the template: \"" + s + "\"");
                                    }
                                    RTPBot.writeDebugLine(st);
                                    //if (found0) break;
                                }
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        proc.writeToLog("" + e);
                        proc.writeToLog(
                            "WARNING! A problem was encountered when trying to process the input: " +
                            request.rawInput + " with the template: \"" + p + "\"");
                    }
                    finally
                    {
                        request.Graph = g;
                    }
                }
            }
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