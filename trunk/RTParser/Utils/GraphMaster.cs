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
                Console.WriteLine(s);
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
            Node.addCategoryTag(RootNode, generatedPath, patternInfo, category, outerNode, templateNode, guard, thatInfo,
                                this);
            // keep count of the number of categories that have been processed
            this.Size++;
        }

        public UList evaluate(UPath unifiable, SubQuery query, Request request, MatchState state, Unifiable appendable)
        {
            var templs = RootNode.evaluate(unifiable, query, request, state, appendable);
            if (templs == null || templs.Count == 0)
            {
                Console.WriteLine("no templates for " + this);
                foreach (GraphMaster graphMaster in Parents)
                {
                    templs = graphMaster.evaluate(unifiable, query, request, state, appendable);
                    if (templs!=null && templs.Count>0)
                    {
                        Console.WriteLine("ussing parent templates from " + templs);
                        return templs;
                    }
                }
            }
            return templs;
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
            //System.Console.WriteLine("removing " + templateInfo.CategoryInfo.ToString());
            Templates.Remove(templateInfo);
        }

        internal void WriteConfig()
        {
            lock (Topics)
            {
                foreach (KeyValuePair<string, TopicInfo> info in Topics)
                {
                    Console.WriteLine("topic = " + info.Key);
                }
                foreach (KeyValuePair<string, ThatInfo> info in Thats)
                {
                    Console.WriteLine("that = " + info.Key);
                }
            }
        }
    }
}