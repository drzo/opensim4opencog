using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;
using System.Web;
using System.IO;
using AltAIMLParser;
using AltAIMLbot.Normalize;
using LAIR.ResourceAPIs.WordNet;
using LAIR.Collections.Generic;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using AltAIMLbot.Utils;
using RaptorDB;
using System.Linq;
using AltAIMLbot;
using AltAIMLbot.Normalize;
using Unifiable = System.String;
using UPath = AltAIMLbot.Unifiable;
using UList = System.Collections.Generic.List<AltAIMLbot.Utils.TemplateInfo>;
//using CategoryInfo = RTParser.Utils.TemplateInfo;
//using StringAppendableUnifiable = System.Text.StringBuilder;
using PatternInfo = AltAIMLbot.Unifiable;
using ThatInfo = AltAIMLbot.Unifiable;
using TopicInfo = AltAIMLbot.Unifiable;
using GuardInfo = AltAIMLbot.Unifiable;
using ResponseInfo = AltAIMLbot.Unifiable;
using SNode = AltAIMLbot.Utils.Node;
using TemplateInfo = AltAIMLbot.Utils.TemplateInfo;
using MushDLR223.Virtualization;

namespace AltAIMLbot.Utils
{
    public class shortCategory
    {
        public string path;
        public string template;
    }

    /// <summary>
    /// Encapsulates a node in the graphmaster tree structure
    /// </summary>
    [Serializable]
    public class Node
    {
#if INCLUDE_UUNODE
        internal RTParser.Utils.UUNode ToUUNode()
        {
            throw new NotImplementedException();
        }

#endif
        public AltAIMLbot.Utils.GraphMaster Graph
        {
            get { throw new NotImplementedException(); }
        }

        public int TemplateInfoCount
        {
            get { throw new NotImplementedException(); }
        }

        public bool disabled
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public UList TemplateInfoCopy
        {
            get { throw new NotImplementedException(); }
        }


        public Node[] AllDecendants
        {
            get
            {
                List<Node> TIs = new List<Node>();
                var lts = this.TemplateInfos;
                if (lts != null && lts.Count > 0) TIs.Add(this);
                if (_c0 != null)
                    foreach (Node cn in children.Values)
                    {
                        TIs.AddRange(cn.AllDecendants);
                    }
                return TIs.ToArray();
            }
        }

        public void SetDisabled(TemplateInfo impl, bool value)
        {
            throw new NotImplementedException();
        }

        public Node addPathNodeChilds(Unifiable unifiable, AltAIMLbot.Utils.NodeAdder adder)
        {
            throw new NotImplementedException();
        }

        public List<AltAIMLbot.Utils.CategoryInfo> addTerminal(XmlNode node, XmlNode cateNode, Unifiable unifiable, Unifiable info, Unifiable thatInfo, 
            AltAIMLbot.Utils.LoaderOptions options, Unifiable patternInfo, List<AltAIMLbot.Utils.ConversationCondition> conditions, out bool removal)
        {
            throw new NotImplementedException();
        }

        public bool IsSatisfied(SubQuery query)
        {
            throw new NotImplementedException();
        }

        public void RotateTemplate(TemplateInfo info)
        {
            throw new NotImplementedException();
        }

        public long RunLowMemHooks()
        {
            throw new NotImplementedException();
        }

        public Node evaluateU(string s, SubQuery query, Request request, MatchState matchstate, StringBuilder wildcardsb)
        {
            throw new NotImplementedException();
        }
        public TemplateInfo[] AllDecendantTemplates
        {
            get
            {
                List<TemplateInfo> TIs = new List<TemplateInfo>();
                foreach (var node in AllDecendants)
                {
                    TIs.AddRange(node.TemplateInfos);
                }
                return TIs.ToArray();
            }
        }

        protected List<TemplateInfo> TemplateInfos
        {
            get { throw new NotImplementedException(); }
        }

        #region Attributes

        /// <summary>
        /// Contains the child nodes of this node
        /// </summary>
        public Dictionary<string, Node> children
        {
            get
            {
                if (_c0 == null)
                {
                    _c0 = new Dictionary<string, Node>(2);
                }
                return _c0;
            }
        }
        public Node Parent;
        private Dictionary<string, Node> _c0;

        public string childrenStr = null;
        public int childnum = 0;
        public int childmax = 0;
        /// <summary>
        /// The number of direct children (non-recursive) of this node
        /// </summary>
        public int NumberOfChildNodes
        {
            get
            {
                if (_c0 == null) return 0;
                return this.children.Count;
            }
        }
        public static IEnumerable EmptyKeys = new string[0];
        public IEnumerable ChildKeys
        {
            get
            {
                if (NumberOfChildNodes == 0) return EmptyKeys;
                lock (children) return LockInfo.CopyOf(children.Keys);
            }
        }
        public static IEnumerable<Node> EmptyNodes = new Node[0];
        public IEnumerable<Node> ChildNodes
        {
            get
            {
                if (NumberOfChildNodes == 0) return EmptyNodes;
                lock (children)
                {
                    string absPath = GetPath();
                    var cl = new List<Node>();
                    foreach (var key in ChildKeys)
                    {
                        cl.Add(ChildNode((string)key));
                    }
                    return cl;
                }
            }
        }
        public bool ContainsChildKey(string s)
        {
            if (NumberOfChildNodes == 0) return false;
            return children.ContainsKey(s);
        }
        public Node ChildNode(string s)
        {
            if (NumberOfChildNodes == 0)
            {
                return null;
            }
            lock (children)
            {
                var node = children[s];
                if (node == null)
                {
                    children[s] = node = fetchChild(GetPath(), s, extDB);
                }
                return node;
            }
        }
        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        public List<OutputTemplate> templates;

        /// <summary>
        /// The score (if any) associated with this node
        /// </summary>
        public double score = 1.0;

        /// <summary>
        /// The AIML source for the category that defines the template
        /// </summary>
        //public string filename = string.Empty;

        /// <summary>
        /// The word that identifies this node to it's parent node
        /// </summary>
        public string word=string.Empty;

        public bool fullChildSet = false;
        [NonSerialized]
        public ExternDB extDB;

        public string absPath;

        #endregion

        #region Methods

        #region Add category

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="template">the template to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        /// <param name="score"> computed score for the path so far</param>
        public void addCategory(string path, string template, string filename, double score, double scale)
        {
            if (template.Length == 0)
            {
                string warn = "The category with a pattern: " + path + " found in file: " + filename +
                              " has an empty template tag.";
                if (true)
                {
                    // give @warn
                }
                else
                {
                    throw new XmlException(warn + " ABORTING");
                }
            }

            // check we're not at the leaf node
            if (path.Trim().Length == 0)
            {
                this.AddTemplate(template, filename);
                this.score = score;
                return;
            }

            // otherwise, this sentence requires further child nodemappers in order to
            // be fully mapped within the GraphMaster structure.

            // split the input into its component words
            string[] words = path.Trim().Split(" ".ToCharArray());

            // get the first word (to form the key for the child nodemapper)
            string firstWord = MakeCaseInsensitive.TransformInput(words[0]);

            // concatenate the rest of the sentence into a suffix (to act as the
            // path argument in the child nodemapper)
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length).Trim();


            // Create a score for the next level
            double newScore = score;
            double newScale = scale /4;
            double indexv = 1.0;
            if (IsSectionHeader(firstWord))
            {
                // multiply by a significant amount for the big seperators
                newScale = newScale/10;
            }
            // increment for this level
            indexv = 2.0;
            if (firstWord == "_") indexv = 3.0;
            if (firstWord == "*") indexv = 1.0;
            // local shift (so you can tell "you *" from "* you")
            newScore = score + (indexv * newScale);

            // o.k. check we don't already have a child with the key from this sentence
            // if we do then pass the handling of this sentence down the branch to the 
            // child nodemapper otherwise the child nodemapper doesn't yet exist, so create a new one
            if (this.ContainsChildKey(firstWord))
            {
                Node childNode = this.ChildNode(firstWord);

                childNode.addCategory(newPath, template, filename, newScore, newScale);
            }
            else
            {
                Node childNode = new Node();
                childNode.extDB = extDB;
                childNode.Parent = this;
                childNode.word = firstWord;
                childNode.addCategory(newPath, template, filename, newScore,newScale);
                lock (children)
                {
                    this.children.Add(childNode.word, childNode);
                    childNode.Parent = this;
                }
            }
        }

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="template">the template to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        /// <param name="score"> computed score for the path so far</param>
        public static void addCategoryDB(string myWord, string path, string template, string filename, double score, double scale, string absPath, ExternDB pathDB)
        {
            if (template.Length == 0)
            {
                //string problem = string.Format("The category with a pattern: {0} found in file: {1} has an empty template tag.", path, filename);
                //Console.WriteLine("ERROR, This is a template 'Delete '(which is not implmneted for addCatagoryDB: " + problem);
                //return;
                //throw new XmlException(problem + " ABORTING");

                // Allowing to pass since a null will clear the template property at the final destination
            }
            lock (ExternDB.mylock) addCategoryDB0(myWord, path, template, filename, score, scale, absPath, pathDB);
        }
        public static Node addCategoryDB0(string myWord, string path, string template, string filename, double score, double scale, string absPath, ExternDB pathDB)
        {
            Node myNode = pathDB.fetchNode(absPath,false);
            myNode.word = myWord;

            // check we're not at the leaf node
            if (path.Trim().Length == 0)
            {
                myNode.AddTemplate(template, filename);
                myNode.score = score;
                pathDB.saveNode(absPath, myNode);
                return myNode;
            }


            // otherwise, this sentence requires further child nodemappers in order to
            // be fully mapped within the GraphMaster structure.

            // split the input into its component words
            string[] words = path.Trim().Split(" ".ToCharArray());

            // get the first word (to form the key for the child nodemapper)
            string firstWord = MakeCaseInsensitive.TransformInput(words[0]);

            // concatenate the rest of the sentence into a suffix (to act as the
            // path argument in the child nodemapper)
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length).Trim();
            string newdAbsPath = (absPath + " " + firstWord).Trim();

            // Create a score for the next level
            double newScore = score;
            double newScale = scale / 4;
            double indexv = 1.0;
            if (IsSectionHeader(firstWord))
            {
                // multiply by a significant amount for the big seperators
                newScale = newScale / 10;
            }
            // increment for this level
            indexv = 2.0;
            if (firstWord == "_") indexv = 3.0;
            if (firstWord == "*") indexv = 1.0;
            // local shift (so you can tell "you *" from "* you")
            newScore = score + (indexv * newScale);

            // o.k. check we don't already have a child with the key from this sentence
            // if we do then pass the handling of this sentence down the branch to the 
            // child nodemapper otherwise the child nodemapper doesn't yet exist, so create a new one
            //if (myNode.ContainsChildKey(firstWord))
            if ((myNode.ContainsChildKey(firstWord))
                ||( myNode.dbContainsNode(newdAbsPath, pathDB)) )
            {
                //Node childNode = myNode.this.ChildNode(firstWord);

                var retNode = addCategoryDB0(firstWord, newPath, template, filename, newScore, newScale, newdAbsPath, pathDB);
                retNode.Parent = myNode;
                return retNode;
            }
            else
            {
                //Node childNode = new Node();
                //childNode.word = firstWord;
                var retNode = addCategoryDB0(firstWord, newPath, template, filename, newScore, newScale, newdAbsPath, pathDB);
                //myNode.children.Add(childNode.word, childNode);
                //myNode.children.Add(firstWord,null);
                //myNode.childrenList.Add(firstWord);

                myNode.addChild(firstWord, null);

                // We only need to save it if we updated it with a child
                pathDB.saveNode(absPath, myNode);
                return retNode;
            }
        }

        public void AddTemplate(string template, string filename1)
        {
            if (string.IsNullOrEmpty(template))
            {
                templates = null;
                return;                
            }

            //Tempory PATCH
            if (AltBot.PadAroundTemplateTags)
            {
                if ((template.Contains("<star")) && (!template.Contains(" <star")))
                {
                    template = template.Replace("<star", " <star");
                }
            }
            if (templates == null) templates = new List<OutputTemplate>();
            templates.Insert(0, new OutputTemplate() {Template = template, filename = filename1});
        }

        public void collectPaths(string inpath, List<string> collector)
        {
            string curWord = this.word;
            string ourPath = inpath + " " + curWord;
            var template = FirstTemplate();
            if ((template != null) && (template.Length >1))
            {
                // a leaf node
                if (template.Length < 96)
                {
                    collector.Add(ourPath.Trim()+"| ["+template.Replace("\r"," ").Replace("\n"," ").Trim()+"]");
                }
                else
                {
                    collector.Add(ourPath .Trim ()+"| [template]");

                }
            }
            foreach (Node childNode in ChildNodes)
            {
                childNode.collectPaths(ourPath, collector);
            }

        }

        public string FirstTemplate()
        {
            if (templates == null || templates.Count == 0) return string.Empty;
            foreach (OutputTemplate list in templates)
            {
                if (list.disable) continue;
                string temp = templates[0].Template;
                string tempTrim = temp.Trim();
                if (!tempTrim.StartsWith("<template")) temp =  "<template>" + temp + "</template>";
                return temp;
            }
            return string.Empty;
        }


        public string FirstFilename()
        {
            if (templates == null || templates.Count == 0) return string.Empty;
            foreach (OutputTemplate list in templates)
            {
                if (list.disable) continue;
                return list.filename;
            }
            return string.Empty;
        }

        public void collectFullPaths(string inpath, List<string> collector, ExternDB pathDB)
        {
            string curWord = this.word;
            string ourPath = inpath + " " + curWord;
            var template = FirstTemplate();
            if ((template != null) && (template.Length > 1))
            {
                var encoded = HttpUtility.HtmlEncode(ourPath.Trim());
                string serTemplate = String.Format("<ser path=\"{0}\"> {1} </ser>", encoded, template);
                collector.Add(serTemplate);
            }
            foreach (var childNode in ChildNodes)
            {
                childNode.collectFullPaths(ourPath, collector, pathDB);
            }

        }

        public void searchFullPaths(string targetPath, string inpath, List<string> collector)
        {
            string curWord = this.word;
            string ourPath = inpath + " " + curWord;
            if ((targetPath.StartsWith(ourPath.Trim())) || (targetPath == ""))
            {
                var template = FirstTemplate();
                if ((template != null) && (template.Length > 1))
                {
                    var encoded = HttpUtility.HtmlEncode(ourPath.Trim());
                    string serTemplate = String.Format("<ser path=\"{0}\"> {1} </ser>", encoded, template);
                    collector.Add(serTemplate);
                }
                foreach (Node childNode in ChildNodes)
                {
                    childNode.searchFullPaths(targetPath, ourPath, collector);
                }
            }
        }

        public Hashtable pathFields(string path)
        {
            Hashtable fields = new Hashtable();
            string[] frags = path.Split('<');
            int stateindex = 0;

            fields["state1"] = "";
            fields["pattern"] = "";
            fields["that"] = "";
            fields["state2"] = "";
            fields["topic"] = "";
            // XML Style
            foreach (string s in frags)
            {
                string[] args = s.Split('>');
                if (args.Length == 2)
                {
                    string key = args[0].ToLower().Trim();
                    key = key.Remove('/');
                    string value = args[1].Trim();
                    // we have two state positions
                    if (key == "state")
                    {
                        stateindex++;
                        key = String.Format("state{0}", stateindex);
                    }
                    fields[key] = value;
                }
            }
            
            frags = path.Split(new string[] { "TAG-" }, StringSplitOptions.None);
            stateindex = 0;
            foreach (string s in frags)
            {
                // not quite ...
                string[] args = s.Split(' ');
                if (args.Length >= 2)
                {
                    string key = args[0].ToLower().Trim();
                    // was "TAG-STATE xxx yyy" => "|STATE xxx yyy"
                    string value = "|"+s;
                    value = value.Replace("|"+key.ToUpper () + " ", "");
                    
                    // we have two state positions
                    if (key == "state")
                    {
                        stateindex++;
                        key = String.Format("state{0}", stateindex);
                    }
                    fields[key] = value;
                    //TAG-INPUT => pattern
                    if (key == "input")
                    {
                        fields["pattern"] = value;
                    }
                }
            }

            return fields;
        }

        public void searchFullPathsShortCategory(string targetPath, string inpath, ArrayList collector)
        {
            string curWord = this.word;
            string ourPath = inpath + " " + curWord;
            if ((targetPath.StartsWith(ourPath.Trim())) || (targetPath==""))
            {
                //var template = FirstTemplate();
                if (templates != null)
                {
                    foreach (OutputTemplate list in templates)
                    {
                        if (list.disable) continue;

                        string temp = list.Template;
                        string vfile = list.filename;
                        string tempTrim = temp.Trim();
                        if (!tempTrim.StartsWith("<template")) temp = "<template>" + temp + "</template>";
                        if ((temp != null) && (temp.Length > 1))
                        {
                            var encoded = HttpUtility.HtmlEncode(ourPath.Trim());

                            //string serTemplate = String.Format("<ser path=\"{0}\"> {1} </ser>", encoded, template);
                            //Hashtable myTemp = new Hashtable();
                            Hashtable myTemp = pathFields(ourPath.Trim());
                            myTemp["path"] = ourPath.Trim();
                            myTemp["template"] = HttpUtility.HtmlEncode(temp);
                            myTemp["vfilename"] = vfile;
                            collector.Add(myTemp);
                        }
                    }
                }
                foreach (Node childNode in ChildNodes)
                {
                    try
                    {
                        childNode.searchFullPathsShortCategory(targetPath, ourPath, collector);
                    }
                    catch
                    {
                    }
                }
            }
        }


        #endregion

        #region Evaluate Node


        public double ratePath(string path)
        {
            double score = 1.0;
            double scale = 1.0;

            if (path.Trim().Length == 0) return score;
            // split the input into its component words
            string[] words = path.Trim().Split(" ".ToCharArray());

            foreach (string word in words)
            {
                // get the first word (to form the key for the child nodemapper)
                string firstWord = MakeCaseInsensitive.TransformInput(word);
                double indexv = 1.0;
                scale = scale / 4;
                if (IsSectionHeader(firstWord))
                {
                    // multiply by a significant amount for the big seperators
                    scale = scale / 10;
                }

                // increment for this level
                indexv = 2.0;
                if (firstWord == "_") indexv = 3.0;
                if (firstWord == "*") indexv = 1.0;
                // local shift (so you can tell "you *" from "* you")
                score = score + (indexv * scale);
            }
            return score;
        }

        private static bool IsSectionHeader(string firstWord)
        {
            return (firstWord.StartsWith("TAG-") || firstWord == "<THAT>") || (firstWord == "<TOPIC>") ||
                   (firstWord == "<STATE>") || (firstWord == "<PATTERN>");
        }

        /// <summary>
        /// Navigates this node (and recusively into child nodes) for a match to the path passed as an argument
        /// and returns the score associated with the node
        /// </summary>
        /// <param name="path">The normalized path derived from the user's input</param>
         /// <returns>The score associated with matched path</returns>

        public double getPathScore(string path)
        {
            // check for timeout
            /*
            if (request.StartedOn.AddMilliseconds(request.bot.TimeOut) < DateTime.Now)
            {
                request.bot.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\"");
                request.hasTimedOut = true;
                return 0;
            }
            */

            // so we still have time!
            path = path.Trim();

            // check if this is the end of a branch in the GraphMaster 
            // return the cCategory for this node
            if (this.NumberOfChildNodes==0)
            {
                if (path.Length > 0)
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    //this.storeWildCard(path, wildcard);
                }
                return this.score;
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.Length == 0)
            {
                return this.score;
            }

            // otherwise split the input into it's component words
            string[] splitPath = path.Split(" \r\n\t".ToCharArray());

            // get the first word of the sentence
            string firstWord = MakeCaseInsensitive.TransformInput(splitPath[0]);

            // and concatenate the rest of the input into a new path for child nodes
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            if (this.ContainsChildKey("_"))
            {
                Node childNode = this.ChildNode("_");

                // add the next word to the wildcard match 
                //StringBuilder newWildcard = new StringBuilder();
                //this.storeWildCard(splitPath[0],newWildcard);
                
                // move down into the identified branch of the GraphMaster structure
                double result = childNode.getPathScore(newPath);

                // and if we get a result from the branch process the wildcard matches and return 
                // the result
                if (result>0)
                {
                    return result;
                }
            }

            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            if (this.ContainsChildKey(firstWord))
            {
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                /*
                MatchState newMatchstate = matchstate;
                if (firstWord == "<THAT>")
                {
                    newMatchstate = MatchState.That;
                }
                else if (firstWord == "<TOPIC>")
                {
                    newMatchstate = MatchState.Topic;
                }
                */
                Node childNode = this.ChildNode(firstWord);
                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                //StringBuilder newWildcard = new StringBuilder();
                double result = childNode.getPathScore(newPath);
                // and if we get a result from the child return it
                if (result > 0)
                {
                    return result;
                }
            }

            // third option - the input part of the path might have been matched so far but hasn't
            // returned a match, so check to see it contains the "*" wildcard. "*" comes last in
            // precedence in the AIML alphabet.
            if (this.ContainsChildKey("*"))
            {
                // o.k. look for the path in the child node denoted by "*"
                Node childNode = this.ChildNode("*");

                // add the next word to the wildcard match 
                //StringBuilder newWildcard = new StringBuilder();
                //this.storeWildCard(splitPath[0], newWildcard);

                double result = childNode.getPathScore(newPath);
                // and if we get a result from the branch process and return it
                if (result > 0)
                {
                    return result;
                }
            }

            // o.k. if the nodemapper has failed to match at all: the input contains neither 
            // a "_", the sFirstWord text, or "*" as a means of denoting a child node. However, 
            // if this node is itself representing a wildcard then the search continues to be
            // valid if we proceed with the tail.
            if ((this.word == "_") || (this.word == "*"))
            {
                //this.storeWildCard(splitPath[0], wildcard);
                return this.getPathScore(newPath);
            }

            // If we get here then we're at a dead end so return an empty string. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            //wildcard = new StringBuilder();
            return 0;
        }

        /// <summary>
        /// Navigates this node (and recusively into child nodes) for a match to the path passed as an argument
        /// whilst processing the referenced request
        /// </summary>
        /// <param name="path">The normalized path derived from the user's input</param>
        /// <param name="query">The query that this search is for</param>
        /// <param name="request">An encapsulation of the request from the user</param>
        /// <param name="matchstate">The part of the input path the node represents</param>
        /// <param name="wildcard">The contents of the user input absorbed by the AIML wildcards "_" and "*"</param>
        /// <returns>The template to process to generate the output</returns>
        public string evaluate(string path, SubQuery query, Request request, MatchState matchstate, StringBuilder wildcard)
        {
            // check for timeout
            if (request.MayTimeOut && request.StartedOn.AddMilliseconds(request.bot.TimeOut) < DateTime.Now)
            {
                request.bot.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\"");
                request.hasTimedOut = true;
                return string.Empty;
            }

            // so we still have time!
            path = path.Trim();

            // check if this is the end of a branch in the GraphMaster 
            // return the cCategory for this node
            if (this.NumberOfChildNodes==0)
            {
                if (path.Length > 0)
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    this.storeWildCard(path, wildcard);
                }
                query.TemplatePath = "";
                return this.FirstTemplate();
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.Length == 0)
            {
                query.TemplatePath = "";
                return this.FirstTemplate();
            }

            // otherwise split the input into it's component words
            string[] splitPath = path.Split(" \r\n\t".ToCharArray());

            // get the first word of the sentence
            string firstWord = MakeCaseInsensitive.TransformInput(splitPath[0]);

            // and concatenate the rest of the input into a new path for child nodes
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            if (this.ContainsChildKey("_"))
            {
                Node childNode = this.ChildNode("_");

                // add the next word to the wildcard match 
                StringBuilder newWildcard = new StringBuilder();
                this.storeWildCard(splitPath[0],newWildcard);
                
                // move down into the identified branch of the GraphMaster structure
                string result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);

                // and if we get a result from the branch process the wildcard matches and return 
                // the result
                if (result.Length>0)
                {
                    // capture and push the star content appropriate to the matchstate if it exists and clear afterward
                    AcceptWildcard(query, newWildcard, matchstate);
                    query.TemplatePath = "_ " + query.TemplatePath;
                    return result;
                }
            }

            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            if (this.ContainsChildKey(firstWord))
            {
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                MatchState newMatchstate = GetNewMatchstate(firstWord, matchstate);

                Node childNode = this.ChildNode(firstWord);
                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                StringBuilder newWildcard = new StringBuilder();
                string result = childNode.evaluate(newPath, query, request, newMatchstate,newWildcard);
                // and if we get a result from the child return it
                if (result.Length > 0)
                {
                    // capture and push the star content appropriate to the matchstate if it exists and clear afterward
                    AcceptWildcard(query, newWildcard, matchstate);
                    query.TemplatePath = firstWord + " " + query.TemplatePath;
                    return result;
                }
            }

            // Patch option: input contains a semantic wildcard
            // like "*dog" or "*dog:hypo:n" or "*planet" ,"*celestial_body"
            bool hasSemantic = hasWildSense(this.children);
            bool isSemantic = isWildPattern(this.word);
            if (hasSemantic)
            {
                foreach (var sn in this.children)
                {
                    string s = sn.Key;
                    if (isWildPattern (s))
                    {
                        if( matchesWildSense(s ,firstWord,request ))
                        {
                          // Treat like matching a wildcard

                            // o.k. look for the path in the child node denoted by "*"
                            Node childNode = sn.Value;

                            // add the next word to the wildcard match 
                            StringBuilder newWildcard = new StringBuilder();
                            this.storeWildCard(splitPath[0], newWildcard);

                            string result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);
                            // and if we get a result from the branch process and return it
                            if (result.Length > 0)
                            {
                                // capture and push the star content appropriate to the matchstate if it exists and clear afterward
                                AcceptWildcard(query, newWildcard, matchstate);
                                query.TemplatePath = s+" " + query.TemplatePath;
                                return result;
                            }
                        }
                    }
                }

            }
            // third option - the input part of the path might have been matched so far but hasn't
            // returned a match, so check to see it contains the "*" wildcard. "*" comes last in
            // precedence in the AIML alphabet.
            if (this.ContainsChildKey("*"))
            {
                // o.k. look for the path in the child node denoted by "*"
                Node childNode = this.ChildNode("*");

                // add the next word to the wildcard match 
                StringBuilder newWildcard = new StringBuilder();
                this.storeWildCard(splitPath[0], newWildcard);

                string result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);
                // and if we get a result from the branch process and return it
                if (result.Length > 0)
                {
                    // capture and push the star content appropriate to the matchstate if it exists and clear afterward
                    AcceptWildcard(query, newWildcard, matchstate);
                    query.TemplatePath = "* " + query.TemplatePath;
                    return result;
                }
            }

            // we have a semantic node in our children but we may neet to fill a multi-word
            // wildcard. So allow "*semantic" to absorb multi-word if it makes sense
            if (hasSemantic)
            {
                // Test append the current word to the wild card 
                //to produce canidate multi-word
                string multiWord = wildcard.ToString();
                if (multiWord.Length > 0)
                {
                    multiWord += " " + splitPath[0];
                }
                else
                {
                    multiWord += splitPath[0];
                }
                //Scan children for matching semantic pattern
                foreach (string s in this.ChildKeys)
                {
                    if (isWildPattern(s))
                    {
                        if (matchesWildSense(s, multiWord, request))
                        {
                            // We found one so treat like the expanding multi-word "*" case
                            this.storeWildCard(splitPath[0], wildcard);

                            string result = this.evaluate(newPath, query, request, matchstate, wildcard);
                            query.TemplatePath = s + " " + query.TemplatePath;
                            return result;
                        }
                    }
                }
            }
            // o.k. if the nodemapper has failed to match at all: the input contains neither 
            // a "_", the sFirstWord text, or "*" as a means of denoting a child node. However, 
            // if this node is itself representing a wildcard then the search continues to be
            // valid if we proceed with the tail.
            if ((this.word == "_") || (this.word == "*"))
            {
                this.storeWildCard(splitPath[0], wildcard);
                string result= this.evaluate(newPath, query, request, matchstate, wildcard);
                query.TemplatePath = this.word+" " + query.TemplatePath;
                return result;
            }

            // If we get here then we're at a dead end so return an empty string. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            wildcard = new StringBuilder();
            query.TemplatePath = "";
            return string.Empty;
        }

        private static MatchState GetNewMatchstate(string firstWord, MatchState matchstate)
        {
            firstWord = firstWord.ToUpper();
            MatchState newMatchstate = matchstate;
            firstWord = firstWord.Trim('<', '>');
            if (firstWord.StartsWith("TAG-"))
            {
                firstWord = firstWord.Substring(4);
            }
            if (firstWord == "THAT")
            {
                newMatchstate = MatchState.That;
            }
            else if (firstWord == "TOPIC")
            {
                newMatchstate = MatchState.Topic;
            }
            else if (firstWord == "STATE")
            {
                newMatchstate = MatchState.State;
            }
            else if (firstWord == "PATTERN" || firstWord == "INPUT")
            {
                newMatchstate = MatchState.Pattern;
            }
            else if (firstWord == "FLAG")
            {
                newMatchstate = MatchState.Flag;
            }
            return newMatchstate;
        }

        /// <summary>
        /// Navigates this node (and recusively into child nodes) for a match to the path passed as an argument
        /// whilst processing the referenced request
        /// </summary>
        /// <param name="path">The normalized path derived from the user's input</param>
        /// <param name="query">The query that this search is for</param>
        /// <param name="request">An encapsulation of the request from the user</param>
        /// <param name="matchstate">The part of the input path the node represents</param>
        /// <param name="wildcard">The contents of the user input absorbed by the AIML wildcards "_" and "*"</param>
        /// <returns>The template to process to generate the output</returns>
        public string evaluateDB(string path, SubQuery query, Request request, MatchState matchstate, StringBuilder wildcard, string absPath, ExternDB pathDB)
        {
            lock (ExternDB.mylock)
            {
                return evaluateDB0(path, query, request, matchstate, wildcard, absPath, pathDB);
            }
        }

        public string evaluateDB0(string path, SubQuery query, Request request, MatchState matchstate, StringBuilder wildcard, string absPath, ExternDB pathDB)
        {
            // check for timeout
            if (request.StartedOn.AddMilliseconds(request.bot.TimeOut) < DateTime.Now)
            {
                request.bot.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\"");
                request.hasTimedOut = true;
                return string.Empty;
            }

            // so we still have time!
            path = path.Trim();

            // check if this is the end of a branch in the GraphMaster 
            // return the cCategory for this node
            if (this.NumberOfChildNodes == 0)
            {
                if (path.Length > 0)
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    this.storeWildCard(path, wildcard);
                }
                query.TemplatePath = "";
                return this.FirstTemplate();
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.Length == 0)
            {
                query.TemplatePath = "";
                return this.FirstTemplate();
            }

            // otherwise split the input into it's component words
            string[] splitPath = path.Split(" \r\n\t".ToCharArray());

            // get the first word of the sentence
            string firstWord = MakeCaseInsensitive.TransformInput(splitPath[0]);
            string nextPath = (absPath + " " + firstWord).Trim();

            // and concatenate the rest of the input into a new path for child nodes
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            if (this.ContainsChildKey("_"))
            {
                //Node childNode = this.ChildNode("_"];
                Node childNode = fetchChild(absPath, "_", pathDB);
                nextPath = (absPath + " " + "_").Trim();

                // add the next word to the wildcard match 
                StringBuilder newWildcard = new StringBuilder();
                this.storeWildCard(splitPath[0], newWildcard);

                // move down into the identified branch of the GraphMaster structure
                string result = childNode.evaluateDB(newPath, query, request, matchstate, newWildcard, nextPath, pathDB);

                // and if we get a result from the branch process the wildcard matches and return 
                // the result
                if (result.Length > 0)
                {
                    // capture and push the star content appropriate to the matchstate if it exists
                    AcceptWildcard(query, newWildcard, matchstate);
                    query.TemplatePath = "_ " + query.TemplatePath;
                    return result;
                }
            }

            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            if (this.ContainsChildKey(firstWord))
            {
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                MatchState newMatchstate = GetNewMatchstate(firstWord, matchstate);

                //Node childNode = this.ChildNode(firstWord];
                Node childNode = fetchChild(absPath, firstWord, pathDB);
                nextPath = (absPath + " " + firstWord).Trim();

                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                StringBuilder newWildcard = new StringBuilder();
                string result = childNode.evaluateDB(newPath, query, request, newMatchstate, newWildcard, nextPath, pathDB);
                // and if we get a result from the child return it
                if (result.Length > 0)
                {
                    // capture and push the star content appropriate to the matchstate if it exists and clear afterward
                    AcceptWildcard(query, newWildcard, matchstate);
                    query.TemplatePath = firstWord + " " + query.TemplatePath;
                    return result;
                }
            }
            // Patch option: input contains a semantic wildcard
            // like "*dog" or "*dog:hypo:n" or "*planet" ,"*celestial_body"
            bool hasSemantic = hasWildSense(this.children);
            bool isSemantic = isWildPattern(this.word);
            if (hasSemantic)
            {
                foreach (string s in this.ChildKeys)
                {
                    if (isWildPattern(s))
                    {
                        if (matchesWildSense(s, firstWord, request))
                        {
                            // Treat like matching a wildcard
                             Console.WriteLine ("A matchesWildSense({0},{1},{2})==true",s, firstWord, request);
                            // o.k. look for the path in the child node denoted by "*"
                            //Node childNode = this.ChildNode(s);
                            Node childNode = fetchChild(absPath, s, pathDB);
                            nextPath = (absPath + " " + s).Trim();

                            // add the next word to the wildcard match 
                            StringBuilder newWildcard = new StringBuilder();
                            this.storeWildCard(splitPath[0], newWildcard);

                            //string result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);
                            string result = childNode.evaluateDB(newPath, query, request, matchstate, newWildcard, nextPath, pathDB);
                            // and if we get a result from the branch process and return it
                            if (result.Length > 0)
                            {
                                // capture and push the star content appropriate to the matchstate if it exists and clear afterward
                                AcceptWildcard(query, newWildcard, matchstate);
                                query.TemplatePath = s + " " + query.TemplatePath;
                                return result;
                            }
                        }
                    }
                }

            }

            // third option - the input part of the path might have been matched so far but hasn't
            // returned a match, so check to see it contains the "*" wildcard. "*" comes last in
            // precedence in the AIML alphabet.
            if (this.ContainsChildKey("*"))
            {
                // o.k. look for the path in the child node denoted by "*"
                //Node childNode = this.ChildNode("*"];
                Node childNode = fetchChild(absPath, "*", pathDB);
                nextPath = (absPath + " " + "*").Trim();

                // add the next word to the wildcard match 
                StringBuilder newWildcard = new StringBuilder();
                this.storeWildCard(splitPath[0], newWildcard);

                string result = childNode.evaluateDB(newPath, query, request, matchstate, newWildcard, nextPath, pathDB);
                // and if we get a result from the branch process and return it
                if (result.Length > 0)
                {
                    // capture and push the star content appropriate to the matchstate if it exists
                    AcceptWildcard(query, newWildcard, matchstate);
                    query.TemplatePath = "* " + query.TemplatePath;
                    return result;
                }
            }
            // we have a semantic node in our children but we may neet to fill a multi-word
            // wildcard. So allow "*semantic" to absorb multi-word if it makes sense
            if (hasSemantic)
            {
                // Test append the current word to the wild card 
                //to produce canidate multi-word
                string multiWord = wildcard.ToString();
                if (multiWord.Length > 0)
                {
                    multiWord += " " + splitPath[0];
                }
                else
                {
                    multiWord += splitPath[0];
                }
                //Scan children for matching semantic pattern
                foreach (string s in this.ChildKeys)
                {
                    if (isWildPattern(s))
                    {
                        if (matchesWildSense(s, multiWord, request))
                        {
                            Console.WriteLine("B matchesWildSense({0},{1},{2})==true", s, multiWord, request);
                            // We found one so treat like the expanding multi-word "*" case
                            this.storeWildCard(splitPath[0], wildcard);

                            string result = this.evaluateDB(newPath, query, request, matchstate, wildcard, absPath, pathDB);
                            query.TemplatePath = s + " " + query.TemplatePath;
                            return result;
                        }
                    }
                }
            }
            // o.k. if the nodemapper has failed to match at all: the input contains neither 
            // a "_", the sFirstWord text, or "*" as a means of denoting a child node. However, 
            // if this node is itself representing a wildcard then the search continues to be
            // valid if we proceed with the tail.
            if ((this.word == "_") || (this.word == "*"))
            {
                this.storeWildCard(splitPath[0], wildcard);
                string result = this.evaluateDB(newPath, query, request, matchstate, wildcard, absPath, pathDB);
                query.TemplatePath = this.word + " " + query.TemplatePath;
                return result;
            }

            // If we get here then we're at a dead end so return an empty string. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            wildcard = new StringBuilder();
            query.TemplatePath = "";
            return string.Empty;
        }

        private void AcceptWildcard(SubQuery query, StringBuilder newWildcard, MatchState matchstate)
        {
            if (newWildcard.Length > 0)
            {
                // capture and push the star content appropriate to the current matchstate
                switch (matchstate)
                {
                    case MatchState.Pattern:
                        GetStarCollector(query, matchstate).Add(newWildcard.ToString());
                        // added due to this match being the end of the line
                        newWildcard.Remove(0, newWildcard.Length);
                        break;
                    default:
                        GetStarCollector(query, matchstate).Add(newWildcard.ToString());
                        // @todo is this right?
                        newWildcard.Remove(0, newWildcard.Length);
                        break;
                }
            }
        }

        private List<string> GetStarCollector(SubQuery query, MatchState matchstate)
        {
            List<string> StarCollector = null;
            StarCollector = query.GetStars(matchstate.ToString());
            return StarCollector;
        }

        public Node fetchChild(string myPath, string childWord, ExternDB pathDB)
        {
            if (myPath != GetPath().Trim())
            {

            }
            string childPath = (myPath + " " + childWord).Trim();
            lock (ExternDB.mylock)
            {
                Node childNode = pathDB.fetchNode(childPath, true);
                childNode.word = childWord;
                childNode.Parent = this;
                return childNode;
            }
        }
 
        // does the dictionary contain any patterns like "*dog" or "*canine:syno:n"
        bool hasWildSense(Dictionary <string,Node> dic)
        {
            foreach (string s in dic.Keys)
            {
                if (isWildPattern(s))
                {
                   return true;
                }
            }
            return false;
        }

        static bool isWildPattern(string s)
        {
            if (!(s.Length > 1)) return false;
            char c = s[0];
            bool v = (c == '*' || c == '~');
            if (!v)
            {
                return false;
            }
            return v;
        }

        /// <summary>
        /// isInvalidRegex allows us to skip generating an exception when the regex is a known bad
        ///   this is not supposed to be complete.. just some cases
        /// </summary>
        /// <param name="wnWord"></param>
        /// <returns></returns>
        static bool isInvalidRegex(string wnWord)
        {
            if (wnWord == null || wnWord == "*")
            {
                return true;
            }
            return false;
        }

        static bool matchesWildSense(string sense, string queryWord,Request request)
        {
            if (sense == queryWord) return true;
            // always clip off the first "*";
            sense = sense.Substring(1);
            if (sense.Length == 0) return false;
            AltBot contextBot = request.bot;

            // ported from unifying AIML bot's <lexis> tag
            // form is "*<word>:<relation>:<part-of-speech>:<+/~>"
            // <word> can be a regular expression 
            // Should support "*canine", "*canine:hypo","*canine:hypo:n"
            // and "*can::v" 
            // Also POS can be "bot" or "user" for reference to the predicates
            // so  "*location::bot" or "*girlfriend::user"
            // can also have negation, 
            //   for "*girfriend::user:~" or or "*disease::n:~"
            // default is POS=noun, Relations = InstanceHypernym +Hypernym
            string[] senseArgs = sense.ToLower().Split(':');
            string wnWord = "";
            string wnRelation ="";
            string wnPos = "";
            bool negation = false;

            wnWord = senseArgs[0];
            if (senseArgs.Length >=2) wnRelation = senseArgs[1];
            if (senseArgs.Length >= 3) wnPos = senseArgs[2];
            if (senseArgs.Length >= 4) negation = senseArgs[3].ToLower().Contains("~");

            Console.WriteLine("MWS:{0},{1},{2},{3} [{4}]", wnWord, wnRelation, wnPos, negation, queryWord);

            // Can you find a match inside (using regex while we're here)?
            if (!isInvalidRegex(wnWord))
            {
                try
                {
                    var matcher = new Regex(wnWord);
                    if (matcher.IsMatch(queryWord))
                    {
                        Console.WriteLine("MWS:Regex Match");
                        return (true ^ negation);
                    }

                }
                catch (Exception e)
                {
                    contextBot.writeToLog("ERROR: " + e);
                }
            }
            // bot settings check

            if (wnPos == "bot")
            {
                string val = contextBot.GlobalSettings.grabSetting(wnWord);
                if (val == null) return (false ^ negation);
                if (queryWord.ToLower().Contains(val.ToLower()))
                {
                    Console.WriteLine("MWS:bot pred Match");
                    return (true ^ negation);
                }
                return (false ^ negation);
            }

            if (wnPos == "user")
            {
                string val = request.user.Predicates.grabSetting(wnWord);
                if (val == null) return (false ^ negation);
                if (queryWord.ToLower().Contains(val.ToLower()))
                {
                    Console.WriteLine("MWS:user pred Match");
                    return (true ^ negation);
                }
                return (false ^ negation);
            }

            // Ok, lets try WordNet
            //if ((contextBot == null) || (contextBot.wordNetEngine == null)) return (false ^ negation);
           
            // NO ENGINE == JUST PLAIN FALSE (unknowable == false)
            if ((contextBot == null) || (contextBot.wordNetEngine == null)) return (false);
           
            WordNetEngine ourWordNetEngine = contextBot.wordNetEngine;
            Set<SynSet> synPatternSet = null;
            // find our POS domain if possible
            WordNetEngine.POS ourPOS = WordNetEngine.POS.Noun;
            if (wnPos.Length > 0)
            {
                // populate POS list
                foreach (WordNetEngine.POS p in Enum.GetValues(typeof(WordNetEngine.POS)))
                    if (p != WordNetEngine.POS.None)
                    {
                        if (p.ToString().ToLower().Equals(wnPos) || p.ToString().ToLower().StartsWith(wnPos))
                        {
                            ourPOS = p;
                        }
                    }
            }
            try { synPatternSet = ourWordNetEngine.GetSynSets(wnWord, ourPOS); }
            catch (Exception)
            {
                return (false ^ negation);
            }
            if (synPatternSet.Count == 0)
            {
                try { synPatternSet = ourWordNetEngine.GetSynSets(wnWord.ToLower(), ourPOS); }
                catch (Exception)
                {
                    return (false ^ negation);
                }

            }

            Set<SynSet> synInputSet = null;
            try { synInputSet = ourWordNetEngine.GetSynSets(queryWord, ourPOS); }
            catch (Exception)
            {
                return (false ^ negation);
            }
            if (synInputSet.Count == 0)
            {
                try { synInputSet = ourWordNetEngine.GetSynSets(queryWord.ToLower(), ourPOS); }
                catch (Exception)
                {
                    return (false ^ negation);
                }

            }


            List<WordNetEngine.SynSetRelation> vlist = new List<WordNetEngine.SynSetRelation>(); //[2];
            //vlist[0] = WordNetEngine.SynSetRelation.Hyponym;
            //vlist[1] = WordNetEngine.SynSetRelation.InstanceHyponym;
            //vlist[0] = WordNetEngine.SynSetRelation.Hypernym ;
            //vlist[1] = WordNetEngine.SynSetRelation.InstanceHypernym;
            if (wnRelation.Length == 0)
            {
                vlist.Add(WordNetEngine.SynSetRelation.Hypernym);
                vlist.Add(WordNetEngine.SynSetRelation.InstanceHypernym);
            }
            else
            {
                // populate Relation list
                foreach (WordNetEngine.SynSetRelation r in Enum.GetValues(typeof(WordNetEngine.SynSetRelation)))
                    if (r != WordNetEngine.SynSetRelation.None)
                    {
                        if (r.ToString().ToLower().Contains(wnRelation))
                        {
                            vlist.Add(r);
                        }
                    }

            }

            if ((synInputSet.Count > 0) && (synPatternSet.Count > 0))
            {
                foreach (SynSet synDstSet in synInputSet)
                {
                    foreach (SynSet synSrcSet in synPatternSet)
                    {
                        //synSets.Items.Add(synSet);
                        List<SynSet> linkageList = null;

                        linkageList = synDstSet.GetShortestPathTo(synSrcSet, vlist);
                        if ((linkageList != null) && (linkageList.Count > 0))
                        {
                            Console.WriteLine("MWS:WordNetMatch Match");
                            foreach (SynSet link in linkageList)
                            {
                                Console.WriteLine("MWS: link({0})", link.ToString());
                            }
                            return (true ^ negation);
                        }
                    }
                }
                return (false ^ negation);
            }


            return (false ^ negation);
        }

        public void addChild(string childWord,Node nd)
        {
            lock (children)
            {                
                childmax++;
                bool ndNull = nd == null;
                if (!children.ContainsKey(childWord))
                {
                    children.Add(childWord, nd);
                }
                else
                {
                    if (ndNull)
                    {
                    }
                }
                if (!ndNull) nd.Parent = this;
                childrenStr += "(" + childWord + ")";
                if (fullChildSet) return;
                if ((childnum + NumberOfChildNodes) != childmax)
                {
                    Console.WriteLine("WARNING : {0} myNode.childnum({1}) + myNode.NumberOfChildNodes({2}) != myNode.childmax({3})", "IN addChild", childnum, NumberOfChildNodes, childmax);
                    if ((childnum + NumberOfChildNodes) > childmax) childmax = (childnum + NumberOfChildNodes);
                }
            }
        }

        public bool dbContainsNode(string abspath, ExternDB pathdb)
        {
            return pathdb.containsNode(abspath);

        }
        /// <summary>
        /// Correctly stores a word in the wildcard slot
        /// </summary>
        /// <param name="word">The word matched by the wildcard</param>
        /// <param name="wildcard">The contents of the user input absorbed by the AIML wildcards "_" and "*"</param>
        private void storeWildCard(string word, StringBuilder wildcard)
        {
            if (wildcard.Length > 0)
            {
                wildcard.Append(" ");
            }
            wildcard.Append(word);
        }
        #endregion

        #endregion

        public void WithFilename(string filename, bool remove, bool enable)
        {
            if (_c0 != null)
                foreach (var c in ChildNodes)
                {
                    c.WithFilename(filename, remove, enable);
                }
            if (templates != null && templates.Count > 0)
            {
                foreach (OutputTemplate list in LockInfo.CopyOf(templates))
                {
                    if (list.IsFile(filename))
                    {
                        if (remove)
                        {
                            templates.Remove(list);
                        }
                        else
                        {
                            list.disable = !enable;
                        }
                    }
                }
            }
        }
#if UNUSED
    /// <summary>
    /// The AIML source for the category that defines the template
    /// </summary>
        private string filename = Unifiable.Empty;
#endif
        //private XmlNode GuardText;

        public override string ToString()
        {
            return GetPath();
        }

        public string GetPath()
        {
            //if (absPath != null) return absPath;
            var p = Parent;
            if (p == null)
            {
                return word;
            }
            StringBuilder sb = new StringBuilder(word);
            sb.Insert(0, " ");
            sb.Insert(0, p.word);
            p = p.Parent;
            while (p != null)
            {
                sb.Insert(0, " ");
                sb.Insert(0, p.word);
                p = p.Parent;
            }
            return sb.ToString();
        }


        public bool Equals(Node other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return false;
            if (!Equals(other.word, this.word)) return false;
            //if (!Equals(other.Graph, Graph)) return false;
            if (!Equals(other.Parent, Parent)) return false;
            throw new AbandonedMutexException("ERROR optimally this should be imposible");
            return true;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((Parent != null ? Parent.GetHashCode() : 0) * 397) ^ (word != null ? word.GetHashCode() : 0);
            }
        }
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof(Node)) return false;
            return Equals((Node)obj);
        }
        #region IComparable<Node> Members

        public int CompareTo(Node that)
        {
            return CompareNodes(this, that);
        }
        public static int CompareNodes(Node thiz, Node that)
        {
            if (thiz.Equals(that)) return 0;
            var thispath = thiz.ToPath();
            var thatpath = that.ToPath();
            int cmp = ComparePaths(thiz, that, thispath, thatpath);
            if (cmp == 0) return 0;
            return cmp;
        }

        static int ComparePaths(Node thiz, Node that, IList<Unifiable> thispath, IList<Unifiable> thatpath)
        {
#if USE_COMPARE_CACHE
            double a1 = thiz._variance;
            double b1 = that._variance;
#else
            double a1 = 0;
            double b1 = 0;
#endif
            int diff0 = a1.CompareTo(b1);
            if (diff0 != 0)
            {
                return -diff0;
            }
            int cmpthis = thispath.Count;
            int cmpthat = thatpath.Count;

            //smaller of the two
            if (cmpthis < cmpthat) cmpthis = cmpthat;
            //if (cmpthis == cmpthat)
            //{
            // double strictnessThis = 0;
            //  double detailThat = 0;
            for (int i = 0; i < cmpthis; i++)
            {
                Unifiable thatpath1 = thatpath[i];
                Unifiable thispath1 = thispath[i];
                int diff = thispath1.CompareTo(thatpath1);
                if (diff != 0) return diff;
                a1 -= Strictness(thatpath1);
                b1 -= Strictness(thispath1);
            }
            if (a1 == b1)
            {
                return ReferenceCompare(thiz, that);
            }
            return a1.CompareTo(b1);
            //}
            //   return cmpthis.CompareTo(cmpthat);
        }
        public static int ReferenceCompare(Object thiz, Object other)
        {
            if (ReferenceEquals(thiz, other)) return 0;
            int cmpthis = RuntimeHelpers.GetHashCode(thiz);
            int cmpthat = RuntimeHelpers.GetHashCode(other);
            if (cmpthis == cmpthat)
            {
                throw new InvalidCastException(thiz + " == " + other);
            }
            return cmpthis.CompareTo(cmpthat);
        }
#if USE_COMPARE_CACHE
        public IList<Unifiable> _ToPath;
        public double _variance;
#endif
        public IList<Unifiable> ToPath()
        {
#if USE_COMPARE_CACHE
#else
            IList<Unifiable> _ToPath = null;
            double _variance = double.NaN;
#endif
            _ToPath = null;
            if (_ToPath != null) return _ToPath;
            _variance = Strictness(word);

            var p = Parent;
            if (p == null)
            {
                return (_ToPath = new[] {(Unifiable) word});
            }
            var sb = new List<Unifiable> { word };
            var pword = p.word;
            sb.Add(pword);
            _variance += Strictness(pword);
            p = p.Parent;
            while (p != null)
            {
                pword = p.word;
                sb.Add(pword);
                _variance += Strictness(pword);
                p = p.Parent;
            }
            return (_ToPath = sb.ToArray());
        }

        static double Strictness(string pword)
        {
            return pword.Length;
        }

        #endregion
    }

    public class OutputTemplate
    {
        public string Template;
        public bool disable;
        public string filename;

        public bool IsFile(string s)
        {
            return filename.ToLower().Contains(s);
        }
    }

    public class ExternDB
    {

        //using RaptorDB for persistent local key-value dictionary like storage
        //http://www.codeproject.com/Articles/190504/RaptorDB
        //http://www.codeproject.com/Articles/316816/RaptorDB-The-Key-Value-Store-V2
        public static bool AppendGraphmasterNameToDirectory = false;

        public RaptorDB.KeyStoreString[] templatedb = null;
        public RaptorDB.KeyStoreString[] childdb = null;
        public RaptorDB.KeyStoreString[] childtrunkdb = null;
        public RaptorDB.KeyStoreString[] childcntdb = null;
        public RaptorDB.KeyStoreString[] scoredb = null;
        public RaptorDB.KeyStoreString[] filenamedb = null;
        public RaptorDB.KeyStoreString[] worddb = null;
        public RaptorDB.KeyStoreString loadeddb;
        public RaptorDB.KeyStoreString crondb;

        public Dictionary<string, string> childcache = new Dictionary<string, string>();
        public Dictionary<string, Node> nodecache = new Dictionary<string, Node>();

        public int slices = 7;//63; //7;
        public int trunkLevel = 5;//6; //5;
        public bool verify = true;
        public AltBot bot = null;
        public string _dbdir = "";
        public static object mylock = new object ();
        public bool allLoaded = false;
        public bool expectedOpen = false;

        public GraphMaster Graph;

        public ExternDB()
        {

            if (Servitor.DebugLevelExternalDb > 0) Console.WriteLine("ExternDB()");
            string dbdirectory = ".\\rapstore\\";
            string _dbdir = dbdirectory;
            
            string ourPath = Directory.CreateDirectory(dbdirectory).FullName;
            string ourDirectory = Path.GetDirectoryName(ourPath);
            loadeddb = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "loadeddb", false);
            crondb = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "crondbdb", false);
            templatedb = new KeyStoreString[slices];
            childdb = new KeyStoreString[slices];
            childtrunkdb = new KeyStoreString[slices];
            childcntdb = new KeyStoreString[slices];
            scoredb = new KeyStoreString[slices];
            filenamedb = new KeyStoreString[slices];
            worddb = new KeyStoreString[slices];
            allLoaded = false;
        }

        public ExternDB(string dbdirectory)
        {
            dbdirectory = HostSystem.FileSystemPath(dbdirectory);
            if (Servitor.DebugLevelExternalDb > 0) Console.WriteLine("ExternDB({0})", dbdirectory);
            string _dbdir = dbdirectory;
            string ourPath = Directory.CreateDirectory(dbdirectory).FullName;
            string ourDirectory = Path.GetDirectoryName(ourPath);

            loadeddb = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "loadeddb", false);
            crondb = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "crondbdb", false);
            templatedb = new KeyStoreString[slices];
            childdb = new KeyStoreString[slices];
            childtrunkdb = new KeyStoreString[slices];
            childcntdb = new KeyStoreString[slices];
            scoredb = new KeyStoreString[slices];
            filenamedb = new KeyStoreString[slices];
            worddb = new KeyStoreString[slices];
            allLoaded = false;
        }

        public void OpenAll()
        {
            lock (mylock)
            {
                if (expectedOpen)
                {
                    return;
                }
                expectedOpen = true;
            }
            if (Servitor.DebugLevelExternalDb > 0) Console.WriteLine("OpenAll()");
            string dbdirectory = _dbdir;
            dbdirectory = HostSystem.FileSystemPath(dbdirectory);
            string ourPath = Directory.CreateDirectory(dbdirectory).FullName;
            string ourDirectory = Path.GetDirectoryName(ourPath);

            if (bot != null)
            {
               if (bot.rapStoreSlices>0) slices = bot.rapStoreSlices;
               if (bot.rapStoreTrunkLevel > 0) trunkLevel = bot.rapStoreTrunkLevel;
            }


            for (int i = 0; i < slices; i++)
            {
                templatedb[i] = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "templatedb" + i.ToString(), false);
                childdb[i] = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "childdb" + i.ToString(), false);
                childtrunkdb[i] = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "childtrunkdb" + i.ToString(), false);
                childcntdb[i] = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "childcntdb" + i.ToString(), false);
                scoredb[i] = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "scoredb" + i.ToString(), false);
                filenamedb[i] = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "filenamedb" + i.ToString(), false);
                worddb[i] = new RaptorDB.KeyStoreString(ourDirectory + Path.DirectorySeparatorChar + "worddb" + i.ToString(), false);

                if (Servitor.DebugLevelExternalDb > 1)
                {
                    Console.WriteLine("OpenAll {0}:'{1}'", i,
                                      ourDirectory + Path.DirectorySeparatorChar + "templatedb" + i.ToString());
                }
            }
            if (Servitor.DebugLevelExternalDb == 1)
            {
                Console.WriteLine("OpenAll 0-{0}:'{1}{2}templatedb(0-{0})'", slices, ourDirectory,
                                  Path.DirectorySeparatorChar);
            }
            allLoaded = true;
        }
        public void SaveIndex()
        {
            for (int i = 0; i < slices; i++)
            {

                if (templatedb[i] != null) templatedb[i].SaveIndex();
                if (childdb[i] != null) childdb[i].SaveIndex();
                if (childtrunkdb[i] != null) childtrunkdb[i].SaveIndex();
                if (childcntdb[i] != null) childcntdb[i].SaveIndex();
                if (scoredb[i] != null) scoredb[i].SaveIndex();
                if (filenamedb[i] != null) filenamedb[i].SaveIndex();
                if (worddb[i] != null) worddb[i].SaveIndex();

            }
            loadeddb.Shutdown();
            crondb.Shutdown();
        }

        public void logText(string msg)
        {
            BehaviorSet.logTextToBTTrace(msg);
        }

        public void ClearCache()
        {
            //flush our cache out
            List<string> trunklist = new List<string>();
            if (nodecache != null)
            {
                foreach (string k in nodecache.Keys)
                {
                    if (trunklist != null) trunklist.Add(k);
                }
            }
            if (trunklist != null)
            {
                foreach (string k in trunklist)
                {
                    saveNode(k, nodecache[k], true);
                }
            }
            if (nodecache != null) nodecache.Clear();
            if (trunklist != null) trunklist.Clear();
        }
        public void Close()
        {
            //flush our cache out
            ClearCache();

            /*
            foreach (string childkey in childcache.Keys)
            {
                string childtxt = childcache[childkey];
                if (isTrunk (childkey))
                {
                    childtrunkdb.RemoveKey(childkey);
                    childtrunkdb.Set(childkey, childtxt);
                }
                else
                {
                    childdb.RemoveKey(childkey);
                    childdb.Set(childkey, childtxt);
                }
            }
            */

            for (int i = 0; i < slices; i++)
            {
               //Console.WriteLine("Close {0}", i);
                int dbp = 0;
                try
                {
                    if (templatedb[i] != null) templatedb[i].Shutdown();
                    dbp++;
                    if (childdb[i] != null) childdb[i].Shutdown();
                    dbp++;
                    if (childtrunkdb[i] != null) childtrunkdb[i].Shutdown();
                    dbp++;
                    if (childcntdb[i] != null) childcntdb[i].Shutdown();
                    dbp++;
                    if (scoredb[i] != null) scoredb[i].Shutdown();
                    dbp++;
                    if (filenamedb[i] != null) filenamedb[i].Shutdown();
                    dbp++;
                    if (worddb[i] != null) worddb[i].Shutdown();
                }
                catch (Exception e)
                {
                    
                    logText("ERR index=" + i +"("+ dbp+"):" + e.Message);
                    logText("ERR STK:" + e.StackTrace);
                }
            }
            if (loadeddb != null) loadeddb.Shutdown();
            if (crondb != null) crondb.Shutdown();
            //GC.Collect();
            if (Servitor.DebugLevelExternalDb > 0) Console.WriteLine("ExternDB.Close()");
        }
        public void prune(int prunelimit)
        {
            childcache.Clear();
            List<string> trunklist = new List<string>();
            foreach (string k in nodecache.Keys)
            {
                trunklist.Add(k);
            }
            foreach (string k in trunklist)
            {
                int v = nodecache[k].childmax;
                if (v < prunelimit)
                {
                    string[] s = k.Split(' ');
                    int depth = s.Length;
                    if (depth > 4)
                    {
                        // deeper than "<state> * <pattern> *"
                        nodecache.Remove(k);
                    }
                }
            }

        }
        public void rememberLoaded(string filename)
        {
            bool isMt = IsMt(filename);
            bool isVf = IsVf(filename);
            string orig = filename;
            if (!isMt && !isVf) filename = HostSystem.FileSystemPath(filename);
            string reftime = DateTime.Now.ToUniversalTime().ToString(); // "indefinite";
            if (!isMt && !isVf && File.Exists(filename))
            {
                DateTime lastWriteTimeUtc = File.GetLastWriteTimeUtc(filename);
                reftime = lastWriteTimeUtc.ToString();
            }
            //loadeddb.Set(filename, filename);
            Console.WriteLine("\nrememberLoaded: {0} ==> {1}", filename, reftime);
            loadeddb.Set(filename, reftime);
        }
        public void forgetLoaded(string filename)
        {
            bool isMt = IsMt(filename);
            bool isVf = IsVf(filename);
            string orig = filename;
            if (!isMt && !isVf) filename = HostSystem.FileSystemPath(filename);
            string reftime = "never";
            Console.WriteLine("\nforgetLoaded: {0} ==> {1}", filename, reftime);
            loadeddb.Set(filename, reftime);
        }
        public bool wasLoaded(string filename)
        {
            if (GraphMaster.DeferingSaves) return false;

            bool isMt = IsMt(filename);
            bool isVf = IsVf(filename);
            string orig = filename;
            if (!isMt && !isVf)
            {
                // convert to actual file
                filename = HostSystem.FileSystemPath(filename);
            }
            string lf = "";
            string reftime = "indefinite";
            if (!isMt && !isVf && File.Exists(filename))
            {
                DateTime lastWriteTimeUtc = File.GetLastWriteTimeUtc(filename);
                reftime = lastWriteTimeUtc.ToString();
            }
            bool ret = loadeddb.Get(filename, out lf);
            //return (filename == lf);
            if (string.IsNullOrEmpty(lf)) lf = "Unknown";
            Console.WriteLine("\nwasLoaded:{0} {1}<=>{2}", filename, reftime, lf);
            if (reftime != lf)
            {
                return false;
            }
            if (GlobalSharedSettings.IsDougsMachine)
            {
                return false;
            }
            return true;
        }

        private bool IsMt(string filename)
        {
            return (filename.StartsWith("mt:"));
        }
        private bool IsVf(string filename)
        {
            return (filename.StartsWith("vf:"));
        }

        public void saveCronList(Cron sourceCron)
        {
            Dictionary<string,string> cronDict = sourceCron.cronLinesDictionary();
            string keylist = "";
            foreach (string cronID in cronDict.Keys)
            {
                string cronCode = cronDict[cronID];
                crondb.Set(cronID, cronCode);
                keylist += cronID + "|";
            }
            crondb.Set("savedCronKeys8123", keylist);
        }

        public void loadCronList(Cron sourceCron)
        {
            string keylistStr = "";
            bool keysFound = crondb.Get("savedCronKeys8123", out keylistStr);
            if (keysFound)
            {
                string[] cronIDs = keylistStr.Split('|');
                foreach (string cronID in cronIDs)
                {
                    string cronCode = "";
                    bool ret = crondb.Get(cronID, out cronCode);
                    if (ret)
                    {
                        sourceCron.addLine(cronCode);
                    }

                }
            }
        }

        public bool isTrunk(string absPath)
        {
            string[] s = absPath.Split(' ');
            return (s.Length <= trunkLevel);
        }
        public int pathToSlice(string absPath)
        {
            int v = Math.Abs(absPath.GetHashCode() % slices);
            return v;
        }
        public bool containsNode(string absPath)
        {
            string cntStr = null;
            int pslice = pathToSlice(absPath);
            childcntdb[pslice].Get(absPath, out cntStr);
            return (cntStr != null);
        }
        public Node fetchNode(string absPath,bool full)
        {
            //Console.WriteLine("Check: fetchNode({0}) in _dbdir='{1}'", absPath, _dbdir);
            try
            {
                int pslice = pathToSlice(absPath);


                if (nodecache.ContainsKey(absPath))
                {
                    return nodecache[absPath];
                }

                Node myNode = new Node();
                myNode.extDB = this;
                myNode.absPath = absPath;
                myNode.fullChildSet = full;
                bool trunk = isTrunk(absPath);

                string scoreStr = "1.0";
                string cntStr = "0";
                myNode.score = 1.0;
                myNode.childnum = 0;
                myNode.childmax = 0;

                scoredb[pslice].Get(absPath, out scoreStr);
                string t, f;
                templatedb[pslice].Get(absPath, out t);
                filenamedb[pslice].Get(absPath, out f);
                myNode.AddTemplate(t,f);
                //worddb.Get(absPath, out  myNode.word);
                childcntdb[pslice].Get(absPath, out cntStr);
                if (absPath.Length == 0)
                {
                    childcntdb[pslice].Get(absPath, out cntStr);
                    trunk = trunk;
                    if (full) cntStr = "1";
                }

                if (scoreStr != null) myNode.score = double.Parse(scoreStr);
                if (cntStr != null)
                {
                    myNode.childnum = int.Parse(cntStr);
                    myNode.childmax = myNode.childnum;

                }
                // Restore the list from memory
                /*
                string origChildren = myNode.childrenStr;
                //myNode.childrenList = new List<string>();
                if (origChildren != null)
                {
                    string[] childList = origChildren.Split('|');
                    foreach (string c in childList)
                    {
                        //myNode.childrenList.Add(c);
                        myNode.children.Add(c, null);
                    }
                }
                */
                if (full)
                {
                    lock (myNode.children)
                    {
                        for (int i = 0; i < myNode.childnum; i++)
                        {
                            string childkey = absPath + "#" + i.ToString();
                            string childtxt = "";
                            if (childcache.ContainsKey(childkey))
                            {
                                childtxt = childcache[childkey];
                            }
                            else
                            {
                                if (trunk)
                                {
                                    childtrunkdb[pslice].Get(childkey, out  childtxt);
                                }
                                else
                                {
                                    childdb[pslice].Get(childkey, out childtxt);
                                }
                            }

                            if ((childtxt != null) && (!myNode.ContainsChildKey(childtxt)))
                            {
                                //myNode.childrenList.Add(childtxt);

                                myNode.children.Add(childtxt, null);
                                if (childcache.ContainsKey(childkey))
                                {
                                    childcache[childkey] = childtxt;
                                }
                                else
                                {
                                    childcache.Add(childkey, childtxt);
                                }

                            }

                            if (childtxt == null)
                            {
                                byte[] bkey = Encoding.Unicode.GetBytes(childkey);
                                int hc = (int)RaptorDB.Helper.MurMur.Hash(bkey);
                                logText(String.Format("WARNING: Get({0} ({1}) ) returns null", childkey, hc));

                            }
                        }
                    }
                }
                string childrenStr = "";
                //foreach (string c in myNode.ChildKeys)
                //{
                //    childrenStr += "(" + c + ")";
                //}
                myNode.childrenStr = childrenStr;

                PrepNode(myNode);

                //Save to local if it's needed
                if ((trunk) || (myNode.childnum > 8))
                {
                    if (nodecache.ContainsKey(absPath))
                    {
                        nodecache[absPath] = myNode;
                    }
                    else
                    {
                        nodecache.Add(absPath, myNode);
                    }
                }
                return myNode;
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                Console.WriteLine(e.StackTrace);

            }
            return null;
        }

        public void saveNode(string absPath, Node myNode)
        {
            //Console.WriteLine("Check: saveNode({0})", absPath);
            if (Graph != null)
            {
                Graph.Size++;
            }
            if (GraphMaster.DeferingSaves) return;
            saveNode(absPath, myNode, false);
        }

        public void saveNode(string absPath, Node myNode,bool flushing)
        {
            try
            {
                int pslice = pathToSlice(absPath);
                bool trunk = isTrunk(absPath);
                if (trunk == true)
                {
                    // SAFE TO JUST LOCAL CACHE
                    if (nodecache.ContainsKey(absPath))
                    {
                        nodecache[absPath] = myNode;
                    }
                    else
                    {
                        nodecache.Add(absPath, myNode);
                    }
                }
                if ((flushing == false) && (trunk == true))
                {
                    return;
                }
                if (myNode.fullChildSet) return; // we are in eval mode so read-only

                // We are a writable node, in the trunk or flushing

                PrepNode(myNode);

                string childrenStr = "";
                //foreach (string c in myNode.ChildKeys)
               // {
                //    childrenStr += "(" + c + ")";
                //}


               // if ((childrenStr != myNode.childrenStr) && 
                if ((myNode.childnum + myNode.NumberOfChildNodes) != myNode.childmax )
                {
                    logText(String.Format("WARNING : {0} myNode.childnum({1}) + myNode.NumberOfChildNodes({2}) != myNode.childmax({3})", absPath, myNode.childnum, myNode.NumberOfChildNodes, myNode.childmax));
                    if ((myNode.childnum + myNode.NumberOfChildNodes) > myNode.childmax) myNode.childmax = (myNode.childnum + myNode.NumberOfChildNodes);
                }
                if (myNode.NumberOfChildNodes  > 0)
                {
                    int offset = myNode.childnum;
                    if (myNode.fullChildSet) offset = 0;
                    for (int i = 0; i < myNode.NumberOfChildNodes; i++)
                    {
                        int trueindex = i + offset;
                        string childkey = absPath + "#" + trueindex.ToString();
                        
                        //string childtxt = myNode.childrenList[i];
                        string childtxt = myNode.children.Keys.ElementAt(i);
                        string chk = "(" + childtxt + ")";
                        //if (!myNode.childrenStr.Contains(chk))
                          if (myNode.childrenStr.Contains(chk))
                            {
                            if (trunk)
                            {
                                //childtrunkdb[pslice].RemoveKey(childkey);
                                childtrunkdb[pslice].Set(childkey, childtxt);
                            }
                            else
                            {
                                //childdb[pslice].RemoveKey(childkey);
                                childdb[pslice].Set(childkey, childtxt);
                            }
                            if (childcache.ContainsKey(childkey))
                            {
                                childcache[childkey] = childtxt;
                            }
                            else
                            {
                                childcache.Add(childkey, childtxt);
                            }
                            if (verify)
                            {
                                string vstr = "";
                                if (trunk)
                                {
                                    childtrunkdb[pslice].Get(childkey, out vstr);
                                }
                                else
                                {
                                    childdb[pslice].Get(childkey, out vstr);
                                }
                                if (vstr != childtxt)
                                {

                                    logText(String.Format("WARNING : Childkey({0}) returns ({1}) when set was ({2})",childkey,vstr,childtxt));
                                }
                                if (childkey.Contains("TAG-STATE * TAG-PATTERN#"))
                                {
                                    //logText(String.Format("TRACE : Childkey({0}) set to ({1})",childkey,childtxt));

                                }
                            }

                        }
                    }
                }
                //myNode.childnum += myNode.NumberOfChildNodes;
                myNode.childmax = myNode.childnum + myNode.NumberOfChildNodes; 

                //myNode.childrenStr = childrenStr;

                if (absPath.Length == 0)
                {
                    trunk = trunk;
                }
                var template = myNode.FirstTemplate();
                if (template.Length >= 0) templatedb[pslice].Set(absPath, template);
                var filename = myNode.FirstFilename();
                if (filename.Length >= 0) filenamedb[pslice].Set(absPath, filename);
                //if (myNode.word.Length > 0) worddb.Set(absPath, myNode.word);
                if (myNode.score != 1.0) scoredb[pslice].Set(absPath, myNode.score.ToString());
                //if (myNode.childnum > 0) 
                childcntdb[pslice].Set(absPath, myNode.childmax.ToString());
                //Save to local if it's needed
                if ((trunk) || (myNode.childmax > 8))
                {
                    if (nodecache.ContainsKey(absPath))
                    {
                        nodecache[absPath] = myNode;
                    }
                    else
                    {
                        nodecache.Add(absPath, myNode);
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                
                Console.WriteLine(e.StackTrace);
            }
        }

        private void PrepNode(Node myNode)
        {
            //if (myNode.template == null) myNode.template = "";
            //if (myNode.filename == null) myNode.filename = "";
            if (myNode.word == null) myNode.word = "";
            if (myNode.childrenStr == null) myNode.childrenStr = "";
        }
    }
}