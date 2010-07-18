using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;


namespace RTParser.Utils
{
    /// <summary>
    /// Encapsulates a node in the graphmaster tree structure
    /// </summary>
    [Serializable]
    public class Node
    {

        private void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine("!NODE: " +  message + " in " +ToString(), args);
        }

        private Node Parent;
        public Node(Node P)
        {
            Parent = P;
        }
        #region Attributes

        /// <summary>
        /// Contains the child nodes of this node
        /// </summary>
        private Dictionary<Unifiable, Node> children = new Dictionary<Unifiable, Node>();

        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        public UList TemplateInfos = null;//Unifiable.Empty;

#if UNUSED
        /// <summary>
        /// The AIML source for the category that defines the template
        /// </summary>
        private string filename = Unifiable.Empty;
#endif
        /// <summary>
        /// The word that identifies this node to it's ParentResult node
        /// </summary>
        private Unifiable word = Unifiable.Empty;

        //private XmlNode GuardText;

        #endregion

        #region Methods

        #region Add category

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="template">the template to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        static public Node addCategoryTag(Node start, Unifiable path, PatternInfo patternInfo, CategoryInfo category, XmlNode outerTemplate, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo, GraphMaster master)
        {
            if (templateNode == null)
            {
                throw new XmlException("The category with a pattern: " + path + " found in file: " + category +
                                       " has an empty template tag. ABORTING");
            }
            //String ts = outTemplate.OuterXml;
            Node thiz = start.addPathNodeChilds(path);
            thiz.addTerminal(templateNode, category, guard, thatInfo, master, patternInfo);
            return thiz;
        }

        private void addTerminal(XmlNode templateNode, CategoryInfo category, GuardInfo guard, ThatInfo thatInfo, GraphMaster master, PatternInfo patternInfo)
        {
            const bool RemoveDupes = true; //slows it down but maybe important to do
            if (this.TemplateInfos == null)
            {
                this.TemplateInfos = new UList();
            }
            else if (RemoveDupes)
            {
                lock (this.TemplateInfos)
                {
                    // search for old
                    string newStr = templateNode.OuterXml;
                    int count = TemplateInfos.Count;
                    string newGuard = guard != null ? guard.OuterXml : null;
                    string newThat = thatInfo != null ? thatInfo.OuterXml : null;
                    List<TemplateInfo> dupes = null;
                    this.TemplateInfos.ForEach(delegate(TemplateInfo temp)
                                                   {
                                                       var categoryinfo1 = category;
                                                       var categoryinfo2 = temp.CategoryInfo;
                                                       string oldGuard = temp.Guard != null ? temp.Guard.OuterXml : null;
                                                       string oldThat = temp.That != null ? temp.That.OuterXml : null;
                                                       if (AIMLLoader.AimlSame(newStr, temp.Output.OuterXml))
                                                           if (AIMLLoader.AimlSame(newGuard, oldGuard))
                                                               if (AIMLLoader.AimlSame(newThat, oldThat))
                                                               {
                                                                   if (true) return;
                                                                   if (count==1)
                                                                   {
                                                                       // writeToLog("AIMLTRACE REDUNDANT " + TemplateInfos[0]);
                                                                       return;
                                                                   }
                                                                   if (dupes == null) dupes = new List<TemplateInfo>();
                                                                   dupes.Add(temp);
                                                               }
                                                   });
                    if (dupes != null)
                    {
                        if (TemplateInfos.Count == 1)
                        {
                            //writeToLog("AIMLTRACE REDUNDANT " + TemplateInfos[0]);
                            if (true) return;
                            // no side effect!
                            TemplateInfo temp = dupes[0];
                            master.RemoveTemplate(temp);
                            TemplateInfos = null;
                        } else
                            dupes.ForEach(delegate(TemplateInfo temp)
                                          {
                                              if (true)
                                              {
                                                //  writeToLog("AIMLTRACE REDUNDANT " + temp);
                                                  master.RemoveTemplate(temp);
                                                  this.TemplateInfos.Remove(temp);
                                              }
                                          });
                        dupes.Clear();
                        dupes = null;
                    }
                }
            }

            // last in first out addition
            TemplateInfo newTemplateInfo = TemplateInfo.GetTemplateInfo(templateNode, guard, thatInfo, this, category);
            newTemplateInfo.That = thatInfo;
            this.That = thatInfo;
            PatternInfo pat = patternInfo;
            if (category != null)
            {
                category.AddTemplate(newTemplateInfo);
                pat = category.Pattern;
            }
            if (pat != null)
            {
                if (thatInfo == null || thatInfo.FullPath.AsString() == "*")
                {
                    if (patternInfo.LoopsFrom(newTemplateInfo))
                    {
                        writeToLog("SKIPPING! " + pat + "==" + newTemplateInfo + "");
                        if (this.TemplateInfos.Count == 0)
                        {
                            this.TemplateInfos = null;
                        }
                        return;
                    }
                    Unifiable from;
                    Unifiable to;
                    if (false && patternInfo.DivergesFrom(newTemplateInfo, out from, out to))
                    {
                        writeToLog("SKIPPING! " + pat + "==" + newTemplateInfo + "");
                        if (this.TemplateInfos.Count == 0)
                        {
                            this.TemplateInfos = null;
                        }
                        return;
                    }
                }
                pat.GraphmasterNode = this;
                if (category != null) pat.AddCategory(category);

            }

            master.AddTemplate(newTemplateInfo);
            if (pat != patternInfo)
            {
                writeToLog("Wierd! " + pat);
                throw new InvalidCastException("weird");
            }
            this.TemplateInfos.Insert(0, newTemplateInfo);
        }

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="outTemplate">the outTemplate to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        private Node addPathNodeChilds(Unifiable path)
        {
            Node initial = null;
            // check we're not at the leaf node
            if (!path.IsWildCard() && path.AsString().Trim().Length == 0)
            {
                //this.GuardText = guard;
                //this.filename = filename;
                return this;
            }

            // otherwise, this sentence requires further child nodemappers in order to
            // be fully mapped within the GraphMaster structure.

            // split the input into its component words
            //Unifiable[] words0 = path./*Trim().*/Split();//" ".ToCharArray());

            Unifiable firstRaw = path.First(); // words0[0];
            string w = firstRaw.AsString();

            // get the first word (to form the key for the child nodemapper)
            //Unifiable firstWord = Normalize.MakeCaseInsensitive.TransformInput(firstRaw);
            Unifiable firstWord = firstRaw;

            // concatenate the rest of the sentence into a suffix (to act as the
            // path argument in the child nodemapper)
            Unifiable newPath = path.Rest(); // Unifiable.Join(" ", words0, 1, words0.Length - 1);
            // path.Rest();// Substring(firstWord.Length, path.Length - firstWord.Length).Trim();

            // o.k. check we don't already have a child with the key from this sentence
            // if we do then pass the handling of this sentence down the branch to the 
            // child nodemapper otherwise the child nodemapper doesn't yet exist, so create a new one  \
            bool found = false;
            lock (children)
            foreach (var c in this.children)
            {
                string ks = c.Key.AsString();
                string fs = firstWord.AsString();
                if (ks == fs)
                {
                    Node childNode = c.Value;
                    initial = childNode.addPathNodeChilds(newPath);
                    found = true;
                    break;
                }
                if (ks.ToLower().Trim() == fs.ToLower().Trim())
                {
                    Node childNode = c.Value;
                    initial = childNode.addPathNodeChilds(newPath);
                    found = true;
                    break;
                }
            }
            if (!found)
            {
                Node childNode = new Node(this);
                childNode.word = firstWord;
                initial = childNode.addPathNodeChilds(newPath);
                lock (children) this.children.Add(childNode.word, childNode);

            }
            if (initial == null) throw new NullReferenceException("no child node: " + this);
            return initial;
        }

        #endregion

        private ThatInfo That;

        #region Evaluate Node


        internal Node GetNextNode()
        {
            if (Parent == null) return null;
            bool useNext = false;
            lock (children) foreach (KeyValuePair<Unifiable, Node> v in Parent.children)
            {
                if (useNext) return v.Value;
                if (v.Value == this)
                {
                    useNext = true;
                }
            }
            if (useNext)
            {
                //     writeToLog(String.Format("Last key {0}", ToString()));
                return Parent.GetNextNode();
            }
            return null;
        }

        public override string ToString()
        {
            if (Parent != null) return String.Format("{0} {1}", Parent, Unifiable.ToVMString(word));
            return word;
        }

        private Node evaluate(string path, SubQuery query, Request request, MatchState matchstate, StringAppendableUnifiable wildcard)
        {
            var vv = evaluate00(path, query, request, matchstate, wildcard);
            return (vv == null || vv.disabled || vv.TemplateInfos == null || vv.TemplateInfos.Count == 0) ? null : vv;
        }

        public bool disabled = false;

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
        public Node evaluate00(string path, SubQuery query, Request request, MatchState matchstate, StringAppendableUnifiable wildcard)
        {
            // check for timeout
            // check for timeout
            if (DateTime.Now > request.TimesOutAt)
            {
                request.Proccessor.writeToLog("TIMEOUT! User: " +
                                              request.user.UserID + " raw input: \"" +
                                              request.rawInput + "\" in " + this);
                request.IsTraced = true;
                request.hasTimedOut = true;
                return null; // Unifiable.Empty;
            }

            // so we still have time!
            path = path.Trim();

            // check if this is the end of a branch in the GraphMaster 
            // return the cCategory for this node
            if (this.children.Count == 0)
            {
                if (path.Length > 0)
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    this.storeWildCard(path, wildcard);
                }
                return this;
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.Length == 0)
            {
                if (TemplateInfos == null || TemplateInfos.Count == 0)
                {
                }
                return this;
            }

            // otherwise split the input into it's component words
            string[] splitPath = path.Split(" \r\n\t".ToCharArray());

            // get the first word of the sentence
            string firstWord = Normalize.MakeCaseInsensitive.TransformInput(splitPath[0]);

            // and concatenate the rest of the input into a new path for child nodes
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            lock (children) foreach (KeyValuePair<Unifiable, Node> childNodeKV in this.children)
            {
                Unifiable childNodeWord = childNodeKV.Key;
                if (!childNodeWord.IsAnyWord()) continue;

                Node childNode = childNodeKV.Value;

                // add the next word to the wildcard match 
                StringAppendableUnifiable newWildcard = Unifiable.CreateAppendable();
                this.storeWildCard(splitPath[0], newWildcard);

                // move down into the identified branch of the GraphMaster structure
                var result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);

                // and if we get a result from the branch process the wildcard matches and return 
                // the result
                if (result != null)
                {
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the current matchstate
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Insert(0, newWildcard.ToString());
                                // added due to this match being the end of the line
                                newWildcard.Clear(); // Remove(0, newWildcard.Length);
                                break;
                            default:
                                var stars = query.GetMatchList(matchstate);
                                stars.Insert(0, newWildcard.ToString());
                                break;
                        }
                    }
                    return result;
                }
            }

            bool isTag = firstWord.StartsWith("TAG-");
            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            while (true) 
            {
                string fw;
                string np;
                Node childNode = LitteralChild(splitPath, out fw, out np, query);
                if (childNode==null) break;
                firstWord = fw;
                newPath = np;
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                MatchState newMatchstate = matchstate;
                if (isTag)
                {
                    if (firstWord == "TAG-THAT")
                    {
                        newMatchstate = MatchState.That;
                    }
                    else if (firstWord == "TAG-TOPIC")
                    {
                        newMatchstate = MatchState.Topic;
                    }
                    else if (firstWord == "TAG-FLAG")
                    {
                        newMatchstate = MatchState.Flag;
                    }
                    else if (firstWord == "TAG-INPUT")
                    {
                        newMatchstate = MatchState.UserInput;
                    }
                }

                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                var newWildcard = Unifiable.CreateAppendable();
                var result = childNode.evaluate(newPath, query, request, newMatchstate, newWildcard);
                // and if we get a result from the child return it
                if (result != null)
                {
                    var childNodeWord = childNode.word;
                    if (!isTag && childNodeWord.IsWildCard() && childNodeWord.StoreWildCard())
                    {
                        writeToLog("should store WC for " + childNodeWord + " from " + firstWord);
                    }
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the matchstate if it exists
                        // and then clear it for subsequent wildcards
                        var stars = query.GetMatchList(matchstate);
                        stars.Insert(0, newWildcard.ToString());
                        newWildcard.Clear();
                    }
                    return result;
                }
                break;
            }

            // third option - the input part of the path might have been matched so far but hasn't
            // returned a match, so check to see it contains the "*" wildcard. "*" comes last in
            // precedence in the AIML alphabet.
            lock (children) foreach (KeyValuePair<Unifiable, Node> childNodeKV in this.children)
            {
                Unifiable childNodeWord = childNodeKV.Key;
                if (!childNodeWord.IsLongWildCard()) continue;
            
                // o.k. look for the path in the child node denoted by "*"
                Node childNode = childNodeKV.Value;

                // add the next word to the wildcard match 
                var newWildcard = Unifiable.CreateAppendable();
                this.storeWildCard(splitPath[0], newWildcard);

                var result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);
                // and if we get a result from the branch process and return it
                if (result!=null)
                {
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the current matchstate
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Insert(0,newWildcard.ToString());
                                // added due to this match being the end of the line
                                newWildcard.Clear();// Remove(0, newWildcard.Length);
                                break;
                            default:
                                var stars = query.GetMatchList(matchstate);
                                stars.Insert(0, newWildcard.ToString());
                                break;
                        }
                    }
                    return result;
                }
            }

            // o.k. if the nodemapper has failed to match at all: the input contains neither 
            // a "_", the sFirstWord text, or "*" as a means of denoting a child node. However, 
            // if this node is itself representing a wildcard then the search continues to be
            // valid if we proceed with the tail.
            //if ((this.word == "_") || (this.word == "*"))
            if (word.IsAnyWord() || word.IsLongWildCard())
            {
                this.storeWildCard(splitPath[0], wildcard);
                var result = this.evaluate(newPath, query, request, matchstate, wildcard);
                return result;
            }

            // If we get here then we're at a dead end so return an empty string. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            //wildcard = new StringBuilder();
            wildcard.Clear();
            return null;/// string.Empty;
        }

        private Node LitteralChild(string[] splitPath, out string firstWord, out string newPath, SubQuery query)
        {
            IList<Node> childrenS = new List<Node>();
            Node childNode;
            firstWord = splitPath[0];
            int rw = 1;
            newPath = string.Join(" ", splitPath, rw, splitPath.Length - rw);
            if (children.TryGetValue(firstWord, out childNode))
            {
                if (childNode.word == firstWord)
                {
                    return childNode;
                }
                 return childNode;
            }
            foreach (KeyValuePair<Unifiable, Node> childNodeKV in children)
            {                                
                Unifiable childNodeWord = childNodeKV.Key;
                if (childNodeWord.IsAnyWord()) continue;
                if (childNodeWord.IsLongWildCard()) continue;
               // if (childNodeWord.IsWildCard()) continue;

                childNode = childNodeKV.Value;
                childrenS.Add(childNode);
                string fw;
                if (!childNode.word.ConsumePath(splitPath, out firstWord, out rw, query))
                {
                    continue;
                }
                newPath = string.Join(" ", splitPath, rw, splitPath.Length - rw);
                return childNode;
            }
            return null;
        }

        /// <summary>
        /// Correctly stores a word in the wildcard slot
        /// </summary>
        /// <param name="word">The word matched by the wildcard</param>
        /// <param name="wildcard">The contents of the user input absorbed by the AIML wildcards "_" and "*"</param>
        private void storeWildCard(Unifiable word, Unifiable wildcard)
        {
            if (!wildcard.IsEmpty)
            {
                wildcard.Append(" ");
            }
            wildcard.Append(word);
        }
        #endregion

        #endregion
    }

#if false
    private class UPath
    {
        private Unifiable lp;

        private UPath(Unifiable unifiable)
        {
            lp = unifiable;
        }

        private Unifiable LegacyPath
        {
            get
            {
                return lp;
            }
        }
        static private bool operator ==(UPath t, UPath s)
        {
            return t.LegacyPath == s.LegacyPath;
        }

        private static bool operator !=(UPath t, UPath s)
        {
            return !(t == s);
        }
        private override string ToString()
        {
            return lp.ToString();
        }

        private static UPath MakePath(Unifiable unifiable)
        {
            return new UPath(unifiable);
        }
    }
#endif
}