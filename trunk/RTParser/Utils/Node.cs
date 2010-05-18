using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using UPath = RTParser.Unifiable;

namespace RTParser.Utils
{
    /// <summary>
    /// Encapsulates a node in the graphmaster tree structure
    /// </summary>
    [Serializable]
    public class Node
    {
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
        /// The number of direct children (non-recursive) of this node
        /// </summary>
        public int NumberOfChildNodes
        {
            get
            {
                return this.children.Count;
            }
        }

        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        public UList TemplateInfos = null;//Unifiable.Empty;

#if UNUSED
        /// <summary>
        /// The AIML source for the category that defines the template
        /// </summary>
        public string filename = Unifiable.Empty;
#endif
        /// <summary>
        /// The word that identifies this node to it's parent node
        /// </summary>
        public Unifiable word = Unifiable.Empty;

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
        static public void addCategoryTag(Node start, Unifiable path, PatternInfo patternInfo, CategoryInfo category, XmlNode outerTemplate, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo, GraphMaster master)
        {
            if (templateNode == null)
            {
                throw new XmlException("The category with a pattern: " + path + " found in file: " + category +
                                       " has an empty template tag. ABORTING");
            }
            //String ts = outTemplate.OuterXml;
            Node thiz = start.addCategoryTagChild0(path);
            thiz.addTerminal(templateNode, category,  guard, thatInfo, master, patternInfo);
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
                                                                   if (dupes == null) dupes = new List<TemplateInfo>();
                                                                   dupes.Add(temp);
                                                               }
                                                   });
                    if (dupes != null)
                    {
                        dupes.ForEach(delegate(TemplateInfo temp)
                                          {
                                              if (true)
                                              {
                                                  master.RemoveTemplate(temp);
                                                  this.TemplateInfos.Remove(temp);
                                              }
                                          });
                        dupes.Clear();
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
                pat.GraphmasterNode = this;
                if (category != null) pat.AddCategory(category);
            }

            master.AddTemplate(newTemplateInfo);
            if (pat != patternInfo)
            {
                throw new InvalidCastException("weird");
                Console.WriteLine("Wierd!");
            }
            this.TemplateInfos.Insert(0, newTemplateInfo);
        }

        ///// <summary>
        ///// Adds a category to the node
        ///// </summary>
        ///// <param name="path">the path for the category</param>
        ///// <param name="outTemplate">the outTemplate to find at the end of the path</param>
        ///// <param name="filename">the file that was the source of this category</param>
        //public Node addCategoryTagChild(Unifiable path, PatternInfo pi, CategoryInfo category, XmlNode outerNode, XmlNode outTemplate, GuardInfo guard, ThatInfo thatInfo, GraphMaster master)
        //{
        //    Node initial = null;
        //    // check we're not at the leaf node
        //    if (!path.IsWildCard() && path.AsString().Trim().Length == 0)
        //    {
        //        //this.GuardText = guard;
        //        //this.filename = filename;
        //        return this;
        //    }

        //    // otherwise, this sentence requires further child nodemappers in order to
        //    // be fully mapped within the GraphMaster structure.

        //    // split the input into its component words
        //    //Unifiable[] words0 = path./*Trim().*/Split();//" ".ToCharArray());

        //    Unifiable firstRaw = path.First(); // words0[0];
        //    string w = firstRaw.AsString();

        //    // get the first word (to form the key for the child nodemapper)
        //    //Unifiable firstWord = Normalize.MakeCaseInsensitive.TransformInput(firstRaw);

        //    Unifiable firstWord = firstRaw;

        //    // concatenate the rest of the sentence into a suffix (to act as the
        //    // path argument in the child nodemapper)
        //    Unifiable newPath = path.Rest(); // Unifiable.Join(" ", words0, 1, words0.Length - 1);
        //    // path.Rest();// Substring(firstWord.Length, path.Length - firstWord.Length).Trim();

        //    // o.k. check we don't already have a child with the key from this sentence
        //    // if we do then pass the handling of this sentence down the branch to the 
        //    // child nodemapper otherwise the child nodemapper doesn't yet exist, so create a new one  \
        //    bool found = false;
        //    foreach (var c in this.children)
        //    {
        //        if (c.Key.AsString() == firstWord.AsString())
        //        {
        //            Node childNode = c.Value;
        //            initial = childNode.addCategoryTagChild(newPath, pi, category, outerNode, outTemplate, guard, thatInfo, master);
        //            found = true;
        //            break;
        //        }
        //    }
        //    if (!found)
        //    {
        //        Node childNode = new Node(this);
        //        childNode.word = firstWord;
        //        initial = childNode.addCategoryTagChild(newPath, pi, category, outerNode, outTemplate, guard, thatInfo, master);
        //        this.children.Add(childNode.word, childNode);
        //    }
        //    return initial;
        //}

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="outTemplate">the outTemplate to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        public Node addCategoryTagChild0(Unifiable path)
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
            foreach (var c in this.children)
            {
                if (c.Key.AsString() == firstWord.AsString())
                {
                    Node childNode = c.Value;
                    initial = childNode.addCategoryTagChild0(newPath);
                    found = true;
                    break;
                }
            }
            if (!found)
            {
                Node childNode = new Node(this);
                childNode.word = firstWord;
                initial = childNode.addCategoryTagChild0(newPath);
                this.children.Add(childNode.word, childNode);

            }
            if (initial == null) throw new NullReferenceException("no child node: " + this);
            return initial;
        }

        #endregion

        public static bool AlwaysFail = true;
        public ThatInfo That;

        #region Evaluate Node


        public Node GetNextNode()
        {
            if (Parent == null) return null;
            bool useNext = false;
            foreach (KeyValuePair<Unifiable, Node> v in Parent.children)
            {
                if (useNext) return v.Value;
                if (v.Value == this)
                {
                    useNext = true;
                }
            }
            if (useNext)
            {
                //     Console.WriteLine(String.Format("Last key {0}", ToString()));
                return Parent.GetNextNode();
            }
            return null;
        }

        public override string ToString()
        {
            if (Parent != null) return String.Format("{0} {1}", Parent, word);
            return word;
        }


        /// <summary>
        /// Navigates this node (and recursively into child nodes) for a match to the path passed as an argument
        /// whilst processing the referenced request
        /// </summary>
        /// <param name="path">The normalized path derived from the user's input</param>
        /// <param name="query">The query that this search is for</param>
        /// <param name="request">An encapsulation of the request from the user</param>
        /// <param name="matchstate">The part of the input path the node represents</param>
        /// <param name="wildcard">The contents of the user input absorbed by the AIML wildcards "_" and "*"</param>
        /// <returns>The template to process to generate the output</returns>
        public UList evaluateNew(UPath upath, SubQuery query, Request request, MatchState matchstate, Unifiable wildcard)
        {
            throw new NotSupportedException("");
            UList template = new UList();
            var v = evaluateNew(upath, query, request, matchstate, wildcard);
            return template;
        }



        /// <summary>
        /// Navigates this node (and recursively into child nodes) for a match to the path passed as an argument
        /// whilst processing the referenced request
        /// </summary>
        /// <param name="path">The normalized path derived from the user's input</param>
        /// <param name="query">The query that this search is for</param>
        /// <param name="request">An encapsulation of the request from the user</param>
        /// <param name="matchstate">The part of the input path the node represents</param>
        /// <param name="wildcard">The contents of the user input absorbed by the AIML wildcards "_" and "*"</param>
        /// <returns>The template to process to generate the output</returns>
        public bool evaluate(UPath upath, SubQuery query, Request request, MatchState matchstate, Unifiable wildcard, QueryList res)
        {
            bool bres = false;
            Unifiable path = upath.LegacyPath;
            // check for timeout
            if (request.StartedOn.AddMilliseconds(request.Proccessor.TimeOut) < DateTime.Now)
            {
                request.Proccessor.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\"");
                request.hasTimedOut = true;
                return false;// Unifiable.Empty;
            }

            // so we still have time!
            path = path.Trim();

            // check if this is the end of a branch in the GraphMaster 
            // return the cCategory for this node
            if (this.children.Count == 0)
            {
                if (!path.IsEmpty)
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    this.storeWildCard(path, wildcard);
                }
                if (AddSubQueris(res, query, this)) bres = true;
                return bres;
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.IsEmpty)
            {
                if (AddSubQueris(res, query, this)) bres = true;
                return bres;
            }

            // otherwise split the input into it's component words
            //  Unifiable[] splitPath0 = path.Split();
            Unifiable first = path.First();

            // get the first word of the sentence
            Unifiable firstWord = first;//Normalize.MakeCaseInsensitive.TransformInput(first);

            // and concatenate the rest of the input into a new path for child nodes
            Unifiable newPath = UPath.MakePath(path.Rest()).LegacyPath;// Unifiable.Join(" ", splitPath0, 1, splitPath0.Length - 1);// path.Rest();// Substring(firstWord.Length, path.Length - firstWord.Length);

            bool childTrue = false;
            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            foreach (KeyValuePair<Unifiable, Node> childNodeKV in this.children)
            {
                Unifiable childNodeWord = childNodeKV.Key;
                if (!childNodeWord.IsShortWildCard()) continue;
                if (childNodeWord.Unify(first, query) > 0) continue;
                Node childNode = childNodeKV.Value;
                // add the next word to the wildcard match 
                Unifiable newWildcard = Unifiable.CreateAppendable();
                this.storeWildCard(first, newWildcard);

                // move down into the identified branch of the GraphMaster structure
                if (childNode.evaluate(UPath.MakePath(newPath), query, request, matchstate, newWildcard, res))
                {
                    childTrue = true;
                }

                // and if we get a result from the branch process the wildcard matches and return 
                // the result
                string freezit = newWildcard.Frozen();
                if (ResultStateReady(res, newWildcard, matchstate, query))
                {
                    if (freezit.Contains(" "))
                    {
                        // cannot match input containing spaces
                        continue;
                    }
                    return childTrue;
                }
            }

            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            foreach (var childNodeKV in this.children)
            {
                Node childNode = childNodeKV.Value;
                if (childNode.word.IsWildCard()) continue;
                if (childNode.word.Unify(firstWord, query) > 0) continue;
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                MatchState newMatchstate = matchstate;

                if (firstWord.IsTag("THAT"))
                {
                    newMatchstate = MatchState.That;
                }
                else if (firstWord.IsTag("TOPIC"))
                {
                    newMatchstate = MatchState.Topic;
                }
                else if (firstWord.IsTag("FLAG"))
                {
                    newMatchstate = MatchState.Flag;
                }

                //Node childNode = (Node)this.children[firstWord];
                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                Unifiable newWildcard = Unifiable.CreateAppendable();
                if (childNode.evaluate(UPath.MakePath(newPath), query, request, newMatchstate, newWildcard, res))
                {
                    childTrue = true;
                }
                // and if we get a result from the child return it
                if (ResultStateReady(res, newWildcard, matchstate, query)) return childTrue;

            }

            // third option - the input part of the path might have been matched so far but hasn't
            // returned a match, so check to see it contains the "*" wildcard. "*" comes last in
            // precedence in the AIML alphabet.
            foreach (var childNodeKV in this.children)
            {
                Unifiable childNodeWord = childNodeKV.Key;
                int matchLen = 0;
                if (!childNodeWord.IsLongWildCard()) continue;
                Node childNode = childNodeKV.Value;
                // o.k. look for the path in the child node denoted by "*"
                //Node childNode = (Node)this.children["*"];
                //UList result = null;
                // add the next word to the wildcard match 
                Unifiable newWildcard = Unifiable.CreateAppendable();
                // normal * and LazyMatch on first word
                if (childNodeWord.Unify(first, query) == 0)
                {
                    this.storeWildCard(first, newWildcard);
                    if (childNode.evaluate(UPath.MakePath(newPath), query, request, matchstate, newWildcard, res))
                    {
                        childTrue = true;
                    }
                    if (ResultStateReady(res, newWildcard, matchstate, query)) return true;
                }
                else
                {
                    // some lazy matches take two words
                    Unifiable second = newPath.First();
                    if (!second.IsEmpty && childNodeWord.IsLazyStar())
                    {
                        Unifiable firstAndSecond = first + " " + second;
                        if (childNodeWord.Unify(firstAndSecond, query) == 0)
                        {
                            this.storeWildCard(firstAndSecond, newWildcard);
                            if(childNode.evaluate(UPath.MakePath(newPath.Rest()), query,
                                                        request, matchstate, newWildcard, res))
                            {
                                childTrue = true;
                            }
                            if (ResultStateReady(res, newWildcard, matchstate, query)) return childTrue;
                        }
                    }
                }
            }

            // o.k. if the nodemapper has failed to match at all: the input contains neither 
            // a "_", the sFirstWord text, or "*" as a means of denoting a child node. However, 
            // if this node is itself representing a wildcard then the search continues to be
            // valid if we proceed with the tail.
            if (this.word.IsWildCard())
            {
                this.storeWildCard(first, wildcard);
                if (this.evaluate(UPath.MakePath(newPath), query, request, matchstate, wildcard, res))
                {
                    return true;
                }
                return false;
            }

            // If we get here then we're at a dead end so return an empty Unifiable. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            //wildcard.Clear();// = new Unifiable();
            return false;// Unifiable.Empty;
        }

        private bool AddSubQueris(QueryList list, SubQuery infos, Node node)
        {
            if (TemplateInfos == null || TemplateInfos.Count == 0) return false;
            lock (TemplateInfos)
            {
                var sq =  infos.CopyOf();
                infos.Templates.AddRange(TemplateInfos);
                foreach (TemplateInfo info in TemplateInfos)
                {
                    info.Query = sq;
                    list.Templates.root.Add(info);
                }
            }
            return true;
        }

        /// <summary>
        // If we get a result from the branch process the wildcard matches and return 
        // the result
        /// </summary>
        /// <param name="result"></param>
        /// <param name="newWildcard"></param>
        /// <param name="matchstate"></param>
        /// <param name="query"></param>
        /// <returns></returns>
        private static bool ResultStateReady(QueryList result, Unifiable newWildcard, MatchState matchstate, SubQuery query)
        {
            // and if we get a result from the branch process and return it
            if (result != null && result.Count > 0)
            {
                if (!newWildcard.IsEmpty)
                {
                    // capture and push the star content appropriate to the matchstate if it exists
                    // and then clear it for subsequent wildcards

                    switch (matchstate)
                    {
                        case MatchState.UserInput:
                            query.InputStar.Add(newWildcard.Frozen());
                            // added due to this match being the end of the line
                            newWildcard.Clear();
                            break;
                        case MatchState.That:
                            query.ThatStar.Add(newWildcard.Frozen());
                            break;
                        case MatchState.Topic:
                            query.TopicStar.Add(newWildcard.Frozen());
                            break;
                        case MatchState.Flag:
                            // query.TopicStar.Add(newWildcard.Frozen());
                            break;
                    }
                }
                return true;
            }
            switch (matchstate)
            {
                case MatchState.UserInput:
                    break;
                case MatchState.That:
                    break;
                case MatchState.Topic:
                    break;
                case MatchState.Flag:
                    break;
            }
            return false;
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

    public class QueryList
    {
        public int Count
        {
            get { return Templates.Count; }
        }
        public UList Templates = new UList();
        public UList ToUList()
        {
            return Templates;
        }
    }

#if false
    public class UPath
    {
        private Unifiable lp;

        private UPath(Unifiable unifiable)
        {
            lp = unifiable;
        }

        public Unifiable LegacyPath
        {
            get
            {
                return lp;
            }
        }
        static public bool operator ==(UPath t, UPath s)
        {
            return t.LegacyPath == s.LegacyPath;
        }

        public static bool operator !=(UPath t, UPath s)
        {
            return !(t == s);
        }
        public override string ToString()
        {
            return lp.ToString();
        }

        public static UPath MakePath(Unifiable unifiable)
        {
            return new UPath(unifiable);
        }
    }
#endif
}