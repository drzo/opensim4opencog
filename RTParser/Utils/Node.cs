using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;

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
        public List<Template> template = null;//Unifiable.Empty;

        /// <summary>
        /// The AIML source for the category that defines the template
        /// </summary>
        public string filename = Unifiable.Empty;

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
        public void addCategoryTag(Unifiable path, XmlNode template, XmlNode guard, string filename)
        {
            if (template == null)
            {
                throw new XmlException("The category with a pattern: " + path + " found in file: " + filename + " has an empty template tag. ABORTING");
            }

            // check we're not at the leaf node
            if (!path.IsWildCard() && path.AsString().Trim().Length == 0)
            {
                if (this.template == null) this.template = new List<Template>();
                // last in first out addition
                this.template.Insert(0, new Template(template, guard, this));
                //this.GuardText = guard;
                this.filename = filename;
                return;
            }

            // otherwise, this sentence requires further child nodemappers in order to
            // be fully mapped within the GraphMaster structure.

            // split the input into its component words
            Unifiable[] words0 = path./*Trim().*/Split();//" ".ToCharArray());

            Unifiable firstRaw = words0[0];
            string w = firstRaw.AsString();

            if (w.Contains("COMEHERE"))
            {
                if (!w.Contains("<t"))
                {

                }
            }
            // get the first word (to form the key for the child nodemapper)
            //Unifiable firstWord = Normalize.MakeCaseInsensitive.TransformInput(firstRaw);
            string uc = firstRaw.AsString().ToUpper();
            if (uc != firstRaw.AsString() && !uc.Contains("<"))
            {

            }
            Unifiable firstWord = firstRaw;

            // concatenate the rest of the sentence into a suffix (to act as the
            // path argument in the child nodemapper)
            Unifiable newPath = Unifiable.Join(" ", words0, 1, words0.Length - 1);
            // path.Rest();// Substring(firstWord.Length, path.Length - firstWord.Length).Trim();

            // o.k. check we don't already have a child with the key from this sentence
            // if we do then pass the handling of this sentence down the branch to the 
            // child nodemapper otherwise the child nodemapper doesn't yet exist, so create a new one
            if (this.children.ContainsKey(firstWord))
            {
                Node childNode = this.children[firstWord];
                childNode.addCategoryTag(newPath, template, guard, filename);
            }
            else
            {
                Node childNode = new Node(this);
                childNode.word = firstWord;
                childNode.addCategoryTag(newPath, template, guard, filename);
                this.children.Add(childNode.word, childNode);
            }
        }

        #endregion

        public static bool AlwaysFail = true;
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
        public List<Template> evaluate(Unifiable path, SubQuery query, Request request, MatchState matchstate, Unifiable wildcard)
        {
            // check for timeout
            if (request.StartedOn.AddMilliseconds(request.Proccessor.TimeOut) < DateTime.Now)
            {
                request.Proccessor.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\"");
                request.hasTimedOut = true;
                return null;// Unifiable.Empty;
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
                return this.template;
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.IsEmpty)
            {
                return this.template;
            }

            // otherwise split the input into it's component words
            Unifiable[] splitPath0 = path.Split();
            Unifiable first = splitPath0[0];// path.First();

            // get the first word of the sentence
            Unifiable firstWord = Normalize.MakeCaseInsensitive.TransformInput(first);

            // and concatenate the rest of the input into a new path for child nodes
            Unifiable newPath = Unifiable.Join(" ", splitPath0, 1, splitPath0.Length - 1);// path.Rest();// Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            foreach (Unifiable childNodeWord in this.children.Keys)
            {
                if (!childNodeWord.IsShortWildCard()) continue;
                if (!childNodeWord.Unify(first, query)) continue;
                Node childNode = this.children[childNodeWord];
                // add the next word to the wildcard match 
                Unifiable newWildcard = Unifiable.CreateAppendable();
                this.storeWildCard(first, newWildcard);

                // move down into the identified branch of the GraphMaster structure
                List<Template> result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);

                // and if we get a result from the branch process the wildcard matches and return 
                // the result
                string freezit = newWildcard.Frozen();
                if (ResultStateReady(result, newWildcard, matchstate, query))
                {
                    if (freezit.Contains(" "))
                    {
                        // cannot match input containing spaces
                        continue;
                    }
                    return result;
                }
            }

            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            foreach (Unifiable childNodeWord in this.children.Keys)
            {
                Node childNode = this.children[childNodeWord];
                if (childNode.word.IsWildCard()) continue;
                if (!childNode.word.Unify(firstWord, query)) continue;
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

                //Node childNode = (Node)this.children[firstWord];
                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                Unifiable newWildcard = Unifiable.CreateAppendable();
                List<Template> result = childNode.evaluate(newPath, query, request, newMatchstate, newWildcard);
                // and if we get a result from the child return it
                if (ResultStateReady(result, newWildcard, matchstate, query)) return result;

            }

            // third option - the input part of the path might have been matched so far but hasn't
            // returned a match, so check to see it contains the "*" wildcard. "*" comes last in
            // precedence in the AIML alphabet.
            foreach (Unifiable childNodeWord in this.children.Keys)
            {
                int matchLen = 0;
                if (!childNodeWord.IsLongWildCard()) continue;
                Node childNode = this.children[childNodeWord];
                // o.k. look for the path in the child node denoted by "*"
                //Node childNode = (Node)this.children["*"];
                List<Template> result = null;
                // add the next word to the wildcard match 
                Unifiable newWildcard = Unifiable.CreateAppendable();
                // normal * and LazyMatch on first word
                if (childNodeWord.Unify(first, query))
                {
                    this.storeWildCard(first, newWildcard);
                    result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);
                    if (ResultStateReady(result, newWildcard, matchstate, query)) return result;
                }
                else // some lazy matches take two words
                    if (splitPath0.Length > 1 && childNodeWord.IsLazyStar())
                    {
                        Unifiable firstAndSecond = first + " " + splitPath0[1];
                        if (childNodeWord.Unify(firstAndSecond, query))
                        {
                            this.storeWildCard(firstAndSecond, newWildcard);
                            result = childNode.evaluate(Unifiable.Join(" ", splitPath0, 2, splitPath0.Length - 2), query,
                                                        request, matchstate, newWildcard);
                            if (ResultStateReady(result, newWildcard, matchstate, query)) return result;
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
                return this.evaluate(newPath, query, request, matchstate, wildcard);
            }

            // If we get here then we're at a dead end so return an empty Unifiable. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            //wildcard.Clear();// = new Unifiable();
            return null;// Unifiable.Empty;
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
        private static bool ResultStateReady(ICollection<Template> result, Unifiable newWildcard, MatchState matchstate, SubQuery query)
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
                    }
                }
                return true;
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
}