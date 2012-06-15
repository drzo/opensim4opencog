using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Web;
using System.IO;
using LAIR.ResourceAPIs.WordNet;
using LAIR.Collections.Generic;
using RaptorDB;
using System.Linq;

namespace AltAIMLbot.Utils
{
    /// <summary>
    /// Encapsulates a node in the graphmaster tree structure
    /// </summary>
    [Serializable]
    public class Node
    {
        #region Attributes

        /// <summary>
        /// Contains the child nodes of this node
        /// </summary>
        public Dictionary<string, Node> children = new Dictionary<string, Node>();

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
                return this.children.Count;
            }
        }

        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        public string template = string.Empty;

        /// <summary>
        /// The score (if any) associated with this node
        /// </summary>
        public double score = 1.0;

        /// <summary>
        /// The AIML source for the category that defines the template
        /// </summary>
        public string filename = string.Empty;

        /// <summary>
        /// The word that identifies this node to it's parent node
        /// </summary>
        public string word=string.Empty;

        public bool fullChildSet = false;
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
                throw new XmlException("The category with a pattern: " + path + " found in file: " + filename + " has an empty template tag. ABORTING");
            }

            // check we're not at the leaf node
            if (path.Trim().Length == 0)
            {
                this.template = template;
                this.filename = filename;
                this.score = score;
                return;
            }

            // otherwise, this sentence requires further child nodemappers in order to
            // be fully mapped within the GraphMaster structure.

            // split the input into its component words
            string[] words = path.Trim().Split(" ".ToCharArray());

            // get the first word (to form the key for the child nodemapper)
            string firstWord = Normalize.MakeCaseInsensitive.TransformInput(words[0]);

            // concatenate the rest of the sentence into a suffix (to act as the
            // path argument in the child nodemapper)
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length).Trim();


            // Create a score for the next level
            double newScore = score;
            double newScale = scale /4;
            double indexv = 1.0;
            if ((firstWord == "<THAT>") || (firstWord == "<TOPIC>") || (firstWord == "<STATE>") || (firstWord == "<PATTERN>"))
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
            if (this.children.ContainsKey(firstWord))
            {
                Node childNode = this.children[firstWord];

                childNode.addCategory(newPath, template, filename,newScore,newScale);
            }
            else
            {
                Node childNode = new Node();
                childNode.word = firstWord;
                childNode.addCategory(newPath, template, filename, newScore,newScale);
                lock (children)
                {
                    this.children.Add(childNode.word, childNode);
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
                throw new XmlException("The category with a pattern: " + path + " found in file: " + filename + " has an empty template tag. ABORTING");
            }

            Node myNode = pathDB.fetchNode(absPath,false);
            myNode.word = myWord;

            // check we're not at the leaf node
            if (path.Trim().Length == 0)
            {
                myNode.template = template;
                myNode.filename = filename;
                myNode.score = score;
                pathDB.saveNode(absPath, myNode);
                return;
            }


            // otherwise, this sentence requires further child nodemappers in order to
            // be fully mapped within the GraphMaster structure.

            // split the input into its component words
            string[] words = path.Trim().Split(" ".ToCharArray());

            // get the first word (to form the key for the child nodemapper)
            string firstWord = Normalize.MakeCaseInsensitive.TransformInput(words[0]);

            // concatenate the rest of the sentence into a suffix (to act as the
            // path argument in the child nodemapper)
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length).Trim();
            string newdAbsPath = (absPath + " " + firstWord).Trim();

            // Create a score for the next level
            double newScore = score;
            double newScale = scale / 4;
            double indexv = 1.0;
            if ((firstWord == "<THAT>") || (firstWord == "<TOPIC>") || (firstWord == "<STATE>") || (firstWord == "<PATTERN>"))
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
            //if (myNode.children.ContainsKey(firstWord))
            if ((myNode.children.ContainsKey(firstWord))
                ||( myNode.dbContainsNode(newdAbsPath, pathDB)) )
            {
                //Node childNode = myNode.children[firstWord];

                addCategoryDB(firstWord, newPath, template, filename, newScore, newScale, newdAbsPath, pathDB);
            }
            else
            {
                //Node childNode = new Node();
                //childNode.word = firstWord;
                addCategoryDB(firstWord, newPath, template, filename, newScore, newScale, newdAbsPath, pathDB);
                //myNode.children.Add(childNode.word, childNode);
                //myNode.children.Add(firstWord,null);
                //myNode.childrenList.Add(firstWord);
                myNode.addChild(firstWord, null);

                // We only need to save it if we updated it with a child
                pathDB.saveNode(absPath, myNode);
            }

        }

        public void collectPaths(string inpath, List<string> collector)
        {
            string curWord = this.word;
            string ourPath = inpath + " " + curWord;
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
            foreach (string childWord in children.Keys)
            {
                Node childNode = children[childWord];
                childNode.collectPaths(ourPath, collector);
            }

        }
        public void collectFullPaths(string inpath, List<string> collector)
        {
            string curWord = this.word;
            string ourPath = inpath + " " + curWord;
            if ((template != null) && (template.Length > 1))
            {
                var encoded = HttpUtility.HtmlEncode(ourPath.Trim());
                string serTemplate = String.Format("<ser path=\"{0}\"> {1} </ser>", encoded, template);
                collector.Add(serTemplate);
            }
            foreach (string childWord in children.Keys)
            {
                Node childNode = children[childWord];
                childNode.collectFullPaths(ourPath, collector);
            }

        }
        public void searchFullPaths(string targetPath,string inpath, List<string> collector)
        {
            string curWord = this.word;
            string ourPath = inpath + " " + curWord;
            if (targetPath.StartsWith(ourPath.Trim()))
            {
                if ((template != null) && (template.Length > 1))
                {
                    var encoded = HttpUtility.HtmlEncode(ourPath.Trim());
                    string serTemplate = String.Format("<ser path=\"{0}\"> {1} </ser>", encoded, template);
                    collector.Add(serTemplate);
                }
                foreach (string childWord in children.Keys)
                {
                    Node childNode = children[childWord];
                    childNode.searchFullPaths(targetPath,ourPath, collector);
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
                string firstWord = Normalize.MakeCaseInsensitive.TransformInput(word);
                double indexv = 1.0;
                scale = scale / 4;
                if ((firstWord == "<THAT>") || (firstWord == "<TOPIC>") || (firstWord == "<STATE>") || (firstWord == "<PATTERN>"))
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
            if (this.children.Count==0)
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
            string firstWord = Normalize.MakeCaseInsensitive.TransformInput(splitPath[0]);

            // and concatenate the rest of the input into a new path for child nodes
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            if (this.children.ContainsKey("_"))
            {
                Node childNode = (Node)this.children["_"];

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
            if (this.children.ContainsKey(firstWord))
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
                Node childNode = (Node)this.children[firstWord];
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
            if (this.children.ContainsKey("*"))
            {
                // o.k. look for the path in the child node denoted by "*"
                Node childNode = (Node)this.children["*"];

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
            if (this.children.Count==0)
            {
                if (path.Length > 0)
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    this.storeWildCard(path, wildcard);
                }
                query.TemplatePath = "";
                return this.template;
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.Length == 0)
            {
                query.TemplatePath = "";
                return this.template;
            }

            // otherwise split the input into it's component words
            string[] splitPath = path.Split(" \r\n\t".ToCharArray());

            // get the first word of the sentence
            string firstWord = Normalize.MakeCaseInsensitive.TransformInput(splitPath[0]);

            // and concatenate the rest of the input into a new path for child nodes
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            if (this.children.ContainsKey("_"))
            {
                Node childNode = (Node)this.children["_"];

                // add the next word to the wildcard match 
                StringBuilder newWildcard = new StringBuilder();
                this.storeWildCard(splitPath[0],newWildcard);
                
                // move down into the identified branch of the GraphMaster structure
                string result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);

                // and if we get a result from the branch process the wildcard matches and return 
                // the result
                if (result.Length>0)
                {
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the current matchstate
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Add(newWildcard.ToString());
                                // added due to this match being the end of the line
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.That:
                                query.ThatStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.Topic:
                                query.TopicStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.State:
                                query.StateStar.Add(newWildcard.ToString());
                                break;
                        }
                    }
                    query.TemplatePath = "_ " + query.TemplatePath;
                    return result;
                }
            }

            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            if (this.children.ContainsKey(firstWord))
            {
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                MatchState newMatchstate = matchstate;
                if (firstWord == "<THAT>")
                {
                    newMatchstate = MatchState.That;
                }
                else if (firstWord == "<TOPIC>")
                {
                    newMatchstate = MatchState.Topic;
                }
                else if (firstWord == "<STATE>")
                {
                    newMatchstate = MatchState.State;
                }
                else if (firstWord == "<PATTERN>")
                {
                    newMatchstate = MatchState.UserInput;
                }

                Node childNode = (Node)this.children[firstWord];
                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                StringBuilder newWildcard = new StringBuilder();
                string result = childNode.evaluate(newPath, query, request, newMatchstate,newWildcard);
                // and if we get a result from the child return it
                if (result.Length > 0)
                {
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the matchstate if it exists
                        // and then clear it for subsequent wildcards
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.That:
                                query.ThatStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.Topic:
                                query.TopicStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.State:
                                query.StateStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                        }
                    }
                    query.TemplatePath = firstWord +" " + query.TemplatePath;
                    return result;
                }
            }

            // Patch option: input contains a semantic wildcard
            // like "*dog" or "*dog:hypo:n" or "*planet" ,"*celestial_body"
            bool hasSemantic = hasWildSense(this.children);
            bool isSemantic = isWildPattern(this.word);
            if (hasSemantic)
            {
                foreach (string s in this.children.Keys)
                {
                    if (isWildPattern (s))
                    {
                        string wildSense = s.Replace("*", "");
                        if( matchesWildSense(wildSense ,firstWord,request ))
                        {
                          // Treat like matching a wildcard

                            // o.k. look for the path in the child node denoted by "*"
                            Node childNode = (Node)this.children[s];

                            // add the next word to the wildcard match 
                            StringBuilder newWildcard = new StringBuilder();
                            this.storeWildCard(splitPath[0], newWildcard);

                            string result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);
                            // and if we get a result from the branch process and return it
                            if (result.Length > 0)
                            {
                                if (newWildcard.Length > 0)
                                {
                                    // capture and push the star content appropriate to the current matchstate
                                    switch (matchstate)
                                    {
                                        case MatchState.UserInput:
                                            query.InputStar.Add(newWildcard.ToString());
                                            // added due to this match being the end of the line
                                            newWildcard.Remove(0, newWildcard.Length);
                                            break;
                                        case MatchState.That:
                                            query.ThatStar.Add(newWildcard.ToString());
                                            break;
                                        case MatchState.Topic:
                                            query.TopicStar.Add(newWildcard.ToString());
                                            break;
                                        case MatchState.State:
                                            query.StateStar.Add(newWildcard.ToString());
                                            break;
                                    }
                                }
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
            if (this.children.ContainsKey("*"))
            {
                // o.k. look for the path in the child node denoted by "*"
                Node childNode = (Node)this.children["*"];

                // add the next word to the wildcard match 
                StringBuilder newWildcard = new StringBuilder();
                this.storeWildCard(splitPath[0], newWildcard);

                string result = childNode.evaluate(newPath, query, request, matchstate, newWildcard);
                // and if we get a result from the branch process and return it
                if (result.Length > 0)
                {
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the current matchstate
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Add(newWildcard.ToString());
                                // added due to this match being the end of the line
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.That:
                                query.ThatStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.Topic:
                                query.TopicStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.State:
                                query.StateStar.Add(newWildcard.ToString());
                                break;
                        }
                    }
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
                foreach (string s in this.children.Keys)
                {
                    if (isWildPattern(s))
                    {
                        string wildSense = s.Replace("*", "");
                        if (matchesWildSense(wildSense, multiWord, request))
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
            if (this.children.Count == 0)
            {
                if (path.Length > 0)
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    this.storeWildCard(path, wildcard);
                }
                query.TemplatePath = "";
                return this.template;
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (path.Length == 0)
            {
                query.TemplatePath = "";
                return this.template;
            }

            // otherwise split the input into it's component words
            string[] splitPath = path.Split(" \r\n\t".ToCharArray());

            // get the first word of the sentence
            string firstWord = Normalize.MakeCaseInsensitive.TransformInput(splitPath[0]);
            string nextPath = (absPath + " " + firstWord).Trim();

            // and concatenate the rest of the input into a new path for child nodes
            string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            if (this.children.ContainsKey("_"))
            {
                //Node childNode = (Node)this.children["_"];
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
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the current matchstate
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Add(newWildcard.ToString());
                                // added due to this match being the end of the line
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.That:
                                query.ThatStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.Topic:
                                query.TopicStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.State:
                                query.StateStar.Add(newWildcard.ToString());
                                break;
                        }
                    }
                    query.TemplatePath = "_ " + query.TemplatePath;
                    return result;
                }
            }

            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            if (this.children.ContainsKey(firstWord))
            {
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                MatchState newMatchstate = matchstate;
                if (firstWord == "<THAT>")
                {
                    newMatchstate = MatchState.That;
                }
                else if (firstWord == "<TOPIC>")
                {
                    newMatchstate = MatchState.Topic;
                }
                else if (firstWord == "<STATE>")
                {
                    newMatchstate = MatchState.State;
                }
                else if (firstWord == "<PATTERN>")
                {
                    newMatchstate = MatchState.UserInput;
                }

                //Node childNode = (Node)this.children[firstWord];
                Node childNode = fetchChild(absPath, firstWord, pathDB);
                nextPath = (absPath + " " + firstWord).Trim();

                // move down into the identified branch of the GraphMaster structure using the new
                // matchstate
                StringBuilder newWildcard = new StringBuilder();
                string result = childNode.evaluateDB(newPath, query, request, newMatchstate, newWildcard, nextPath, pathDB);
                // and if we get a result from the child return it
                if (result.Length > 0)
                {
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the matchstate if it exists
                        // and then clear it for subsequent wildcards
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.That:
                                query.ThatStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.Topic:
                                query.TopicStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.State:
                                query.StateStar.Add(newWildcard.ToString());
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                        }
                    }
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
                foreach (string s in this.children.Keys)
                {
                    if (isWildPattern(s))
                    {
                        string wildSense = s.Replace("*", "");
                        if (matchesWildSense(wildSense, firstWord, request))
                        {
                            // Treat like matching a wildcard

                            // o.k. look for the path in the child node denoted by "*"
                            //Node childNode = (Node)this.children[s];
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
                                if (newWildcard.Length > 0)
                                {
                                    // capture and push the star content appropriate to the current matchstate
                                    switch (matchstate)
                                    {
                                        case MatchState.UserInput:
                                            query.InputStar.Add(newWildcard.ToString());
                                            // added due to this match being the end of the line
                                            newWildcard.Remove(0, newWildcard.Length);
                                            break;
                                        case MatchState.That:
                                            query.ThatStar.Add(newWildcard.ToString());
                                            break;
                                        case MatchState.Topic:
                                            query.TopicStar.Add(newWildcard.ToString());
                                            break;
                                        case MatchState.State:
                                            query.StateStar.Add(newWildcard.ToString());
                                            break;
                                    }
                                }
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
            if (this.children.ContainsKey("*"))
            {
                // o.k. look for the path in the child node denoted by "*"
                //Node childNode = (Node)this.children["*"];
                Node childNode = fetchChild(absPath, "*", pathDB);
                nextPath = (absPath + " " + "*").Trim();

                // add the next word to the wildcard match 
                StringBuilder newWildcard = new StringBuilder();
                this.storeWildCard(splitPath[0], newWildcard);

                string result = childNode.evaluateDB(newPath, query, request, matchstate, newWildcard, nextPath, pathDB);
                // and if we get a result from the branch process and return it
                if (result.Length > 0)
                {
                    if (newWildcard.Length > 0)
                    {
                        // capture and push the star content appropriate to the current matchstate
                        switch (matchstate)
                        {
                            case MatchState.UserInput:
                                query.InputStar.Add(newWildcard.ToString());
                                // added due to this match being the end of the line
                                newWildcard.Remove(0, newWildcard.Length);
                                break;
                            case MatchState.That:
                                query.ThatStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.Topic:
                                query.TopicStar.Add(newWildcard.ToString());
                                break;
                            case MatchState.State:
                                query.StateStar.Add(newWildcard.ToString());
                                break;
                        }
                    }
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
                foreach (string s in this.children.Keys)
                {
                    if (isWildPattern(s))
                    {
                        string wildSense = s.Replace("*", "");
                        if (matchesWildSense(wildSense, multiWord, request))
                        {
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

        public Node fetchChild(string myPath, string childWord, ExternDB pathDB)
        {
            string childPath = (myPath + " " + childWord).Trim();
            Node childNode = pathDB.fetchNode(childPath,true);
            childNode.word = childWord;
            return childNode;
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

        bool isWildPattern(string s)
        {
            bool v = (s.StartsWith("*")) && (s.Length > 1);
            return v;
        }
        bool matchesWildSense(string sense, string queryWord,Request request)
        {
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

            // Can you find a match inside (using regex while we're here)?
            var matcher = new Regex(wnWord);
            if (matcher.IsMatch(wnWord)) return ( true ^ negation );

            // bot settings check

            if (wnPos == "bot")
            {
                string val = contextBot.GlobalSettings.grabSetting(wnWord);
                if (val == null) return (false ^ negation);
                if (queryWord.ToLower().Contains(val.ToLower()))
                    return (true ^ negation);
                return (false ^ negation);
            }

            if (wnPos == "user")
            {
                string val = request.user.Predicates.grabSetting(wnWord);
                if (val == null) return (false ^ negation);
                if (queryWord.ToLower().Contains(val.ToLower()))
                    return (true ^ negation);
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
                children.Add(childWord, nd);
                childrenStr += "(" + childWord + ")";
                if (fullChildSet) return;
                if ((childnum + children.Count) != childmax)
                {
                    Console.WriteLine("WARNING : {0} myNode.childnum({1}) + myNode.children.Count({2}) != myNode.childmax({3})", "IN addChild", childnum, children.Count, childmax);
                    if ((childnum + children.Count) > childmax) childmax = (childnum + children.Count);
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
    }

    public class ExternDB
    {

        //using RaptorDB for persistent local key-value dictionary like storage
        //http://www.codeproject.com/Articles/190504/RaptorDB
        //http://www.codeproject.com/Articles/316816/RaptorDB-The-Key-Value-Store-V2

        public RaptorDB.RaptorDBString templatedb;
        public RaptorDB.RaptorDBString childdb;
        public RaptorDB.RaptorDBString childtrunkdb;
        public RaptorDB.RaptorDBString childcntdb;
        public RaptorDB.RaptorDBString scoredb;
        public RaptorDB.RaptorDBString filenamedb;
        public RaptorDB.RaptorDBString worddb;
        public RaptorDB.RaptorDBString loadeddb;
        public Dictionary<string, string> childcache = new Dictionary<string, string>();
        public Dictionary<string, Node> nodecache = new Dictionary<string, Node>();

        public const int trunkLevel = 9;


        public ExternDB()
        {

            string dbdirectory = ".\\rapstore\\";
            string ourPath = Directory.CreateDirectory(dbdirectory).FullName;
            string ourDirectory = Path.GetDirectoryName(ourPath);

            templatedb = new RaptorDB.RaptorDBString(ourDirectory + "\\templatedb", false);
            childdb = new RaptorDB.RaptorDBString(ourDirectory + "\\childdb", false);
            childtrunkdb = new RaptorDB.RaptorDBString(ourDirectory + "\\childtrunkdb", false);
            childcntdb = new RaptorDB.RaptorDBString(ourDirectory + "\\childcntdb", false);
            scoredb = new RaptorDB.RaptorDBString(ourDirectory + "\\scoredb", false);
            filenamedb = new RaptorDB.RaptorDBString(ourDirectory + "\\filenamedb", false);
            worddb = new RaptorDB.RaptorDBString(ourDirectory + "\\worddb", false);
            loadeddb = new RaptorDB.RaptorDBString(ourDirectory + "\\loadeddb", false);
        }

        public ExternDB(string dbdirectory)
        {
            string ourPath = Directory.CreateDirectory(dbdirectory).FullName;
            string ourDirectory = Path.GetDirectoryName(ourPath);

            templatedb = new RaptorDB.RaptorDBString(ourDirectory + "\\templatedb", false);
            childdb = new RaptorDB.RaptorDBString(ourDirectory + "\\childdb", false);
            childtrunkdb = new RaptorDB.RaptorDBString(ourDirectory + "\\childtrunkdb", false);
            childcntdb = new RaptorDB.RaptorDBString(ourDirectory + "\\childcntdb", false);
            scoredb = new RaptorDB.RaptorDBString(ourDirectory + "\\scoredb", false);
            filenamedb = new RaptorDB.RaptorDBString(ourDirectory + "\\filenamedb", false);
            worddb = new RaptorDB.RaptorDBString(ourDirectory + "\\worddb", false);
            loadeddb = new RaptorDB.RaptorDBString(ourDirectory + "\\loadeddb", false);
        }

        public void SaveIndex()
        {
            templatedb.SaveIndex();
            childdb.SaveIndex();
            childtrunkdb.SaveIndex();
            childcntdb.SaveIndex();
            scoredb.SaveIndex();
            filenamedb.SaveIndex();
            worddb.SaveIndex();
            loadeddb.Shutdown();
        }

        public void Close()
        {
            //flush our cache out
            List <string> trunklist =new List<string> ();
            foreach (string k in nodecache.Keys)
            {
                trunklist.Add (k);
            }
            foreach (string k in trunklist)
            {
                saveNode(k, nodecache[k], true);
            }
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
            templatedb.Shutdown();
            childdb.Shutdown();
            childtrunkdb.Shutdown();
            childcntdb.Shutdown();
            scoredb.Shutdown();
            filenamedb.Shutdown();
            worddb.Shutdown();
            loadeddb.Shutdown();
            GC.Collect();
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
            loadeddb.Set(filename, filename);

        }
        public bool wasLoaded(string filename)
        {
            string lf = "";
            bool ret = loadeddb.Get(filename, out lf);
            return (filename == lf);
        }
        public bool isTrunk(string absPath)
        {
            string[] s = absPath.Split(' ');
            return (s.Length < trunkLevel);
        }

        public bool containsNode(string absPath)
        {
            string cntStr = null;
            childcntdb.Get(absPath, out cntStr);
            return (cntStr != null);
        }
        public Node fetchNode(string absPath,bool full)
        {
            try
            {
                if (nodecache.ContainsKey(absPath))
                {
                    return nodecache[absPath];
                }

                Node myNode = new Node();
                myNode.fullChildSet = full;
                bool trunk = isTrunk(absPath);

                string scoreStr = "1.0";
                string cntStr = "0";
                myNode.score = 1.0;
                myNode.childnum = 0;
                myNode.childmax = 0;

                scoredb.Get(absPath, out scoreStr);
                templatedb.Get(absPath, out myNode.template);
                filenamedb.Get(absPath, out  myNode.filename);
                //worddb.Get(absPath, out  myNode.word);
                childcntdb.Get(absPath, out cntStr);
                if (absPath.Length == 0)
                {
                    childcntdb.Get(absPath, out cntStr);
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
                                    childtrunkdb.Get(childkey, out  childtxt);
                                }
                                else
                                {
                                    childdb.Get(childkey, out childtxt);
                                }
                            }

                            if ((childtxt != null) && (!myNode.children.ContainsKey(childtxt)))
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
                                Console.WriteLine("WARNING: Get({0} ({1}) ) returns null", childkey, hc);

                            }
                        }
                    }
                }
                string childrenStr = "";
                //foreach (string c in myNode.children.Keys)
                //{
                //    childrenStr += "(" + c + ")";
                //}
                myNode.childrenStr = childrenStr;

                if (myNode.template == null) myNode.template = "";
                if (myNode.filename == null) myNode.filename = "";
                if (myNode.word == null) myNode.word = "";
                //if (myNode.childrenStr == null) myNode.childrenStr = "";

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
            saveNode(absPath, myNode, false);
        }

        public void saveNode(string absPath, Node myNode,bool flushing)
        {
            try
            {
                bool trunk = isTrunk(absPath);
                if ((flushing == false) && (trunk == true))
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
                    return;
                }
                if (myNode.fullChildSet) return; // we are in eval mode so read-only

                // We are a writable node, in the trunk or flushing

                if (myNode.template == null) myNode.template = "";
                if (myNode.filename == null) myNode.filename = "";
                if (myNode.word == null) myNode.word = "";
                if (myNode.childrenStr == null) myNode.childrenStr = "";

                string childrenStr = "";
                //foreach (string c in myNode.children.Keys)
               // {
                //    childrenStr += "(" + c + ")";
                //}


               // if ((childrenStr != myNode.childrenStr) && 
                if ((myNode.childnum + myNode.children.Count) != myNode.childmax )
                {
                    Console.WriteLine("WARNING : {0} myNode.childnum({1}) + myNode.children.Count({2}) != myNode.childmax({3})", absPath, myNode.childnum, myNode.children.Count, myNode.childmax);
                    if ((myNode.childnum + myNode.children.Count) > myNode.childmax) myNode.childmax = (myNode.childnum + myNode.children.Count);
                }
                if (myNode.children.Count  > 0)
                {
                    int offset = myNode.childnum;
                    if (myNode.fullChildSet) offset = 0;
                    for (int i = 0; i < myNode.children.Count; i++)
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
                                childtrunkdb.RemoveKey(childkey);
                                childtrunkdb.Set(childkey, childtxt);
                            }
                            else
                            {
                                childdb.RemoveKey(childkey);
                                childdb.Set(childkey, childtxt);
                            }
                            if (childcache.ContainsKey(childkey))
                            {
                                childcache[childkey] = childtxt;
                            }
                            else
                            {
                                childcache.Add(childkey, childtxt);
                            }

                        }
                    }
                }
                //myNode.childnum += myNode.children.Count;
                myNode.childmax = myNode.childnum + myNode.children.Count; 

                //myNode.childrenStr = childrenStr;

                if (absPath.Length == 0)
                {
                    trunk = trunk;
                }
                if (myNode.template.Length > 0) templatedb.Set(absPath, myNode.template);
                if (myNode.filename.Length > 0) filenamedb.Set(absPath, myNode.filename);
                //if (myNode.word.Length > 0) worddb.Set(absPath, myNode.word);
                if (myNode.score != 1.0) scoredb.Set(absPath, myNode.score.ToString());
                //if (myNode.childnum > 0) 
                childcntdb.Set(absPath, myNode.childmax.ToString());
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
    }
}