using System;
using System.Collections.Generic;
using System.Xml;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;
using StringAppendableUnifiable = RTParser.StringAppendableUnifiableImpl;

//using StringAppendableUnifiable = System.Text.StringBuilder;

namespace RTParser.Utils
{
    /// <summary>
    /// Encapsulates a node in the graphmaster tree structure
    /// </summary>
    [Serializable]
    public class Node : StaticAIMLUtils
    {
        private void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine("!NODE: " + message + " in " + ToString(), args);
        }

        public void RotateTemplate(TemplateInfo templateInfo)
        {
            lock (SyncObject)
            {
                if (TemplateInfos != null && TemplateInfos.Count > 1)
                {
                    int moveLast = TemplateInfos.IndexOf(templateInfo);
                    if (moveLast>0) return; 
                    var last = TemplateInfos[moveLast];
                    TemplateInfos.RemoveAt(moveLast);
                    TemplateInfos.Add(last);
                }
            }
        }
        private GraphMaster _graph;

        public GraphMaster Graph
        {
            get
            {
                if (_graph != null) return _graph;
                return SyncObject.Graph;
            }
            set { _graph = value; }
        }

        private readonly Node Parent;

        public Node(Node P)
        {
            Parent = P;
            SyncObject = P ?? this;
        }

        #region Attributes

        /// <summary>
        /// Contains the child nodes of this node
        /// </summary>
        private Dictionary<string, Node> children;

        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        internal UList TemplateInfos; //Unifiable.Empty;

        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        internal UList TemplateInfosDisabled; //Unifiable.Empty;

        public UList TemplateInfoCopy
        {
            get
            {
                if (TemplateInfos == null) return null;
                lock (TemplateInfos)
                {
                    if (TemplateInfos.Count == 0) return null;
                    var copy = new UList();
                    copy.AddRange(TemplateInfos);
                    return copy;
                }
            }
        }

        public int TemplateInfoCount
        {
            get
            {
                if (TemplateInfos == null) return 0;
                lock (TemplateInfos)
                {
                    if (TemplateInfos.Count == 0) return 0;
                    return TemplateInfos.Count;
                }
            }
        }

        public object ChildCount
        {
            get
            {
                if (children == null) return -1;
                return children.Count;
            }
        }

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
        public TemplateInfo addTerminal(XmlNode templateNode, CategoryInfo category, GuardInfo guard, ThatInfo thatInfo,
                                        GraphMaster master, PatternInfo patternInfo, List<XmlNode> additionalRules)
        {
            bool onlyNonSilent = true;
            lock (SyncObject)
            {
                // first look in template node.. then afterwards the category node
                var nodes = new[] { templateNode, category.Category };

                // does the metaprops only operate on verbal tags
                bool sentientTags;
                if (TryParseBool(nodes, "verbal", out sentientTags))
                {
                    onlyNonSilent = sentientTags;
                }

                // this is a removall specfier!
                if (templateNode == StaticAIMLUtils.TheTemplateOverwrite)
                {
                    return DeleteTemplates(onlyNonSilent);
                }

                // does the metaprops special normal aiml way of "replace"
                bool removeAllFirst;
                if (TryParseBool(nodes, "replace", out removeAllFirst))
                {
                    if (removeAllFirst)
                    {
                        DeleteTemplates(onlyNonSilent);
                    }
                }
                if (TryParseBool(nodes, "ifMissing", out removeAllFirst))
                {
                    TemplateInfo first = FirstTemplate(onlyNonSilent);
                    if (first != null) return first;
                }
                if (TryParseBool(nodes, "append", out removeAllFirst))
                {
                    TemplateInfo first = FirstTemplate(onlyNonSilent);
                    if (first != null)
                    {
                        first.AppendTemplate(templateNode, category, additionalRules);
                        return first;
                    }
                }
                return addTerminal_0_Lock(templateNode, category, guard, thatInfo, master, patternInfo, additionalRules);
            }
        }

        internal TemplateInfo FirstTemplate(bool onlyNonSilent)
        {
            lock (SyncObject)
            {
                if (TemplateInfos != null && TemplateInfos.Count > 0)
                {
                    TemplateInfo NonSilentDisabled = null;
                    foreach (var info in TemplateInfos)
                    {
                        if (onlyNonSilent) if (info.IsSilent) continue;
                        if (!info.IsDisabled) return info;
                        NonSilentDisabled = NonSilentDisabled ?? info;
                    }
                    return NonSilentDisabled ?? TemplateInfos[0];
                }
                return null;
            }
        }

        private TemplateInfo addTerminal_0_Lock(XmlNode templateNode, CategoryInfo category, GuardInfo guard, ThatInfo thatInfo,
                                        GraphMaster master, PatternInfo patternInfo, List<XmlNode> additionalRules)
        {
            string templateKey = TemplateInfo.MakeKey(templateNode, (guard != null ? guard.Output : null),
                                                      thatInfo != null ? thatInfo.PatternNode : StaticAIMLUtils.XmlStar);

            if (this.TemplateInfos == null)
            {
                this.TemplateInfos = new UList();
            }
            else if (master.RemoveDupicateTemplatesFromNodes)
            {
                TemplateInfo returnIt = null;
                {
                    int count = TemplateInfos.Count;
                    // search for old
                    List<TemplateInfo> dupes = null;
                    int nodeNum = 0;
                    foreach (TemplateInfo temp in TemplateInfos)
                    {
                        if (temp.AimlSameKey(templateKey))
                        {
                            if (nodeNum == 0)
                            {
                                //TemplateInfo redundant = TemplateInfo.GetTemplateInfo(templateNode, guard, thatInfo, this, category);
                                master.AddRedundantCate(category, temp);
                                return temp;
                            }
                            nodeNum++;
                            dupes = dupes ?? new List<TemplateInfo>();
                            dupes.Add(temp);
                        }
                    }
                    if (dupes != null)
                    {
                        if (TemplateInfos.Count == 1)
                        {
                            TemplateInfo temp = dupes[0];
                            return temp;
                            if (true)
                            {
                                writeToLog("ERROR!! AIMLLOADER ONE DUPE REDUNDANT " + TemplateInfos[0]);
                                // no side effect!
                                master.RemoveTemplate(temp);
                                TemplateInfos = null;
                                return temp;
                            }
                        }
                        else
                        {
                            foreach (TemplateInfo temp in dupes)
                            {
                                if (temp == TemplateInfos[0])
                                {
                                    return temp;
                                }
                                if (true)
                                {
                                    writeToLog("AIMLLOADER MOVING FIRST \n" + temp + "\n from: " + category.Filename);
                                    master.RemoveTemplate(temp);
                                    this.TemplateInfos.Remove(temp);
                                    this.TemplateInfos.Insert(0, temp);
                                    return temp;
                                }
                            }
                        }
                        //   dupes.Clear();
                        //   dupes = null;
                    }
                }
            }

            // last in first out addition
            TemplateInfo newTemplateInfo = TemplateInfo.GetTemplateInfo(templateNode, guard, thatInfo, this, category);
            newTemplateInfo.TemplateKey = templateKey;
            // this.That = thatInfo;
            PatternInfo pat = patternInfo;
            if (category != null)
            {
                category.That = thatInfo;
                category.AddTemplate(newTemplateInfo);
                pat = category.Pattern;
            }
            if (pat != null)
            {
                if (thatInfo != null && thatInfo.FullPath.AsString() == "*")
                {
                    if (patternInfo.LoopsFrom(newTemplateInfo.InnerXml))
                    {
                        writeToLog("ERROR because LoopsFrom so SKIPPING! " + pat + "==" + newTemplateInfo + "");
                        if (this.TemplateInfos.Count == 0)
                        {
                            this.TemplateInfos = null;
                        }
                        return null;
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
                        return null;
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
            return newTemplateInfo;
        }

        private TemplateInfo DeleteTemplates(bool onlyNonSilent)
        {
            lock (SyncObject)
            {
                if (TemplateInfos != null)
                {
                    if (TemplateInfos.Count > 0)
                    {
                        foreach (TemplateInfo list in new UList(TemplateInfos))
                        {
                            if (onlyNonSilent && list.IsSilent) continue;                           
                            DisableTemplate(list);
                        }
                    }
                    this.TemplateInfos.Clear();
                    this.TemplateInfos = null;
                }
                return null;
            }
        }

        private void DisableTemplate(TemplateInfo info)
        {
            lock (SyncObject)
            {
                if (TemplateInfos != null) TemplateInfos.Remove(info);
                if (TemplateInfosDisabled == null) TemplateInfosDisabled = new UList();
                info.Graph.DisableTemplate(info);
                TemplateInfosDisabled.Add(info);
            }
        }

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="outTemplate">the outTemplate to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        public Node addPathNodeChilds(Unifiable path)
        {
            return addPathNodeChilds(0, path.ToArray());
        }

        private Node addPathNodeChilds(int from, Unifiable[] path)
        {
            Node initial = null;

            // check we're not at the leaf node
            if (from >= path.Length)
            {
                return this;
            } // was the nex block comment

            /*
            if (!path.IsWildCard() && path.AsString().Trim().Length == 0)
            {
                //this.GuardText = guard;
                //this.filename = filename;
                return this;
            }
            */
            // otherwise, this sentence requires further child nodemappers in order to
            // be fully mapped within the GraphMaster structure.

            // split the input into its component words
            //Unifiable[] words0 = path./*Trim().*/Split();//" ".ToCharArray());

            //Unifiable firstRaw = path[from];//.First(); // words0[0];
            //string w = firstRaw.AsString();

            // get the first word (to form the key for the child nodemapper)
            //Unifiable firstWord = Normalize.MakeCaseInsensitive.TransformInput(firstRaw);
            Unifiable firstWord = path[from];

            // concatenate the rest of the sentence into a suffix (to act as the
            // path argument in the child nodemapper)
            //Unifiable newPath = path.Rest(); // Unifiable.Join(" ", words0, 1, words0.Length - 1);
            // path.Rest();// Substring(firstWord.Length, path.Length - firstWord.Length).Trim();

            // o.k. check we don't already have a child with the key from this sentence
            // if we do then pass the handling of this sentence down the branch to the 
            // child nodemapper otherwise the child nodemapper doesn't yet exist, so create a new one  \
            bool found = false;
            string fs = firstWord.ToUpper();
            fs = ToKey(fs);
            Node childNode;
            lock (SyncObject)
            {
                if (this.children != null && this.children.TryGetValue(fs, out childNode))
                {
                    initial = childNode.addPathNodeChilds(from + 1, path);
                    found = true;
                }


                if (false) // see if we need ot check new indexing system!
                    if (!found)
                        foreach (KeyValuePair<string, Node> c in this.children)
                        {
                            string ks = c.Key.ToUpper();
                            if (ks == fs)
                            {
                                childNode = c.Value;
                                initial = childNode.addPathNodeChilds(from + 1, path);
                                found = true;
                                break;
                            }
                            else
                            {
                                string kks = c.Value.word.ToUpper();
                                if (kks == fs || ks != kks)
                                {
                                    childNode = c.Value;
                                    initial = childNode.addPathNodeChilds(from + 1, path);
                                    found = true;
                                    break;
                                }
                            }
                        }


                if (!found)
                {
                    childNode = new Node(this);
                    childNode.word = firstWord;
                    initial = childNode.addPathNodeChilds(from + 1, path);
                    children = children ?? new Dictionary<string, Node>();
                    this.children.Add(fs, childNode);
                }
            }
            if (initial == null) throw new NullReferenceException("no child node: " + this);
            return initial;
        }

        private static string ToKey(string fs0)
        {
            const bool doEs = true;
            const bool doSEs = true;
            fs0 = fs0.ToUpper().Trim();
            string fs00 = fs0;
            string fs = fs0;
            int fl = fs.Length;
            if (fl == 0)
            {
                return "";
            }
            char c0 = fs0[fl - 1];
            char c = c0;
            if (fl > 4)
            {
                if (c == 'S')
                {
                    if (doSEs && fs.EndsWith("SSES"))
                    {
                        fs = fs.Substring(0, fl - 2);
                        fl -= 2;
                    }
                    else if (doSEs && fs.EndsWith("SSED"))
                    {
                        fs = fs.Substring(0, fl - 2);
                        fl -= 2;
                    }
                    else
                    {
                        fl = fs.Length;
                        char c2 = fs[fl - 2];
                        if (c2 == 'S') // preserve ss 
                        {
                        }
                        else if (c2 == 'E') // preserve [C]es 
                        {
                            if (doEs)
                            {
                                fs = fs.Substring(0, fl - 2);
                                fl -= 2;
                            }
                            else
                            {
                                fs = fs.Substring(0, fl - 1);
                                fl -= 1;
                            }
                        }
                        else if ("AEIOU".IndexOf(c2) >= 0)
                        {
                            if (doEs && c2 == 'E') fs = fs.Substring(0, fl - 2);
                            fl -= 2;
                        }
                        else
                        {
                            fs = fs.Substring(0, fl - 1);
                            fl -= 1;
                        }
                    }
                }
                if (fl > 3)
                {
                    c = fs[fl - 1];
                    if (c == 'D')
                    {
                        if (doSEs && fs.EndsWith("SSED"))
                        {
                            fs = fs.Substring(0, fl - 2);
                            fl = fl - 2;
                        }
                        else
                        {
                            char c2 = fs[fl - 2];
                            if (c2 == 'E') // preserve 
                            {
                                c2 = fs[fl - 3];
                                if ("AEIOU".IndexOf(c2) == -1)
                                {
                                    fs = fs.Substring(0, fl - 2);
                                    fl = fl - 2;
                                }
                            }
                        }
                    }
                }
                if (fl > 3)
                {
                    c = fs[fl - 1];
                    if (doEs && c == 'E')
                    {
                        fs = fs.Substring(0, fl - 1);
                        fl = fl - 1;
                    }
                }
                if (fl > 3)
                {
                    c = fs[fl - 1];
                    if (doEs && c == 'S')
                    {
                        fs = fs.Substring(0, fl - 1);
                        fl = fl - 1;
                    }
                }
            }
            //if (c0 == 'E' || c0 == 'S' || c0 == 'D') Console.Error.WriteLine(fs00 + "->" + fs);
            if (fs0 == fs) return fs0;
            return fs;
        }

        #endregion

        //private ThatInfo That;

        #region Evaluate Node

        internal Node GetNextNode()
        {
            if (Parent == null) return null;
            bool useNext = false;
            lock (SyncObject)
                foreach (KeyValuePair<string, Node> v in Parent.children)
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

        private readonly Node SyncObject;

        public bool disabled;
        public static bool UseZeroArgs;
        public static StringAppendableUnifiableImpl EmptyStringAppendable = new StringAppendableUnifiableImpl();

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
        public Node evaluate(string path, SubQuery query, Request request, MatchState matchstate,
                             StringAppendableUnifiableImpl wildcard)
        {
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
            var splitPath = path.Split(" \r\n\t".ToCharArray());
            Node location = evaluateFirst(0, splitPath, query, request, matchstate, wildcard);
            return location;
        }

        private Node evaluateNext(int at, string[] splitPath, SubQuery query, Request request, MatchState matchstate,
                                  StringAppendableUnifiableImpl wildcard)
        {
            Node vv = evaluateFirst(at, splitPath, query, request, matchstate, wildcard);
            if (wildcard.ToString().Trim().Length > 0)
            {
                if (vv == null || vv.disabled || vv.NoEnabledTemplates) return null;
            }
            if (vv == null || vv.disabled || vv.NoEnabledTemplates) return null;
            return vv;
        }

        public Node evaluateFirst(int at, string[] splitPath, SubQuery query, Request request, MatchState matchstate,
                                  StringAppendableUnifiableImpl wildcard)
        {
            // check for timeout
            // check for timeout
            if (DateTime.Now > request.TimesOutAt)
            {
                request.writeToLog("TIMEOUT! User: " +
                                   request.Requester.UserID + " raw input: \"" +
                                   request.rawInput + "\" in " + this);
                request.IsTraced = true;
                if (!request.hasTimedOut)
                {
                    request.TimesOutAt = DateTime.Now + TimeSpan.FromSeconds(5);
                    request.hasTimedOut = true;
                }
                else
                {
                    return null; // Unifiable.Empty;                    
                }
            }

            int pathLength = splitPath.Length - at;

            // so we still have time!
            //path = path.Trim();

            // check if this is the end of a branch in the GraphMaster 
            // return the cCategory for this node
            if (this.children == null || this.children.Count == 0)
            {
                if (pathLength > 0 && UseWildcard(EmptyStringAppendable))
                {
                    // if we get here it means that there is a wildcard in the user input part of the
                    // path.
                    storeWildCard(string.Join(" ", splitPath, at, pathLength), wildcard);
                }
                return this;
            }

            // if we've matched all the words in the input sentence and this is the end
            // of the line then return the cCategory for this node
            if (pathLength <= 0)
            {
                if (NoEnabledTemplates)
                {
                }
                return this;
            }

            // otherwise split the input into it's component words
            //string[] splitPath = path.Split(" \r\n\t".ToCharArray());

            // get the first word of the sentence
            string firstWord = splitPath[at];

            // and concatenate the rest of the input into a new path for child nodes
            //string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);

            // first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            lock (SyncObject)
                foreach (KeyValuePair<string, Node> childNodeKV in this.children)
                {
                    Node childNode = childNodeKV.Value;
                    Unifiable childNodeWord = childNode.word;
                    if (!childNodeWord.IsAnySingleUnit()) continue;

                    // add the next word to the wildcard match 
                    StringAppendableUnifiableImpl newWildcard = Unifiable.CreateAppendable();
                    storeWildCard(firstWord, newWildcard);

                    // move down into the identified branch of the GraphMaster structure
                    Node result = childNode.evaluateNext(at + 1, splitPath, query, request, matchstate, newWildcard);

                    // and if we get a result from the branch process the wildcard matches and return 
                    // the result
                    if (result != null)
                    {
                        if (UseWildcard(newWildcard))
                        {
                            // capture and push the star content appropriate to the current matchstate
                            switch (matchstate)
                            {
                                case MatchState.UserInput:
                                    if (childNodeWord.StoreWildCard()) Insert(query.InputStar, newWildcard.ToString());
                                    // added due to this match being the end of the line
                                    newWildcard.Length = 0; // Remove(0, newWildcard.Length);
                                    break;
                                default:
                                    var stars = query.GetMatchList(matchstate);
                                    if (childNodeWord.StoreWildCard()) Insert(stars, newWildcard.ToString());
                                    newWildcard.Length = 0;
                                    break;
                            }
                        }
                        return result;
                    }
                }


            // second option - the nodemapper may have contained a "_" child, but led to no match
            // or it didn't contain a "_" child at all. So get the child nodemapper from this 
            // nodemapper that matches the first word of the input sentence.
            while (true)
            {
                string firstWord0;
                //string np;
                int newAt;
                Node childNode = LitteralChild(at, splitPath, out firstWord0, out newAt, query);
                if (childNode == null) break;
                if (firstWord0 != firstWord)
                {
                    writeToLog(firstWord + "!=" + firstWord0);
                }
                //firstWord = fw0;
                //at = newAt;
                // process the matchstate - this might not make sense but the matchstate is working
                // with a "backwards" path: "topic <topic> that <that> user input"
                // the "classic" path looks like this: "user input <that> that <topic> topic"
                // but having it backwards is more efficient for searching purposes
                MatchState newMatchstate = matchstate;
                bool isTag = firstWord.StartsWith("TAG-");
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
                StringAppendableUnifiableImpl newWildcard = Unifiable.CreateAppendable();
                Node result = childNode.evaluateNext(newAt, splitPath, query, request, newMatchstate, newWildcard);
                // and if we get a result from the child return it
                if (result != null)
                {
                    Unifiable childNodeWord = childNode.word;
                    if (!isTag)
                    {
                        if (childNodeWord.IsLitteral())
                        {
                        }
                        if (childNodeWord.IsWildCard())
                        {
                            if (childNodeWord.StoreWildCard())
                            {
                                writeToLog("should store WC for " + childNodeWord + " from " + firstWord);
                                var stars = query.GetMatchList(matchstate);
                                Insert(stars, firstWord);
                            }
                        }
                        else if (childNodeWord.IsLazy())
                        {
                            if (childNodeWord.StoreWildCard())
                            {
                                writeToLog("should store WC for " + childNodeWord + " from " + firstWord);
                                var stars = query.GetMatchList(matchstate);
                                Insert(stars, firstWord);
                            }
                        }
                    }
                    if (UseWildcard(newWildcard))
                    {
                        // capture and push the star content appropriate to the matchstate if it exists
                        // and then clear it for subsequent wildcards
                        var stars = query.GetMatchList(matchstate);
                        if (childNodeWord.StoreWildCard()) Insert(stars, newWildcard.ToString());
                        newWildcard.Length = 0;
                    }
                    return result;
                }
                break;
            }

            // third option - the input part of the path might have been matched so far but hasn't
            // returned a match, so check to see it contains the "*" wildcard. "*" comes last in
            // precedence in the AIML alphabet.
            bool wisTag = firstWord.StartsWith("TAG-");
            if (!wisTag)
                lock (SyncObject)
                    foreach (KeyValuePair<string, Node> childNodeKV in this.children)
                    {
                        Node childNode = childNodeKV.Value;
                        Unifiable childNodeWord = childNode.word; //.Key;
                        if (!childNodeWord.IsLongWildCard()) continue;

                        // o.k. look for the path in the child node denoted by "*"
                        //Node childNode = childNodeKV.Value;

                        // add the next word to the wildcard match 
                        StringAppendableUnifiableImpl newWildcard = Unifiable.CreateAppendable();
                        storeWildCard(firstWord, newWildcard);

                        Node result = childNode.evaluateNext(at + 1, splitPath, query, request, matchstate, newWildcard);
                        // and if we get a result from the branch process and return it
                        if (result != null)
                        {
                            if (UseWildcard(newWildcard))
                            {
                                // capture and push the star content appropriate to the current matchstate
                                switch (matchstate)
                                {
                                    case MatchState.UserInput:
                                        if (childNodeWord.StoreWildCard())
                                        {
                                            Insert(query.InputStar, newWildcard.ToString());
                                            // added due to this match being the end of the line
                                            newWildcard.Length = 0; // Remove(0, newWildcard.Length);
                                        }
                                        break;
                                    default:
                                        var stars = query.GetMatchList(matchstate);
                                        if (childNodeWord.StoreWildCard()) Insert(stars, newWildcard.ToString());
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
            if (!wisTag)
                if (word.IsAnySingleUnit() || word.IsLongWildCard())
                {
                    storeWildCard(firstWord, wildcard);
                    Node result = this.evaluateNext(at + 1, splitPath, query, request, matchstate, wildcard);
                    return result;
                }

            // If we get here then we're at a dead end so return an empty string. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            //wildcard = new StringBuilder();
            wildcard.Length = 0;
            return null; /// string.Empty;
        }

        protected bool NoEnabledTemplates
        {
            get
            {
                if (TemplateInfos == null) return true;
                int tc = TemplateInfos.Count;
                if (tc == 0) return true;
                if (tc == 1) return TemplateInfos[0].IsDisabled;
                return false;
            }
        }

        private static void Insert(List<Unifiable> unifiables, string s)
        {
            s = s.Replace("TAG-START", "");
            s = s.Replace("TAG-END", "").Trim();
            unifiables.Insert(0, s);
        }


        private Node LitteralChild(int at, string[] splitPath, out string firstWord, out int newAt, SubQuery query)
        {
            //IList<Node> childrenS = new List<Node>();
            Node childNode;
            firstWord = splitPath[at];
            string fs = ToKey(firstWord);
            if (children.TryGetValue(fs, out childNode))
            {
                if (query.CanUseNode(childNode))
                {
                    //newPath = string.Join(" ", splitPath, rw, splitPath.Length - rw);
                    newAt = at + 1;
                    return childNode;
                }
            }
            foreach (KeyValuePair<string, Node> childNodeKV in children)
            {
                Unifiable childNodeWord = childNodeKV.Value.word;
                if (childNodeWord.IsAnySingleUnit()) continue;
                // if (childNodeWord.IsLongWildCard()) continue;
                // if (childNodeWord.IsWildCard()) continue;
                childNode = childNodeKV.Value;
                if (!query.CanUseNode(childNode))
                {
                    continue;
                }
                //childrenS.Add(childNode);
                string fw;
                Unifiable newPath0;
                if (!childNode.word.ConsumePath(at, splitPath, out firstWord, out newPath0, out newAt, query))
                {
                    continue;
                }
                //newPath = newPath0;
                return childNode;
            }

            newAt = at;
            return null;
        }


        public static bool UseWildcard(StringAppendableUnifiableImpl newWildcard)
        {
            if (newWildcard.Length > 0) return true;
            return UseZeroArgs;
        }

        /// <summary>
        /// Correctly stores a word in the wildcard slot
        /// </summary>
        /// <param name="word">The word matched by the wildcard</param>
        /// <param name="wildcard">The contents of the user input absorbed by the AIML wildcards "_" and "*"</param>
        private static void storeWildCard(Unifiable word, StringAppendableUnifiableImpl wildcard)
        {
            if (word.AsString().StartsWith("TAG-"))
            {
                return;
            }

            if (wildcard.Length > 0)
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