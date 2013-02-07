#if INCLUDE_UUNODE

using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Text;
using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLParser;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;
//using CategoryInfo = RTParser.Utils.TemplateInfo;
//using StringAppendableUnifiable = System.Text.StringBuilder;
using PatternInfo = RTParser.Unifiable;
using ThatInfo = RTParser.Unifiable;
using TopicInfo = RTParser.Unifiable;
using GuardInfo = RTParser.Unifiable;
using ResponseInfo = RTParser.Unifiable;
using Node = RTParser.Utils.UUNode;
using SNode = AltAIMLbot.Utils.Node;
namespace RTParser.Utils
{
    /// <summary>
    /// Encapsulates a node in the graphmaster tree structure
    /// </summary>
    [Serializable]
    public class UUNode : IComparable<Node>, ParentChild
    {
        [NonSerialized]
        private bool inLowMemoryHooks = false;
        public long RunLowMemHooks()
        {
            long saved = 0;
            if (inLowMemoryHooks) return saved;
            inLowMemoryHooks = true;
            if (word != null)
            {
                // saved += word.RunLowMemHooks();
            }
            if (TemplateInfos != null)
                foreach (TemplateInfo list in TemplateInfoCopy)
                {
                    saved += list.RunLowMemHooks();
                }
            if (TemplateInfosDisabled != null)
                foreach (TemplateInfo list in TemplateInfosDisabled)
                {
                    saved += list.RunLowMemHooks();
                }
            if (children0 != null)
                foreach (KeyValuePair<string, Node> pair in children0)
                {
                    saved += pair.Value.RunLowMemHooks();
                }
            if (specialChildren != null)
                foreach (KeyValuePair<string, Node> pair in specialChildren)
                {
                    saved += pair.Value.RunLowMemHooks();
                }
            inLowMemoryHooks = false;
            return saved;
        }

        const bool needsKeySanityCheck = false;
        public static bool UseZeroArgs;
        public static StringAppendableUnifiableImpl EmptyStringAppendable = new StringAppendableUnifiableImpl();

        #region Attributes

        /// <summary>
        /// Contains the child nodes of this node
        /// </summary>
        private Dictionary<string, Node> children0;

        private SortedDictionary<string, Node> specialChildren;
        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        internal UList TemplateInfos; //Unifiable.Empty;

        /// <summary>
        /// The template (if any) associated with this node
        /// </summary>
        internal UList TemplateInfosDisabled; //Unifiable.Empty;

        private readonly Node _parentObject;
        public bool disabled;

        /// <summary>
        /// The word that identifies this node to it's ParentResult node
        /// </summary>
        readonly Unifiable word = null;//Unifiable.Empty;

        private string _graph;
        
        public GraphMaster Graph
        {
            get
            {
                if (_graph != null) return AltBot.FindGlobalGraph(_graph);
                Node Parent0 = _parentObject;
                if (Parent0 != null)
                {
                    return Parent0.Graph;
                }
                return null;
            }
            set
            {
                _graph = value.ScriptingName;
            }
        }
        

        public UUNode(Node P, Unifiable W)
        {
            _parentObject = P;
            if (W != null && W.IsEmpty)
            {
               // throw new InvalidCastException("Empty Node!");
            }
            word = W;
            //SyncObject = this;// P ?? this;
        }


        public UList TemplateInfoCopy
        {
            get
            {
                lock (SyncObject)
                {
                    if (TemplateInfos == null) return null;
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
                lock (SyncObject)
                {
                    if (TemplateInfos == null) return 0;
                    if (TemplateInfos.Count == 0) return 0;
                    return TemplateInfos.Count;
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
            var p = Parent;
            if (p == null)
            {
                return word;
            }
            StringBuilder sb = new StringBuilder(Unifiable.ToVMString(word));
            sb.Insert(0, " ");
            sb.Insert(0, Unifiable.ToVMString(p.word));
            p = p.Parent;
            while (p != null)
            {
                sb.Insert(0, " ");
                sb.Insert(0, Unifiable.ToVMString(p.word));
                p = p.Parent;
            }
            return sb.ToString();
        }


        public bool Equals(Node other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return false;
            if (!Equals(other.word, word)) return false;
            //if (!Equals(other.Graph, Graph)) return false;
            if (!Equals(other.ParentObject, ParentObject)) return false;
            writeToLog("ERROR optimally this should be imposible");
            return true;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return ((ParentObject != null ? ParentObject.GetHashCode() : 0) * 397) ^ (word != null ? word.GetHashCode() : 0);
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
                a1 -= thatpath1.Strictness;
                b1 -= thispath1.Strictness;
            }
            if (a1 == b1)
            {
                return StaticAIMLUtils.ReferenceCompare(thiz, that);
            }
            return a1.CompareTo(b1);
            //}
            //   return cmpthis.CompareTo(cmpthat);
        }

        #endregion

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
            _variance = word.Strictness;

            var p = Parent;
            if (p == null)
            {
                return (_ToPath = new[] { word });
            }
            var sb = new List<Unifiable> { word };
            var pword = p.word;
            sb.Add(pword);
            _variance += pword.Strictness;
            p = p.Parent;
            while (p != null)
            {
                pword = p.word;
                sb.Add(pword);
                _variance += pword.Strictness;
                p = p.Parent;
            }
            return (_ToPath = sb.ToArray());
        }

        //private readonly Node SyncObject;
        private Node SyncObject
        {
            get { return this; }
        }

        public Node Parent
        {
            get { return _parentObject as Node; }
        }
        #endregion

        #region Methods

        private void writeToLog(string message, params object[] args)
        {
            AltBot.writeDebugLine("!NODE: " + message + " in " + ToString(), args);
        }

        public void RotateTemplate(TemplateInfo templateInfo)
        {
            lock (SyncObject)
            {
                if (TemplateInfos != null && TemplateInfos.Count > 1)
                {
                    int moveLast = TemplateInfos.IndexOf(templateInfo);
                    if (moveLast < 0)
                    {
                        if (TemplateInfosDisabled != null)
                        {
                            moveLast = TemplateInfosDisabled.IndexOf(templateInfo);
                            if (moveLast < 0)
                            {
                                writeToLog("WARN: Not rotatating disabled template: " + templateInfo);
                                return;
                            }
                        }
                        writeToLog("WARN: Cant find template: " + templateInfo);
                        return;
                    }
                    if (moveLast == TemplateInfos.Count - 1)
                    {
                        // writeToLog("WARN: template already last " + templateInfo);
                        // already last
                        return;
                    }
                    TemplateInfo last = TemplateInfos[moveLast];
                    TemplateInfos.RemoveAt(moveLast);
                    TemplateInfos.Add(last);
                }
            }
        }

        #region Add category

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="template">the template to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        public List<CategoryInfo> addTerminal(XmlNode templateNode, XmlNode cateNode, GuardInfo guard, TopicInfo topicInfo, ThatInfo thatInfo,
                                        LoaderOptions master, PatternInfo patternInfo, List<ConversationCondition> additionalRules, out bool wouldBeRemoval)
        {
            List<CategoryInfo> categoryInfos = null;
            wouldBeRemoval = false;
            bool onlyNonSilent = master.DistinguishSilenetTags;
            lock (SyncObject)
            {
                // first look in template node.. then afterwards the category node
                var nodes = new[] { templateNode, cateNode };

                // does the metaprops only operate on verbal tags
                bool sentientTags;
                if (StaticAIMLUtils.TryParseBool(nodes, "verbal", out sentientTags))
                {
                    onlyNonSilent = sentientTags;
                }

                if (templateNode == null)
                {
                    writeToLog("TheTemplateOverwrite0: onlyNonSilent=" + onlyNonSilent + " " + StaticAIMLUtils.LocationInfo(cateNode));
                    wouldBeRemoval = true;
                    DeleteTemplates(onlyNonSilent);
                    return null;
                }
                // this is a removall specfier!
                if (templateNode.ChildNodes.Count == 0)
                {
                    if (templateNode != StaticAIMLUtils.TheTemplateOverwrite.Value)
                    {
                        //  writeToLog("TheTemplateOverwrite1: onlyNonSilent=" + onlyNonSilent + " " + templateNode.OuterXml + " " + LocationInfo(cateNode));
                    }
                    else
                    {
                        //  writeToLog("TheTemplateOverwrite2: onlyNonSilent=" + onlyNonSilent + " " + templateNode.OuterXml);
                    }
                    wouldBeRemoval = true;
                    DeleteTemplates(onlyNonSilent);
                    return null;
                }

                // does the metaprops special normal aiml way of "replace"
                bool removeAllFirst = master.RemovePreviousTemplatesFromNodes;
                bool tf;
                if (StaticAIMLUtils.TryParseBool(nodes, "replace", out tf))
                {
                    if (tf)
                    {
                        removeAllFirst = true;
                    }
                }
                if (StaticAIMLUtils.TryParseBool(nodes, "ifMissing", out tf))
                {
                    TemplateInfo first = FirstTemplate(onlyNonSilent);
                    if (first != null)
                    {
                        categoryInfos = new List<CategoryInfo> { first };
                        return categoryInfos;
                    }
                }
                if (StaticAIMLUtils.TryParseBool(nodes, "append", out tf))
                {
                    TemplateInfo first = FirstTemplate(onlyNonSilent);
                    if (first != null)
                    {
                        first.AppendTemplate(templateNode, cateNode, additionalRules);
                        categoryInfos = new List<CategoryInfo> { first };
                        return categoryInfos;
                    }
                }

                if (removeAllFirst)
                {
                    wouldBeRemoval = true;
                    DeleteTemplates(onlyNonSilent);
                }


                var t = addTerminal_0_Lock(templateNode, cateNode, guard, topicInfo, thatInfo, master, patternInfo, additionalRules);
                if (t == null)
                {
                    return null;
                }
                t.IsSearchDisabled = false;
                bool isTraced;
                if (StaticAIMLUtils.TryParseBool(nodes, "isTraced", out isTraced))
                {
                    t.IsTraced = isTraced;
                }
                t.IsDisabledOutput = false;
                if (t != null) t.AddRules(additionalRules);

                bool isDisabled;
                if (StaticAIMLUtils.TryParseBool(nodes, "disabled", out isDisabled))
                {
                    t.IsDisabled = isDisabled;
                }
                return new List<CategoryInfo> { t };
            }
        }

        private TemplateInfo FirstTemplate(bool onlyNonSilent)
        {
            // lock (SyncObject)
            {
                if (TemplateInfos != null && TemplateInfos.Count > 0)
                {
                    TemplateInfo NonSilentDisabled = null;
                    foreach (TemplateInfo info in TemplateInfos)
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
        /*
         
         XmlNode templateNode, XmlNode cateNode, GuardInfo guard, ThatInfo thatInfo,
                                        LoaderOptions master, PatternInfo patternInfo, List<ConversationCondition> additionalRules,

         */
        private TemplateInfo addTerminal_0_Lock(XmlNode templateNode, XmlNode cateNode, GuardInfo guard,
            TopicInfo topicInfo,
                                                ThatInfo thatInfo,
                                                LoaderOptions loaderOptions, PatternInfo patternInfo,
                                                List<ConversationCondition> additionalRules)
        {
            string templateKey = TemplateInfoImpl.MakeKey(templateNode, (guard != null ? guard.PatternNode : null),
                                                      additionalRules);

            GraphMaster master = loaderOptions.CtxGraph;
            if (TemplateInfos == null)
            {
                TemplateInfos = new UList();
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
                                master.AddRedundantCate((CategoryInfo)temp, temp);
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
                                    writeToLog("AIMLLOADER MOVING FIRST \n" + temp + "\n from: " + temp.Filename);
                                    master.RemoveTemplate(temp);
                                    TemplateInfos.Remove(temp);
                                    TemplateInfos.Insert(0, temp);
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
            ResponseInfo responseInfo = templateNode.InnerXml;
            TemplateInfo newTemplateInfo =
                (TemplateInfo)CategoryInfoImpl1.GetCategoryInfo(patternInfo, cateNode, loaderOptions, templateNode,
                                                            responseInfo, guard, topicInfo, this.ToUNode(), thatInfo,
                                                            additionalRules);
            /*
            Unifiable categoryPath = GetPath();
            //categoryInfo.SetCategoryTag(categoryPath, patternInfo, categoryInfo,
            //                          outerNode, templateNode, guard, thatInfo);


            foreach (var node in additionalRules)
            {
                newTemplateInfo.AddPrecondition(node);
            }
             **/
            // return categoryInfo;


            newTemplateInfo.TemplateKey = templateKey;
            newTemplateInfo.That = thatInfo;
            CategoryInfo category = newTemplateInfo.CategoryInfo;
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
                    if (patternInfo.LoopsFrom(responseInfo))
                    {
                        writeToLog("ERROR because LoopsFrom so SKIPPING! " + pat + "==" + newTemplateInfo + "");
                        if (TemplateInfos.Count == 0)
                        {
                            TemplateInfos = null;
                        }
                        return null;
                    }
                    Unifiable from;
                    Unifiable to;
                    if (false && patternInfo.DivergesFrom(newTemplateInfo, out from, out to))
                    {
                        writeToLog("SKIPPING! " + pat + "==" + newTemplateInfo + "");
                        if (TemplateInfos.Count == 0)
                        {
                            TemplateInfos = null;
                        }
                        return null;
                    }
                }
                category.GraphmasterNode = this.ToUNode();
                if (category != null) pat.AddCategory(category);
            }

            master.AddTemplate(newTemplateInfo);
            if (pat != patternInfo)
            {
                writeToLog("Wierd! " + pat);
                throw new InvalidCastException("weird");
            }
            TemplateInfos.Insert(0, newTemplateInfo);
            newTemplateInfo.BuildIndexes();
            return newTemplateInfo;
        }

        private SNode ToUNode()
        {
            throw new NotImplementedException();
        }

        private void DeleteTemplates(bool onlyNonSilent)
        {
            if (TemplateInfos != null)
            {
                lock (SyncObject)
                {
                    List<TemplateInfo> newUList = new UList(TemplateInfos);
                    if (TemplateInfos.Count > 0)
                    {
                        foreach (TemplateInfo list in newUList)
                        {
                            if (onlyNonSilent && list.IsSilent) continue;
                            DeleteTemplate(list);
                        }
                    }
                    if (TemplateInfos != null)
                    {
                        TemplateInfos.Clear();
                        TemplateInfos = null;
                    }
                }
            }
        }


        public void DeleteTemplate(TemplateInfo info)
        {
            lock (SyncObject)
            {
                //writeToLog("DeleteTemplate: " + info);
                info.IsTraced = true;
                Node prevNode = info.GraphmasterNode.ToUUNode();
                if (prevNode == this)
                {
                    info.GraphmasterNode = null;
                }
                else if (prevNode != null)
                {
                    writeToLog("DeleteTemplate: otherNode " + prevNode + " " + info);
                    prevNode.DeleteTemplate(info);
                }
                else
                {
                    writeToLog("DeleteTemplate: nodeless " + info);
                }
                bool found = false;
                info.IsDisabled = true;
                if (TemplateInfosDisabled != null)
                {
                    if (TemplateInfosDisabled.Remove(info))
                    {
                        found = true;
                        if (TemplateInfosDisabled.Count == 0) TemplateInfosDisabled = null;
                    }
                }
                if (TemplateInfos != null)
                {
                    if (TemplateInfos.Remove(info))
                    {
                        found = true;
                        if (TemplateInfos.Count == 0) TemplateInfos = null;

                    }
                }
                if (!found)
                {
                    writeToLog("DeleteTemplate: Unknown " + info);
                }
                if (info.InGraph == Graph)
                {
                    Graph.RemoveTemplate(info);
                }
                info.RemoveIndexes();
                info.GraphmasterNode = prevNode.ToUNode();
            }
        }

        /// <summary>
        /// Adds a category to the node
        /// </summary>
        /// <param name="path">the path for the category</param>
        /// <param name="outTemplate">the outTemplate to find at the end of the path</param>
        /// <param name="filename">the file that was the source of this category</param>
        public Node addPathNodeChilds(Unifiable path, NodeAdder categoryInfo)
        {
            return addPathNodeChilds(0, path.ToArray(), categoryInfo);
        }

        private Node addPathNodeChilds(int from, Unifiable[] path, NodeAdder categoryInfo)
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
            string fs = ToKey(firstWord);
            Node childNode;
            lock (SyncObject)
            {
                if (TryGetValueChild(fs, out childNode))
                {
                    initial = childNode.addPathNodeChilds(from + 1, path, categoryInfo);
                    found = true;
                }


                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                if (needsKeySanityCheck) // see if we need ot check new indexing system!
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    if (!found)
                        foreach (var c in children0)
                        {
                            string ks = StaticAIMLUtils.ToUpper(c.Key);
                            if (ks == fs)
                            {
                                childNode = c.Value;
                                initial = childNode.addPathNodeChilds(from + 1, path, categoryInfo);
                                found = true;
                                break;
                            }
                            else
                            {
                                string kks = c.Value.word.ToUpper();
                                if (kks == fs || ks != kks)
                                {
                                    childNode = c.Value;
                                    initial = childNode.addPathNodeChilds(from + 1, path, categoryInfo);
                                    found = true;
                                    break;
                                }
                            }
                        }


                if (!found)
                {
                    childNode = new Node(this, firstWord);
                    //childNode.word = firstWord;
                    initial = childNode.addPathNodeChilds(from + 1, path, categoryInfo);
                    AddChildNode(fs, childNode, (CategoryInfo)categoryInfo);
                }
            }
            if (initial == null)
                throw new NullReferenceException("no child node: " + this + "CategoryInfo: " + categoryInfo);
            return initial;
        }
        static IComparer<string> KeySorter = new KeySorterImpl();
        private void AddChildNode(string fs, Node childNode, CategoryInfo categoryInfo)
        {
            if (IsLazy(fs, categoryInfo))
            {
                specialChildren = specialChildren ?? new SortedDictionary<string, Node>(KeySorter);
                specialChildren.Add(fs, childNode);
                return;
            }
            children0 = children0 ?? new Dictionary<string, Node>();
            children0.Add(fs, childNode);
        }

        static private bool IsLazy(string fs, CategoryInfo categoryInfo)
        {
            //if (fs == "*" || fs == "_" || fs.StartsWith("TAG-")) return false;
            //foreach (var fs1 in fs)
            {
                var fs1 = fs[0];
                if (fs1 == '<') return true;
                if (fs1 == '*') return true;
                if (fs1 == '|') return true;
                if (fs1 == '(') return true;
                if (fs1 == '#') return true;
                //if (char.IsLetterOrDigit(fs1)) continue; // return true;
            }
            return false;
        }

        private bool TryGetValueChild(string fs, out Node childNode)
        {
            if (children0 != null && children0.TryGetValue(fs, out childNode)) return true;
            if (specialChildren != null && specialChildren.TryGetValue(fs, out childNode)) return true;
            childNode = null;
            return false;
        }

        public static bool DontDoKeyIndexingHacks = true;
        private static string ToKey(Unifiable fs0W)
        {
            bool doEs = !DontDoKeyIndexingHacks;
            bool doSEs = !DontDoKeyIndexingHacks;
            string fs0 = fs0W.ToKey();
            
            if (false && NatLangDb.BeAUX.Contains(" " + fs0 + " ")) return "BeAux";

            if (!DontDoKeyIndexingHacks) if (fs0.StartsWith("FAV")) return "FAV";

            string fs00 = fs0;
            string fs = fs0;
            int fl = fs.Length;
            if (fl == 0)
            {
                return "";
            }
            if (DontDoKeyIndexingHacks) return fs0;

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


        private static char[] OtherwiseSplitInputInto = " \r\n\t".ToCharArray();

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
            lock (SyncObject)
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
                string[] splitPath = path.Split(OtherwiseSplitInputInto, StringSplitOptions.RemoveEmptyEntries);
                Node location = evaluateFirst(0, splitPath, query, request, matchstate, wildcard);
                return location;
            }
        }

        private Node evaluateNext(int at, string[] splitPath, SubQuery query, Request request, MatchState matchstate,
                                  StringAppendableUnifiableImpl wildcard)
        {
            // check for timeout           
            if (request.IsTimedOutOrOverBudget)
            {
                return null;
            }

            Node vv = evaluateFirst(at, splitPath, query, request, matchstate, wildcard);
            if (false && Unifiable.ToVMString(wildcard.Frozen(query)).Trim().Length > 0)
            {
                if (vv == null || vv.disabled || vv.NoEnabledTemplates) return null;
            }
            if (vv == null || vv.disabled || vv.NoEnabledTemplates) return null;
            return vv;
        }

        private Node evaluateFirst(int at, string[] splitPath, SubQuery query, Request request, MatchState matchstate,
                                  StringAppendableUnifiableImpl wildcard)
        {
            // check for timeout           
            if (request.IsTimedOutOrOverBudget)
            {
                request.TimeOutFromNow = TimeSpan.FromSeconds(3);
                string mesg = "FINISHED " + request.WhyComplete + " User: " +
                                                   request.Requester.UserID + " raw input: \"" +
                                                   request.rawInput + "\" in " + this;
                request.writeToLog(mesg);
                if (false)throw new ChatSignalOverBudget(request, mesg) { request = request };
                request.SuspendSearchLimits = true;
            }

            int pathLength = splitPath.Length - at;

            // so we still have time!
            //path = path.Trim();

            // check if this is the end of a branch in the GraphMaster 
            // return the cCategory for this node
            if (NoChildren())
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
            //Unifiable firstWordU = splitPath[at];
            string firstWordU = StaticAIMLUtils.ToUpper(firstWord);
            // and concatenate the rest of the input into a new path for child nodes
            //string newPath = path.Substring(firstWord.Length, path.Length - firstWord.Length);
    
            // second first option is to see if this node has a child denoted by the "_" 
            // wildcard. "_" comes first in precedence in the AIML alphabet
            //lock (SyncObject)
            foreach (var childNodeKV in ChildrenMatchingKey("_", children0, true))
            {
                Node childNode = childNodeKV.Value;
                Unifiable childNodeWord = childNode.word;
                if (!childNodeWord.IsHighPriority) continue;
                if (!childNodeWord.WillMatch(firstWord, query))
                {
                    continue;
                }
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
                            case MatchState.Pattern:
                                if (childNodeWord.StoreWildCard()) Insert(query.InputStar, newWildcard.Frozen(query));
                                // added due to this match being the end of the line
                                newWildcard.Length = 0; // Remove(0, newWildcard.Length);
                                break;
                            default:
                                var stars = query.GetMatchList(matchstate);
                                if (childNodeWord.StoreWildCard()) Insert(stars, newWildcard.Frozen(query));
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
                        newMatchstate = MatchState.Pattern;
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
                        if (childNodeWord.IsWildCard)
                        {
                            if (childNodeWord.StoreWildCard())
                            {
                                writeToLog("should store WC for " + childNodeWord + " from " + firstWord);
                                var stars = query.GetMatchList(matchstate);
                                Insert(stars, firstWord);
                            }
                        }
                        else if (childNodeWord.IsLazy)
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
                        if (childNodeWord.StoreWildCard()) Insert(stars, newWildcard.Frozen(query));
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
                //   lock (SyncObject)
                foreach (var childNodeKV in ChildrenMatchingKey("*", specialChildren, false))
                {
                    Node childNode = childNodeKV.Value;
                    Unifiable childNodeWord = childNode.word; //.Key;
                    if ( childNodeWord.IsHighPriority)
                    {
                        continue;
                    }
                    //if (!(childNodeWord is BestUnifiable) && !childNodeWord.IsCatchAll) continue;
                    if (!childNodeWord.WillMatch(firstWord, query))
                    {
                        if (childNodeWord.IsExactKey) continue;
                        //writeToLog("!WillMatch " + firstWord + " with " + childNodeWord);
                        continue;
                    }
                    // o.k. look for the path in the child node denoted by "*"
                    //Node childNode = childNodeKV.Value;
                    //specialChildren.Remove(childNodeKV.Key);
                    //specialChildren.Add(childNodeKV.Key, childNode);

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
                                case MatchState.Pattern:
                                    if (childNodeWord.StoreWildCard())
                                    {
                                        Insert(query.InputStar, newWildcard.Frozen(query));
                                        // added due to this match being the end of the line
                                        newWildcard.Length = 0; // Remove(0, newWildcard.Length);
                                    }
                                    break;
                                default:
                                    var stars = query.GetMatchList(matchstate);
                                    if (childNodeWord.StoreWildCard()) Insert(stars, newWildcard.Frozen(query));
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
                if (word.IsAnyText)
                {
                    storeWildCard(firstWord, wildcard);
                    Node result = evaluateNext(at + 1, splitPath, query, request, matchstate, wildcard);
                    return result;
                }

            // If we get here then we're at a dead end so return an empty string. Hopefully, if the
            // AIML files have been set up to include a "* <that> * <topic> *" catch-all this
            // state won't be reached. Remember to empty the surplus to requirements wildcard matches
            //wildcard = new StringBuilder();
            wildcard.Length = 0;
            return null; /// string.Empty;
        }

        private bool NoChildren()
        {
            if (children0 != null && children0.Count > 0) return false;
            if (specialChildren != null && specialChildren.Count > 0) return false;
            return true;
        }

        private IEnumerable<KeyValuePair<string, Node>> ChildrenMatchingKey(string match, IDictionary<string, Node> dictionary, bool canDoSingle)
        {            
            if (dictionary == null || dictionary.Count == 0) return OneKV.EMPTY;
            if (!canDoSingle)
            {
                return dictionary;
            }
            lock (dictionary)
            {
                Node v;
                if (dictionary.TryGetValue(match, out v))
                {
                    return new OneKV(match, v);
                }
            }
            return OneKV.EMPTY;
        }

        private bool NoEnabledTemplates
        {
            get
            {
                if (TemplateInfos == null) return true;
                lock (SyncObject)
                {
                    int tc = TemplateInfos.Count;

                    if (tc == 0) return true;
                    if (tc == 1) return TemplateInfos[0].IsDisabled;
                }
                return false;
            }
        }

        private static void Insert(List<string> unifiables, string s)
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
            if (TryGetValueChild(fs, out childNode))
            {
                if (query.CanUseNode(childNode.ToUNode()))
                {
                    //newPath = string.Join(" ", splitPath, rw, splitPath.Length - rw);
                    newAt = at + 1;
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                    if (needsKeySanityCheck && !childNode.word.CanUnify(firstWord, query))
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    {
                        throw new Exception("failed sanity check trying to unify " + firstWord + " " + childNode.word);
                    }
                    return childNode;
                }
            }
            foreach (var childNodeKV in ChildrenMatchingKey(fs, children0, true))
            {
                Unifiable childNodeWord = childNodeKV.Value.word;
                // if (childNodeWord.IsHighPriorityWildCard) continue;
                // if (childNodeWord.IsLongWildCard()) continue;
                //if (childNodeWord.IsWildCard) continue;
                childNode = childNodeKV.Value;
                if (!query.CanUseNode(childNode.ToUNode()))
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

        public bool IsSatisfied(SubQuery query)
        {
            return true;
        }

        public ParentChild ParentObject
        {
            get { return _parentObject; }
        }

        public Node[] AllDecendants
        {
            get
            {
                List<Node> TIs = new List<Node>();
                var lts = this.TemplateInfos;
                if (lts != null && lts.Count > 0) TIs.Add(this);
                if (children0 != null)
                    foreach (Node cn in children0.Values)
                    {
                        TIs.AddRange(cn.AllDecendants);
                    }
                if (specialChildren != null)
                    foreach (Node cn in specialChildren.Values)
                    {
                        TIs.AddRange(cn.AllDecendants);
                    }
                return TIs.ToArray();
            }
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
        internal void SetDisabled(TemplateInfo templateInfo, bool value)
        {
            lock (SyncObject)
            {
                if (value)
                {
                    bool found = false;
                    if (this.TemplateInfos != null)
                    {
                        found = this.TemplateInfos.Remove(templateInfo);
                        if (found && this.TemplateInfos.Count == 0) this.TemplateInfos = null;
                    }

                    this.TemplateInfosDisabled = this.TemplateInfosDisabled ?? new List<TemplateInfo>();
                    if (!found && this.TemplateInfosDisabled.Contains(templateInfo)) return;
                    this.TemplateInfosDisabled.Add(templateInfo);
                }
                else
                {
                    bool found = false;                        
                    if (this.TemplateInfosDisabled != null)
                    {
                        found = this.TemplateInfosDisabled.Remove(templateInfo);
                        if (found && this.TemplateInfosDisabled.Count == 0) this.TemplateInfosDisabled = null;
                    }

                    this.TemplateInfos = this.TemplateInfos ?? new List<TemplateInfo>();
                    if (!found && this.TemplateInfos.Contains(templateInfo)) return;
                    this.TemplateInfos.Add(templateInfo);
                }
            }
        }
    }

    [Serializable]
    internal class KeySorterImpl : IComparer<string>
    {
        #region Implementation of IComparer<string>

        /// <summary>
        /// Compares two objects and returns a value indicating whether one is less than, equal to, or greater than the other.
        /// </summary>
        /// <returns>
        /// Value 
        ///                     Condition 
        ///                     Less than zero
        ///                 <paramref name="x"/> is less than <paramref name="y"/>.
        ///                     Zero
        ///                 <paramref name="x"/> equals <paramref name="y"/>.
        ///                     Greater than zero
        ///                 <paramref name="x"/> is greater than <paramref name="y"/>.
        /// </returns>
        /// <param name="x">The first object to compare.
        ///                 </param><param name="y">The second object to compare.
        ///                 </param>
        public int Compare(string x, string y)
        {
            if (x == y) return 0;
            if (x == "*") return 1;
            if (y == "*") return -1;
            return string.Compare(x, y);
        }

        #endregion
    }


    internal class OneKV : IEnumerable<KeyValuePair<string, Node>>, IEnumerator<KeyValuePair<string, Node>>
    {
        readonly public static Dictionary<string, Node> EMPTY = new Dictionary<string, Node>();
        readonly KeyValuePair<string, Node> ValuePair;

        public OneKV(string k, Node v)
        {
            ValuePair = new KeyValuePair<string, Node>(k, v);
        }

        public IEnumerator<KeyValuePair<string, Node>> GetEnumerator()
        {
            return this;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public void Dispose()
        {
        }

        private bool IsStarted = false;
        public bool MoveNext()
        {
            if (IsStarted) return false;
            IsStarted = true;
            return true;
        }

        public void Reset()
        {
            IsStarted = false;
        }

        public KeyValuePair<string, Node> Current
        {
            get
            {
                if (IsStarted) return ValuePair;
                return default(KeyValuePair<string, Node>);
            }
        }

        object IEnumerator.Current
        {
            get { return Current; }
        }
    }
}
#endif