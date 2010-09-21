using System;
using System.Collections.Generic;
using System.Threading;
using System.Xml;
using RTParser.Database;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;


namespace RTParser.Utils
{
    /// <summary>
    /// A container class for holding wildcard matches encountered during an individual path's 
    /// interrogation of the graphmaster.
    /// </summary>
    [Serializable]
    public class SubQuery : ISettingsDictionary
    {
        public static object TagHandlerLock = new object();
        public static Dictionary<string, AIMLTagHandler> TagHandlers;
        private RTPBot ov_TargetBot;
        public TemplateInfo CurrentTemplate;
        public AIMLTagHandler LastTagHandler;
        public Node Pattern;
        public string prefix;
        public Request Request;
        public Result Result;
        public GraphQuery TopLevel;

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="fullPath">The path that this query relates to</param>
        public SubQuery(Unifiable fullPath, Result res, Request request)
        {
            Result = res;
            Request = request;
            Graph = request.Graph;
            this.FullPath = fullPath;
        }

        public ISettingsDictionary TargetSettings
        {
            get { return Request.TargetSettings; }
        }

        public User CurrentUser
        {
            get { return Request.Requester; }
        }

        public RTPBot TargetBot
        {
            get { return ov_TargetBot ?? Request.TargetBot; }
            set { ov_TargetBot = value; }
        }

        #region Attributes

        #region Overrides of QuerySettings

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public GraphMaster Graph { get; set; }

        #endregion

        public List<Unifiable> Flags = new List<Unifiable>();

        /// <summary>
        /// The path that this query relates to
        /// </summary>
        public string FullPath;

        public List<Unifiable> GuardStar = new List<Unifiable>();

        /// <summary>
        /// If the raw input matches a wildcard then this attribute will contain the block of 
        /// text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<Unifiable> InputStar = new List<Unifiable>();

        /// <summary>
        /// The template found from searching the graphmaster brain with the path 
        /// </summary>
        public UList Templates = new UList();

        /// <summary>
        /// If the "that" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<Unifiable> ThatStar = new List<Unifiable>();

        /// <summary>
        /// If the "topic" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<Unifiable> TopicStar = new List<Unifiable>();

        public int HasFailed = 0;
        public int HasSuceeded = 0;
        public int GetDictValue;
        public int SetDictValue;


        /// <summary>
        /// All conditions must be right
        /// </summary>
        public bool UseLuceneForSet
        {
            get
            {
                return (NamedValuesFromSettings.UseLuceneForSet && Request != null &&
                       Request.depth < Request.UseLuceneForSetMaxDepth);
            }
        }
        /// <summary>
        /// All conditions must be right
        /// </summary>
        public bool UseLuceneForGet
        {
            get
            {
                return (NamedValuesFromSettings.UseLuceneForGet && Request != null &&
                       Request.depth < Request.UseLuceneForGetMaxDepth);
            }
        }

        #endregion

        #region ISettingsDictionary Members

        public bool IsTraced { get; set; }

        public IEnumerable<string> SettingNames(int depth)
        {
            //get 
            {
                return Request.TargetSettings.SettingNames(depth);
            }
        }


        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            return RequesterPredicates.removeSetting(name);
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, Unifiable value)
        {
            return RequesterPredicates.updateSetting(name, value);
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsSettingCalled(string name)
        {
            Unifiable value = grabSetting(name);
            return !Unifiable.IsNullOrEmpty(value);
        }

        public bool containsLocalCalled(string name)
        {
            return RequesterPredicates.containsLocalCalled(name);
        }

        public string NameSpace
        {
            get { return RequesterPredicates.NameSpace; }
        }

        public ISettingsDictionary RequesterPredicates
        {
            get { return Request.RequesterPredicates; }
        }
        public ISettingsDictionary ResponderPredicates
        {
            get { return Request.ResponderPredicates; }
        }

        public Unifiable grabSetting(string name)
        {
            string realName;
            ISettingsDictionary dict = Request.TargetSettings;
            bool succeed;
            Unifiable v;
            if (!UseLuceneForGet)
                v = SettingsDictionary.grabSettingDefaultDict(dict, name, out realName);
            else
            {
                v = NamedValuesFromSettings.GetSettingForType(dict.NameSpace, this, dict, name, out realName, name, null,
                                                              out succeed, null);
            }
            return v;
        }


        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        public bool addSetting(string name, Unifiable value)
        {
            if (!UseLuceneForSet)
            {
                return Request.addSetting(name, value);
            }
            else
            {
                string realName;
                ISettingsDictionary dict = Request.TargetSettings;
                bool succeed;

                var prev = NamedValuesFromSettings.UseLuceneForGet;
                NamedValuesFromSettings.UseLuceneForGet = true;
                try
                {

                    NamedValuesFromSettings.SetSettingForType(dict.NameSpace, this, dict, name, null, value, null, null);
                }
                finally
                {
                    NamedValuesFromSettings.UseLuceneForGet = prev;
                }
                return true;
            }
        }

        #endregion

        public override string ToString()
        {
            string nodePattern =((object)Pattern ?? "-no-pattern-").ToString().Trim();
            string s = string.Format("\nINPUT='{6}'\nPATTERN='{0}' InThToGu={1}:{2}:{3}:{4} Tc={5} Graph={7}\n",
                         nodePattern,
                                     InputStar.Count, ThatStar.Count, TopicStar.Count,
                                     GuardStar.Count, Templates == null ? 0 : Templates.Count,
                         FullPath, Graph);
            if (Templates != null)
                foreach (TemplateInfo path in Templates)
                {
                    s += "\r\n t: " + path.ToString();
                }
            s += " \r\n";
            Result r = Result;
            if (r == null) s += " Result: -no-result- ";
            else s += " result.Count=" + r.OutputSentenceCount;
            Request rq = Request;
            int depth = 0;
            {
                while (rq != null)
                {
                    depth++;
                    rq = rq.ParentRequest;
                }
            }
            s += " depth: " + depth;
            s += " " + Graph;
            return s;
        }

        public static void PurgeTagHandlers()
        {
            lock (TagHandlerLock)
            {
                if (TagHandlers != null) TagHandlers.Clear();
            }
        }

        public double GetSucceedReward(string type)
        {
            return 1.0;
        }

        public SubQuery CopyOf()
        {
            SubQuery sq = new SubQuery(FullPath, Result, Request);
            sq.InputStar.AddRange(InputStar);
            sq.ThatStar.AddRange(ThatStar);
            sq.TopicStar.AddRange(TopicStar);
            sq.GuardStar.AddRange(GuardStar);
            sq.Flags.AddRange(Flags);
            sq.Graph = Graph;
            return sq;
        }

        public Func<int, int, User, Unifiable> GetMatcher(string listName)
        {
            switch (listName)
            {
                case "input":
                    return CurrentUser.getInputSentence;
                    break;
                case "request":
                    return CurrentUser.getRequestSentence;
                    break;
                case "that":
                    return CurrentUser.getThat;
                case "response":
                    return CurrentUser.getResultSentence;
                    break;
                case "topic":
                    return (a, b, c) => CurrentUser.Topic;
                    break;
                case "flag":
                    return (a, b, c) => this.Flags[a];
                    break;
                case "guard":
                    {
                        return (a, b, c) => null;
                    }
                default:
                    throw new ArgumentOutOfRangeException("listName");
            }
            return (a, b, c) => "*";
        }

        public List<Unifiable> GetMatchList(MatchState matchstate)
        {
            switch (matchstate)
            {
                case MatchState.UserInput:
                    return InputStar;
                    break;
                case MatchState.That:
                    return ThatStar;
                    break;
                case MatchState.Topic:
                    return TopicStar;
                    break;
                case MatchState.Flag:
                    return Flags;
                    break;
                default:
                    throw new ArgumentOutOfRangeException("matchstate");
            }
        }

        public AIMLTagHandler GetTagHandler(XmlNode node)
        {
            lock (TagHandlerLock)
            {
                string str = node.OuterXml;
                str = TextPatternUtils.CleanWhitepaces(str).ToLower();
                AIMLTagHandler handler;
                if (TagHandlers == null)
                {
                    TagHandlers = new Dictionary<string, AIMLTagHandler>();
                }
                else if (TagHandlers.TryGetValue(str, out handler))
                {
                    return handler;
                }
                SubQuery subquery = this;

                User user = subquery.CurrentUser;
                Request request = subquery.Request;
                RTPBot bot = subquery.TargetBot;
                Result result = subquery.Result;

                // if (node.ChildNodes.Count == 0) ;         
                {
                    user = CurrentUser;
                    result = result ?? subquery.Result;
                    request = request ?? subquery.Request ?? result.request;
                    result = result ?? request.CurrentResult;
                    user = user ?? result.Requester;
                    bot = request.TargetBot;
                }
                handler = bot.GetTagHandler(user, subquery, request, result, node, null);
                TagHandlers[str] = handler;
                return handler;
            }
        }

        public void UndoAll()
        {
            UndoStack.FindUndoAll(this);
        }

        public void AddUndo(ThreadStart undo)
        {
            lock (this)
            {
                UndoStack.GetStackFor(this).AddUndo(undo);
            }
        }

        public UndoStack GetFreshUndoStack()
        {
            lock (this)
            {
                UndoStack.FindUndoAll(this);
                return UndoStack.GetStackFor(this);
            }
        }

        public bool CanUseNode(Node node)
        {
            return TopLevel.CanUseNode(node);
        }

        public ISettingsDictionary GetDictionary(string named)
        {
            SubQuery query = this;
            if (named == null) return query;
            string lnamed = named.ToLower();
            if (lnamed == "query") return query;
            return Request.GetDictionary(named,(ISettingsDictionary)this);
        }

        public T ReduceStarAttribute<T>(IConvertible value) where T : IConvertible
        {
            return StaticAIMLUtils.ReduceStar<T>(value, this, this);
        }

        /// <summary>
        /// This a really a Request/Response AddSideEffect.. 
        ///  however putting it here on SubQuery is lasss ambiguous
        /// </summary>
        /// <param name="effectName"></param>
        /// <param name="action"></param>
        public void AddSideEffect(string effectName, ThreadStart action)
        {
            Request.AddSideEffect(effectName, action);
        }

        public void LocalSideEffect(string effectName, ThreadStart enter, ThreadStart exit)
        {
            enter();
            AddUndo(exit);
        }
    }

#if _FALSE_
    public class UList : IEnumerable<TemplateInfo>
    {
        public List<TemplateInfo> root = new List<TemplateInfo>();
        public int Count
        {
            get { lock (root) return root.Count; }
        }

        public void Insert(int i, TemplateInfo info)
        {
            lock (root) root.Insert(i, info);

        }

        public void ForEach(Action<TemplateInfo> action)
        {
            lock (root) root.ForEach(action);
        }

        public void Remove(TemplateInfo info)
        {
            lock (root) root.Remove(info);
        }

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        IEnumerator<TemplateInfo> IEnumerable<TemplateInfo>.GetEnumerator()
        {
            return GetRootEnumerator();
        }

        public IEnumerator GetEnumerator()
        {
            return GetRootEnumerator();
        }

        private IEnumerator<TemplateInfo> GetRootEnumerator()
        {
            var next = new List<TemplateInfo>();
            lock (root)
            {
                next.AddRange(root);
            }
            return next.GetEnumerator();
        }

        public void AddRange(UList infos)
        {
            lock (root)
            {
                lock (infos.root)
                {
                    root.AddRange(infos.root);                    
                }
            }
        }
    }
#endif
}