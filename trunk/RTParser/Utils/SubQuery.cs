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
    public class SubQuery : StaticAIMLUtils, ISettingsDictionary, IComparable<SubQuery>, RequestOrQuery, UndoStackHolder,
        ConversationScopeHolder, SituationInConversation
    {
        public
            //static 
            object TagHandlerLock = new object();
        public
            //static 
            Dictionary<string, AIMLTagHandler> TagHandlers;
        private RTPBot ov_TargetBot;
        public TemplateInfo CurrentTemplate;
        public AIMLTagHandler LastTagHandler;
        public Node Pattern;
        public string prefix;
        public Request Request;
        public Result Result;
        public GraphQuery TopLevel;
        public AIMLTagHandler CurrentTagHandler;
        public XmlNode CurrentNode;

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof(SubQuery)) return false;
            return Equals((SubQuery)obj);
        }
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="fullPath">The path that this query relates to</param>
        public SubQuery(Unifiable fullPath, Result res, Request request)
        {
            Result = res;
            Request = request;
            Graph = request.Graph;
            TargetSettings = request.TargetSettings;
            this.FullPath = fullPath;
            UndoStackValue = new UndoStack(this);
        }

        public ISettingsDictionary TargetSettings
        {
            get { return Request.TargetSettings; }
            set { Request.TargetSettings = value; }
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

        public Result ParentResult
        {
            get { return Result; }
        }
        internal bool useParentSF = false;
        private int _hasFailed = 0;
        public int HasFailed
        {
            get { return _hasFailed + (useParentSF ? ParentResult.HasFailed : 0); }
            set
            {
                if (_hasFailed < 1)
                {
                    if (useParentSF)
                    {
                        if (value == 0)
                        {
                            ParentResult.HasFailed -= 1;
                        }
                        else
                        {
                            ParentResult.HasFailed += 1;
                        }
                    }
                }
                _hasFailed = value;
            }
        }

        private int _hasSuceeded = 0;
        public int HasSuceeded
        {
            get
            {
                int ret = _hasSuceeded + (useParentSF ? ParentResult.HasSuceeded : 0);
                if (ret < 0) throw new InvalidOperationException();
                return ret;
            }
            set
            {
                if (_hasSuceeded < 1)
                {
                    if (useParentSF)
                    {
                        if (value == 0)
                        {
                            ParentResult.HasSuceeded -= 1;
                        }
                        else
                        {
                            ParentResult.HasSuceeded += 1;
                        }
                    }
                }
                _hasSuceeded = value;
            }
        }

        public int GetDictValue;
        public int SetDictValue;

        public bool IsSourceRequest(AIMLTagHandler node, out  string src)
        {
            src = null;
            return false;
        }

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

        /// <summary>
        /// All conditions must be right
        /// </summary>
        public bool CanSetDict
        {
            get
            {
                var Request = this.Request;
                if (Request == null) return false;
                if (Request.MaxSetVars == 0) return false;
                return Request.depth <= Request.UseDictForSetMaxDepth;
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
            return SettingsDictionary.removeSettingWithUndoCommit(this, TargetSettings, name);
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, Unifiable value)
        {
            return SettingsDictionary.addSettingWithUndoCommit(this, TargetSettings, TargetSettings.updateSetting, name, value);
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
            return TargetSettings.containsLocalCalled(name);
        }

        public string NameSpace
        {
            get { return TargetSettings.NameSpace; }
        }

        public UserConversationScope Requester
        {
            get { return CurrentUser; }
        }

        public User Responder
        {
            get { return Request.Responder; }
        }

        public Unifiable That
        {
            get { return Request.That; }
        }

        public Unifiable rawInput
        {
            get { return Request.rawInput; }
        }

        public ISettingsDictionary RequesterPredicates
        {
            get { return Request.RequesterPredicates; }
        }
        public ISettingsDictionary ResponderPredicates
        {
            get { return Request.ResponderPredicates; }
        }

        public SituationInConversation ContextScope { get; set; }

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
            ISettingsDictionary dict = Request.TargetSettings;
            if (!UseLuceneForSet)
            {
                return SettingsDictionary.addSettingWithUndoCommit(this, dict, dict.addSetting, name, value);
            }
            else
            {
                string realName;
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
            string nodePattern = ((object)Pattern ?? "-no-pattern-").ToString().Trim();
            string s = SafeFormat("\nINPUT='{6}'\nPATTERN='{0}' InThToGu={1}:{2}:{3}:{4} Tc={5} Graph={7}\n",
                         nodePattern,
                                     InputStar.Count, ThatStar.Count, TopicStar.Count,
                                     GuardStar.Count, Templates == null ? 0 : Templates.Count,
                         FullPath, Graph);
            if (Templates != null)
                foreach (TemplateInfo path in Templates)
                {
                    s += "\r\n t: " + path.ToFileString(Requester.PrintOptions);
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

        public /*static*/ void PurgeTagHandlers()
        {
            lock (TagHandlerLock)
            {
                if (TagHandlers != null)
                {
                    foreach (KeyValuePair<string, AIMLTagHandler> aimlTagHandler in TagHandlers)
                    {
                        aimlTagHandler.Value.Dispose();
                    }
                    TagHandlers.Clear();
                }
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
                    user = (user ?? result.Requester).Value;
                    bot = request.TargetBot;
                }
                handler = bot.TagHandling.GetTagHandler(user, subquery, request, result, node, null);
                TagHandlers[str] = handler;
                return handler;
            }
        }

        public void UndoAll()
        {
            UndoStack.FindUndoAll(this, true);
        }

        public void AddLocalUndo(string named, ThreadStart undo)
        {
            lock (this)
            {
                UndoStack.GetStackFor(this).AddUndo(named, undo);
            }
        }

        public void AddUndo(string named, Action undo)
        {
            lock (this)
            {
                Request.AddUndo(named, undo);
            }
        }

        public UndoStack GetFreshUndoStack()
        {
            lock (this)
            {
                UndoStack.FindUndoAll(this, false);
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
            return Request.GetDictionary(named, (ISettingsDictionary)this);
        }

        public T ReduceStarAttribute<T>(IConvertible value) where T : IConvertible
        {
            bool found;
            return StaticAIMLUtils.ReduceStar<T>(value, this, this, out found);
        }

        /// <summary>
        /// This a really a Request/Response AddSideEffect.. 
        ///  however putting it here on SubQuery is lasss ambiguous
        /// </summary>
        /// <param name="effectName"></param>
        /// <param name="action"></param>
        public void AddSideEffect(string effectName, ThreadStart action)
        {
            Request.AddSideEffect(effectName, () => DoSideEffect(effectName, action));
        }


        private void DoSideEffect(string effectName, ThreadStart action)
        {
            if (ShouldCommit)
            {
                writeDebugLine("Commiting Sidefffect " + effectName);
                action();
            }
            else
            {
                writeDebugLine("Skipping Sidefffect " + effectName);
            }
        }

        protected bool ShouldCommit
        {
            get
            {
                if (HasSuceeded > 0) return true;
                if (HasFailed == 0)
                {
                    return true;
                }
                return false;
            }
        }


        public void LocalSideEffect(string effectName, ThreadStart enter, ThreadStart exit)
        {
            enter();
            AddLocalUndo("LocalSideEffect exit: " + effectName, exit);
        }


        public int CompareTo(SubQuery other)
        {
            if (Pattern == null)
            {
                return ReferenceCompare(this, other);
            }
            int compare = Pattern.CompareTo(other.Pattern);
            if (compare != 0) return compare;
            compare = CollectionCompare(Templates, other.Templates, TemplateInfoImpl.CompareTemplates);
            if (compare != 0) return compare;
            return ReferenceCompare(this, other);
        }

        public bool Equals(SubQuery other)
        {
            if (ReferenceEquals(this, other)) return true;
            if (ReferenceEquals(null, other)) return false;
            if (!EqualsMeaning(other)) return false;
            bool same = //Equals(other.ov_TargetBot, ov_TargetBot)
                //  && Equals(other.CurrentTemplate, CurrentTemplate)
                //  && Equals(other.LastTagHandler, LastTagHandler)
                //Equals(other.Pattern, Pattern)
                //  && Equals(other.prefix, prefix)
                Equals(other.Request, Request)
                && Equals(other.Result, Result)
                //  && Equals(other.TopLevel, TopLevel)                        
                && CollectionEquals(other.GuardStar, GuardStar)
                && CollectionEquals(other.InputStar, InputStar)
                && CollectionEquals(other.ThatStar, ThatStar)
                && CollectionEquals(other.TopicStar, TopicStar)
                && SetsEquals(other.Flags, Flags)
                //  && other.HasFailed == HasFailed
                //  && other.HasSuceeded == HasSuceeded
                //  && other.GetDictValue == GetDictValue
                //  && other.SetDictValue == SetDictValue                
                //  && other.IsTraced.Equals(IsTraced)
                //  && SetsEquals(other.Templates, Templates)
                //  && Equals(other.Graph, Graph)
                ;
            if (!same)
            {
                return true;
                return false;
            }
            // ReSharper disable ConditionIsAlwaysTrueOrFalse
            return same;
            // ReSharper restore ConditionIsAlwaysTrueOrFalse
        }

        public bool EqualsMeaning(SubQuery other)
        {
            if (ReferenceEquals(this, other)) return true;
            if (Pattern == null) return false;
            //if (!Equals(other.FullPath.ToLower(), FullPath.ToLower())) return false;
            bool same = //Equals(other.ov_TargetBot, ov_TargetBot)
                //  && Equals(other.CurrentTemplate, CurrentTemplate)
                //  && Equals(other.LastTagHandler, LastTagHandler)
                Equals(other.Pattern, Pattern)
                //  && Equals(other.prefix, prefix)
                //  && Equals(other.Request, Request)
                //  && Equals(other.Result, Result)
                //  && Equals(other.TopLevel, TopLevel)                        
                //  && CollectionEquals(other.GuardStar, GuardStar)
                //  && CollectionEquals(other.InputStar, InputStar)
                //  && CollectionEquals(other.ThatStar, ThatStar)
                //  && CollectionEquals(other.TopicStar, TopicStar)
                //  && SetsEquals(other.Flags, Flags)
                //  && other.HasFailed == HasFailed
                //  && other.HasSuceeded == HasSuceeded
                //  && other.GetDictValue == GetDictValue
                //  && other.SetDictValue == SetDictValue                
                //  && other.IsTraced.Equals(IsTraced)
                && SetsEquals(other.Templates, Templates)
                && Equals(other.Graph, Graph)
                ;
            if (!same) return false;
            // ReSharper disable ConditionIsAlwaysTrueOrFalse
            return same;
            // ReSharper restore ConditionIsAlwaysTrueOrFalse
        }

        public override int GetHashCode()
        {
            unchecked
            {
                int result = 0;// (ov_TargetBot != null ? ov_TargetBot.GetHashCode() : 0);
                ////result = (result * 397) ^ (CurrentTemplate != null ? CurrentTemplate.GetHashCode() : 0);
                ////result = (result * 397) ^ (LastTagHandler != null ? LastTagHandler.GetHashCode() : 0);
                result = (result * 397) ^ (Pattern != null ? Pattern.GetHashCode() : 0);
                ////result = (result * 397) ^ (prefix != null ? prefix.GetHashCode() : 0);
                ////result = (result * 397) ^ (Request != null ? Request.GetHashCode() : 0);
                ////result = (result * 397) ^ (Result != null ? Result.GetHashCode() : 0);
                ////result = (result * 397) ^ (TopLevel != null ? TopLevel.GetHashCode() : 0);
                ////result = (result * 397) ^ (Flags != null ? Flags.GetHashCode() : 0);
                //  result = (result * 397) ^ (FullPath != null ? FullPath.GetHashCode() : 0);
                ////result = (result * 397) ^ (GuardStar != null ? GuardStar.GetHashCode() : 0);
                ////result = (result * 397) ^ (InputStar != null ? InputStar.GetHashCode() : 0);
                ////result = (result * 397) ^ (Templates != null ? Templates.GetHashCode() : 0);
                ////result = (result * 397) ^ (ThatStar != null ? ThatStar.GetHashCode() : 0);
                ////result = (result * 397) ^ (TopicStar != null ? TopicStar.GetHashCode() : 0);
                //result = (result*397) ^ HasFailed;
                //result = (result*397) ^ HasSuceeded;
                //result = (result*397) ^ GetDictValue;
                //result = (result*397) ^ SetDictValue;
                // result = (result * 397) ^ (Graph != null ? Graph.GetHashCode() : 0);
                //result = (result*397) ^ IsTraced.GetHashCode();
                return result;
            }
        }

        public UndoStack UndoStackValue { get; set; }

        public void EnterContent()
        {
            Request.EnterContext();
        }

        public bool UseDictionaryForSet(ISettingsDictionary dictionary)
        {
            if (dictionary == null) return false;
            if (!CanSetDict) return false;
            return true;
        }

        public void ExitContext()
        {
            Request.ExitContext();
        }

        #region SituationInConversation Members

        public Utterance TheUtterence
        {
            get { throw new NotImplementedException(); }
        }

        public IEnumerable<UserConversationScope> Receivers
        {
            get { throw new NotImplementedException(); }
        }

        public SubQuery CurrentQuery
        {
            get
            {
                throw new NotImplementedException();
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        #endregion
    }

    public interface RequestOrQuery : ConversationScopeHolder
    {

        /// <summary>
        /// The get/set user dictionary
        /// </summary>
        ISettingsDictionary RequesterPredicates { get; }
        /// <summary>
        /// The get/set bot dictionary (or user)
        /// </summary>
        ISettingsDictionary ResponderPredicates { get; }
        /// <summary>
        /// If loading/saing settings from this request this may be eitehr the requestor/responders Dictipoanry
        /// </summary>
        ISettingsDictionary TargetSettings { get; set; }
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