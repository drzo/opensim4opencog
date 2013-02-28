using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using System.Xml;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using AltAIMLbot;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;
using UPath = AltAIMLbot.Unifiable;
using UList = System.Collections.Generic.List<AltAIMLbot.Utils.TemplateInfo>;
//using List<Unifiable> = System.Collections.Generic.List<string>;
using DataUnifiable = System.String;
using DataUnifiableYYY = AltAIMLbot.Unifiable;

namespace AltAIMLbot.Utils
{
    /// <summary>
    /// A container class for holding wildcard matches encountered during an individual path's 
    /// interrogation of the graphmaster.
    /// </summary>
    [Serializable]
    public class SubQuery : CommonStaticUtils, ISettingsDictionary, IComparable<SubQuery>, UndoStackHolder
    {
        #region Attributes

        /// <summary>
        /// The path that this query relates to
        /// </summary>
        //public string FullPath;

        /// <summary>
        /// The template found from searching the graphmaster brain with the path 
        /// </summary>
        public string Template = string.Empty;

        /// <summary>
        /// The path in the graphmaster brain to find the Template 
        /// </summary>
        public string TemplatePath = string.Empty;

        /// <summary>
        /// If the raw input matches a wildcard then this attribute will contain the block of 
        /// text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<string> InputStars
        {
            get { return Stars["pattern"]; }
        }

        /// <summary>
        /// If the "that" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<string> ThatStars
        {
            get { return Stars["that"]; }
        }

        public Dictionary<string, List<string>> Stars = new Dictionary<string, List<string>>();

        /// <summary>
        /// If the "topic" part of the normalized path contains a wildcard then this attribute 
        /// will contain the block of text that the user has inputted that is matched by the wildcard.
        /// </summary>
        public List<string> TopicStar
        {
            get { return Stars["topic"]; }
        }

        public List<string> StateStar
        {
            get { return Stars["state"]; }
        }

        //public List<string> StateStar2 = new List<string>();

        #endregion

        /*
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="fullPath">The path that this query relates to</param>
        public SubQuery(string fullPath, Request request)
        {
            this.FullPath = fullPath;
            this.Request = request;
        }

        public AltBot TargetBot
        {
            get { return Request.TargetBot; }
        }

        public SettingsDictionary TargetSettings
        {
            get { return Request.TargetSettings; }
        }

        public Request Request;
        */

        public List<string> GetStars(string s)
        {
            s = s.ToLower();
            if (s == "input") s = "pattern";
            List<string> dict;
            lock (Stars)
            {
                if (!Stars.TryGetValue(s, out dict))
                {
                    dict = Stars[s] = new List<string>();
                }
            }
            return dict;
        }

        /*
        public string ReduceStarAttribute<T>(IConvertible arg)
        {
            throw new NotImplementedException();
        }

        public bool UseDictionaryForSet(ISettingsDictionary dictionary)
        {
            throw new NotImplementedException();
        }
        */

        public
            //static 
            object TagHandlerLock = new object();

        public
            //static 
            Dictionary<string, AIMLTagHandler> TagHandlers;

        private AltBot ov_TargetBot;
        public TemplateInfo CurrentTemplate;
        public AIMLTagHandler LastTagHandlerU;
        public Node Pattern;
        public string prefix;
        public Request Request;
        public Result Result;
        public GraphQuery TopLevel;
        public AIMLTagHandler CurrentTagHandlerU;
        public XmlNode CurrentNode;

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof (SubQuery)) return false;
            return Equals((SubQuery) obj);
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="fullPath">The path that this query relates to</param>
        public SubQuery(Unifiable fullPath, Result res, Request request)
        {
            foreach (var node in GraphMaster.StarTypes)
            {
                Stars[node] = new List<string>();
            }
            Result = res;
            res._CurrentQuery = this;
            request._CurrentQuery = this;
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

        public AltBot TargetBot
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

        public List<string> Flags = new List<string>();

        /// <summary>
        /// The path that this query relates to
        /// </summary>
        public string FullPath;

        public List<string> GuardStar
        {
            get { return GetStars("guard"); }
        }

        /// <summary>
        /// The template found from searching the graphmaster brain with the path 
        /// </summary>*/
        public UList Templates = new UList();

        public Result ParentResult
        {
            get { return Result; }
        }

        internal bool useParentSF = false;
        private int _hasFailed = 0;

        public int HasFailed
        {
            get
            {
                if (!ChatOptions.AIML_MAY_USE_FAILURE) return 0;
                return _hasFailed + (useParentSF ? ParentResult.HasFailed : 0);
            }
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
                if (_hasFailed == value) return;
                _hasFailed = value;
            }
        }

        private int _hasSuceeded = 0;

        public int HasSuceeded
        {
            get
            {
                if (!ChatOptions.AIML_MAY_USE_FAILURE) return 1;
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

        public bool IsSourceRequest(AIMLTagHandler node, out string src)
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

        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            //get 
            {
                return Request.TargetSettings.SettingNames(requester, depth);
            }
        }

        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            return SettingsDictionaryReal.removeSettingWithUndoCommit(this, TargetSettings, name);
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, object value)
        {
            return SettingsDictionaryReal.addSettingWithUndoCommit(this, TargetSettings, TargetSettings.updateSetting,
                                                                   name, value);
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

        public User Requester
        {
            get { return CurrentUser; }
        }

        public User Responder
        {
            get { return Request.Responder; }
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

        public DataUnifiable grabSetting(string name)
        {
            string realName;
            ISettingsDictionary dict = Request.TargetSettings;
            bool succeed;
            Unifiable v;
            if (!UseLuceneForGet)
                v = SettingsDictionaryReal.grabSettingDefaultDict(dict, name, out realName);
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
        public bool addSetting(string name, object value)
        {
            ISettingsDictionary dict = Request.TargetSettings;
            if (!UseLuceneForSet)
            {
                return SettingsDictionaryReal.addSettingWithUndoCommit(this, dict, dict.addSetting, name, value);
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
            string nodePattern = ((object) Pattern ?? "-no-pattern-").ToString().Trim();
            string s = SafeFormat("\nINPUT='{6}'\nPATTERN='{0}' InThToGu={1}:{2}:{3}:{4} Tc={5} Graph={7}\n",
                                  nodePattern,
                                  InputStars.Count, ThatStars.Count, TopicStar.Count,
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
            sq.InputStars.AddRange(InputStars);
            sq.ThatStars.AddRange(ThatStars);
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
                    return CurrentUser.getResponseSentence;
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

        public List<string> GetMatchList(MatchState matchstate)
        {
            switch (matchstate)
            {
                case MatchState.Pattern:
                    return InputStars;
                    break;
                case MatchState.That:
                    return ThatStars;
                    break;
                case MatchState.Topic:
                    return TopicStar;
                    break;
                case MatchState.Flag:
                    return Flags;
                    break;
                case MatchState.State:
                    return StateStar;
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
                AIMLTagHandler handlerU;
                if (TagHandlers == null)
                {
                    TagHandlers = new Dictionary<string, AIMLTagHandler>();
                }
                else if (TagHandlers.TryGetValue(str, out handlerU))
                {
                    return handlerU;
                }
                SubQuery subquery = this;

                User user = subquery.CurrentUser;
                Request request = subquery.Request;
                AltBot bot = subquery.TargetBot;
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
                handlerU = bot.TagHandling.GetTagHandler(user, subquery, request, result, node, null);
                TagHandlers[str] = handlerU;
                return handlerU;
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
            return Request.GetDictionary(named, (ISettingsDictionary) this);
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
                StaticAIMLUtils.writeDebugLine("Commiting Sidefffect " + effectName);
                action();
            }
            else
            {
                StaticAIMLUtils.writeDebugLine("Skipping Sidefffect " + effectName);
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
                return StaticAIMLUtils.ReferenceCompare(this, other);
            }
            int compare = Pattern.CompareTo(other.Pattern);
            if (compare != 0) return compare;
            compare = StaticAIMLUtils.CollectionCompare(Templates, other.Templates, TemplateInfoImpl.CompareTemplates);
            if (compare != 0) return compare;
            return StaticAIMLUtils.ReferenceCompare(this, other);
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
                && StaticAIMLUtils.CollectionEquals(other.GuardStar, GuardStar)
                && StaticAIMLUtils.CollectionEquals(other.InputStars, InputStars)
                && StaticAIMLUtils.CollectionEquals(other.ThatStars, ThatStars)
                && StaticAIMLUtils.CollectionEquals(other.TopicStar, TopicStar)
                && StaticAIMLUtils.SetsEquals(other.Flags, Flags)
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
                && StaticAIMLUtils.SetsEquals(other.Templates, Templates)
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
                int result = 0; // (ov_TargetBot != null ? ov_TargetBot.GetHashCode() : 0);
                ////result = (result * 397) ^ (CurrentTemplate != null ? CurrentTemplate.GetHashCode() : 0);
                ////result = (result * 397) ^ (LastTagHandler != null ? LastTagHandler.GetHashCode() : 0);
                result = (result*397) ^ (Pattern != null ? Pattern.GetHashCode() : 0);
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

        public bool UseDictionaryForSet(ISettingsDictionary dictionary)
        {
            if (dictionary == null) return false;
            if (!CanSetDict) return false;
            return true;
        }
    }
}