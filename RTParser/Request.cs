using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;
using AIMLbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser
{

    public interface Request : QuerySettingsSettable, QuerySettingsReadOnly
    {
        int depth { get; set; }
        Unifiable Topic { get; set; }
        Request ParentRequest { get; set; }
        GraphMaster Graph { get; set; }
        bool IsTraced { get; set; }
        TimeSpan Durration { get; set; }
        //int DebugLevel { get; set; }
        //List<SubQuery> SubQueries { get; set; }
        /// <summary>
        /// The user that made this request
        /// </summary>
        User Requester { get; set; }
        /// <summary>
        /// The user respoinding to the request
        /// </summary>
        User Responder { get; set; }
        /// <summary>
        /// The get/set user dictionary
        /// </summary>
        ISettingsDictionary RequesterPredicates { get; }
        /// <summary>
        /// The get/set bot dictionary (or user)
        /// </summary>
        ISettingsDictionary ResponderPredicates { get; set; }
        /// <summary>
        /// If loading/saing settings from this request this may be eitehr the requestor/responders Dictipoanry
        /// </summary>
        ISettingsDictionary TargetSettings { get; set; }

        IEnumerable<Unifiable> ResponderOutputs { get; }
        Result CurrentResult { get; set; }
        Unifiable Flags { get; }
        IList<Unifiable> Topics { get; }
        Proof Proof { get; set; }
        GraphQuery TopLevel { get; set; }
        SubQuery CurrentQuery { get; set; }
        RTPBot TargetBot { get; set; }
        Unifiable rawInput { get; }
        ParsedSentences ChatInput { get; }
        IList<Result> UsedResults { get; set; }
        IList<TemplateInfo> RequestTemplates { get; }
        int MaxInputs { get; set; }

        DateTime StartedOn { get; set; }
        DateTime TimesOutAt { get; set; }
        TimeSpan TimeOut { get; }
        TimeSpan TimeOutFromNow { set; }

        bool GraphsAcceptingUserInput { get; set; }
        LoaderOptions LoadOptions { get; set; }
        AIMLLoader Loader { get; }
        string Filename { get; set; }
        string LoadingFrom { get; set; }

        void WriteLine(string s, params object[] args);

        bool IsComplete(Result o);
        bool IsTimedOutOrOverBudget { get; }
        string WhyComplete { get; set; }

        bool addSetting(string name, Unifiable unifiable);
        void AddSubResult(Result result);
        //int GetCurrentDepth();
        Unifiable grabSetting(string name);
        QuerySettingsSettable GetQuerySettings();
        AIMLbot.MasterResult CreateResult(Request res);
        bool IsToplevelRequest { get; set; }

        AIMLbot.MasterRequest CreateSubRequest(string cmd);
        AIMLbot.MasterRequest CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, User requestee);
        bool CanUseResultTemplate(TemplateInfo info, Result request);
        bool CanUseRequestTemplate(TemplateInfo sol);
        OutputDelegate writeToLog { get; set; }

        Unifiable That { get; set; }
        PrintOptions WriterOptions { get; }
        MasterRequest ParentMostRequest { get; }
        RequestImpl OriginalSalientRequest { get; set; }
        AIMLTagHandler LastHandler { get; set; }
        ChatLabel PushScope { get; }
        ChatLabel CatchLabel { get; set; }
        SideEffectStage Stage { get; set; }
        bool SuspendSearchLimits { get; set; }
        string WhyRequestComplete { get; }
        string WhyResultComplete { get; }
        int MaxCanEvalResult { get; set; }
        // inherited from base  string GraphName { get; set; }
        ISettingsDictionary GetSubstitutions(string name, bool b);
        GraphMaster GetGraph(string srai);
        void ExcludeGraph(string srai);
        void AddOutputSentences(TemplateInfo ti, string nai, Result result);
        ISettingsDictionary GetDictionary(string named);
        ISettingsDictionary GetDictionary(string named, ISettingsDictionary dictionary);

        void AddUndo(Action action);
        void Commit();
        void UndoAll();
        void AddSideEffect(string effect, ThreadStart start);
        Dictionary<string, GraphMaster> GetMatchingGraphs(string graphname, GraphMaster master);
        bool TryGetSalientSRAI(Unifiable templateNodeInnerValue, out Unifiable prevResults, out Dictionary<Unifiable, Unifiable> dictionary);

        bool RemoveSalientSRAI(Unifiable templateNodeInnerValue, string resultOutput);
        string ToRequestString();

        void SetFromUserToUser(User BotAsUser1, User BotAsUser1_2);
    }

    /// <summary>
    /// Encapsulates all sorts of information about a request to the Proccessor for processing
    /// </summary>
    abstract public class RequestImpl : QuerySettings
    {
        #region Attributes

        private int _MaxCanEvalResult = -1;
        public int MaxCanEvalResult
        {
            get
            {
                if (_MaxCanEvalResult > -1) return _MaxCanEvalResult;
                Result currentResult = CurrentResult;
                return currentResult.MaxCanEvalResult;
            }
            set
            {
                _MaxCanEvalResult = value;
                CurrentResult.MaxCanEvalResult = value;
            }
        }

        public void ResetValues(bool b)
        {
            var TheDurration = Durration;
            if (TheDurration.TotalMilliseconds <= 1d)
            {
                TheDurration = TimeSpan.FromMilliseconds(TargetBot.TimeOut);
            }
            Result currentResult = CurrentResult;
            if (currentResult != null) currentResult.ResetAnswers(b);
            StartedOn = RTPBot.Now;
            TimeOut = TheDurration;
            _Durration = TimeSpan.Zero;
            _SRAIResults.Clear();
            RequestTemplates1.Clear();
        }

        /// <summary>
        /// The amount of time the request took to process
        /// </summary>
        public TimeSpan Durration
        {
            get
            {
                if (_Durration == TimeSpan.Zero) return RTPBot.Now - StartedOn;
                return _Durration;
            }
            set { _Durration = value; }
        }

        public Proof Proof { get; set; }

        // How many subqueries are going to be submitted with combos ot "that"/"topic" tags 
        public int MaxInputs { get; set; }

        public int depth
        {
            get { return SraiDepth.Current; }
            set { SraiDepth.Current = value; }
        }
        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Unifiable rawInput
        {
            get
            {
                if (ChatInput == null) return "@echo -no ChatInput yet-";
                return ChatInput.RawText;
            }
        }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        public ParsedSentences ChatInput { get; set; }

        public Request ParentRequest { get; set; }

        /// <summary>
        /// The time at which this request was created within the system
        /// </summary>
        public DateTime StartedOn { get; set; }

        /// <summary>
        /// The user who made this request
        /// </summary>
        public User Requester { get; set; }

        /// <summary>
        /// The user who is the target of this request
        /// </summary>
        public User Responder
        {
            get
            {
                if (_responderUser != null) return _responderUser;
                return ParentRequest.Responder;
            }
            set { _responderUser = value; }
        }

        /// <summary>
        /// The Proccessor to which the request is being made
        /// </summary>
        public RTPBot TargetBot { get; set; }

        /// <summary>
        /// The final result produced by this request
        /// </summary>
        private Result _result;
        public Result CurrentResult
        {
            get
            {
                if (_result == null)
                {
                    return null;
                }
                return _result;
            }
            set { _result = value; }
        }


        public ISettingsDictionary TargetSettings { get; set; }

        public TimeSpan TimeOut
        {
            get
            {
                if (TimesOutAt > StartedOn) return TimesOutAt - StartedOn;
                return TimeSpan.FromMilliseconds(TargetBot.TimeOut);
            }
            set
            {
                TimesOutAt = StartedOn + value;
                WhyComplete = null;
            }
        }

        /// <summary>
        /// Flag to show that the request has timed out
        /// </summary>
        public virtual string WhyRequestComplete
        {
            get
            {
                if (SuspendSearchLimits) return null;
                return _WhyRequestComplete;
            }
            set { if (!SuspendSearchLimits) _WhyRequestComplete = value; }
        }

        public string WhyResultComplete
        {
            get
            {
                var currentResult = CurrentResult;
                if (currentResult == null) return null;
                return currentResult.WhyResultComplete;
            }
        }

        public string _WhyRequestComplete;

        public bool IsTimedOutOrOverBudget
        {
            get
            {
                if (SuspendSearchLimits) return false;
                var WhyComplete = WhyRequestComplete;
                if (WhyComplete != null) return true;                
                if (SraiDepth.IsOverMax)
                {
                    WhyComplete = (WhyComplete ?? "") + "SraiDepth ";
                }
                if (RTPBot.Now > TimesOutAt)
                {
                    WhyComplete = (WhyComplete ?? "") + "TimesOutAt ";
                }
                string whyNoSearch = WhyNoSearch(CurrentResult);
                if (whyNoSearch!=null)
                {
                    WhyComplete = (WhyComplete ?? "") + whyNoSearch;
                }
                if (WhyComplete != null)
                {
                    _WhyRequestComplete = WhyComplete;
                }
                return _WhyRequestComplete != null;
            }
        }

        public string WhyComplete
        {
            get
            {
                Result currentResult = CurrentResult;
                return WhyRequestComplete ?? (currentResult == null ? null : currentResult.WhyResultComplete);
            }
            set { _WhyRequestComplete = value; }
        }

#if LESS_LEAN
        public readonly int framesAtStart;
#endif


        private ISettingsDictionary _responderYouPreds;
        private User _responderUser;

        public ISettingsDictionary ResponderPredicates
        {
            get
            {
                if (_responderYouPreds != null) return _responderYouPreds;
                var resp = Responder;
                if (resp != null) return resp.Predicates;
                if (ParentRequest != null)
                {
                    return ParentRequest.ResponderPredicates;
                } 
                RTPBot.writeDebugLine("ERROR Cant find responder Dictionary !!!");
                return null; // TargetSettings;
            }
            set { _responderYouPreds = value; }
        }
        #endregion

        public override string ToString()
        {
            return ToRequestString();
        }
        public string ToRequestString()
        {
            string whyComplete = WhyComplete;
            string unifiableToVMString = Unifiable.ToVMString(rawInput);
            var cq = CurrentQuery;

            if (IsNullOrEmpty(unifiableToVMString))
            {
                if (cq != null)
                {
                    unifiableToVMString = cq.FullPath;
                    if (IsNullOrEmpty(unifiableToVMString))
                    {
                        unifiableToVMString = cq.ToString();
                    }
                }
            }
            return String.Format("{0}: {1}, \"{2}\"", Requester == null ? "NULL" : (string) Requester.UserID,
                                 Responder == null ? "Anyone" : (string) Responder.UserID,
                                 unifiableToVMString);
        }

        public RequestImpl OriginalSalientRequest { get; set; }


        protected RequestImpl(QuerySettingsReadOnly defaults)
            : base(defaults)
        {
            ApplySettings(defaults, this);
        }
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="rawInput">The raw input from the user</param>
        /// <param name="user">The user who made the request</param>
        /// <param name="bot">The bot to which this is a request</param>
        protected RequestImpl(string rawInput, User user, RTPBot bot, Request parent, User targetUser)
            :  this(bot.GetQuerySettings()) // Get query settings intially from user
        {
            SraiDepth.Max = 19;
            IsToplevelRequest = parent == null;
            this.Stage = SideEffectStage.UNSTARTED;
            qsbase = this;
            SuspendSearchLimits = true;
            if (parent != null)
            {
                //ChatInput = parent.ChatInput;
                ChatInput = ParsedSentences.GetParsedUserInputSentences(thisRequest, rawInput);
                Requester = parent.Requester;
                depth = parent.depth + 1;
                OriginalSalientRequest = parent.OriginalSalientRequest;
            }
            else
            {
                ChatInput = ParsedSentences.GetParsedUserInputSentences(thisRequest, rawInput);
            }
            DebugLevel = -1;
            if (parent != null) targetUser = parent.Responder;
            Requester = Requester ?? user;            
            if (Requester != null)
            {
                ApplySettings(Requester.GetQuerySettings(), this);
                if (targetUser == null)
                {
                    if (user == bot.BotAsUser) targetUser = bot.LastUser;
                    else targetUser = bot.BotAsUser;
                }
            }
            if (targetUser != null) Responder = targetUser;
            _responderUser = targetUser;
            UsedResults = new List<Result>();
            Flags = "Nothing";
            QuerySettingsSettable querySettings = GetQuerySettings();

            ApplySettings(qsbase, querySettings);

            if (parent != null)
            {
                ApplySettings(parent.GetQuerySettings(), querySettings);
                Proof = parent.Proof;
                this.ParentRequest = parent;
                this.lastOptions = parent.LoadOptions;
                this.writeToLog = parent.writeToLog;
                Graph = parent.Graph;
                MaxInputs = 1;
                OriginalSalientRequest = parent.OriginalSalientRequest;
                ReduceMinMaxesForSubRequest(user.GetQuerySettingsSRAI());
            }
            else
            {
                if (user != null)
                {
                    writeToLog = user.WriteLine;
                    MaxInputs = user.MaxInputs;
                }
                else MaxInputs = 1;

                Proof = new Proof();
            }
            writeToLog = writeToLog ?? bot.writeToLog;
            if (user != null)
            {
                Request pmaybe = null;
                pmaybe = user.CurrentRequest;
                this.Requester = user;
                if (user.CurrentRequest == null) user.CurrentRequest = thisRequest;
                TargetSettings = user.Predicates;
                if (parent == null)
                {
                    if (pmaybe != null)
                    {
                       // ParentRequest = pmaybe;
                    }
                    user.CurrentRequest = thisRequest;
                }
            }
            this.TargetBot = bot;
            this.TimeOutFromNow = TimeSpan.FromMilliseconds(TargetBot.TimeOut);
            //this.framesAtStart = new StackTrace().FrameCount;
            if (parent != null)
            {
                TargetSettings = parent.TargetSettings;
            }
        }


        public void SetFromUserToUser(User user, User targetUser)
        {
            SetUser(user);
            SetTargetUser(targetUser);
        }

        private void SetTargetUser(User targetUser)
        {
            Request parent = this.ParentRequest;
            if (targetUser == null && parent != null) targetUser = parent.Responder;
            if (targetUser == null)
            {
                if (Requester != null)
                {
                    if (Requester == TargetBot.BotAsUser) targetUser = TargetBot.LastUser;
                    else targetUser = TargetBot.BotAsUser;
                }
            }
            if (targetUser != null) Responder = targetUser;
            _responderUser = targetUser;
        }

        private void SetUser(User user)
        {
            if (user != null)
            {
                Requester = user;
                ApplySettings(user.GetQuerySettings(), this);
                if (IsToplevelRequest)
                {
                    user.CurrentRequest = thisRequest;
                }
                else
                {
                    ReduceMinMaxesForSubRequest(user.GetQuerySettingsSRAI());
                }
                IsTraced = user.IsTraced;
                writeToLog = user.WriteLine;
                MaxInputs = user.MaxInputs;
                TargetSettings = user.Predicates;
                if (user.CurrentRequest == null) user.CurrentRequest = thisRequest;
            }
        }

        public void ReduceMinMaxesForSubRequest(QuerySettingsReadOnly parent)
        {
            var thisQuerySettings = GetQuerySettings();
            const int m = 2;
            const int M = 3;
            thisQuerySettings.MinPatterns = Math.Max(1, ((QuerySettingsReadOnly)parent).MinPatterns - m);
            thisQuerySettings.MaxPatterns = Math.Max(1, ((QuerySettingsReadOnly)parent).MaxPatterns - M);
            thisQuerySettings.MinTemplates = Math.Max(1, ((QuerySettingsReadOnly)parent).MinTemplates - m);
            thisQuerySettings.MaxTemplates = Math.Max(1, ((QuerySettingsReadOnly)parent).MaxTemplates - M);
        }

        public bool GraphsAcceptingUserInput
        {
            get { return Graph.GraphsAcceptingUserInput; }
            set { Graph.GraphsAcceptingUserInput = value; }
        }

        public LoaderOptions lastOptions;
        public LoaderOptions LoadOptions
        {
            get
            {
                // when we change to s struct, lastOptions will never be null
// ReSharper disable ConditionIsAlwaysTrueOrFalse
                if (lastOptions == null || lastOptions.TheRequest == null)
// ReSharper restore ConditionIsAlwaysTrueOrFalse
                {
                    lastOptions = new LoaderOptions(thisRequest, Graph);
                }
                return lastOptions;
            }
            set
            {
                lastOptions = value;
                Graph = value.CtxGraph;
                Filename = value.CurrentFilename;
                LoadingFrom = value.CurrentlyLoadingFrom;
            }
        }


        private AIMLLoader _aimlloader = null;
        public AIMLLoader Loader
        {
            get
            {
                if (_aimlloader == null) _aimlloader = new AIMLLoader(TargetBot, thisRequest);
                return _aimlloader;
            }
        }

        public string Filename
        {
            get { return _filename; }
            set
            {
                //if (_loadingfrom == null)
                //{
                //    _loadingfrom = _filename;
                //}
               if (_loadingfrom == null)
               {
                   _loadingfrom = null;
                    //_loadingfrom = value;
               }
                _filename = value;
            }
        }
        public string LoadingFrom
        {
            get { return _loadingfrom; }
            set
            {
                if (_filename == null)
                {
                  //   _filename = value;
                } else
                {
                 //   Filename = null;
                }
                _loadingfrom = value;
            }
        }

        private string _filename;
        private string _loadingfrom;

        internal GraphMaster sGraph = null;
        public GraphMaster Graph
        {
            get
            {
                if (sGraph != null)
                    return sGraph;
                if (ParentRequest != null)
                {
                    var pg = ParentRequest.Graph;
                    if (pg != null) return pg;
                }
                if (Requester == null)
                {
                    if (ovGraph == null) return null;
                    return TargetBot.GetGraph(ovGraph, null);
                }
                GraphMaster probably = Requester.ListeningGraph;
                if (ovGraph != null)
                {
                    if (probably != null)
                    {
                        // very good!
                        if (probably.ScriptingName == ovGraph) return probably;
                        if (probably.ScriptingName == null)
                        {
                            // not  so good!
                            return probably;
                        }
                        if (probably.ScriptingName.Contains(ovGraph)) return probably;
                        // transtiton
                        var newprobably = TargetBot.GetGraph(ovGraph, probably);
                        if (newprobably != probably)
                        {
                            Requester.WriteLine("Changing request graph " + probably + " -> " + newprobably + " for " + this);
                            probably = newprobably;
                        }
                    }
                    else
                    {
                        probably = TargetBot.GetGraph(ovGraph, TargetBot.GraphMaster);
                        {
                            Requester.WriteLine("Changing request graph " + probably + " -> " + null + " for " + this);
                        }
                    }
                }
                return probably;
            }
            set
            {
                if (value != null)
                {
                    if (ovGraph == value.ScriptingName) return;
                    ovGraph = value.ScriptingName;
                }
                LoaderOptions lo = LoadOptions;
                lo.CtxGraph = value;
                sGraph = value;
            }
        }

        private string ovGraph = null;

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public override string GraphName
        {
            get
            {
                if (ovGraph != null)
                    return ovGraph;
                if (ParentRequest != null)
                {
                    var pg = ((QuerySettingsReadOnly)ParentRequest).GraphName;
                    if (pg != null) return pg;
                }
                string ugn = Requester.GraphName;
                if (ugn != null) return ugn;
                GraphMaster gm = Graph;
                if (gm != null) ugn = gm.ScriptingName;
                return ugn;
            }
            set
            {
               // if (sGraph != null)
                sGraph = GetGraph(value);
                ovGraph = value;
            }
        }

        public GraphMaster GetGraph(string value)
        {
           return TargetBot.GetGraph(value, sGraph);
        }

        public void ExcludeGraph(string srai)
        {
            ExcludeGraph(GetGraph(srai));
        }
        public void ExcludeGraph(GraphMaster getGraph)
        {
            ICollection<GraphMaster> disallowedGraphs = DisallowedGraphs;
            lock (disallowedGraphs)
            {
                if (disallowedGraphs.Contains(getGraph)) return;
                disallowedGraphs.Add(getGraph);
            }
            AddSideEffect("restore graph access " + getGraph,
                          () => { lock (disallowedGraphs) disallowedGraphs.Remove(getGraph); });
        }

        public ICollection<GraphMaster> DisallowedGraphs
        {
            get { return Requester.DisallowedGraphs; }
        }

        public void AddOutputSentences(TemplateInfo ti, string nai, Result result)
        {
            result = result ?? CurrentResult;
            result.AddOutputSentences(ti, nai);   
        }

        private Unifiable _topic;

        public Unifiable Flags { get; set; }
        virtual public GraphQuery TopLevel { get; set; }

        public Unifiable Topic
        {
            get
            {
                return Requester.TopicSetting;
                if (_topic != null) return _topic;
                if (ParentRequest != null) return ParentRequest.Topic;
                return Requester.TopicSetting;
            }
            set
            {
                if(true)
                {
                    Requester.TopicSetting = value;
                    return;
                }
                Unifiable prev = Topic;
                Requester.TopicSetting = value;
                if (value == TargetBot.NOTOPIC)
                {
                    if (_topic != null)
                    {
                        _topic = null;
                    }
                    else
                    {

                    }
                }
                if (prev == value) return;
                if (_topic != null)
                {
                    _topic = value;
                    return;
                }
                if (ParentRequest != null)
                {
                    ParentRequest.Topic = value;
                    return;
                }
                _topic = value;
            }
        }

        public IList<Unifiable> Topics
        {
            get
            {
                var tops = Requester.Topics;
                if (_topic!=null)
                {
                    if (!tops.Contains(_topic)) tops.Insert(0, _topic);
                }
                if (tops.Count == 0) return new List<Unifiable>() { TargetBot.NOTOPIC };
                return tops;
            }
        }

        public IEnumerable<Unifiable> ResponderOutputs
        {
            get { return Requester.BotOutputs; }
        }

        public ISettingsDictionary RequesterPredicates
        {
            get
            {
                if (TargetSettings is SettingsDictionary) return (SettingsDictionary) TargetSettings;
                var currentResult = CurrentResult;
                return currentResult.Predicates;
            }
        }
        #if LESS_LEAN
        public int GetCurrentDepth()
        {
           // int here = new StackTrace().FrameCount - framesAtStart;
            return here / 6;
        }
#endif
        public Unifiable grabSetting(string name)
        {
            return RequesterPredicates.grabSetting(name);
        }

        public bool addSetting(string name, Unifiable value)
        {
            return RequesterPredicates.addSetting(name, value);
        }

        readonly private IList<TemplateInfo> RequestTemplates1 = new List<TemplateInfo>();
        public IList<TemplateInfo> RequestTemplates
        {
            get
            {                
                return RequestTemplates1;
            }
        }
        
        public DateTime TimesOutAt { get; set; }

        public void WriteLine(string s, params object[] args)
        {
            var currentResult = CurrentResult;
            if (currentResult != null)
            {
                currentResult.writeToLog(s, args);
            }
            else
            {
                writeToLog(s, args);
            }
        }

        public bool IsComplete(Result result1)
        {
            if (SuspendSearchLimits) return false;
            string s = WhyNoSearch(result1);
            if (s == null) return false;
            return true;
        }

        public string WhyNoSearch(Result result1)
        {
            if (result1 == null)
            {
                return null;
            }
            if (WhyComplete != null) return WhyComplete;
            QuerySettingsReadOnly qs = GetQuerySettings();
            string w = null;
            if (result1.OutputSentenceCount >= qs.MaxOutputs)
            {
                w = w ?? "";
                w += "MaxOutputs ";
            }

            if (SuspendSearchLimits) return WhyComplete;
            if (result1.SubQueries.Count >= qs.MaxPatterns)
            {
                if (result1.OutputSentenceCount > 0)
                {
                    w = w ?? "";
                    w += "MaxPatterns ";
                }
                // return null;
            }
            if (result1.ResultTemplates.Count >= qs.MaxTemplates)
            {
                if (result1.OutputSentenceCount > 0)
                {
                    w = w ?? "";
                    w+= "MaxTemplates ";
                }
                // return null;
            }
            if (w != null) WhyComplete = w;
            return WhyComplete;
        }

        public QuerySettingsSettable GetQuerySettings()
        {
            return qsbase;
        }

        public MasterResult CreateResult(Request parentReq)
        {
            if (parentReq != this)
            {
                return parentReq.CreateResult(parentReq);
            }

            Result currentResult = parentReq.CurrentResult;
            if (currentResult == null)
            {
                var r = new AIMLbot.MasterResult(rawInput, Requester, TargetBot, parentReq, parentReq.Responder);
                parentReq.CurrentResult = r;
                r.request = thisRequest;
                currentResult = r;
            }
            TimeOutFromNow = parentReq.TimeOut;
            return (MasterResult) currentResult;
        }

        private MasterRequest thisRequest
        {
            get { return (MasterRequest)this; }
        }

        public MasterRequest CreateSubRequest(string templateNodeInnerValue)
        {
            var request = (MasterRequest)this;
            var user= this.Requester;
            var  rTPBot = this.TargetBot;
            var requestee = this.Responder;
            Request thisRequest = (Request)this;
            var subRequest = new MasterRequest(templateNodeInnerValue, user ?? Requester,
                                                       rTPBot ?? this.TargetBot, thisRequest, requestee ?? Responder);
            Result res = request.CurrentResult;
            subRequest.CurrentResult = res;
            subRequest.Graph = request.Graph;
            depth = subRequest.depth = request.depth + 1;
            subRequest.ParentRequest = request;
            subRequest.StartedOn = request.StartedOn;
            subRequest.TimesOutAt = request.TimesOutAt;
            subRequest.TargetSettings = request.TargetSettings;
            //subRequest.Responder = request.Responder;
            return subRequest;
        }

        public MasterRequest CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, User requestee)
        {
            var request = (MasterRequest) this;
            user = user ?? this.Requester;
            rTPBot = rTPBot ?? this.TargetBot;
            Request thisRequest = (Request)this;
            var subRequest = new MasterRequest(templateNodeInnerValue, user ?? Requester,
                                                       rTPBot ?? this.TargetBot, thisRequest, requestee ?? Responder);
            //Result res = request.TheCurrentResult;
            //subRequest.currentResult = res;
            subRequest.Graph = request.Graph;
            depth = subRequest.depth = request.depth + 1;
            subRequest.ParentRequest = request;
            subRequest.StartedOn = request.StartedOn;
            subRequest.TimesOutAt = request.TimesOutAt;
            subRequest.TargetSettings = request.TargetSettings;
            //subRequest.Responder = request.Responder;
            return subRequest;
        }
        public bool CanUseRequestTemplate(TemplateInfo info)
        {
            if (FoundInParents(info, thisRequest))
            {
                //user.WriteLine("!CanUseTemplate ", info);
                return false;
            }
            return true;
        }
        public bool CanUseResultTemplate(TemplateInfo info, Result result)
        {
            if (info == null) return true;
            if (info.IsDisabled) return false;
            if (!Requester.CanUseTemplate(info, result)) return false;
            if (!result.CanResultUseTemplate(info))
            {
                return false;
            }
            return true;
        }

        private bool FoundInParents(TemplateInfo info, Request requestOrResult)
        {
            if (requestOrResult == null) return true;
            while (requestOrResult != null)
            {
                var resultUsedTemplates = requestOrResult.RequestTemplates;
                if (resultUsedTemplates != null)
                {
                    lock (resultUsedTemplates)
                    {
                        if (resultUsedTemplates.Contains(info))
                        {                            
                            return true;
                        }
                    }
                }
                requestOrResult = requestOrResult.ParentRequest;
            }
            return false;
        }

        public OutputDelegate writeToLog { get; set; }

        public void writeToLog0(string message, params object[] args)
        {
            if (!message.Contains(":")) message = "REQUEST: " + message;
            string prefix = ToString();
            prefix = DLRConsole.SafeFormat(message + " while " + prefix, args);

            message = prefix.ToUpper();
            if (message.Contains("ERROR") || message.Contains("WARN"))
            {
                DLRConsole.DebugWriteLine(prefix);
            }
            DLRConsole.SystemFlush();
            if (writeToLog != writeToLog0)
            {
                //   writeToLog(prefix);
            }
            TargetBot.writeToLog(prefix);
        }

        public override sealed int DebugLevel
        {
            get
            {
        
                int baseDebugLevel = base.DebugLevel;
                if (baseDebugLevel > 0) return baseDebugLevel;
                var parentRequest = (QuerySettingsReadOnly)this.ParentRequest;
                if (IsToplevelRequest || parentRequest == null)
                {
                    if (Requester == null) return baseDebugLevel;
                    return Requester.GetQuerySettings().DebugLevel;
                }
                return parentRequest.DebugLevel;
            }
            set { base.DebugLevel = value; }
        }

        public bool IsToplevelRequest { get; set; }

        internal Unifiable ithat = null;
        public Unifiable That
        {
            get
            {
                string something;
                if (User.ThatIsStoredBetweenUsers)
                {
                    if (Requester != null && IsSomething(Requester.That, out something)) return something;
                    if (Responder != null && IsSomething(Responder.JustSaid, out something)) return something;
                    return "Nothing";
                }
                if (IsSomething(ithat, out something)) return something;
                string u1 = null;
                string u2 = null;
                string r1 = RequestThat();

                if (Requester != null)
                {
                    if (IsSomething(Requester.That, out something)) u1 = something;
                }
                if (Responder != null && Responder != Requester)
                {
                    if (IsSomething(Responder.JustSaid, out something))
                    {
                        u2 = something;
                        if (u1 != null)
                        {
                            char[] cs = " .?".ToCharArray();
                            if (u1.Trim(cs).ToLower() != u2.Trim(cs).ToLower())
                            {
                                writeToLog("Responder.JustSaid=" + u2 + " Requester.That=" + u1);
                                if (u2.Contains(u1))
                                {
                                    u2 = u1;
                                }
                            }
                        }
                    }
                }
                string u = u2 ?? u1;
                if (IsSomething(r1, out something))
                {
                    if (r1 != u)
                    {
                        writeToLog("ERROR That Requester !" + r1);
                    }
                }
                return u ?? "Nothing";
            }
            set
            {
                if (User.ThatIsStoredBetweenUsers) throw new InvalidOperationException("must User.set_That()");
                if (IsNullOrEmpty(value)) throw new NullReferenceException("set_That: " + this);
                ithat = value;
            }
        }

        public string RequestThat()
        {
            if (User.ThatIsStoredBetweenUsers) throw new InvalidOperationException("must User.get_That()");
            var req = this;
            while (req != null)
            {
                string something;
                if (IsSomething(req.ithat, out something))

                    return something;

                req = req.ParentRequest as RequestImpl;
            }
            return null;
        }

        internal PrintOptions iopts;

        public PrintOptions WriterOptions
        {
            get
            {
                if (iopts != null) return iopts;
                if (ParentRequest != null)
                {
                    return ParentRequest.WriterOptions;
                }
                return Requester.WriterOptions;
            }
            set { iopts = value; }
        }

        public MasterRequest ParentMostRequest
        {
            get
            {
                if (IsToplevelRequest) return thisRequest;
                if (ParentRequest == null) return thisRequest;
                return ParentRequest.ParentMostRequest;
            }
        }

        private AIMLTagHandler _lastHandler;

        public AIMLTagHandler LastHandler
        {
            get { return _lastHandler; }
            set { if (value != null) _lastHandler = value; }
        }

        private ChatLabel _label = null;
        public ChatLabel PushScope
        {
            get
            {
                {
                    _label = new ChatLabel();
                }
                var currentResult = CurrentResult;
                currentResult.CatchLabel = _label;
                return _label;
            }
        }

        public ChatLabel CatchLabel
        {
            get { return _label; }
            set { _label = value; }
        }

        public SideEffectStage Stage{ get; set;}

        public ISettingsDictionary GetSubstitutions(string named, bool createIfMissing)
        {
            return TargetBot.GetDictionary(named, "substitutions", createIfMissing);
        }

        public ISettingsDictionary GetDictionary(string named)
        {
            if (CurrentQuery == null)
            {
             //   writeToLog("ERROR: Cannot get CurrentQuery!?");
            }
            return GetDictionary(named, CurrentQuery);
        }

        public void AddUndo(Action undo)
        {
            lock (this)
            {
                UndoStack.GetStackFor(this).AddUndo(() => undo());
            }
        }
        public void UndoAll()
        {
            lock (this)
            {
                UndoStack.GetStackFor(this).UndoAll();
            }
        }
        private readonly List<KeyValuePair<string, ThreadStart>> commitHooks = new List<KeyValuePair<string, ThreadStart>>();
        public static void DoAll(List<KeyValuePair<string, ThreadStart>> todo)
        {
            List<KeyValuePair<string, ThreadStart>> newList = new List<KeyValuePair<string, ThreadStart>>();
            lock (todo)
            {
                newList.AddRange(todo);
                todo.Clear();
            }
            foreach (KeyValuePair<string, ThreadStart> start in newList)
            {
                DoTask(start);
            }
            todo.Clear();
        }

        private static void DoTask(KeyValuePair<string, ThreadStart> start)
        {
            try
            {
                start.Value();
            }
            catch (Exception exception)
            {
                RTPBot.writeDebugLine("ERROR in " + start.Key + " " + exception);
            }
        }

        readonly object RequestSideEffect = new object();
        public void Commit()
        {
            lock (RequestSideEffect)
            {
                if (commitHooks == null || commitHooks.Count == 0)
                {
                    return;
                }
                bool prevCommitNow = CommitNow;
                try
                {
                    while (true)
                    {
                        lock (commitHooks)
                        {
                            if (commitHooks.Count == 0)
                            {
                                return;
                            }
                        }
                        CommitNow = false;
                        DoAll(commitHooks);
                        CommitNow = true;
                    }

                }
                finally
                {
                    CommitNow = prevCommitNow;
                }
            }
        }

        public bool CommitNow = false;
        private readonly QuerySettings qsbase;
        public bool SuspendSearchLimits { get; set; }
        protected Result _CurrentResult;

        public void AddSideEffect(string name, ThreadStart action)
        {
            KeyValuePair<string, ThreadStart> newKeyValuePair = new KeyValuePair<string, ThreadStart>(name, action);
            lock (RequestSideEffect)
            {
                if (CommitNow)
                {
                    DoTask(newKeyValuePair);
                }
                else
                {
                    commitHooks.Add(newKeyValuePair);
                }
            }
        }

        public Dictionary<string, GraphMaster> GetMatchingGraphs(string graphname, GraphMaster master)
        {
            RTPBot t = TargetBot;
            Dictionary<string, GraphMaster> graphs = new Dictionary<string, GraphMaster>();
            if (graphname == "*")
            {
                foreach (KeyValuePair<string, GraphMaster> ggg in GraphMaster.CopyOf(RTPBot.GraphsByName))
                {
                    var G = ggg.Value;
                    if (G != null && !graphs.ContainsValue(G)) graphs[ggg.Key] = G;
                }
                foreach (KeyValuePair<string, GraphMaster> ggg in GraphMaster.CopyOf(t.LocalGraphsByName))
                {
                    var G = ggg.Value;
                    if (G != null && !graphs.ContainsValue(G)) graphs[ggg.Key] = G;
                }
            } else
            {
                var G = t.GetGraph(graphname, master);
                if (G != null && !graphs.ContainsValue(G)) graphs[graphname] = G;
            }
            return graphs;        
        }

        public TimeSpan TimeOutFromNow 
        {
            set
            {
                WhyComplete = null;
                StartedOn = RTPBot.Now;
                if (TimeOut < value)
                {
                    TimeOut = value;
                }
                else
                {
                    TimeOut = value;                    
                }
            }
        }

        public ISettingsDictionary GetDictionary(string named, ISettingsDictionary dictionary)
        {
            ISettingsDictionary dict =  GetDictionary0(named, dictionary);
            if (IsTraced && dict == TargetBot.GlobalSettings)
            {                
             //   writeToLog("BETTER MEAN BOT " + named + " for " + dictionary);
            }
            if (dict != null) return dict;
            writeToLog("ERROR MISSING DICTIONARY " + named + " for " + dictionary);
            return null;
        }

        public ISettingsDictionary GetDictionary0(string named, ISettingsDictionary dictionary)
        {
            if (named == null)
            {
                if (dictionary != null) return dictionary;
                if (CurrentQuery != null) return CurrentQuery;
                if (Requester != null) return Requester;
                return TargetSettings;
            }
            if (named == "user") return CheckedValue(named, Requester);
            if (named == "li") return CheckedValue(named, dictionary);
            if (named == "condition") return CheckedValue(named, dictionary);
            if (named == "set") return CheckedValue(Requester.UserID, Requester);
            if (named == "get") return CheckedValue(Requester.UserID, Requester);
            if (named == "query") return CheckedValue(named, CurrentQuery);
            if (named == "request") return CheckedValue(named, TargetSettings);
            if (named == "bot.globalsettings") return CheckedValue(named, TargetBot.GlobalSettings);     
            if (named == "bot")
            {
                if (ResponderPredicates != null) return ResponderPredicates;
                if (Responder != null) return Responder.Predicates;
                if (CurrentQuery != null)
                    return CheckedValue(named, CurrentQuery.ResponderPredicates ?? TargetBot.GlobalSettings);
                return CheckedValue(named, TargetBot.GlobalSettings);
            }

            string[] path = named.Split(new[] { '.' });
            if (path.Length > 1)
            {
                var v = GetDictionary(path[0]);
                if (v != null)
                {
                    var vp = GetDictionary(String.Join(".", path, 0, path.Length - 1), v);
                    if (vp != null) return vp;
                }
            }
            if (ParentRequest == null)
            {
                return TargetBot.GetDictionary(named);
            }
            else
            {
                var v = ParentRequest.GetDictionary(named);
                if (v != null) return v;
            }
            return null;
        }

        public ISettingsDictionary CheckedValue(string named, ISettingsDictionary d)
        {
            return d;
        }


        public IList<Result> UsedResults { get; set; }

        virtual public SubQuery CurrentQuery
        {
            get
            {
                Result r = _CurrentResult;
                if (r != null && r != this)
                {
                    SubQuery sq = r.CurrentQuery;
                    if (sq != null) return sq;
                    if (ParentRequest != null)
                    {
                        return ParentRequest.CurrentQuery;
                    }
                }
                return null;
            }
            set { _CurrentQuery = value; }
        }
        protected SubQuery _CurrentQuery;


        public void AddSubResult(Result subResult)
        {
            lock (UsedResults)
            {
                UsedResults.Add(subResult);
            }
        }

        public Dictionary<Unifiable, Unifiable> _SRAIResults = new Dictionary<Unifiable, Unifiable>();

        internal static RequestImpl GetOriginalSalientRequest(Request request)
        {
            RequestImpl originalSalientRequest = request as RequestImpl;// .OriginalSalientRequest;
            if (originalSalientRequest == null)
            {
                originalSalientRequest = request as RequestImpl;
                request.OriginalSalientRequest = originalSalientRequest;
            }
            return originalSalientRequest;
        }

        public void ExitSalientSRAI(Unifiable templateNodeInnerValue, string resultOutput)
        {
            //if (IsNull(resultOutput)) resultOutput = "FAILURE on: " + templateNodeInnerValue;
            //else resultOutput = "+++" + resultOutput;
            //_SRAIResults[templateNodeInnerValue] = resultOutput;
            RemoveSalientSRAI(templateNodeInnerValue, resultOutput);
        }

        public bool RemoveSalientSRAI(Unifiable templateNodeInnerValue, string resultOutput)
        {
            return _SRAIResults.Remove(templateNodeInnerValue) ||
                   (ParentRequest != null && ParentRequest.RemoveSalientSRAI(templateNodeInnerValue, resultOutput));
        }



        public bool TryGetSalientSRAI(Unifiable templateNodeInnerValue, out Unifiable prevResults, out Dictionary<Unifiable, Unifiable> dictionary)
        {
            if (_SRAIResults.TryGetValue(templateNodeInnerValue, out prevResults))
            {
                dictionary = _SRAIResults;
                return true;
            }
            if (ParentRequest != null)
                if (ParentRequest.TryGetSalientSRAI(templateNodeInnerValue, out prevResults, out dictionary))
                {
                    return true;
                }
            dictionary = _SRAIResults;
            return false;
        }

        public bool EnterSalientSRAI(Unifiable templateNodeInnerValue, out Unifiable prevResults)
        {
            Dictionary<Unifiable, Unifiable> dict = _SRAIResults;
            lock (dict)
            {
                string errorCycle = "ERROR CYCLE: " + templateNodeInnerValue;
                if (!TryGetSalientSRAI(templateNodeInnerValue, out prevResults, out dict))
                {
                    dict[templateNodeInnerValue] = errorCycle;
                    return true;
                }
                if (prevResults == errorCycle)
                {
                    writeToLog("LOOPED on " + errorCycle);
                    return false;
                }
                if (IsNullOrEmpty(prevResults))
                {
                    writeToLog("EnterSalientSRAI IsNullOrEmpty on: " + templateNodeInnerValue);
                    return true;
                }
                else
                {
                    if (!AIMLTagHandler.IsUnevaluated(templateNodeInnerValue))
                    {
                        writeToLog("Looped maybe '" + prevResults + "' on: " + templateNodeInnerValue);
                        return false;
                    }
                    writeToLog("Looped '" + prevResults + "' on: " + templateNodeInnerValue);
                    return true;
                }
            }
        }

        public Dictionary<Unifiable, Unifiable> CreateSRAIMark()
        {
            var originalSalientRequest = GetOriginalSalientRequest(thisRequest);
            var dict =  new Dictionary<Unifiable, Unifiable>(originalSalientRequest._SRAIResults);
            var pr = ParentRequest;
            while (pr != null)
            {
                var or = GetOriginalSalientRequest(pr);
                if (or != originalSalientRequest)
                {
                    foreach (var unifiable in or._SRAIResults)
                    {
                        try
                        {
                            dict[unifiable.Key] = unifiable.Value;
                        }
                        catch (Exception)
                        {
                        }
                    }
                    originalSalientRequest = or;
                }
                pr = pr.ParentRequest;
            }
            return dict;
        }

        public void ResetSRAIResults(Dictionary<Unifiable, Unifiable> sraiMark)
        {
            _SRAIResults = sraiMark;
        }
    }
}
