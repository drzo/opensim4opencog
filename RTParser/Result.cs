using System;
using System.Collections.Generic;
using AIMLbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser.Utils;
using RTParser.Variables;
using UPath = RTParser.Unifiable;


namespace RTParser
{
#if interfaces   
    public interface Result : InteractionResult, RequestOrQuery
#else
    public interface ResultImpl : InteractionResult, RequestOrQuery
#endif
    {
        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        //     RTPBot TargetBot { get; set; }
        /// The user that is providing the <that/> answer
        //        UserDuringProcessing Responder { get; set; }
        //        string WhyComplete { get; set; }
        bool IsTraced { get; set; }

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        List<Unifiable> InputSentences { get; }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        List<Unifiable> NormalizedPaths { get; }

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        List<Unifiable> OutputSentences { get; }

        /// <summary>
        /// The request from the user
        /// </summary>
        Request request { get; }

        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        List<SubQuery> SubQueries { get; }

        ParsedSentences ChatOutput { get; }

        /// <summary>
        /// The user for whom this is a result
        /// </summary>
        ///UserDuringProcessing Requester { get; set; }
        // OutputDelegate writeToLog { get; set; }
        int TemplatesSucceeded { get; set; }

        int OutputsCreated { get; set; }
        //   GraphQuery TopLevel { get; set; }
        double Score { get; }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        //    bool IsTraced { get; set; }
        string SetOutput { set; }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        //Unifiable rawInput { get; }

        /// <summary>
        /// The result from the bot with logging and checking
        /// </summary>
        Unifiable Output { get; }

        string EnglishOutput { get; }

        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        Unifiable RawOutput { get; }

        bool IsEmpty { get; }
        int OutputSentenceCount { get; }
        ISettingsDictionary RequesterPredicates { get; }
        // SubQuery CurrentQuery { get; set; }
        // bool IsComplete { get; set; }
        bool IsSalient { get; }
        //    IList<TemplateInfo> UsedTemplates { get; }
        //   ChatLabel CatchLabel { get; set; }
        Result ParentResult { get; }
        string NormalizedOutput { get; }
        double TemplateRating { get; set; }
        bool Started { get; set; }
        TimeSpan Durration { get; }
        void CollectRequest();
        string WhyResultComplete { get; set; }
        IList<TemplateInfo> ResultTemplates { get; }
        RTPBot TargetBot { get; }
     //   UserDuringProcessing Requester { get; set; }
        GraphMaster Graph { get; }
        void AddSubqueries(GraphQuery queries);
        void AddOutputSentences(TemplateInfo ti, string unifiable);
        void AddResultFormat(string format, params object[] args);
        string ToString();
        Unifiable GetOutputSentence(int sentence);
        void RotateUsedTemplates();
        void ResetAnswers(bool b);
        bool CanResultUseTemplate(TemplateInfo info);
        OutputDelegate writeToLog { get; set; }
        ChatLabel CatchLabel { get; set; }
    //    UserDuringProcessing Responder { get; }
        SubQuery CurrentQuery { get; set; }
        int MaxCanEvalResult { get; set; }
        bool IsComplete { get; set; }
        int HasFailed { get; set; }
        int HasSuceeded { get; set; }
        void Exit();
    }

    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
#if interfaces   
    public abstract class ResultImpl : QuerySettings, Result, InteractionResult, RequestOrQuery
#else
    public abstract class Result : QuerySettings, ResultImpl, InteractionResult, RequestOrQuery
#endif
   
    {
        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<SubQuery> SubQueries { get; set; }

        public static int MaxPrintResults = 1;
        private string AlreadyUsed = "xtxtxtxtxtxtxtxtxxt";
        private int RotatedTemplate;

        //public double TemplateRating { get; set; }

        public TimeSpan Durration
        {
            get { return request.Durration; }
        }


        public override string StartGraphName
        {
            get { return ParentRequest.StartGraphName; }
            set { throw new NotImplementedException(); }
        }

        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        //   public RTPBot TargetBot { get; set; }
        /// The user that is providing the <that/> answer
        //  public UserDuringProcessing Responder { get; set; }
        public void CollectRequest()
        {
            Request req = request;
            if (request == null || ReferenceEquals(request, this)) return;
            //Responder = req.Responder;
            //Requester = req.Requester;
            //request = null;
        }

        public abstract void CollectResult();

        private string userSetResultComplete;
        public string WhyResultComplete
        {
            get
            {
                lock (this)
                {
                    string s = null, t = null;
                    var graphQuery = this.TopLevel;
                    if (graphQuery != null)
                    {
                        s = graphQuery.WhyToplevelComplete;
                    }
                    if (string.IsNullOrEmpty(s)) s = null;
                    var request1 = this.request;
                    if (request1 != null && !ReferenceEquals(request1, this)) t = request1.WhyRequestComplete;
                    t = (s == null) ? t : (s + " " + t);
                    t = (userSetResultComplete == null) ? t : (s + " " + userSetResultComplete);
                    return t;
                }
            } 
            set { userSetResultComplete = value; }
        }

        public DateTime EndedOn = DateTime.MaxValue;

        //  private readonly ParsedSentences ChatInput;
        public abstract Result result { get; }
        public ParsedSentences ChatOutput { get; private set; }

        //public override bool IsTraced { get; set; }

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<Unifiable> InputSentences
        {
            get { return ChatInput.EnglishSentences; }
        }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<Unifiable> NormalizedPaths
        {
            get { return ChatInput.NormalizedPaths; }
        }

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        public List<Unifiable> OutputSentences { get { return ChatOutput.EnglishSentences; }/* ; private set;*/ }

        //public Result ParentResult;

        /// <summary>
        /// The request from the user
        /// </summary>
        public Request request { get; set; }

        public bool Started { get; set; }

        public TemplateInfo TemplateOfRating { get; set; }

        public double TemplateRating { get; set; }
        private readonly List<TemplateInfo> ResultTemplates1 = new List<TemplateInfo>();

        /// <summary>
        /// The user for whom this is a result
        /// </summary>
        // public UserDuringProcessing Requester { get; set; }
        //  public OutputDelegate writeToLog { get; set; } // = RTPBot.writeDebugLine;
        public int TemplatesSucceeded { get; set; }

        public int OutputsCreated { get; set; }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public 
            
#if interface
            ResultImpl
#else
            Result
#endif // interface
            
            (string rawInput, UserDuringProcessing user, RTPBot bot, Request parent, UserConversationScope targetUser)
            : base(parent)
        {
            this.request = parent;
            ExitQueue = new CommitQueue();
            matchable = matchable ?? MakeMatchable(rawInput);
            SubQueries = new List<SubQuery>();
            MaxCanEvalResult = 10;
            request = parent;
            ChatInput = parent.ChatInput;
            //this.Requester = user;
            altResponder = targetUser;
            request.TargetBot = bot;
            ChatOutput = new ParsedSentences(bot.EnsureEnglish, null, MaxPrintResults);
            //OutputSentences = ChatOutput.SemanticSentences;
            writeToLog = writeToLog ?? user.WriteToUserTrace;
            writeToLog = writeToLog ?? request.WriteLine;
            //this.request.TheCurrentResult = this;
        }

        /* public GraphMaster Graph
        {
            get { return request.Graph; }
            set { request.Graph = value; }
        }
        */

        public GraphQuery TopLevel
        {
            get
            {
                Request request1 = request;
                if (request1 != null && !ReferenceEquals(request1, this)) return request1.TopLevelQuery;
                SubQuery cc = CurrentQuery;
                if (cc != null) return cc.TopLevel;
                return TopLevel;
            }
            set { TopLevel = value; }
        }


        public double Score
        {
            get { return TemplateRating; }
        }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        // public override bool IsTraced { get; set; }
        public string SetOutput
        {
            set
            {
                lock (OutputSentences)
                {
                    AddOutputResultSentences(value);
                    AlreadyUsed = value;
                    IsComplete = true;
                }
            }
        }

        public Unifiable That
        {
            get { return request.That; }
        }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Unifiable RawInput
        {
            get
            {
                return ChatInput.RawText;
                ;
            }
        }

        /// <summary>
        /// The result from the bot with logging and checking
        /// </summary>
        public Unifiable Output
        {
            get
            {
                lock (OutputSentences)
                    if (OutputSentenceCount > 0)
                    {
                        return RawOutput;
                    }
                    else
                    {
                        if (request.IsComplete(this))
                        {
                            writeToLog("ERROR: " + request.WhyComplete + " on " + RawInput +
                                       " from the user with an id: " + Requester.UserID);
                            return Unifiable.INCOMPLETE;
                            return TargetBot.TimeOutMessage;
                        }
                        else
                        {
                            var paths = Unifiable.CreateAppendable();
                            foreach (Unifiable pattern in NormalizedPaths)
                            {
                                //return pattern;
                                paths.Append(pattern.LegacyPath + Environment.NewLine);
                            }
                            writeToLog("The bot could not find any response for the input: " + RawInput +
                                       " with the path(s): " +
                                       Environment.NewLine + Unifiable.DescribeUnifiable(paths) + " from the user with an id: " +
                                       Requester.UserID);
                            return Unifiable.NULL;
                        }
                    }
            }
        }

        public string EnglishOutput
        {
            get { return ChatOutput.RawText; }
        }

        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        public Unifiable RawOutput
        {
            get { return ChatOutput.RawText; }
        }

        public bool IsEmpty
        {
            get { return OutputSentenceCount == 0; }
        }

        public abstract ISettingsDictionary RequesterChanges { get; }
        public abstract ISettingsDictionary ResponderChanges { get; }

        public int OutputSentenceCount
        {
            get { lock (OutputSentences) return OutputSentences.Count; }
        }

        public ISettingsDictionary RequesterPredicates
        {
            get { return CurrentQuery.RequesterPredicates; }
        }

        public ISettingsDictionary ResponderPredicates
        {
            get { return CurrentQuery.ResponderPredicates; }
        }

        public ISettingsDictionary TargetSettings
        {
            get { return CurrentQuery.TargetSettings; }
            set { CurrentQuery.TargetSettings = value; }
        }

        public UserConversationScope altResponder = null;
        public UserConversationScope Responder
        {
            get
            {
                if (altResponder != request.Responder)
                {
                    if (altResponder != null) return altResponder;
                }
                return request.Responder;
            }
          //  set { request.Responder = value; }
        }

        public SubQuery CurrentQuery
        {
            get
            {
                if (_CurrentQuery != null) return _CurrentQuery;
                if (false)
                {
                    Result r = ParentResult;
                    while (r != null)
                    {
                        SubQuery r_CurrentQuery = r.CurrentQuery;
                        if (r_CurrentQuery != null) return r_CurrentQuery;
                        r = r.ParentResult;
                    }
                }
                return null;
            }
            set
            {
                _CurrentQuery = value;
                request.CurrentQuery = value;
            }
        }

        public bool IsComplete
        {
            get
            {
                if (EndedOn < RTPBot.Now)
                    return true;
                if (request.IsTimedOutOrOverBudget)
                {
                    IsComplete = true;
                    return true;
                }
                return false;
            }
            set
            {
                EndedOn = value ? RTPBot.Now : DateTime.MaxValue;
                //_Durration = value ? Durration : TimeSpan.Zero;
            }
        }

        public bool IsSalient
        {
            get
            {
                if (OutputSentenceCount == 0) return false;
                if (IsNullOrEmpty(RawOutput)) return false;
                return true;
            }
        }

        public abstract InteractionResult PreviousInteraction { get; }
        public abstract InteractionResult NextInteraction { get; }
        public int MaxCanEvalResult { get; set; }

        public IList<TemplateInfo> ResultTemplates
        {
            get { return ResultTemplates1; }
        }

        internal bool useParentSF = false;
        private int _hasFailed = -1;
        public int HasFailed
        {
            get { return _hasFailed + (useParentSF ? ParentRequest.HasFailed : 0); }
            set
            {
                if (_hasFailed < 1)
                {
                    if (useParentSF)
                    {
                        if (value == 0)
                        {
                            ParentRequest.HasFailed -= 1;
                        }
                        else
                        {
                            ParentRequest.HasFailed += 1;
                        }
                    }
                }
                _hasFailed = value;
            }
        }

        private int _hasSuceeded = -1;
        public int HasSuceeded
        {
            get
            {
                int ret = _hasSuceeded + (useParentSF ? ParentRequest.HasSuceeded : 0);
                if (ret < 0)
                {
                    throw new InvalidOperationException();
                }
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
                            ParentRequest.HasSuceeded -= 1;
                        }
                        else
                        {
                            ParentRequest.HasSuceeded += 1;
                        }
                    }
                }
                _hasSuceeded = value;
            }
        }

        public CommitQueue ExitQueue { get; set; }
        bool HasExited;
        public void Exit()
        {
            lock (ExitQueue)
            {
                if (HasExited) return;
                this.HasExited = true;
            }
            if (request != null) request.Exit();
            //if (CatchLabel != null) CatchLabel.PopScope();
            ExitQueue.Commit(true);
        }

        public RTPBot TargetBot
        {
            get { return request.TargetBot; }
        }

        public UserConversationScope Requester
        {
            get { return request.Requester; }
           // set { request.Requester = value; }
        }

        public string _normalizedOutput;
        private readonly ParsedSentences ChatInput;
        private SubQuery _CurrentQuery;
        private string matchable;
        private int OutputPings;

        //public ChatLabel CatchLabel { get; set; }

        public Result ParentResult { get; set; }

        protected MasterRequest ParentRequest
        {
            get { return (MasterRequest)request; }
        }

        /// <summary>
        /// @see ChatOutput.TheMainSentence
        /// </summary>
        public string NormalizedOutput
        {
            get
            {
                string something;
                if (IsSomething(ChatOutput.TheMainSentence, out something)) return something;
                return "Nothing";
            }
        }

        public GraphMaster Graph
        {
            get { return request.Graph; }
        }

        public void AddSubqueries(GraphQuery queries)
        {
            if (queries.PatternCount == 0)
            {
                return;
            }
            var queriesGetBindings = queries.GetBindings();
            foreach (SubQuery query in queriesGetBindings)
            {
                if (IsTraced)
                {
                    writeToLog("AIMLTRACE SQ: " + this + " \n" + query.ToString().TrimStart());
                }
                if (!SubQueries.Contains(query)) SubQueries.Add(query);
            }
            DLRConsole.SystemFlush();
            Started = true;
        }

        public void AddOutputSentences(TemplateInfo ti, string unifiable)
        {
            AddOutputSentences0(ti, unifiable);
        }

        public bool IsTemplateNew(TemplateInfo ti, Unifiable tempOut)
        {
            if (ti == null) return false;
            var usedTemplates = ResultTemplates;
            if (usedTemplates.Contains(ti))
            {
                return false;
            }
            usedTemplates.Add(ti);
            string output = ti.TextSaved;
            lock (usedTemplates)
            {
                double ThisRating = ti.Rating;
                if (TemplateOfRating == null || TemplateRating < ThisRating)
                {
                    TemplateOfRating = ti;
                    TemplateRating = ThisRating;
                    writeToLog("AIMLTRACE: OUTPUT RATING={0} {2} TI: {1} \n U: {3}", ThisRating, ti, ti.Graph, output);
                }
                if (!IsNullOrEmpty(output))
                {
                    ti.TextSaved = output;
                }
                else
                {
                    ti.TextSaved = Unifiable.Empty;
                    return false;
                }

                if (IsNullOrEmpty(output))
                {
                    throw new Exception("EmptyUnmif for " + ti);
                }
            }
            return true;
        }

        private void AddOutputSentences0(TemplateInfo ti, string unifiable)
        {
            if (null == unifiable)
            {
                writeToLog("ERROR assing null output " + ti);
                if (ti == null) return;
                return;
            }
            unifiable = Trim(unifiable);
            if (unifiable == "")
            {
                writeToLog("ERROR assing '' output " + ti);
                return;
            }
            if (ti != null)
            {
                if (ti.TextSaved == unifiable)
                {
                }
                else if (ti.TextSaved != null)
                {
                    if (IsTraced) writeToLog("switching '" + ti.TextSaved + "' to '" + unifiable + "'");
                    ti.TextSaved = unifiable;
                }
                else
                {
                    ti.TextSaved = unifiable;
                }
            }
            AddOutputResultSentences(unifiable);
            //AddOutputSentences2(ti,unifiable);
        }
        private void AddOutputResultSentences( string unifiable)
        {
            OutputPings++;
            if (this.OutputPings > 1)
            {
                return;
            }
            ChatOutput.ClearOutput();
            AddOutputSentences11(unifiable);
        }
        private void AddOutputSentences11( string unifiable)
        {
            unifiable = Trim(unifiable).Replace("\n", " ").Replace("\r", " ");
            string[] sentNow = unifiable.Split(new[] {"<br/>", "&p;", "<p/>"}, StringSplitOptions.RemoveEmptyEntries);
            if (sentNow.Length == 1)
            {
                unifiable = sentNow[0];
            } else
            {
                foreach (var s in sentNow)
                {
                    AddOutputSentences11(s);
                }
                return;                
            }
            sentNow = GetSents(new[] { ". ", "? " }, unifiable).ToArray();
            if (sentNow.Length == 1)
            {
                unifiable = sentNow[0];
            }
            else
            {
                foreach (var s in sentNow)
                {
                    AddOutputSentences11(s);
                }
                return;
            }
            if (AlreadyUsed.Contains(unifiable))
            {
                return;
            }

            if (IsNullOrEmpty(unifiable))
            {
                return;
            }
            AlreadyUsed += unifiable;
            lock (OutputSentences)
            {
#if false
                if (unifiable == null || unifiable == "*" || unifiable == Unifiable.Empty)
                {
                    return;
                }
                int found = OutputSentences.IndexOf(unifiable);
                int c = OutputSentences.Count - 1;
                if (found == c)
                {
                    return;
                }
                if (found < 1)
                {
                    OutputSentences.Add(unifiable);
                    return;
                }
                OutputSentences.RemoveAt(found);
#endif
                if (unifiable.Contains("&"))
                {
                    OutputSentences.Remove(unifiable);
                }
                if (ContainsXml(unifiable))
                {
                    writeToLog("ERROR:  AddRssult: " + Requester.UserID + " " + unifiable);
                }
                EndedOn = RTPBot.Now;
                OutputSentences.Add(unifiable);
                return;
            }
        }
        public void AddOutputSentences2(TemplateInfo ti, string unifiable)
        {
            {
                bool isComplete = OutputSentences.Count >=
                                  ((QuerySettingsReadOnly) request.GetQuerySettings()).MinOutputs ||
                                  request.IsComplete(this);

                if (!isComplete) return;

                lock (ChatLabel.Labels)
                {
                    ChatLabel rd = this.CatchLabel;
                    if (rd == null) return;
                    if (!ChatLabel.IsFirst(rd)) return;
                    if (ti != null)
                    {
                        rd.TemplateInfo = ti;
                        rd.CreatedOutput = true;
                        rd.SubQuery = ti.Query;
                        rd.request = request;
                        // rd.KeepThrowing = true;
                        rd.TagHandler = ti.Query.LastTagHandler;
                        rd.result = (MasterResult) this;
                    }
                    throw rd;
                }

            }
        }

        private List<string> GetSents(string[] splitters, string unifiable)
        {
            List<string> strings = new List<string>();
            if (Trim(unifiable).Length == 0) return strings;
            int firstindex = unifiable.Length;
            string fsplitter = null;
            foreach (var splitter in splitters)
            {
                int index = unifiable.IndexOf(splitter);
                if (index == -1)
                {
                    continue;
                }
                if (index < firstindex)
                {
                    fsplitter = splitter;
                    firstindex = index;
                }
            }
            if (fsplitter == null || firstindex == -1)
            {
                strings.Add(unifiable);
                return strings;
            }
            string sub = unifiable.Substring(0, firstindex + fsplitter.Length - 1);
            strings.Add(Trim(sub));
            unifiable = unifiable.Substring(sub.Length);
            strings.AddRange(GetSents(splitters, unifiable));
            return strings;
        }

        public void AddResultFormat(string format, params object[] args)
        {
            lock (OutputSentences) OutputSentences.Add(SafeFormat(format, args));
        }

        /// <summary>
        /// Returns the raw output from the bot
        /// </summary>
        /// <returns>The raw output from the bot</returns>
        public override string ToString()
        {
            string whyComplete = WhyComplete;
            return request.ToRequestString() + " -> " + ToResultString() + " " +
                   (whyComplete != null ? " WhyComplete=" + whyComplete : "");
        }

        public abstract Unifiable GetInputSentence(int sentence);

        public string WhyComplete
        {
            get
            {
                string s = WhyResultComplete;
                if (!String.IsNullOrEmpty(s)) return s + " " + WhyRequestComplete;
                return WhyRequestComplete;
            }
        }


        protected string WhyRequestComplete
        {
            get { return  request.WhyRequestComplete; }
        }

        public string ToResultString()
        {
            string msg = "";
            if (!Started)
            {
                msg = "!Started ";
            }
            if (!IsComplete)
            {
                msg = "Incomplete ";
            }
            if (IsEmpty)
                return msg + "querycount=" + SubQueries.Count + " ";
            return msg + " \"" + Output + "\"";
        }

        public Unifiable GetOutputSentence(int sentence)
        {
            if (sentence == -1) return NormalizedOutput;
            sentence = OutputSentenceCount - sentence - 1;            
            lock (OutputSentences) return OutputSentences[sentence];
        }

        public void RotateUsedTemplates()
        {
            {
                var temps = ResultTemplates;
                if (temps == null) return;
                var tempsLock = temps;
                lock (tempsLock)
                {
                    temps = new List<TemplateInfo>(temps);
                    if (RotatedTemplate == temps.Count) return;
                    RotatedTemplate = temps.Count;
                    foreach (TemplateInfo info in temps)
                    {
                        info.GraphmasterNode.RotateTemplate(info);
                    }
                }
            }
        }

        public TemplateInfo ProofTemplate()
        {
            if (TemplateOfRating != null)
            {
                return TemplateOfRating;
            }
            var temps = ResultTemplates;
            if (temps == null) return null;
            var tempsLock = temps;
            if (temps.Count == 0)
            {
                return null;
            }
            lock (tempsLock)
            {
                return ResultTemplates[0];
            }
        }

        public void ResetAnswers(bool b)
        {
            lock (OutputSentences) OutputSentences.Clear();
            AlreadyUsed = "xtxtxtxtxtxtxtxtxxt";
            var temps = ResultTemplates1;
            if (b)
            {
                lock (SubQueries) if (SubQueries.Count > 0) SubQueries = new List<SubQuery>();
                if (temps != null) lock (temps) temps.Clear();
            }
        }

        public bool CanResultUseTemplate(TemplateInfo info)
        {
            return FoundInParents(info, ParentResult);
        }

        public OutputDelegate writeToLog { get; set; }

        public ChatLabel CatchLabel
        {
            get { return request.CatchLabel; }
            set { request.CatchLabel = value; }
        }

        private bool FoundInParents(TemplateInfo info, Result requestOrResult)
        {
            if (requestOrResult == null) return true;
            while (requestOrResult != null)
            {
                var resultUsedTemplates = requestOrResult.ResultTemplates;
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
            }
            return false;
        }

        public SituationInConversation ContextScope
        {
            get
            {
                ConversationScopeHolder currentScopeHolder;
                SituationInConversation scope = null;

                currentScopeHolder = CurrentQuery as ConversationScopeHolder;
                if (currentScopeHolder != null)
                {
                    scope = currentScopeHolder.ContextScope;
                    if (scope != null) return scope;
                }

                currentScopeHolder = ParentResult as ConversationScopeHolder;
                if (currentScopeHolder != null)
                {
                    scope = currentScopeHolder.ContextScope;
                    if (scope != null) return scope;
                }


                currentScopeHolder = ParentRequest as ConversationScopeHolder;
                if (currentScopeHolder != null)
                {
                    scope = currentScopeHolder.ContextScope;
                    if (scope != null) return scope;
                }

                return scope;    
            }
        }
    }
}