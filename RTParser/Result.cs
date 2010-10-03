using System;
using System.Collections.Generic;
using System.Threading;
using AIMLbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser.Utils;
using RTParser.Variables;
using UPath = RTParser.Unifiable;


namespace RTParser
{
    public interface Result : Request
    {
        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        //     RTPBot TargetBot { get; set; }
        /// The user that is providing the <that/> answer
        //        User Responder { get; set; }
        //        string WhyComplete { get; set; }

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
        ///User Requester { get; set; }

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
        Unifiable RawInput { get; }

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
        ISettingsDictionary Predicates { get; }
        // SubQuery CurrentQuery { get; set; }
        // bool IsComplete { get; set; }
        bool IsSalient { get; }
        //    IList<TemplateInfo> UsedTemplates { get; }
        //   ChatLabel CatchLabel { get; set; }
        Result ParentResult { get; }
        string NormalizedOutput { get; }
        //      HashSet<SubQuery> AllSubQueries { get; set; }
        //       Proof Proof { get; set; }
        //       int MaxInputs { get; set; }
        //int depth { get; set; }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        //     Unifiable rawInput { get; }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        //      ParsedSentences ChatInput { get; set; }

        //   Request ParentRequest { get; set; }

        /// <summary>
        /// The time at which this request was created within the system
        /// </summary>
        //     DateTime StartedOn { get; set; }

        /// <summary>
        /// The user who made this request
        /// </summary>
        //    User Requester { get; set; }

        /// <summary>
        /// The user who is the target of this request
        /// </summary>
        //   User Responder { get; set; }

        /// <summary>
        /// The Proccessor to which the request is being made
        /// </summary>
        //   RTPBot TargetBot { get; set; }

        /// <summary>
        /// The final result produced by this request
        /// </summary>
        /*       Result CurrentResult { get; set; }

               ISettingsDictionary TargetSettings { get; set; }
               TimeSpan TimeOut { get; set; }
               bool IsTimedOutOrOverBudget { get; }
               ISettingsDictionary ResponderPredicates { get; set; }
               bool GraphsAcceptingUserInput { get; set; }
               LoaderOptions LoadOptions { get; set; }
               AIMLLoader Loader { get; }
               string Filename { get; set; }
               string LoadingFrom { get; set; }
               GraphMaster Graph { get; set; }
               */
        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        /*
        string GraphName { get; set; }

        ICollection<GraphMaster> DisallowedGraphs { get; }
        Unifiable Flags { get; set; }
        GraphQuery TopLevel { get; set; }
        Unifiable Topic { get; set; }
        IList<Unifiable> Topics { get; }
        IEnumerable<Unifiable> ResponderOutputs { get; }
        ISettingsDictionary RequesterPredicates { get; }
        // IList<TemplateInfo> UsedTemplates { get; }
        DateTime TimesOutAt { get; set; }
        OutputDelegate writeToLog { get; set; }
        int DebugLevel { get; set; }
        bool IsToplevelRequest { get; set; }
        Unifiable That { get; set; }
        PrintOptions WriterOptions { get; set; }
        RequestImpl ParentMostRequest { get; }
        AIMLTagHandler LastHandler { get; set; }
        ChatLabel PushScope { get; }
        // ChatLabel CatchLabel { get; }
        SideEffectStage Stage { get; set; }
        TimeSpan TimeOutFromNow { set; }
        IList<Result> UsedResults { get; set; }
        SubQuery CurrentQuery { get; set; }
        */
        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        //  public abstract GraphMaster Graph { get; set; }
        /// <summary>
        /// If the query is being traced
        /// </summary>
        //bool IsTraced { get; set; }
        /// <summary>
        /// Some patterns implies multiple templates
        /// </summary>
        //  bool ProcessMultipleTemplates { get; set; }

        /// <summary>
        /// After the first pattern, if the min/maxes are not satisfied.. 
        /// Try a new pattern
        /// </summary>
        //   bool ProcessMultiplePatterns { get; set; }

        /// <summary>
        /// the number of "successfull" (non-empty) templates after "eval"
        /// </summary>
        // int MinOutputs { get; set; }

        //int MaxOutputs { get; set; }

        /// <summary>
        /// The number of sets before the query is stopped
        /// </summary>
        int MinSetVars { get; set; }

        int MaxSetVars { get; set; }

        /// <summary>
        /// The number of gets before the query is stopped
        /// </summary>
        int MinGetVars { get; set; }

        int MaxGetVars { get; set; }

        double TemplateRating { get; set; }
        bool Started { get; set; }
        TimeSpan Durration { get; }

        /// <summary>
        /// The number of templates to harvest in query stage (should be at least one)
        /// </summary>
        //  int MinTemplates { get; set; }

        //  int MaxTemplates { get; set; }

        /// <summary>
        /// The number of patterns to harvest in query stage (should be at least one)
        /// </summary>
        //    int MinPatterns { get; set; }

        //    int MaxPatterns { get; set; }

        /// <summary>
        /// The number srai's one can decend generation stage
        /// the "Min" is what is starts out with (defualt 0)
        /// </summary>
        ///     SettingMinMaxCurrent<int> SraiDepth { get; set; }
        //
        //      int UseLuceneForGetMaxDepth { get; set; }
        //      int UseLuceneForSetMaxDepth { get; set; }

        void CollectRequest();

        string WhyResultComplete { get; }
        IList<TemplateInfo> ResultTemplates { get; }
        void AddSubqueries(GraphQuery queries);
        void AddOutputSentences(TemplateInfo ti, string unifiable);
        //bool IsTemplateNew(TemplateInfo ti);
        //void AddOutputSentences0(TemplateInfo ti, string unifiable);
        void AddResultFormat(string format, params object[] args);

        /// <summary>
        /// Returns the raw output from the bot
        /// </summary>
        /// <returns>The raw output from the bot</returns>
        string ToString();
        Unifiable GetOutputSentence(int sentence);
        void RotateUsedTemplates();
        void ReduceMinMaxesForSubRequest(QuerySettingsReadOnly parent);
        //   GraphMaster GetGraph(string value);
        //  void ExcludeGraph(string srai);
        void ExcludeGraph(GraphMaster getGraph);
        /*
        void AddOutputSentences(TemplateInfo ti, string nai, Result result);
            int GetCurrentDepth();
            Unifiable grabSetting(string name);
            bool addSetting(string name, Unifiable value);
            void WriteLine(string s, params object[] args);
            bool IsComplete(Result result1);*/
        string WhyNoSearch(Result result1);
        //   QuerySettingsSettable GetQuerySettings();
        //  MasterResult CreateResult(Request parentReq);
        //  MasterRequest CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, User requestee);
        //     bool CanUseTemplate(TemplateInfo info, Result result);
        //void writeToLog0(string message, params object[] args);
        string RequestThat();
        //     ISettingsDictionary GetSubstitutions(string named, bool createIfMissing);
        //      ISettingsDictionary GetDictionary(string named);
        //     void AddUndo(Action undo);
        //       void UndoAll();
        //       void Commit();
        //       void AddSideEffect(string name, ThreadStart action);
        //       Dictionary<string, GraphMaster> GetMatchingGraphs(string graphname, GraphMaster master);
        //       ISettingsDictionary GetDictionary(string named, ISettingsDictionary dictionary);
        //ISettingsDictionary GetDictionary0(string named, ISettingsDictionary dictionary);
        ISettingsDictionary CheckedValue(string named, ISettingsDictionary d);
        //      void AddSubResult(Result subResult);
        //      void IncreaseLimits(int minsAndMaxes);
        void ResetAnswers(bool b);
    }

    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
    public abstract class ResultImpl : RequestImpl, Result
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

        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        //   public RTPBot TargetBot { get; set; }

        /// The user that is providing the <that/> answer
        //  public User Responder { get; set; }

        public void CollectRequest()
        {
            Request req = request;
            if (request == null || request == this) return;
            Responder = req.Responder;
            Requester = req.Requester;
            //request = null;
        }
/*
        public override string WhyComplete
        {
            get
            {
                if (Started)
                {
                    return WhyIsResultComplete() ?? WhyRequestComplete;                    
                }
                return base.WhyComplete;
            }
            set { base.WhyComplete = value; }
        }
        */

        public override string WhyResultComplete
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
                    if (request1 != null && request1 != this) t = request1.WhyRequestComplete;
                    return s == null ? t : (s + " " + t);
                }
            }
        }

        public DateTime EndedOn = DateTime.MaxValue;

        //  private readonly ParsedSentences ChatInput;
        public ParsedSentences ChatOutput { get; private set; }

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
        public List<Unifiable> OutputSentences { get; private set; }

        //public Result ParentResult;

        /// <summary>
        /// The request from the user
        /// </summary>
        public Request request
        {
            get { return this; }
        }

        public bool Started { get; set; }

        public TemplateInfo TemplateOfRating { get; set; }
        public double TemplateRating { get; set; }
        private readonly List<TemplateInfo> ResultTemplates1 = new List<TemplateInfo>();

        /// <summary>
        /// The user for whom this is a result
        /// </summary>
       // public User Requester { get; set; }

        //  public OutputDelegate writeToLog { get; set; } // = RTPBot.writeDebugLine;
        public int TemplatesSucceeded { get; set; }
        public int OutputsCreated { get; set; }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public ResultImpl(string rawInput, User user, RTPBot bot, Request parent, User targetUser)
            : base(rawInput, user, bot, parent, targetUser)
        {
            SubQueries = new List<SubQuery>();
            ///ChatInput = request.ChatInput;
            this.Requester = user;
            this.TargetBot = bot;
            //this.request = request;
            ChatOutput = new ParsedSentences(bot.EnsureEnglish, MaxPrintResults);
            OutputSentences = ChatOutput.SemanticSentences;
            //writeToLog = writeToLog ?? user.WriteLine;
            //writeToLog = writeToLog ?? request.WriteLine;
            // this.request.CurrentResult = this;
        }

        /* public GraphMaster Graph
        {
            get { return request.Graph; }
            set { request.Graph = value; }
        }
        */

        public override GraphQuery TopLevel
        {
            get
            {
                Request request1 = request;
                if (request1 != null && request1 != this) return request1.TopLevel;
                SubQuery cc = base.CurrentQuery;
                if (cc != null) return cc.TopLevel;
                return base.TopLevel;
            }
            set { base.TopLevel = value; }
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
                    AlreadyUsed = value;
                    OutputSentences.Clear();
                    OutputSentences.Add(value);
                    IsComplete = true;
                }
            }
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
                                       Environment.NewLine + paths.ToString() + " from the user with an id: " +
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

        public int OutputSentenceCount
        {
            get { lock (OutputSentences) return OutputSentences.Count; }
        }

        public ISettingsDictionary Predicates
        {
            get { return Requester.Predicates; }
        }

        public override SubQuery CurrentQuery
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
                base.CurrentQuery = value;
            }
        }

        public bool IsComplete
        {
            get { return EndedOn < DateTime.Now; }
            set
            {
                EndedOn = value ? DateTime.Now : DateTime.MaxValue;
                _Durration = value ? Durration : TimeSpan.Zero;
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

        public IList<TemplateInfo> ResultTemplates
        {
            get { return ResultTemplates1; }
        }

        public string _normalizedOutput;
        //public ChatLabel CatchLabel { get; set; }

        public Result ParentResult
        {
            get { return (Result)ParentRequest; }
        }

        public string NormalizedOutput
        {
            get
            {
                string something;
                if (IsSomething(ChatOutput.TheMainSentence, out something)) return something;
                return "Nothing";
            }
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

        public void AddOutputSentences0(TemplateInfo ti, string unifiable)
        {
            if (null == unifiable)
            {
                writeToLog("ERROR assing null output " + ti);
                if (ti == null) return;
                return;
            }
            unifiable = unifiable.Trim();
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
            unifiable = unifiable + " ";
            if (false && unifiable.Length > 2 && (unifiable.Contains("<br/>") || unifiable.Contains("&p;")))
            {
                string[] sents = unifiable.Split(new[] { "<br/>", "&p;" }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var s in sents)
                {
                    AddOutputSentences0(ti, s);
                }
                return;
            }
            if (false && unifiable.Length > 2 && (unifiable.Contains(". ") || unifiable.Contains("? ")))
            {
                string[] sents = unifiable.Split(new[] { ". ", "? " }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var s in sents)
                {
                    AddOutputSentences0(ti, s);
                }
                return;
            }
            if (AlreadyUsed.Contains(unifiable)) return;
            if (!IsTemplateNew(ti, unifiable))
            {
                //return;
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
                EndedOn = DateTime.Now;
                OutputSentences.Add(unifiable);
                return;
                bool isComplete = OutputSentences.Count >=
                                  ((QuerySettingsReadOnly)request.GetQuerySettings()).MinOutputs ||
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
                        rd.result = (MasterResult)this;
                    }
                    throw rd;
                }
            }
        }

        public void AddResultFormat(string format, params object[] args)
        {
            lock (OutputSentences) OutputSentences.Add(string.Format(format, args));
        }

        /// <summary>
        /// Returns the raw output from the bot
        /// </summary>
        /// <returns>The raw output from the bot</returns>
        public override string ToString()
        {
            string whyComplete = WhyComplete;
            return ToResultString() + " " + ToRequestString() + " " + (whyComplete != null ? " WhyComplete=" + whyComplete : "");
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
            lock (OutputSentences) return OutputSentences[sentence];
        }

        public void RotateUsedTemplates()
        {
            {
                var temps = ResultTemplates;
                if (temps != null)
                {
                    if (RotatedTemplate == temps.Count) return;
                    RotatedTemplate = temps.Count;
                    foreach (TemplateInfo info in temps)
                    {
                        info.GraphmasterNode.RotateTemplate(info);
                    }
                }
            }
        }

        public void ResetAnswers(bool b)
        {
            lock (OutputSentences) OutputSentences.Clear();
            AlreadyUsed = "xtxtxtxtxtxtxtxtxxt";
            var temps = ResultTemplates1;
            if (temps != null)
            {
                if (b) temps.Clear();
            }
            if (SubQueries.Count > 0)
            {
                if (b) SubQueries = new List<SubQuery>();
            }

        }
    }
}