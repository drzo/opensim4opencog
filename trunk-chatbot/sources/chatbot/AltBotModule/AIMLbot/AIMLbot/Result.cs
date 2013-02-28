using System;
using System.Collections.Generic;
using System.Text;
using AIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using AltAIMLbot;
using AltAIMLbot.Variables;
using UPath = AltAIMLbot.Unifiable;
using MasterRequest = AltAIMLbot.Utils.Request;

namespace AltAIMLbot
{
    /// <summary>
    /// Encapsulates information about the result of a request to the bot
    /// </summary>
    public abstract class Result
    {
#if true
        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<SubQuery> SubQueries { get; set; }

        public static int MaxPrintResults = 20;
        private string AlreadyUsed = "xtxtxtxtxtxtxtxtxxt";
        private int RotatedTemplate;

        //public double TemplateRating { get; set; }

        public TimeSpan Duration
        {
            get { return request.Durration; }
            set { request.Durration = value; }
        }


        public string StartGraphName
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
        public void FreeRequest()
        {
            Request req = request;
            if (request == null || ReferenceEquals(request, this)) return;
            //Responder = req.Responder;
            //Requester = req.Requester;
            //request = null;
        }

        //public abstract void FreeResult();

        private string userSetResultComplete;

        public string WhyResultComplete
        {
            get
            {
                lock (this)
                {
                    string s = null, t = null;
                    var graphQuery = this.TopLevelQuery;
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
        public Result result
        {
            get { return (Result) this; }
        }

        private AltAIMLbot.Utterance _chatOutput;

        public AltAIMLbot.Utterance ChatOutput
        {
            get
            {
                if (_chatOutput == null)
                {
                    return null;
                }
                return _chatOutput;
            }
            private set { _chatOutput = value; }
        }

        //public override bool IsTraced { get; set; }

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<Unifiable> InputSentences
        {
            get { return ChatInput.EnglishSentences; }
        }

        internal void ClearInput()
        {
            ChatInput.ClearSentences();
        }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<Unifiable> GraphMasterPaths = new List<Unifiable>();

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        public List<Unifiable> OutputSentences
        {
            get { return ChatOutput.EnglishSentences; } /* ; private set;*/
        }

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
        public Result(User user, AltBot bot, Request request)
            : this(request.rawInput, user, bot, request, request.Responder)
        {
            this.request = request;
            this.request.result = ((Result) this);
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(string rawInput, User user, AltBot bot, Request parent, User targetUser)
            : base()
        {
            this.request = parent;
            ExitQueue = new CommitQueue();
            matchable = matchable ?? StaticAIMLUtils.MakeMatchable(rawInput);
            SubQueries = new List<SubQuery>();
            MaxCanEvalResult = 10;
            request = parent;
            ChatInput = parent.ChatInput;
            //this.Requester = user;
            altResponder = targetUser;
            request.TargetBot = bot;
            ChatOutput = new AltAIMLbot.Utterance(bot.EnsureEnglish, user, altResponder, null, MaxPrintResults)
                             {InResponse = ChatInput};
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

        public GraphQuery TopLevelQuery
        {
            get
            {
                Request request1 = request;
                if (request1 != null && !ReferenceEquals(request1, this)) return request1.TopLevelQuery;
                SubQuery cc = CurrentQuery;
                if (cc != null) return cc.TopLevel;
                return TopLevelQuery;
            }
            set { TopLevelQuery = value; }
        }


        public double Score
        {
            get { return request.TopLevelScore; }
        }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        // public override bool IsTraced { get; set; }
        public string SetOutput
        {
            set
            {
                IsComplete = true;
                AlreadyUsed = value;
                if (string.IsNullOrEmpty(value))
                {
                    return;
                }
                lock (OutputSentences)
                {
                    OutputPings = 0;
                    // OutputSentences.Clear();
                    AddOutputResultSentences(value, true);
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
                        if (request.IsComplete(((Result) this)))
                        {
                            writeToLog("ERROR: " + request.WhyComplete + " on " + RawInput +
                                       " from the user with an id: " + Requester.UserID);
                            return Unifiable.INCOMPLETE;
                            return TargetBot.TimeOutMessage;
                        }
                        else
                        {
                            var paths = Unifiable.CreateAppendable();
                            foreach (Unifiable pattern in GraphMasterPaths)
                            {
                                //return pattern;
                                paths.Append(pattern.LegacyPath + Environment.NewLine);
                            }
                            writeToLog("The bot could not find any response for the input: " + RawInput +
                                       " with the path(s): " +
                                       Environment.NewLine + Unifiable.DescribeUnifiable(paths) +
                                       " from the user with an id: " +
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

        public User altResponder = null;

        public User Responder
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
                if (EndedOn < AltBot.Now)
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
                EndedOn = value ? AltBot.Now : DateTime.MaxValue;
                //_Durration = value ? Durration : TimeSpan.Zero;
            }
        }

        public bool IsSalient
        {
            get
            {
                if (OutputSentenceCount == 0) return false;
                if (Unifiable.IsNullOrEmpty(RawOutput)) return false;
                return true;
            }
        }

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
        private bool HasExited;

        public void Exit()
        {
            lock (ExitQueue)
            {
                if (HasExited)
                {
                    TargetBot.RaiseError("Result.Exit() called too many times! " + this);
                    return;
                }
                this.HasExited = true;
            }
           //if (request != null) request.Exit();
           //if (CatchLabel != null) CatchLabel.PopScope();
           ExitQueue.Commit(true);
        }

        public AltBot TargetBot
        {
            get { return request.TargetBot; }
        }

        public User Requester
        {
            get { return request.Requester; }
            // set { request.Requester = value; }
        }

        public string _normalizedOutput;
        public readonly AltAIMLbot.Utterance ChatInput;
        internal SubQuery _CurrentQuery;
        private string matchable;
        private int OutputPings;
        public char resultCount;

        public bool isValidOutput
        {
            get
            {
                if (OutputSentences.Count > 0)
                {
                    return true;
                }
                else
                {
                    if (resultCount > 0)
                    {
                        return true;
                    }
                    return false;
                }
            }
        }

        //public ChatLabel CatchLabel { get; set; }

        public Result ParentResult { get; set; }

        protected MasterRequest ParentRequest
        {
            get { return (MasterRequest) request; }
        }

        /// <summary>
        /// @see ChatOutput.TheMainSentence
        /// </summary>
        public Unifiable NormalizedOutput
        {
            get
            {
                Unifiable something;
                if (Unifiable.IsSomething(ChatOutput.TheMainSentence, out something)) return something;
                return Unifiable.MISSING;
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
                if (request.IsTraced)
                {
                    // writeToLog("AIMLTRACE SQ: " + this + " \n" + query.ToString().TrimStart());
                }
                if (!SubQueries.Contains(query)) SubQueries.Add(query);
            }
            DLRConsole.SystemFlush();
            Started = true;
        }

        public void AddOutputSentences(string sentence)
        {
            this.resultCount++;
            List<string> sents = StaticAIMLUtils.SentenceBreaker(sentence, null);
            if (sents.Count == 0 && sentence == " , ")
            {
                if (resultCount == 1)
                {
                    OutputSentencesAdd(sentence);
                }
                return;
            }
            foreach (var s in sents)
            {
                if (s.Contains(". "))
                {

                }
                OutputSentencesAdd(s);
            }
        }

        public string BestSentence
        {
            get
            {
                string last = "";
                foreach (string s in OutputSentences)
                {
                    if (s.EndsWith("?")) return s;
                    last = s;
                }
                return last;
            }
        }

        public string LastSentence
        {
            get
            {
                string last = "";
                foreach (string s in OutputSentences)
                {
                    last = s;
                }
                return last;
            }
        }

        public void AddOutputSentences(TemplateInfo ti, string unifiable, double score)
        {
            AddOutputSentences0(ti, unifiable, score);
        }

        /*
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
                        double ThisRating = ti.TemplateRating;
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
        */

        private void AddOutputSentences0(TemplateInfo ti, string unifiable, double score)
        {
            if (null == unifiable)
            {
                writeToLog("ERROR assing null output " + ti);
                if (ti == null) return;
                return;
            }
            unifiable = StaticAIMLUtils.Trim(unifiable);
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
                    if (request.IsTraced) writeToLog("switching '" + ti.TextSaved + "' to '" + unifiable + "'");
                    ti.TextSaved = unifiable;
                }
                else
                {
                    ti.TextSaved = unifiable;
                }
            }

            bool addToFront = false;
            if (score > TemplateRating)
            {
                if (ti != null) TemplateOfRating = ti;
                TemplateRating = score;
                addToFront = true;
                OutputSentences.Clear();
            }
            else
            {
                return;
            }
            AddOutputResultSentences(unifiable, addToFront);
            //AddOutputSentences2(ti,unifiable);
        }

        private void AddOutputResultSentences(string unifiable, bool addToFront)
        {
            OutputPings++;
            if (this.OutputPings > 1)
            {
                //   return;
            }
            //ChatOutput.ClearSentences();
            AddOutputSentences11(unifiable, addToFront);
        }

        private void AddOutputSentences11(string unifiable, bool addToFront)
        {
            unifiable = StaticAIMLUtils.Trim(unifiable).Replace("\n", " ").Replace("\r", " ");
            string[] sentNow = unifiable.Split(new[] {"<br/>", "&p;", "<p/>"}, StringSplitOptions.RemoveEmptyEntries);
            if (AlreadyUsed.Contains(unifiable))
            {
                return;
            }
            if (sentNow.Length == 1)
            {
                unifiable = sentNow[0];
            }
            else
            {
                AddOutputs(sentNow, addToFront);
                return;
            }
            sentNow = GetSents(new[] {". ", "? "}, unifiable).ToArray();
            if (sentNow.Length == 1)
            {
                unifiable = sentNow[0];
            }
            else
            {
                AddOutputs(sentNow, addToFront);
                return;
            }

            if (Unifiable.IsNullOrEmpty(unifiable))
            {
                return;
            }
            AlreadyUsed += unifiable;
            lock (OutputSentences)
            {
                if (unifiable.Contains("&"))
                {
                    OutputSentences.Remove(unifiable);
                }
                if (StaticAIMLUtils.ContainsXml(unifiable))
                {
                    writeToLog("ERROR:  AddRssult: " + Requester.UserID + " " + unifiable);
                }
                EndedOn = AltBot.Now;
                if (addToFront)
                {
                    OutputSentencesInsert(0, unifiable);
                }
                else
                {
                    OutputSentencesAdd(unifiable);
                }
                return;
            }
        }


        public void SetOutputSentence(int sent, string data)
        {
            OutputSentencesInsert(sent, data);
        }

        private void OutputSentencesInsert(int i, string unifiable)
        {
            lock (OutputSentences)
            {
                if (i == OutputSentences.Count)
                {
                    OutputSentencesAdd(unifiable);
                }
                else
                {
                    if (OutputSentences.Contains(unifiable) /*|| oneLastSentence == unifiable*/)
                    {
                        if (i < OutputSentences.Count)
                        {
                            var prev = OutputSentences[i];
                            if (prev == unifiable)
                            {
                                return;
                            }
                        }
                        return;
                    }
                    OutputSentences.Insert(i, unifiable);
                }
            }
        }

        private void AddOutputs(IEnumerable<string> sentNow, bool addToFront)
        {
            var OutputSentencesBefore = new List<Unifiable>(OutputSentences);
            if (addToFront) OutputSentences.Clear();
            foreach (var s in sentNow)
            {
                AddOutputSentences11(s, false);
            }
            if (addToFront) OutputSentencesAddRange(OutputSentencesBefore);
        }

        private void OutputSentencesAddRange(List<Unifiable> l)
        {
            OutputSentences.AddRange(l);
        }

        private List<string> GetSents(string[] splitters, string unifiable)
        {
            List<string> strings = new List<string>();
            if (StaticAIMLUtils.Trim(unifiable).Length == 0) return strings;
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
            strings.Add(StaticAIMLUtils.Trim(sub));
            unifiable = unifiable.Substring(sub.Length);
            strings.AddRange(GetSents(splitters, unifiable));
            return strings;
        }

        public void AddResultFormat(string format, params object[] args)
        {
            lock (OutputSentences) OutputSentencesAdd(DLRConsole.SafeFormat(format, args));
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
            get { return request.WhyRequestComplete; }
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

        public void ResetAnswers(bool clearSubQueries)
        {
            lock (OutputSentences) OutputSentences.Clear();
            AlreadyUsed = "xtxtxtxtxtxtxtxtxxt";
            var temps = ResultTemplates1;
            if (clearSubQueries)
            {
                lock (SubQueries) if (SubQueries.Count > 0) SubQueries = new List<SubQuery>();
                if (temps != null) lock (temps) temps.Clear();
            }
        }

        public bool CanResultUseTemplate(TemplateInfo info)
        {
            return true;
            return FoundInParents(info, ParentResult);
        }

        public OutputDelegate writeToLog { get; set; }

        public ChatLabel CatchLabel
        {
            get { return request.CatchLabel; }
            set { request.CatchLabel = value; }
        }

        public bool IsTraced
        {
            get { return request.IsTraced; }
            set { request.IsTraced = true; }
        }

        private bool FoundInParents(TemplateInfo info, Result requestOrResult)
        {
            return false;
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


#else
    /// <summary>
    /// The bot that is providing the answer
    /// </summary>
        public AltBot bot;

        /// <summary>
        /// The user for whom this is a result
        /// </summary>
        public User user;

        /// <summary>
        /// The request from the user
        /// </summary>
        public Request request;


        /// <summary>
        /// The raw input from the user
        /// </summary>
        public string RawInput
        {
            get
            {
                return this.request.rawInput;
            }
        }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<string> NormalizedPaths = new List<string>();

        /// <summary>
        /// The amount of time the request took to process
        /// </summary>
        public TimeSpan Duration;

        /// <summary>
        /// The result from the bot with logging and checking
        /// </summary>
        public string Output
        {
            get
            {
                if (OutputSentences.Count > 0)
                {
                    return this.RawOutput;
                }
                else
                {
                    if (this.request.hasTimedOut)
                    {
                        return this.bot.TimeOutMessage;
                    }
                    else
                    {
                        if (resultCount > 0)
                        {
                            return string.Empty;
                        }
                        StringBuilder paths = new StringBuilder();
                        foreach (string pattern in this.NormalizedPaths)
                        {
                            paths.Append(pattern + Environment.NewLine);
                        }
                        this.bot.writeToLog("The bot could not find any response for the input: " + this.RawInput + " with the path(s): " + Environment.NewLine + paths.ToString() + " from the user with an id: " + this.user.UserID);
                        return string.Empty;
                    }
                }
            }
        }

        public bool isValidOutput
        {
            get
            {
                if (OutputSentences.Count > 0)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }

        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        public string RawOutput
        {
            get
            {
                StringBuilder result = new StringBuilder();
                foreach (string sentence in OutputSentences)
                {
                    string sentenceForOutput = sentence.Trim();
                    if (!this.checkEndsAsSentence(sentenceForOutput))
                    {
                        sentenceForOutput += ".";
                    }
                    result.Append(sentenceForOutput + " ");
                }
                return result.ToString().Trim();
            }
        }

        /// <summary>
        /// The subQueries processed by the bot's graphmaster that contain the templates that 
        /// are to be converted into the collection of Sentences
        /// </summary>
        public List<Utils.SubQuery> SubQueries = new List<Utils.SubQuery>();

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        public List<string> OutputSentences = new List<string>();

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public List<string> InputSentences = new List<string>();

        public int MaxCanEvalResult { get; set; }
        private string matchable;
        public static int MaxPrintResults = 10;

        public readonly Utterance ChatInput;
        public User altResponder = null;
        private Utterance _chatOutput;
        public Utterance ChatOutput
        {
            get
            {
                if (_chatOutput == null)
                {
                    return null;
                }
                return _chatOutput;
            }
            private set { _chatOutput = value; }
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(User user, AltBot bot, Request request)
        {
            this.user = user;
            this.bot = bot;
            this.request = request;
            this.request.result = this;
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="user">The user for whom this is a result</param>
        /// <param name="bot">The bot providing the result</param>
        /// <param name="request">The request that originated this result</param>
        public Result(string rawInput, User user, AltBot bot, Request parent, User targetUser)
        {
            this.request = parent;
            this.user = user;
            this.bot = bot;
            this.request.result = this;
            //ExitQueue = new CommitQueue();
            matchable = matchable ?? MakeMatchable(rawInput);
            SubQueries = new List<AltAIMLbot.Utils.SubQuery>();
            MaxCanEvalResult = 10;
            request = parent;
            ChatInput = parent.ChatInput;
            // //this.Requester = user;
            altResponder = targetUser;
            request.TargetBot = bot;
            ChatOutput = new Utterance(bot.EnsureEnglish, user, altResponder, null, MaxPrintResults)
                              {InResponse = ChatInput};
            // //OutputSentences = ChatOutput.SemanticSentences;
            //writeToLog = writeToLog ?? user.WriteToUserTrace;
            //writeToLog = writeToLog ?? request.WriteLine;
            // //this.request.TheCurrentResult = this;
        }

        /// <summary>
        /// Returns the raw output from the bot
        /// </summary>
        /// <returns>The raw output from the bot</returns>
        public override string ToString()
        {
            return this.RawOutput;
        }

        /// <summary>
        /// Checks that the provided sentence ends with a sentence splitter
        /// </summary>
        /// <param name="sentence">the sentence to check</param>
        /// <returns>True if ends with an appropriate sentence splitter</returns>
        private bool checkEndsAsSentence(string sentence)
        {
            foreach (string splitter in AltBot.Splitters)
            {
                if (sentence.Trim().EndsWith(splitter))
                {
                    return true;
                }
            }
            return false;
        }

        // === PATCH AREA ===
        public TimeSpan Durration
        {
            get { return request.Durration; }
        }

        public  void FreeResult(){}

        static public T GetSentence<T>(IList<T> outputSentences, int sentence)
        {
            int count = outputSentences.Count;
            if ((sentence >= 0) & (sentence < count))
            {
                return outputSentences[count - sentence - 1];
            }
            return default(T);
        }
        public string GetInputSentence(int sentence)
        {
            return GetSentence(InputSentences, sentence);
        }
        public string GetOutputSentence(int sentence)
        {
            return GetSentence(OutputSentences, sentence);
        }

        public virtual Result result { get { return this; } }

        public object Requester
        {
            get { throw new NotImplementedException(); }
            // set { request.Requester = value; }
        }
        private string userSetResultComplete;
        public int resultCount;

        public string LastSentence
        {
            get
            {
                string last = "";
                foreach (string s in OutputSentences)
                {
                    if (s.EndsWith("?")) return s;
                    last = s;
                }
                return last;
            }
        }

        public string WhyResultComplete
        {
            get
            {
                throw new NotImplementedException();
            }
            set { userSetResultComplete = value; }
        }

        #region StaticXMLUTils
        public static bool ContainsXml(string s)
        {
            if (s.Contains(">") && s.Contains("<")) return true;
            if (s.Contains("&") && s.Contains(";"))
            {
                return true;
            }
            return false;
        }
        public static string ReplaceMap(string strTrim, string[][] pairs)
        {
            foreach (string[] pair in pairs)
            {
                string pair1 = pair[0];
                string pair2 = pair[1];
                if (strTrim.Contains(pair1))
                {
                    strTrim = ReplaceAll(strTrim, pair1, pair2);
                    if (strTrim.Contains(pair1))
                    {
                        strTrim = strTrim.Replace(pair1, pair2);
                    }
                }
            }
            return strTrim;
        }
        public static string ReplacePairs(string strTrim, params string[] pairs)
        {
            int index = 0;
            int pairsLength = pairs.Length;
            while (index < pairsLength)
            {
                strTrim = ReplaceAll(strTrim, pairs[index++], pairs[index++]);
            }
            return strTrim;
        }
        /// <summary>
        /// Helps keep new strings garbage collectable by using the previous
        /// version if they are the same string
        /// </summary>
        /// <param name="before"></param>
        /// <param name="after"></param>
        /// <returns></returns>
        public static string OlderReference(string before, string after)
        {
            if (after == before)
            {
                return before;
            }
            return after;
        }

        public static string ReplaceAll(string source, string b, string a)
        {
            string sourceReplace = source.Replace(b, a);
            var result = OlderReference(source, sourceReplace);
            while (result.Contains(b))
            {
                sourceReplace = result.Replace(b, a);
                result = OlderReference(source, sourceReplace);
            }
            return result;
        }

        public static string ReTrimAndspace(string substitute)
        {
            if (substitute == null) return null;
            var s = OlderReference(substitute, substitute.Replace("> ", ">").Replace(" <", "<").Replace("  ", " "));
            if (s.Length == 1)
            {
                if (s != " ") return s;
                return s;
            }
            return Trim(substitute);
        }
        public static bool Unused(char arg1, char arg2)
        {
            throw new NotImplementedException();
            return false;
        }

        public static string Trim(string param1)
        {
            var outp = param1.Trim();
            return OlderReference(param1, outp);
        }
        public static string CleanWhitepaces(string xml2)
        {
            return CleanWhitepaces(xml2, null, Unused, Unused);
        }
        public static string CleanWhitepaces(string xml2, string padchars,
                                              Func<char, char, bool> ifBefore, Func<char, char, bool> ifAfter)
        {
            if (xml2 == null) return xml2;
            const long maxCleanSize = 2 << 14;
            int inlen = xml2.Length;
            if (inlen > maxCleanSize || inlen < 2)
            {
                return xml2;
            }

            if (padchars != null)
            {
                if (xml2.IndexOfAny(padchars.ToCharArray(), 0) == -1 || (xml2.IndexOfAny(new[] { '\\', ':', '/' }, 0) > 0))
                {
                    padchars = null;
                }
            }

            StringBuilder s = new StringBuilder(inlen);

            bool chgd = false;
            bool xmlFound = false;
            bool inwhite = true;
            bool pendingWhitespace = false;
            char lastChar = '\0';
            int charIndex = -1;
            foreach (char c0 in xml2)
            {
                charIndex++;
                if (c0 <= 32)
                {
                    if (inwhite)
                    {
                        chgd = true;
                        continue;
                    }
                    inwhite = true;
                    pendingWhitespace = true;
                    continue;
                }
                switch (c0)
                {
                    case '/':
                        if (lastChar == 'r')
                        {
                            xmlFound = true;
                        }
                        int nxtIndex = charIndex + 1;
                        if (nxtIndex < inlen && xml2[nxtIndex] == '>')
                        {
                            if (!pendingWhitespace)
                            {
                                chgd = true;
                                pendingWhitespace = true;
                            }
                        }
                        else
                        {
                            inwhite = true;
                            if (pendingWhitespace)
                            {
                                chgd = true;
                                pendingWhitespace = false;
                            }
                        }
                        break;
                    case '>':
                    case '<':
                    case '\\':
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
                        }
                        break;
                    default:
                        if (padchars != null)
                        {
                            bool before = padchars.Contains("" + lastChar);
                            if (before && ifBefore(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }

                            bool after = padchars.Contains("" + c0);
                            if (after && ifAfter(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }
                        }
                        inwhite = false;
                        break;
                }
                if (pendingWhitespace)
                {
                    s.Append(' ');
                    pendingWhitespace = false;
                }
                s.Append(c0);
                lastChar = c0;
            }
            if (pendingWhitespace) chgd = true;
            int len = s.Length;
            if (xmlFound)
            {
                s = s.Replace("<sr />", "<srai><star /></srai>");
                //s = s.Replace("star/>", "star index=\"1\"/>");
                if (len != s.Length)
                {
                    chgd = true;
                }
            }
            if (!chgd)
            {
                if (len != inlen)
                {
                    return s.ToString();
                }
                return xml2;
            }
            //s = s.Replace("<star index=\"1\"", "<star");

            return s.ToString();
        }
        public  string MakeMatchable(string xml2)
        {
            if (xml2 == null) return xml2;
            if (!ContainsXml(xml2))
            {
                xml2 = ReplaceMap(xml2, new[]
                                     {
                                         new string[] {".", " "},
                                         new string[] {",", " "},
                                         new string[] {"?", " "},
                                         new string[] {"!", " "},
                                     });
                return ReTrimAndspace(xml2);
            }
            string xml22 = CleanWhitepaces(xml2);
            return OlderReference(xml2, xml22.ToUpper());
        }
#endregion

        //public RTParser.Variables.ISettingsDictionary RequesterChanges { get { throw new NotImplementedException(); } }
        //public RTParser.Variables.ISettingsDictionary ResponderChanges { get { throw new NotImplementedException(); } }

        public void AddOutputSentences(string sentence)
        {
            this.resultCount++;
            List<string> sents = StaticAIMLUtils.SentenceBreaker(sentence, null);
            if (sents.Count == 0 && sentence == " , ")
            {
                if (resultCount == 1) OutputSentencesAdd(sentence);
                return;
            }
            foreach (var s in sents)
            {
                if (s.Contains(". "))
                {
                    
                }
                OutputSentencesAdd(s);
            }
        }
   
#endif
        public static string oneLastSentence = null;
        private void OutputSentencesAdd(string unifiable)
        {
            lock (OutputSentences)
            {
                if (OutputSentences.Contains(unifiable) /*|| oneLastSentence == unifiable*/)
                {
                    return;
                }
                oneLastSentence = unifiable;
                OutputSentences.Add(unifiable);
            }
        }
    }
}