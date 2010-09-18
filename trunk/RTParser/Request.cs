using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading;
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

        //int DebugLevel { get; set; }
        //bool IsTraced { get; set; }

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
        Result CurrentResult { get;  set; }
        Unifiable Flags { get; }
        IList<Unifiable> Topics { get; }
        Proof Proof { get; set; }

        //int MaxTemplates { get; set; }
        //int MaxPatterns { get; set; }
        //int MaxOutputs { get; set; }
        //bool ProcessMultiplePatterns { get; set; }
        //bool ProcessMultipleTemplates { get; set; }
        //void IncreaseLimits(int i);
        GraphQuery TopLevel { get; set; }
        SubQuery CurrentQuery { get; }
        RTPBot TargetBot { get; set; }
        Unifiable rawInput { get; }
        ParsedSentences ChatInput { get; set; }
        IList<Result> UsedResults { get; set; }
        IList<TemplateInfo> UsedTemplates { get; }
        int MaxInputs { get; set; }

        DateTime StartedOn { get; set; }
        DateTime TimesOutAt { get; set; }
        TimeSpan TimeOut { get; set; }

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
        int GetCurrentDepth();
        Unifiable grabSetting(string name);
        QuerySettingsSettable GetQuerySettings();
        AIMLbot.Result CreateResult(Request res);

        AIMLbot.Request CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, AIMLbot.Request request);
        bool CanUseTemplate(TemplateInfo info, Result request);
        OutputDelegate writeToLog { get; set; }
        
        Unifiable That { get; set; }
        PrintOptions WriterOptions { get; }
        RequestImpl ParentMostRequest { get; }
        AIMLTagHandler LastHandler { get; set; }
        ChatLabel PushScope { get; }
        ChatLabel CatchLabel { get; }
        // inherited from base  string GraphName { get; set; }
        ISettingsDictionary GetSubstitutions(string name, bool b);
        GraphMaster GetGraph(string srai);
        void ExcludeGraph(string srai);
        void AddOutputSentences(TemplateInfo ti, string nai, Result result);
        ISettingsDictionary GetDictionary(string named);

        void AddUndo(Action action);
        void Commit();
        void UndoAll();
        void AddSideEffect(string effect, ThreadStart start);
        Dictionary<string, GraphMaster> GetMatchingGraphs(string graphname, GraphMaster master);
    }

    /// <summary>
    /// Encapsulates all sorts of information about a request to the Proccessor for processing
    /// </summary>
    abstract public class RequestImpl : QuerySettings, Request
    {
        #region Attributes

        public Proof Proof { get; set; }

        // How many subqueries are going to be submitted with combos ot "that"/"topic" tags 
        public int MaxInputs { get; set; }
        
        public int depth { get; set; }
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

        public ParsedSentences ChatInput { get; set; }

        /// <summary>
        /// The raw input from the user
        /// </summary>
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
        public Result CurrentResult
        {
            get { return _result; }
            set { _result = value; }
        }

        private Result _result;

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
        public string WhyComplete { get; set; }

        public bool IsTimedOutOrOverBudget
        {
            get
            {
                if (WhyComplete != null) return true;
                if (DateTime.Now > TimesOutAt)
                {
                    WhyComplete = "TIMEOUT";
                    return true;
                }
                string whyNoSearch = WhyNoSearch(CurrentResult);
                if (whyNoSearch!=null) 
                {
                    WhyComplete = whyNoSearch;
                    return true;
                }
                return false;
            }
        }

        public readonly int framesAtStart;


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
            string whyComplete = WhyComplete;
            return string.Format("{0}: {1}, {2}", Requester == null ? "NULL" : (string) Requester.UserID,
                                 Responder == null ? "Anyone" : (string) Responder.UserID,
                                 Unifiable.ToVMString(rawInput)) +
                   (whyComplete != null ? " WhyComplete=" + whyComplete : "");
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="rawInput">The raw input from the user</param>
        /// <param name="user">The user who made the request</param>
        /// <param name="bot">The bot to which this is a request</param>
        public RequestImpl(string rawInput, User user, RTPBot bot, Request parent, User targetUser)
            :  base(bot.GetQuerySettings()) // Get query settings intially from user
        {
            qsbase = this;
            if (parent != null)
            {
                ChatInput = parent.ChatInput;
                Requester = parent.Requester;
            }
            else
            {
                ChatInput = bot.GetParsedUserInputSentences(this, rawInput);
            }
            Request pmaybe = null;
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

            QuerySettings.ApplySettings(qsbase, querySettings);

            if (parent != null)
            {
                QuerySettings.ApplySettings(parent.GetQuerySettings(), querySettings);
                Proof = parent.Proof;
                this.ParentRequest = parent;
                this.lastOptions = parent.LoadOptions;
                this.writeToLog = parent.writeToLog;
                Graph = parent.Graph;
                MaxInputs = 1;
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
                pmaybe = user.CurrentRequest;
                this.Requester = user;
                if (user.CurrentRequest == null) user.CurrentRequest = this;
                TargetSettings = user.Predicates;
                if (parent == null)
                {
                    if (pmaybe != null)
                    {
                       // ParentRequest = pmaybe;
                    }
                    user.CurrentRequest = this;
                }
            }
            this.TargetBot = bot;
            this.StartedOn = DateTime.Now;
            this.TimesOutAt = StartedOn.AddMilliseconds(TargetBot.TimeOut);
            this.framesAtStart = new StackTrace().FrameCount;
            if (parent != null)
            {
                TargetSettings = parent.TargetSettings;
            }
        }

        private void ReduceMinMaxesForSubRequest(QuerySettingsReadOnly parent)
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
                    lastOptions = new LoaderOptions(this, Graph);
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
                if (_aimlloader == null) _aimlloader = new AIMLLoader(TargetBot, this);
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

        internal ICollection<GraphMaster> DisallowedGraphs
        {
            get { return Requester.DisallowedGraphs; }
        }

        public void AddOutputSentences(TemplateInfo ti, string nai, Result result)
        {
            result = result ?? CurrentResult;
            result.AddOutputSentences0(ti, nai);   
        }

        private Unifiable _topic;
        public Unifiable Flags { get; set; }
        public GraphQuery TopLevel { get; set; }

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
                return CurrentResult.Predicates;
            }
        }

        public int GetCurrentDepth()
        {
            int here = new StackTrace().FrameCount - framesAtStart;
            return here / 6;
        }

        public Unifiable grabSetting(string name)
        {
            return RequesterPredicates.grabSetting(name);
        }

        public bool addSetting(string name, Unifiable value)
        {
            return RequesterPredicates.addSetting(name, value);
        }

        public IList<TemplateInfo> UsedTemplates
        {
            get { return CurrentResult.UsedTemplates; }
        }

        public DateTime TimesOutAt { get; set; }

        public void WriteLine(string s, params object[] args)
        {
            if (CurrentResult != null)
            {
                CurrentResult.writeToLog(s, args);
            }
            else
            {
                writeToLog(s, args);
            }
        }

        public bool IsComplete(RTParser.Result result1)
        {
            string s = WhyNoSearch(result1);
            if (s == null) return false;
            return true;
        }

        public string WhyNoSearch(RTParser.Result result1)
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

            if (result1.SubQueries.Count >= qs.MaxPatterns)
            {
                if (result1.OutputSentenceCount > 0)
                {
                    w = w ?? "";
                    w += "MaxPatterns ";
                }
                // return null;
            }
            if (result1.UsedTemplates.Count >= qs.MaxTemplates)
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

        public AIMLbot.Result CreateResult(Request parentReq)
        {
            if (CurrentResult == null)
            {
                var r = new AIMLbot.Result(Requester, TargetBot, parentReq, parentReq.CurrentResult);
                CurrentResult = r;
                r.request = this;
            }
            return (AIMLbot.Result) CurrentResult;
        }

        public AIMLbot.Request CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, AIMLbot.Request request)
        {
            request = (AIMLbot.Request) (request ?? this);
            user = user ?? this.Requester;
            rTPBot = rTPBot ?? this.TargetBot;
            var subRequest = new AIMLbot.Request(templateNodeInnerValue, user, rTPBot, request);
            Result res = request.CurrentResult;
            subRequest.CurrentResult = res;
            subRequest.Graph = request.Graph;
            depth = subRequest.depth = request.depth + 1;
            subRequest.ParentRequest = request;
            subRequest.StartedOn = request.StartedOn;
            subRequest.TimesOutAt = request.TimesOutAt;
            subRequest.TargetSettings = request.TargetSettings;
            subRequest.Responder = request.Responder;
            return subRequest;
        }

        public bool CanUseTemplate(TemplateInfo info, Result result)
        {
            if (info == null) return true;
            if (info.IsDisabled) return false;
            if (!Requester.CanUseTemplate(info, result)) return false;
            //if (!result.CanUseTemplate(info, result)) return false;
            while (result != null)
            {
                var resultUsedTemplates = result.UsedTemplates;
                if (resultUsedTemplates != null)
                {
                    lock (resultUsedTemplates)
                    {
                        if (resultUsedTemplates.Contains(info))
                        {
                            //user.WriteLine("!CanUseTemplate ", info);
                            return false;
                        }
                    }
                }
                result = result.ParentResult;
            }
            return true;
        }

        public OutputDelegate writeToLog { get; set; }
        internal void writeToLog0(string message, params object[] args)
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
            if (writeToLog!=writeToLog0)
            {
             //   writeToLog(prefix);
            }
            TargetBot.writeToLog(prefix);
        }

        public override int DebugLevel
        {
            get
            {
        
                int baseDebugLevel = base.DebugLevel;
                if (baseDebugLevel > 0) return baseDebugLevel;
                QuerySettingsReadOnly ParentRequest = (QuerySettingsReadOnly)this.ParentRequest;
                if (ParentRequest == null)
                {
                    if (Requester == null) return baseDebugLevel;
                    return Requester.GetQuerySettings().DebugLevel;
                }
                return ParentRequest.DebugLevel;
            }
            set { base.DebugLevel = value; }
        }

        internal Unifiable ithat = null;
        public Unifiable That
        {
            get
            {
                if (ithat != null) return ithat;
                if (ParentRequest != null)
                {
                    return ParentRequest.That;
                }
                return Requester.That;
            }
            set { ithat = value; }
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

        public RequestImpl ParentMostRequest
        {
            get
            {
                if (ParentRequest == null) return this;
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
                CurrentResult.CatchLabel = _label;
                return _label;
            }
        }

        public ChatLabel CatchLabel
        {
            get { return _label; }
        }

        public ISettingsDictionary GetSubstitutions(string named, bool createIfMissing)
        {
            return TargetBot.GetDictionary(named, "substitutions", createIfMissing);
        }

        public ISettingsDictionary GetDictionary(string named)
        {
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
                foreach (KeyValuePair<String, GraphMaster> ggg in GraphMaster.CopyOf(RTPBot.GraphsByName))
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

        public ISettingsDictionary GetDictionary(string named, ISettingsDictionary dictionary)
        {
            ISettingsDictionary dict =  GetDictionary0(named, dictionary);
            if (dict == TargetBot.GlobalSettings)
            {
                writeToLog("BETTER MEAN BOT " + named + " for " + dictionary);
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
            if (named == "set") return CheckedValue(Requester.UserID, Requester);
            if (named == "get") return CheckedValue(Requester.UserID, Requester);
            if (named == "query") return CheckedValue(named, CurrentQuery);
            if (named == "request") return CheckedValue(named, TargetSettings);            
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
                    var vp = GetDictionary(string.Join(".", path, 0, path.Length - 1), v);
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

        private ISettingsDictionary CheckedValue(string named, ISettingsDictionary d)
        {
            return d;
        }


        public IList<Result> UsedResults { get; set; }

        public SubQuery CurrentQuery
        {
            get
            {
                Result r = CurrentResult;
                if (r != null)
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
        }

        public void AddSubResult(Result subResult)
        {
            lock (UsedResults)
            {
                UsedResults.Add(subResult);
            }
        }
    }
}
