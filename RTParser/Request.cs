using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using RTParser;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser
{

    public interface Request : QuerySettingsReadOnly
    {
        int depth { get; set; }
        Unifiable Topic { get; set; }
        Request ParentRequest { get; set; }
        DateTime StartedOn { get; set; }
        GraphMaster Graph { get; set; }
        bool IsTraced { get; set; }
        User user { get; set; }
        IEnumerable<Unifiable> BotOutputs { get; }
        Result CurrentResult { get;  set; }
        Unifiable Flags { get; }
        IList<Unifiable> Topics { get; }
        ISettingsDictionary Predicates { get; }
        Proof Proof { get; set; }

        int MaxTemplates { get; set; }
        int MaxPatterns { get; set; }
        int MaxOutputs { get; set; }
        bool ProcessMultiplePatterns { get; set; }
        bool ProcessMultipleTemplates { get; set; }
        QueryList TopLevel { get; set; }
        SubQuery CurrentQuery { get; }
        bool hasTimedOut { get; set; }
        RTPBot Proccessor { get; set; }
        Unifiable rawInput { get; set; }
        IList<Result> UsedResults { get; set; }
        IList<TemplateInfo> UsedTemplates { get; }
        DateTime TimesOutAt { get; set; }
        ISettingsDictionary Settings { get; set; }
        int MaxInputs { get; set; }


        bool GraphsAcceptingUserInput { get; set; }
        LoaderOptions LoadOptions { get; set; }
        AIMLLoader Loader { get; }
        string Filename { get; set; }
        string LoadingFrom { get; set; }

        void WriteLine(string s, object[] args);
        bool IsComplete(Result o);
        bool addSetting(string name, Unifiable unifiable);
        void AddSubResult(Result result);
        int GetCurrentDepth();
        Unifiable grabSetting(string name);
        QuerySettingsSettable GetQuerySettings();
        AIMLbot.Result CreateResult(Request res);

        AIMLbot.Request CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, AIMLbot.Request request);
        bool CanUseTemplate(TemplateInfo info, Result request);
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
        public Unifiable rawInput { get; set; }

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
        public User user { get; set; }

        /// <summary>
        /// The Proccessor to which the request is being made
        /// </summary>
        public RTPBot Proccessor { get; set; }

        /// <summary>
        /// The final result produced by this request
        /// </summary>
        public Result CurrentResult
        {
            get { return _result; }
            set { _result = value; }
        }

        private Result _result;

        public ISettingsDictionary Settings { get; set; }

        /// <summary>
        /// Flag to show that the request has timed out
        /// </summary>
        public bool hasTimedOut { get; set; }

        public readonly int framesAtStart;


        #endregion

        public override string ToString()
        {

            string s;
            if (user == null) s = "NULL";
            else s = user.UserID;
            return s + ": " + Unifiable.ToVMString(rawInput);
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="rawInput">The raw input from the user</param>
        /// <param name="user">The user who made the request</param>
        /// <param name="bot">The bot to which this is a request</param>
        public RequestImpl(Unifiable rawInput, User user, RTPBot bot, Request parent)
            : base(user) // Get query settings intially from user
        {
            UsedResults = new List<Result>();
            Flags = "Nothing";
            ApplySettings(user, this);

            if (parent != null)
            {
                ApplySettings(parent, this);
                Proof = parent.Proof;
                this.ParentRequest = parent;
                Graph = parent.Graph;
                MaxInputs = 1;
            }
            else
            {
                if (user != null)
                    MaxInputs = user.MaxInputs;
                else MaxInputs = 1;
                Proof = new Proof();
            }
            this.rawInput = rawInput;
            if (user != null)
            {
                this.user = user;
                if (user.CurrentRequest == null) user.CurrentRequest = this;
                Settings = user.Predicates;
            }
            this.Proccessor = bot;
            this.StartedOn = DateTime.Now;
            this.TimesOutAt = StartedOn.AddMilliseconds(Proccessor.TimeOut);
            this.framesAtStart = new StackTrace().FrameCount;
            if (parent != null)
            {
                Settings = parent.Settings;
            }
        }

        public bool GraphsAcceptingUserInput
        {
            get { return Graph.GraphsAcceptingUserInput; }
            set { Graph.GraphsAcceptingUserInput = value; }
        }

        public LoaderOptions LoadOptions
        {
            get
            {
                return new LoaderOptions(this, Graph);
            }
            set
            {
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
                if (_aimlloader == null) _aimlloader = new AIMLLoader(Proccessor, this);
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

        private GraphMaster ovGraph = null;
        public override GraphMaster Graph
        {
            get
            {
                if (ovGraph != null)
                    return ovGraph;
                if (ParentRequest != null)
                {
                    var pg = ParentRequest.Graph;
                    if (pg != null) return pg;
                }
                return user.ListeningGraph;
            }
            set { ovGraph = value; }
        }

        private Unifiable _topic;
        public Unifiable Flags { get; set; }
        public QueryList TopLevel { get; set; }

        public Unifiable Topic
        {
            get
            {
                return user.TopicSetting;
                if (_topic != null) return _topic;
                if (ParentRequest != null) return ParentRequest.Topic;
                return user.TopicSetting;
            }
            set
            {
                if(true)
                {
                    user.TopicSetting = value;
                    return;
                }
                Unifiable prev = Topic;
                user.TopicSetting = value;
                if (value == Proccessor.NOTOPIC)
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
                var tops = user.Topics;
                if (_topic!=null)
                {
                    if (!tops.Contains(_topic)) tops.Insert(0, _topic);
                }
                if (tops.Count == 0) return new List<Unifiable>() { Proccessor.NOTOPIC };
                return tops;
            }
        }

        public IEnumerable<Unifiable> BotOutputs
        {
            get { return user.BotOutputs; }
        }

        public ISettingsDictionary Predicates
        {
            get
            {
                if (Settings is SettingsDictionary) return (SettingsDictionary) Settings;
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
            return Predicates.grabSetting(name);
        }

        public bool addSetting(string name, Unifiable value)
        {
            return Predicates.addSetting(name, value);
        }

        public IList<TemplateInfo> UsedTemplates
        {
            get { return CurrentResult.UsedTemplates; }
        }

        public DateTime TimesOutAt { get; set;}

        public void WriteLine(string s, object[] args)
        {
            if (CurrentResult != null)
            {
                CurrentResult.WriteLine(s, args);
            }
            else
            {
                Proccessor.writeToLog(s, args);
            }
        }

        public bool IsComplete(RTParser.Result result1)
        {
            if (result1 == null)
            {
                return false;
            }
            QuerySettingsReadOnly qs = GetQuerySettings();
            if (result1.OutputSentenceCount >= qs.MaxOutputs)
            {
                return true;
            }

            if (result1.SubQueries.Count >= qs.MaxPatterns)
            {
                return true;
            }
            if (result1.UsedTemplates.Count >= qs.MaxTemplates)
            {
                //return false;
            }
            return false;
        }

        public QuerySettingsSettable GetQuerySettings()
        {
            return this;
        }

        public AIMLbot.Result CreateResult(Request parentReq)
        {
            var r = new AIMLbot.Result(user, Proccessor, parentReq, parentReq.CurrentResult);
            CurrentResult = r;
            r.request = this;
            return (AIMLbot.Result)CurrentResult;
        }

        public AIMLbot.Request CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, AIMLbot.Request request)
        {
            request = (AIMLbot.Request) (request ?? this);
            user = user ?? this.user;
            rTPBot = rTPBot ?? this.Proccessor;
            var subRequest = new AIMLbot.Request(templateNodeInnerValue, user, rTPBot, request);
            Result res = request.CurrentResult;
            subRequest.CurrentResult = res;
            subRequest.Graph = request.Graph;
            depth = subRequest.depth = request.depth + 1;
            subRequest.ParentRequest = request;
            subRequest.StartedOn = request.StartedOn;
            subRequest.TimesOutAt = request.TimesOutAt;
            subRequest.Settings = request.Settings;
            return subRequest;
        }

        public bool CanUseTemplate(TemplateInfo info, Result result)
        {
            if (!user.CanUseTemplate(info, result)) return false;
            while (result != null)
            {
                if (result.UsedTemplates != null && result.UsedTemplates.Contains(info))
                {
                    //user.WriteLine("!CanUseTemplate ", info);
                    return false;
                }
                result = result.ParentResult;
            }
            return true;
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
