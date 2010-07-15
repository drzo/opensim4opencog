using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using RTParser;
using RTParser.Utils;

namespace RTParser
{

    public interface Request
    {
        int depth { get; set; }
        Unifiable Topic { get; set; }
        Request ParentRequest { get; set; }
        DateTime StartedOn { get; set; }
        GraphMaster Graph { get; set; }
        bool IsTraced { get; set; }
        User user { get; set; }
        IEnumerable<Unifiable> BotOutputs { get; }
        Result CurrentResult { set; }
        Unifiable Flags { get; }
        IList<Unifiable> Topics { get; }
        SettingsDictionary Predicates { get; }
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
        Result result { get; }
        Unifiable rawInput { get; set; }
        IList<Result> UsedResults { get; set; }
        IList<TemplateInfo> UsedTemplates { get; }
        void WriteLine(string s, object[] args);
        bool IsComplete(Result o);
        bool addSetting(string name, Unifiable unifiable);
        void AddSubResult(Result result);
        int GetCurrentDepth();
        Unifiable grabSetting(string name);
        QuerySettings GetQuerySettings();
        AIMLbot.Result CreateResult(Request res);

        AIMLbot.Request CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, AIMLbot.Request request);
    }

    /// <summary>
    /// Encapsulates all sorts of information about a request to the Proccessor for processing
    /// </summary>
    abstract public class RequestImpl : RequestSettingsImpl, Request
    {
        #region Attributes

        public Proof Proof { get; set; }

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
        public Result result
        {
            get
            {
                return CurrentResult;
            }
        }
        public Result CurrentResult { private get; set; }


        /// <summary>
        /// Flag to show that the request has timed out
        /// </summary>
        public bool hasTimedOut { get; set; }

        public readonly int framesAtStart;


        #endregion

        public override string ToString()
        {
            string s = user.UserID;
            return s + ": " + Unifiable.ToVMString(rawInput);
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="rawInput">The raw input from the user</param>
        /// <param name="user">The user who made the request</param>
        /// <param name="bot">The bot to which this is a request</param>
        public RequestImpl(Unifiable rawInput, User user, RTPBot bot, Request parent)
        {
            UsedResults = new List<Result>();
            Flags = "Nothing";
            if (user != null)
            {
                base.ApplySettings(user);
            }
            if (parent != null)
            {
                this.ApplySettings((QuerySettings)parent);
                Proof = parent.Proof;
                this.ParentRequest = parent;
                Graph = parent.Graph;

            }
            else
            {
                Proof = new Proof();
            }
            this.rawInput = rawInput;
            this.user = user;
            this.Proccessor = bot;
            this.StartedOn = DateTime.Now;
            this.framesAtStart = new StackTrace().FrameCount;
            this.ProcessMultiplePatterns = true;
            this.ProcessMultipleTemplates = true;
        }

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

        public SettingsDictionary Predicates
        {
            get { return result.Predicates; }
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
            get { return result.UsedTemplates; }
        }

        public void WriteLine(string s, object[] args)
        {
            if (result != null)
            {
                result.WriteLine(s, args);
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
            QuerySettings qs = GetQuerySettings();
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

        public QuerySettings GetQuerySettings()
        {
            return this;
        }

        public AIMLbot.Result CreateResult(Request parentReq)
        {
            var r = new AIMLbot.Result(user, Proccessor, parentReq, parentReq.result);
            CurrentResult = r;
            r.request = this;
            return (AIMLbot.Result) result;
        }

        public AIMLbot.Request CreateSubRequest(Unifiable templateNodeInnerValue, User user, RTPBot rTPBot, AIMLbot.Request request)
        {
            request = (AIMLbot.Request) (request ?? this);
            user = user ?? this.user;
            rTPBot = rTPBot ?? this.Proccessor;
            var subRequest = new AIMLbot.Request(templateNodeInnerValue, user, rTPBot, request);
            if (false)
            {
                Result res = request.CurrentResult;
                subRequest.CurrentResult = res;
            }
            subRequest.Graph = request.Graph;
            depth = subRequest.depth = request.depth + 1;
            subRequest.ParentRequest = request;
            subRequest.StartedOn = request.StartedOn;
            return subRequest;
        }

        public IList<Result> UsedResults { get; set; }

        public SubQuery CurrentQuery
        {
            get
            {
                Result r = result;
                if (result != null)
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
