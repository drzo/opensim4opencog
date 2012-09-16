using System;
using System.Collections.Generic;
using System.Text;
using AltAIMLbot.Utils;

namespace AltAIMLbot
{
    /// <summary>
    /// Encapsulates all sorts of information about a request to the bot for processing
    /// </summary>
    public class Request
    {
        #region Attributes
        /// <summary>
        /// The raw input from the user
        /// </summary>
        public string rawInput;
        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Utterance ChatInput { get; set; }

        /// <summary>
        /// The time at which this request was created within the system
        /// </summary>
        public DateTime StartedOn;

        /// <summary>
        /// The user who made this request
        /// </summary>
        public User user;
        /// <summary>
        /// The user who made this request
        /// </summary>
        public User Requester { get; set; }
        private User _responderUser;
        public Request ParentRequest { get; set; }

        public int depth=0;
        public int depthMax = 128;

        /// <summary>
        /// The user who is the target of this request
        /// </summary>
        public User Responder
        {
            get
            {
                if (_responderUser != null) return _responderUser;
                var parentRequest = this.ParentRequest;
                if (parentRequest != null) return parentRequest.Responder;
                return null;
            }
            set
            {
                _responderUser = value;
                // if (value != null) That = value.JustSaid;
            }
        }

        /// <summary>
        /// The bot to which the request is being made
        /// </summary>
        public AltBot bot;

        /// <summary>
        /// The final result produced by this request
        /// </summary>
        public Result result;

        /// <summary>
        /// Flag to show that the request has timed out
        /// </summary>
        public bool hasTimedOut = false;

        public double thisScore = 1.0;
        protected Result _CurrentResult;
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

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="rawInput">The raw input from the user</param>
        /// <param name="user">The user who made the request</param>
        /// <param name="bot">The bot to which this is a request</param>
        public Request(string rawInput, User user, AltBot bot)
        {
            this.rawInput = rawInput;
            this.user = user;
            this.bot = bot;
            this.StartedOn = DateTime.Now;
            this.depth = 0;
            this.depthMax = 128;
            TargetSettings = user.Predicates;
        }
        public TimeSpan _Durration = TimeSpan.Zero;
        /// <summary>
        /// The Proccessor to which the request is being made
        /// </summary>
        public AltBot TargetBot { get; set; }

        /// <summary>
        /// The amount of time the request took to process
        /// </summary>
        public TimeSpan Durration
        {
            get
            {
                if (_Durration == TimeSpan.Zero) return DateTime.Now - StartedOn;
                return _Durration;
            }
            set { _Durration = value; }
        }
        public DateTime TimesOutAt { get; set; }
        public string _WhyRequestComplete;
        public GraphMaster CurrentGraph;
        public SettingsDictionary TargetSettings;
        public bool MayTimeOut;

        public string WhyResultComplete
        {
            get
            {
                var currentResult = CurrentResult;
                if (currentResult == null) return null;
                return currentResult.WhyResultComplete;
            }
        }
        public bool IsToplevelRequest { get; set; }
        public bool _SuspendSearchLimits { get; set; }
        public bool SuspendSearchLimits
        {
            get { return _SuspendSearchLimits && IsToplevelRequest; }
            set { _SuspendSearchLimits = value; }
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

        public string WhyComplete
        {
            get
            {
                Result currentResult = CurrentResult;
                return WhyRequestComplete ?? (currentResult == null ? null : currentResult.WhyResultComplete);
            }
            set { _WhyRequestComplete = value; }
        }
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

        public TimeSpan TimeOutFromNow
        {
            set
            {
                WhyComplete = null;
                StartedOn = DateTime.Now;
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

        public void AddGraph(object master)
        {
            /// throw new NotImplementedException();
        }
        public Result CreateResult(Request parentReq)
        {
            if (parentReq != this)
            {
                return parentReq.CreateResult(parentReq);
            }

            Result currentResult = parentReq.CurrentResult;
            if (currentResult == null)
            {
                var r = new AltAIMLbot.Result(rawInput, Requester, TargetBot, parentReq, parentReq.Responder);
                parentReq.CurrentResult = r;
                //ExitQueue.Add("exit subResult", r.Exit);
                r.request = thisRequest;
                currentResult = r;
            }
            TimeOutFromNow = parentReq.TimeOut;
            return (Result)currentResult;
        }
        public void Exit()
        {
            /// throw new NotImplementedException();
        }
        private Request thisRequest
        {
            get { return (Request)this; }
        }

        public SettingsDictionary GetDictionary(string type0)
        {
            if (type0 == "user") type0 = user.UserID;
            return (SettingsDictionary)bot.GetDictionary(type0);
        }

        public GraphMaster GetGraph(string name)
        {
            if ((name == null || name == "*") && CurrentGraph != null)
            {
                return CurrentGraph;
            }
            return bot.GetGraph(name) ?? bot.Graphmaster;
        }

        public SettingsDictionary GetSubstitutions(string named, bool createIfMissing)
        {
            return (SettingsDictionary)TargetBot.GetDictionary(named, "substitutions", createIfMissing);
        }

        public void writeToLog(string s, params object[] args)
        {
            TargetBot.writeToLog(s, args);
        }
    }
}
