using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using RTParser;
using RTParser.Utils;

namespace RTParser
{
    /// <summary>
    /// Encapsulates all sorts of information about a request to the Proccessor for processing
    /// </summary>
    abstract public class Request: RequestSettings
    {
        #region Attributes

        public Proof Proof;

        public int depth = 0;
        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Unifiable rawInput;

        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Request ParentRequest;

        /// <summary>
        /// The time at which this request was created within the system
        /// </summary>
        public DateTime StartedOn;

        /// <summary>
        /// The user who made this request
        /// </summary>
        public User user;

        /// <summary>
        /// The Proccessor to which the request is being made
        /// </summary>
        public RTPBot Proccessor;

        /// <summary>
        /// The final result produced by this request
        /// </summary>
        public Result result;

        /// <summary>
        /// Flag to show that the request has timed out
        /// </summary>
        public bool hasTimedOut = false;

        public readonly int framesAtStart;


        #endregion

        public override string ToString()
        {
           return user.UserID + ": " + rawInput;
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="rawInput">The raw input from the user</param>
        /// <param name="user">The user who made the request</param>
        /// <param name="bot">The bot to which this is a request</param>
        public Request(Unifiable rawInput, User user, RTPBot bot, Request parent)
        {
            if (user!=null)
            {
                base.ApplySettings(user);
            }
            if (parent != null)
            {
                this.ApplySettings(parent);
                Proof = parent.Proof;
                this.ParentRequest = parent;
            } else
            {
                Proof = new Proof();
            }
            this.rawInput = rawInput;
            this.user = user;
            this.Proccessor = bot;
            this.StartedOn = DateTime.Now;
            this.Graph = user.ListeningGraph ?? bot.GraphMaster;
            this.framesAtStart = new StackTrace().FrameCount;
            this.ProcessMultiplePatterns = true;
            this.ProcessMultipleTemplates = true;
        }

        private Unifiable _topic;
        public Unifiable Flags = "no flags";
        public QueryList TopLevel;

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

        public bool addSetting(Unifiable name, Unifiable value)
        {
            return Predicates.addSetting(name, value);
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

        public bool IsComplete(Result result1)
        {
            if (result1.OutputSentenceCount >= MaxOutputs)
            {
                return true;
            }

            if (result1.SubQueries.Count >= MaxPatterns)
            {
                return true;
            }
            if (result1.UsedTemplates.Count >= MaxTemplates)
            {
                //return false;
            }
            return false;
        }

        public List<Result> UsedResults = new List<Result>();
        public void AddSubResult(Result subResult)
        {
            lock (UsedResults)
            {
                UsedResults.Add(subResult);
            }
        }
    }
}
