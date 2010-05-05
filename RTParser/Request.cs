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
    public class Request
    {
        #region Attributes

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
        public Request(Unifiable rawInput, User user, RTPBot bot)
        {
            this.rawInput = rawInput;
            this.user = user;
            this.Proccessor = bot;
            this.StartedOn = DateTime.Now;
            this.framesAtStart = new StackTrace().FrameCount;
        }

        private Unifiable _topic;
        public Unifiable Flags = "no flags";

        public Unifiable Topic
        {
            get
            {
                if (_topic != null) return _topic;
                if (ParentRequest != null) return ParentRequest.Topic;
                return user.TopicSetting;
            }
            set
            {
                Unifiable prev = Topic;
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

        public IEnumerable<Unifiable> Topics
        {
            get { return new List<Unifiable>(){Topic}; }
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
    }
}
