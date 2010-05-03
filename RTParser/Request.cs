using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using RTParser;

namespace RTParser
{
    /// <summary>
    /// Encapsulates all sorts of information about a request to the Proccessor for processing
    /// </summary>
    public class Request
    {
        #region Attributes
        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Unifiable rawInput;

        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Request parent;

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
        public Unifiable Topic
        {
            get
            {
                if (_topic != null) return _topic;
                if (parent != null) return parent.Topic;
                return user.TopicSetting;
            }
            set
            {
                Unifiable prev = Topic;
                if (prev==value) return;
                _topic = value;
            }
        }

        public int GetCurrentDepth()
        {
            int here = new StackTrace().FrameCount - framesAtStart;
            return here / 6;
        }
    }
}
