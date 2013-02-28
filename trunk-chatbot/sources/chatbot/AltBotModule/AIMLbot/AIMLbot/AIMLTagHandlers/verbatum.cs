using System;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The version element tells the AIML interpreter that it should substitute the version number
    /// of the AIML interpreter.
    /// 
    /// The version element does not have any content. 
    /// </summary>
    public class verbatum : AIMLTagHandler, EmptyIsNotFailure
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public verbatum(String show, AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            data = show;
            FinalResult = data;
            isRecursive = false;
            SelfProcessing = true;
        }

        public override bool isVerbatum
        {
            get { return true; }
        }

        protected override bool ExpandingSearchWillYieldNoExtras { get { return true; } }
        readonly Unifiable data;

        protected override Unifiable ProcessChangeU()
        {
            FinalResult = data;
            if (FinalResultValid) return FinalResult;
            return data;
        }

        public override float CanUnify(Unifiable with)
        {
            writeToLogWarn("VERBATUM CANUNIFY: " + with);
            IsTraced = true;
            return base.CanUnify(with);
        }

        public override Unifiable FinalResult
        {
            get { return data; }
            set
            {
                if (data != value)
                {
                    base.FinalResult = value;
                }
            }
        }
        public override bool FinalResultValid
        {
            get
            {
                return true;
            }
            set
            {
                base.FinalResultValid = value;
            }
        }
    }
}
