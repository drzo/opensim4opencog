using System;
using System.Xml;
using System.Text;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The version element tells the AIML interpreter that it should substitute the version number
    /// of the AIML interpreter.
    /// 
    /// The version element does not have any content. 
    /// </summary>
    public class verbatum : RTParser.Utils.AIMLTagHandlerU
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
        public verbatum(String show, RTParser.AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            data = show;
            RecurseResult = data;
            isRecursive = false;
        }

        protected override bool ExpandingSearchWillYieldNoExtras { get { return true; } }
        readonly Unifiable data;
        protected override Unifiable ProcessChangeU()
        {
            RecurseResult = data;
            return data;
        }

        public override Unifiable CompleteProcessU()
        {
            RecurseResult = data;
            if (RecurseResultValid) return RecurseResult;
            return data;
        }
        public override Unifiable TransformU()
        {
            RecurseResult = data;
            return data;
        }
        public override float CanUnify(Unifiable with)
        {
            writeToLogWarn("CANUNIFY: " + with);
            return base.CanUnify(with);
        }

        public override Unifiable RecurseResult
        {
            get { return data; }
            set
            {
                if (data != value)
                {
                    base.RecurseResult = value;
                }
            }
        }
        public override bool RecurseResultValid
        {
            get
            {
                return true;
            }
            set
            {
                base.RecurseResultValid = value;
            }
        }
    }
}
