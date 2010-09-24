using System;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The version element tells the AIML interpreter that it should substitute the version number
    /// of the AIML interpreter.
    /// 
    /// The version element does not have any content. 
    /// </summary>
    public class verbatum : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public verbatum(String show, RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            data = show;
            RecurseResult = data;
            isRecursive = false;
        }

        readonly Unifiable data;
        protected override Unifiable ProcessChange()
        {
            RecurseResult = data;
            return data;
        }

        public override Unifiable CompleteProcess()
        {
            RecurseResult = data;
            if (RecurseResultValid) return RecurseResult;
            return data;
        }
        public override string Transform()
        {
            RecurseResult = data;
            return data;
        }
        public override float CanUnify(Unifiable with)
        {
            writeToLogWarn("CANUNIFY: " + with);
            return base.CanUnify(with);
        }
    }
}
