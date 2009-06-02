using System.Xml;

namespace RTParser.Utils
{
    public abstract class UnifibleTagHandler : RTParser.Utils.AIMLTagHandler
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
        public UnifibleTagHandler(RTParser.RTPBot bot,
                                      RTParser.User user,
                                      RTParser.Utils.SubQuery query,
                                      RTParser.Request request,
                                      RTParser.Result result,
                                      XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public override bool CanUnify(Unifiable with)
        {
            return base.CanUnify(with);
        }

    }
}