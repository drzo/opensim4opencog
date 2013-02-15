using System;
using System.Xml;
using System.Text;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot.CycNLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class NumberTemplate : UnifibleTagHandler
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
        public NumberTemplate(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        double _val = 0;
        public override float CanUnify(Unifiable with)
        {
            if (templateNode.NodeType==XmlNodeType.Text)
            {
                if (double.TryParse(with.ToValue(query), out _val))
                {
                    return ISA_TRUE;
                }
                return ISA_FALSE;
            }
            return ISA_FALSE;
        }

        protected override Unifiable ComputeInnerOrNull()
        {
            return "" + _val;
        }
    }
}
