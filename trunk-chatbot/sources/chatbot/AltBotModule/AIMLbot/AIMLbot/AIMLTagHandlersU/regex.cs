using System.Text.RegularExpressions;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class regex : UnifibleTagHandler
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
        public regex(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }
        public override float CanUnify(Unifiable with)
        {

            string re = ComputeInner();
            var matcher = new Regex(re);
            if (matcher.IsMatch(with.ToValue(query)))
            {
                SetWith(templateNode, with);
                return AND_TRUE;
            }
            return AND_FALSE;
        }

        protected override Unifiable ComputeInnerOrNull()
        {
            return base.AsOneOf();
        }
    }
}
