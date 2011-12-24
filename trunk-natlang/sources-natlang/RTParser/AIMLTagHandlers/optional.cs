using System;
using System.Xml;
using System.Text;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class optional : UnifibleTagHandler
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
        public optional(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public override float CanUnify(Unifiable with)
        {
            bool wasTrue;
            string useLess;
            if (TextWith(templateNode, with, out wasTrue, out useLess))
            {
                return OPT_TRUE;
            }
            if (!templateNode.HasChildNodes)
            {
                return OPT_TRUE;
            }
            float bestValue = OPT_TRUE;
            // recursively check
            foreach (XmlNode childNode in templateNode.ChildNodes)
            {
                try
                {
                    float partCallCanUnify = ChildUnify(with, childNode);
                    if (partCallCanUnify < bestValue)
                    {
                        bestValue = partCallCanUnify;
                    }
                    if (partCallCanUnify <= STAR_TRUE)
                    {
                        SetWith(childNode, with);
                    }
                }
                catch (Exception e)
                {
                    Proc.writeToLog(e);
                    writeToLogWarn("" + e);
                }
            }
            return bestValue;
        }
    }
}
