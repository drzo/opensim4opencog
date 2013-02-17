using System;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class or : UnifibleTagHandler
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
        public or(AltBot bot,
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
            bool wasTrue;
            string useLess;
            if (TextWith(templateNode, with, out wasTrue, out useLess))
            {
                return wasTrue ? OR_TRUE : OR_FALSE;
            }
            if (!templateNode.HasChildNodes)
            {
                return OR_FALSE;
            }
            float bestValue = OR_FALSE;
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

        protected override Unifiable ComputeInnerOrNull()
        {
            return AsOneOf();
        }
    }
}
