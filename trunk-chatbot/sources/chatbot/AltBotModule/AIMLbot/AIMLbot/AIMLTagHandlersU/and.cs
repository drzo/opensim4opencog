using System;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class and : UnifibleTagHandler
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
        public and(AltBot bot,
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
                return wasTrue ? AND_TRUE : AND_FALSE;
            }
            if (!templateNode.HasChildNodes)
            {
                return AND_TRUE;
            }
            float worstValue = AND_TRUE;
            // recursively check
            foreach (XmlNode childNode in templateNode.ChildNodes)
            {
                try
                {
                    float partCallCanUnify = ChildUnify(with, childNode);
                    if (partCallCanUnify > worstValue)
                    {
                        worstValue = partCallCanUnify;
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
            return worstValue;
        }

        protected override Unifiable ComputeInnerOrNull()
        {
            return AsOneOf();
        }
    }
}
