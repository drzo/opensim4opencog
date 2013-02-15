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
    public class OptionalOne : UnifibleTagHandler
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
        public OptionalOne(AltBot bot,
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
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    try
                    {
                        float rate1 = ChildUnify(with, childNode);
                        if (rate1 == 0)
                        {
                            SetWith(childNode, with);
                            return rate1 + OR_TRUE;
                        }
                    }
                    catch (Exception e)
                    {
                        AltBot.writeDebugLine("" + e);
                    }
                }
                return OR_FALSE;
            }
            return OR_FALSE;
        }

        protected override Unifiable ComputeInnerOrNull()
        {
            return AsOneOf();
        }
    }
}
