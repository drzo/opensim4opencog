using System;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class isa : UnifibleTagHandler
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
        public isa(AltBot bot,
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
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                return Proc.TheCyc.IsaFilter(with, templateNode.InnerText) ? ISA_TRUE : ISA_FALSE;
            }
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    try
                    {
                        Unifiable processChildNode = ProcessChildNode(childNode);
                        if (!Proc.TheCyc.IsaFilter(with, processChildNode)) return ISA_FALSE;
                        SetWith(childNode, with);
                    }
                    catch (Exception e)
                    {
                        AltBot.writeDebugLine("" + e);
                    }
                }
                return ISA_TRUE;
            }
            return ISA_TRUE;
        }

        protected override Unifiable ComputeInnerOrNull()
        {
            return AsOneOf();
        }
    }
}
