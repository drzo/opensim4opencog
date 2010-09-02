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
            if (templateNode.NodeType==XmlNodeType.Text)
            {
                string srch = (" " + with.ToValue(query) + " ").ToUpper();
                return ((" " + templateNode.InnerText + " ").ToUpper().Contains(srch)) ? OPT_TRUE : OPT_FALSE;
            }
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    try
                    {
                        if (childNode.NodeType == XmlNodeType.Text)
                        {
                            string srch = (" " + with.ToValue(query) + " ").ToUpper();
                            return ((" " + childNode.InnerText + " ").ToUpper().Contains(srch)) ? OPT_TRUE : OPT_FALSE;
                        }
                        AIMLTagHandler part = GetChildTagHandler(childNode);
                        if (part.CallCanUnify(with)==0) return OPT_TRUE;
                    }
                    catch (Exception e)
                    {
                        RTPBot.writeDebugLine("" + e);
                    }
                }
                return OPT_FALSE;
            }
            return OPT_TRUE;
        }

        protected override Unifiable ProcessChange()
        {
            return Unifiable.Empty;
        }
    }
}
