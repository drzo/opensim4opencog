using System;
using System.Xml;
using System.Text;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
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
        /// <param name="templateNode">The node to be processed</param>
        public isa(RTParser.RTPBot bot,
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
            if (templateNode.NodeType==XmlNodeType.Text)
            {
                return Proc.IsaFilter(with, templateNode.InnerText);
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
                            return Proc.IsaFilter(with, childNode.InnerText);
                        }
                        if (!Proc.IsaFilter(with, childNode.InnerText)) return false;
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("" + e);
                    }
                }
                return true;
            }
            return true;
        }

        protected override Unifiable ProcessChange()
        {
            return Unifiable.Empty;
        }
    }
}
