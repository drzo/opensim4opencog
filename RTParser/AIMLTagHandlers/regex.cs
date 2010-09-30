using System;
using System.Text.RegularExpressions;
using System.Xml;
using System.Text;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
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
        /// <param name="templateNode">The node to be processed</param>
        public regex(RTParser.RTPBot bot,
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

            string re = ComputeInner();
            var matcher = new Regex(re);
            if (matcher.IsMatch(with.ToValue(query))) return AND_TRUE;
            return AND_FALSE;
        }

        public string ComputeInner()
        {
            string re = "";
            if (templateNode.NodeType==XmlNodeType.Text)
            {
                re = templateNodeInnerText.AsString();
            }
            else if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    re += childNode.InnerText;
                }
            }
            else
            {
                re = Recurse();
                templateNodeInnerText = re;
            }
            return re;
        }

        protected override Unifiable ProcessChange()
        {
            var v1 = ComputeInner();
            var v2 = templateNodeInnerText;
            return v2;
        }
    }
}
