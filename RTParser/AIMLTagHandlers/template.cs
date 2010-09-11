using System;
using System.Xml;
using System.Text;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class template : RTParser.Utils.AIMLTagHandler
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
        public template(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChange()
        {
            if (!IsStarted && QueryHasFailed)
            {
                QueryHasFailed = false;
            }
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    Unifiable part = ProcessChildNode(childNode, false, false);
                        templateResult.Append(part);
                    }
            }
            return RecurseResult = templateResult;//.ToString();
        }

        public override Unifiable CompleteProcess()
        {
            if (!Unifiable.IsNull(RecurseResult))
            {
                return RecurseResult;
            }
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (query.CurrentTemplate != null)
            {
                Succeed();
            }
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    Unifiable part = ProcessChildNode(childNode, true, false);
                        templateResult.Append(part);
                }
            }
            return templateResult;//.ToString();
        }
    }
}
