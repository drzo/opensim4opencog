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
            Unifiable templateResult = RecurseReal(templateNode, true);
            if (QueryHasFailed)
            {
                return FAIL;
            }
            RecurseResult = templateResult;//.ToString();
            return templateResult;
        }

        public override Unifiable CompleteProcess()
        {
            if (RecurseResultValid) return RecurseResult;
            TemplateInfo queryTemplate = query.CurrentTemplate;
            if (queryTemplate != null)
            {
                if (queryTemplate.IsDisabledOutput)
                {
                    return Unifiable.INCOMPLETE;
                }
                Succeed();
                request.MarkTemplate(queryTemplate);
            }
            Unifiable templateResult = RecurseReal(templateNode, false);
            Unifiable test = templateResult;
            if (Unifiable.IsEMPTY(test))
            {
                if (QueryHasFailed)
                {
                    return FAIL;
                }
                if (IsSilentTag(this.templateNode))
                {
                    return templateResult;
                }
                ResetValues(true);
                templateResult = RecurseReal(templateNode, false);
            }
            string tr = templateResult;
            string tr2 = RTPBot.ReplaceAll(tr.Replace("THINKYTAG.", " "), "THINKYTAG", " ").Replace("  ", " ").Trim();
            if (tr != tr2)
            {
                if (tr2 == "") return "THINKYTAG";
                return tr2;
            }
            return tr2;
            //return templateResult;
        }
    }
}
