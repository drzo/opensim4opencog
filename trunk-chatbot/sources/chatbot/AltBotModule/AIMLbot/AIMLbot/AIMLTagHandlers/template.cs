using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class template : AIMLTagHandler
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
        public template(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            SelfProcessing = true;
        }

        public override bool SelfProcessing
        {
            get { return true; }
            set
            {
                base.SelfProcessing = value;
            }
        }

        public override Unifiable RecurseChildren()
        {
            if (FinalResultValid) return FinalResult;
            if (!IsStarted && QueryHasFailed)
            {
                QueryHasFailed = false;
            }
            Unifiable templateResult = RecurseReal(templateNode, true);
            if (QueryHasFailed)
            {
                return FAIL;
            }
            FinalResult = templateResult;//.ToString();
            return templateResult;
        }

        protected override Unifiable ProcessChangeU()
        {
            if (FinalResultValid) return FinalResult;
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
                if (IsSilentTag(templateNode))
                {
                    return templateResult;
                }
                ResetValues(true);
                templateResult = RecurseReal(templateNode, false);
            }
            if (templateResult == null)
            {
                return null;
            }            
            return AltBot.ReTrimAndspace(AltBot.CleanupCyc(templateResult));
        }
    }
}
