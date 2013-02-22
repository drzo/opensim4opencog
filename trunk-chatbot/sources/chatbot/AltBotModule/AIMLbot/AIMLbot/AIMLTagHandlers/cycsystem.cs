using System.Xml;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycsystem&gt; executes a CycL statement and returns the result 
    /// </summary>
    public class cycsystem : CycTagHandler
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
        public cycsystem(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChangeU()
        {
            if (templateNode.Name.ToLower() == "cycsystem")
            {
                Unifiable filter = base.GetAttribValue("filter", null);
                if (!IsNullOrEmpty(templateNodeInnerText))
                {
                    if (WhenTrue(TheCyc.EvalSubL(((AIMLTagHandler) this).Recurse(), filter)))
                    {
                        base.Succeed();
                        return templateNodeInnerText;
                    }
                }
            }
            return Unifiable.Empty;
        }
    }
}
