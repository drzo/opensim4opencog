using System;
using System.Xml;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycsystem&gt; executes a CycL statement and returns the result 
    /// </summary>
    public class cycsystem : RTParser.Database.CycTagHandler
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
        public cycsystem(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }
        public override Unifiable CompleteProcess()
        {
            return ProcessChange();
        }

        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "cycsystem")
            {
                Unifiable filter = base.GetAttribValue("filter", null);
                if (!templateNodeInnerText.IsEmpty)
                {
                    if (WhenTrue(this.TheCyc.EvalSubL(Recurse(), filter)))
                    {
                        if (query.CurrentTemplate != null) query.CurrentTemplate.Rating *= 1.5;
                        return templateNodeInnerText;
                    }
                }
            }
            return Unifiable.Empty;
        }
    }
}
