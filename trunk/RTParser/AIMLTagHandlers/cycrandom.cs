using System;
using System.Xml;
using System.Text;
using System.Collections.Generic;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The cycrandom element instructs the AIML interpreter to return exactly one of its contained li 
    /// elements cyc randomly. The cycrandom element must contain one or more li elements of type 
    /// defaultListItem, and cannot contain any other elements.
    /// </summary>
    public class cycrandom : RTParser.Database.CycTagHandler
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
        public cycrandom(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
          //  this.isRecursive = false;
        }
        public override Unifiable CompleteProcess()
        {
            return ProcessChange();
        }

        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "cycrandom")
            {
                Unifiable filter = base.GetAttribValue("filter", null);
                if (!templateNodeInnerText.IsEmpty)
                {
                    return
                        this.TheCyc.EvalSubL(
                            String.Format(
                                "(clet ((list {0})) (nth (random (length list)) list))",
                                base.Recurse()), filter);
                }
            }
            return Unifiable.Empty;
        }
    }
}
