using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;guard&gt; translates a Cyc symbol into an English word/phrase
    /// </summary>
    public class guard : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>                    s
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public guard(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = false;
        }


        protected override string ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "guard")
            {
                if (templateNodeInnerText.Length > 0)
                {
                    this.query.GuardFailed = false;

                    string result = this.Proc.SystemExecute(Recurse(), GetAttribValue("lang", "subl"));
                    if (String.IsNullOrEmpty(result) || result == "NIL")
                    {
                        this.query.GuardFailed = true;
                    }
                }
            }
            return string.Empty;
        }

    }
}
