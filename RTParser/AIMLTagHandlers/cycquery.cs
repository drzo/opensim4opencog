using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycquery&gt; executes a CycL statement and returns the result 
    /// </summary>
    public class cycquery : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the query</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public cycquery(RTParser.Bot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override string ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "cycquery")
            {
                string filter = base.GetAttribValue("filter");
                if (templateNodeInnerText.Length > 0)
                {
                    return this.bot.EvalSubL(String.Format("(fi-ask '{0} #$EverythingPSC)", Recurse()),filter);
                }
            }
            return string.Empty;
        }
    }
}
