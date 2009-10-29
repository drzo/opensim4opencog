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
        public cycquery(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "cycquery")
            {
                Unifiable filter = GetAttribValue("filter", null);
                Unifiable varname = base.GetAttribValue("varname", "?REPLY");
                Unifiable mt = GetAttribValue("mt", "EverythingPSC");
                if (!templateNodeInnerText.IsEmpty)
                {
                    return this.Proc.EvalSubL(String.Format("(ask-template '{0} `{1} {2})",varname, Recurse(), Proc.Cyclify(mt)), filter);
                }
            }
            return Unifiable.Empty;
        }
    }
}
