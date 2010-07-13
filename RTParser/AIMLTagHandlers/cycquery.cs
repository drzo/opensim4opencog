using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycquery&gt; executes a CycL statement and returns the result 
    /// </summary>
    public class cycquery : RTParser.Database.CycTagHandler
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
                String sent = Recurse();
                string mt = TheCyc.Cyclify(GetAttribValue("mt", /*Proc.GetUserMt(user)*/ "#$EverythingPSC"));
                if (!templateNodeInnerText.IsEmpty)
                {
                    if (WhenTrue(this.TheCyc.EvalSubL(String.Format("(ask-template '{0} `{1} {2})", varname, sent, mt), filter)))
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
