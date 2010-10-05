using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycassert&gt; simple way to assert a CycL statement
    /// </summary>
    public class cycassert : RTParser.Database.CycTagHandler
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
        public cycassert(RTParser.RTPBot bot,
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
            if (CheckNode("cycassert"))
            {
                string sent = templateNodeInnerText = Recurse();
                if (IsValue(sent))
                {
                    string mt = TheCyc.Cyclify(GetAttribValue("mt", Proc.GetUserMt(user, query)));
                    return this.TheCyc.EvalSubL(
                        String.Format("(eval (subseq `(cyc-assert '{0} {1} ) 0 3) )", sent, mt), null);
                }
            }
            return Unifiable.Empty;
        }
    }
}
