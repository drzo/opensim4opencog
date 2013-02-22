using System;
using System.Xml;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycquery&gt; executes a CycL statement and returns the result 
    /// </summary>
    public class cycquery : CycTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the query</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public cycquery(AltBot bot,
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
            if (CheckNode("cycquery"))
            {
                Unifiable filter = GetAttribValue("filter", null);
                Unifiable varname = base.GetAttribValue("varname", "?REPLY");
                String sent = ((AIMLTagHandler) this).Recurse();
                string mt = TheCyc.Cyclify(GetAttribValue("mt", /*Proc.GetUserMt(user)*/ "#$EverythingPSC"));
                if (!IsEMPTY(sent))
                {
                    if (WhenTrue(TheCyc.EvalSubL(String.Format("(ask-template '{0} `{1} {2})", varname, sent, mt), filter)))
                    {
                        Succeed();
                        return FinalResult;
                    }
                }
            }
            return Unifiable.Empty;
        }
    }
}
