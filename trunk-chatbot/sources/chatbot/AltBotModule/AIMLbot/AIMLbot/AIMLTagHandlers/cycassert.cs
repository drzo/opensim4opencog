using System.Xml;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycassert&gt; simple way to assert a CycL statement
    /// </summary>
    public class cycassert : CycTagHandler
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
        public cycassert(AltBot bot,
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
            if (CheckNode("cycassert"))
            {
                string sent = templateNodeInnerText = ((AIMLTagHandler) this).Recurse();
                if (IsValue(sent))
                {
                    string mt = TheCyc.Cyclify(GetAttribValue("mt", Proc.GetUserMt(user, query)));
                    return TheCyc.EvalSubL(
                        SafeFormat("(eval (subseq `(cyc-assert '{0} {1} ) 0 3) )", sent, mt), null);
                }
            }
            return Unifiable.Empty;
        }
    }
}
