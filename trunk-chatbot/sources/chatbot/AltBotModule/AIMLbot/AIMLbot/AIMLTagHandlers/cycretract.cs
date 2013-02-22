using System;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycretract&gt; simple way to retract a CycL statement
    /// </summary>
    public class cycretract : AIMLTagHandler
    {
        public cycretract(){}
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public cycretract(AltBot bot,
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
            if (templateNode.Name.ToLower() == "cycretract")
            {                
                if (!IsNullOrEmpty(templateNodeInnerText))
                {
                    return Proc.TheCyc.EvalSubL(String.Format("(cyc-unassert `{0})", Recurse()), null);
                }
            }
            return Unifiable.Empty;
        }
    }
}
