using System.Xml;
using AltAIMLParser;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycterm&gt; translates an English word/phrase into a Cyc symbol 
    /// </summary>
    public class cycterm : AIMLTagHandler, CanReturnFailure
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
        public cycterm(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            //isRecursive = false;
        }

        protected override Unifiable ProcessChangeU()
        {
            if (base.CheckNode("cycterm"))
            {
                Unifiable filter = base.GetAttribValue("filter", GetAttribValue("isa", "Thing"));
                Unifiable pos = base.GetAttribValue("pos", null);
                int maxWords = int.Parse(base.GetAttribValue("maxwords", "1"));
                Unifiable r = Recurse();
                if (Unifiable.IsNullOrEmpty(r))
                {
                    QueryHasFailed = true;
                    return FAIL;
                }
                string s = r.ToValue(query);
                if (s.Split(' ').Length > maxWords)
                {
                    QueryHasFailed = true;
                    return FAIL;
                }
                Unifiable term;
                if (Proc.TheCyc.Lookup(r, filter, out term, query))
                {
                    s = term.AsString();
                    if (s.Length < 2)
                    {
                        writeToLog("CYCTERM: " + r + "=>" + s);
                    }
                    return term;
                }
                else
                {
                    QueryHasFailed = true;
                    return FAIL;
                }
            }
            return Unifiable.Empty;
        }
    }
}
