using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycphrase&gt; translates a Cyc symbol into an English word/phrase
    /// </summary>
    public class cycphrase : RTParser.Database.CycTagHandler
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
        public cycphrase(RTParser.RTPBot bot,
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
           // if (this.templateNode.Name.ToLower() == "cycphrase")
            {
                if (!templateNodeInnerText.IsEmpty)
                {
                    return this.TheCyc.Paraphrase(Recurse());
                }
            }
            return Unifiable.Empty;
        }
    }
}
