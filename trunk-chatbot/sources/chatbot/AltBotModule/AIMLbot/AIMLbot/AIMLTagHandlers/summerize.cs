using System;
using System.Xml;
using System.Text;

namespace AltAIMLbot.AIMLTagHandlers
{
     public class summerize : AltAIMLbot.Utils.AIMLTagHandler
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
        public summerize(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = true;
        }

        protected override string ProcessChange()
        {
            string msg = string.Empty ;
            if (this.TemplateNodeName == "summerize")
            {
                if (this.TemplateNodeHasText)
                {
                    // non atomic version of the node
                    msg = this.TemplateNodeInnerText;
                    int limit = 256;
                    if (TemplateNodeAttributes["max"] != null)
                    {
                        string rx = TemplateNodeAttributes["max"].Value;

                        limit = int.Parse(rx);
                    }

                    if (msg.Length > limit)
                    {
                        TokenRanker myRanker = new TokenRanker();
                        myRanker.defineRank(msg);
                        string myRankSummary = myRanker.summaryByRank(limit);
                        string mySeqSummary = myRanker.summaryByOriginalSequence(limit);
                        return mySeqSummary;
                    }
                    else
                    {
                        return msg;
                    }

                }
            }
            return string.Empty;
        }
    }
}
