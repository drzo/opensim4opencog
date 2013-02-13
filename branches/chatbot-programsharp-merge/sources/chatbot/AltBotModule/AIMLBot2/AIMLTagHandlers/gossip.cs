using System;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The gossip element instructs the AIML interpreter to capture the result of processing the 
    /// contents of the gossip elements and to store these contents in a manner left up to the 
    /// implementation. Most common uses of gossip have been to store captured contents in a separate 
    /// file. 
    /// 
    /// The gossip element does not have any attributes. It may contain any AIML template elements.
    /// </summary>
    public class gossip : RTParser.Utils.AIMLFormatingTagHandler
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
        public gossip(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        /// <summary>
        /// The method that does the actual processing of the text.
        /// 
        /// This like the think tag retunns nothihng.. but logs it to the console
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected override Unifiable Format(Unifiable templateNodeInnerText)
        {
            if (CheckNode("gossip"))
            {
                // gossip is merely logged by the Proc and written to log files
                if (!IsNullOrEmpty(templateNodeInnerText))
                {
                    writeToLog(SafeFormat("GOSSIP from user: {0}, '{1}'", this.user.UserID, Unifiable.DescribeUnifiable(templateNodeInnerText)));
                }
            }
            return Succeed(templateNodeInnerText);
        }
    }
}
