using System;
using System.Xml;
using System.Text;
using AltAIMLParser;
using RTParser;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The sr element is a shortcut for: 
    /// 
    /// <srai><star/></srai> 
    /// 
    /// The atomic sr does not have any content. 
    /// </summary>
    public class sr : Utils.AIMLTagHandler
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
        public sr(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        protected override string ProcessChange()
        {
            string starContent = GetStarContent();
            XmlNode sraiNode = Utils.AIMLTagHandler.getNode("<srai>" + starContent + "</srai>");
            srai sraiHandler = new srai(this.bot, this.user, this.query, this.request, this.result, sraiNode);
            return sraiHandler.Transform();
        }
    }
}
