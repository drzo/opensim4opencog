using System;
using System.Xml;
using System.Text;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The sr element is a shortcut for: 
    /// 
    /// <srai><star/></srai> 
    /// 
    /// The atomic sr does not have any content. 
    /// </summary>
    public class sr : RTParser.Utils.AIMLTagHandler
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
        public sr(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "sr")
            {
                Unifiable starContent = GetStarContent();

                XmlNode sraiNode = RTParser.Utils.AIMLTagHandler.getNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
                LineInfoElementImpl.unsetReadonly(sraiNode);
                srai sraiHandler = new srai(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
                return sraiHandler.Transform();
            }
            return Unifiable.Empty;
        }
    }
}
