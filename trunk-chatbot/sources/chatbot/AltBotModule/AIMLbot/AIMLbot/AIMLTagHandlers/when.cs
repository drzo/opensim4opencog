using System;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The date element tells the AIML interpreter that it should substitute the system local 
    /// date and time. No formatting constraints on the output are specified.
    /// 
    /// The date element does not have any content. 
    /// </summary>
    public class when : UnifibleTagHandler
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
        public when(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        public override float CanUnify(Unifiable with)
        {
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                string srch = (" " + with.ToValue(query) + " ").ToUpper();
                return ((" " + templateNode.InnerText + " ").ToUpper().Contains(srch)) ? AND_TRUE : AND_FALSE;
            }
            if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    try
                    {
                        if (childNode.NodeType == XmlNodeType.Text)
                        {
                            string srch = (" " + with.ToValue(query) + " ").ToUpper();
                            return ((" " + childNode.InnerText + " ").ToUpper().Contains(srch)) ? AND_TRUE : AND_FALSE;
                        }
                        AIMLTagHandler part = GetChildTagHandler(childNode);
                        if (part.CallCanUnify(with) > 0) return AND_FALSE;
                    }
                    catch (Exception e)
                    {
                        Proc.writeToLog(e);
                        writeToLogWarn("" + e);
                    }
                }
                return AND_TRUE;
            }
            return AND_TRUE;
        }

        protected override Unifiable ComputeInnerOrNull()
        {
            return Recurse();
        }
    }
}