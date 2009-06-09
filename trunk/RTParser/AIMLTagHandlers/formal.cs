using System;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The formal element tells the AIML interpreter to render the contents of the element 
    /// such that the first letter of each word is in uppercase, as defined (if defined) by 
    /// the locale indicated by the specified language (if specified). This is similar to methods 
    /// that are sometimes called "Title Case". 
    /// 
    /// If no character in this Unifiable has a different uppercase version, based on the Unicode 
    /// standard, then the original Unifiable is returned.
    /// </summary>
    public class formal : RTParser.Utils.AIMLTagHandler
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
        public formal(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "formal")
            {
                Unifiable result = Unifiable.CreateAppendable();
                if (!templateNodeInnerText.IsEmpty)
                {
                    Unifiable[] words = templateNodeInnerText.Split();
                    foreach (Unifiable word in words)
                    {
                        Unifiable newWord = word.ToPropper();
                        result.Append(newWord + " ");
                    }
                }
                return result.ToString().Trim();
            }
            return Unifiable.Empty;
        }
    }
}
