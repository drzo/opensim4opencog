using System;
using System.Linq;
using System.Xml;
using System.Text;
using System.Text.RegularExpressions;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The sentence element tells the AIML interpreter to render the contents of the element 
    /// such that the first letter of each sentence is in uppercase, as defined (if defined) by 
    /// the locale indicated by the specified language (if specified). Sentences are interpreted 
    /// as strings whose last character is the period or full-stop character .. If the Unifiable does 
    /// not contain a ., then the entire Unifiable is treated as a sentence.
    /// 
    /// If no character in this Unifiable has a different uppercase version, based on the Unicode 
    /// standard, then the original Unifiable is returned. 
    /// </summary>
    public class sentence : AIMLFormatingTagHandler
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
        public sentence(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        /// <summary>
        /// The method that does the actual Processing of the text.
        /// </summary>
        /// <returns>The resulting Processed text</returns>
        protected override Unifiable Format(Unifiable templateNodeInnerText)
        {
            if (CheckNode("sentence"))
            {
                string s = templateNodeInnerText;
                if (!IsNullOrEmpty(templateNodeInnerText))
                {
                    s = s.Substring(0, 1).ToUpper() + s.Substring(1);
                    s = s.Replace(" ,", ",");
                    return s;
                }
                return Unifiable.Empty;
            }
            return Unifiable.Empty;
        }
    }
}
