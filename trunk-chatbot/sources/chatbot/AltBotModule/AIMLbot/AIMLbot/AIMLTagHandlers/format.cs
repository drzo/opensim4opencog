using System;
using System.Xml;
using System.Text;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The formal element tells the AIML interpreter to render the contents of the element 
    /// such that the first letter of each word is in uppercase, as defined (if defined) by 
    /// the locale indicated by the specified language (if specified). This is similar to methods 
    /// that are sometimes called "Title Case". 
    /// 
    /// If no character in this string has a different uppercase version, based on the Unicode 
    /// standard, then the original string is returned.
    /// </summary>
    public class format : AltAIMLbot.Utils.AIMLTagHandler
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
        public format(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode, Func<string, string> formatter)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = true;
            Formatter = formatter;
        }

        private readonly Func<string, string> Formatter;

        protected override string ProcessChange()
        {
            if (Formatter == null) return TemplateNodeInnerText;
            StringBuilder result = new StringBuilder();
            if (this.TemplateNodeHasText)
            {
                bool needSpace = true;
                string[] words = this.TemplateNodeInnerText.Split();
                foreach (string word in words)
                {
                    string newWord = Formatter(word);
                    if (needSpace) result.Append(" "); else needSpace = true;
                    result.Append(newWord);                    
                }
            }
            return " " + result.ToString();
        }
    }
}
