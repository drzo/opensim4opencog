using System;
using System.Xml;
using System.Text;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The format element tells the AIML interpreter to render the contents of the element 
    /// </summary>
    public class format_prev : Utils.AIMLTagHandler
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
        public format_prev(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode, Func<string, string> formatter)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = true;
            Formatter = formatter;            
        }

        private readonly Func<string, string> Formatter;
        protected string[] splitter = null;
        public override bool IsFormatter
        {
            get { return true; }
        }
        protected override Unifiable ProcessChangeU()
        {
            if (Formatter == null) return (string) Recurse();
            StringBuilder result = new StringBuilder();            
            if (this.TemplateNodeHasText)
            {
                bool needSpace = false;
                string[] words = splitter != null
                                     ? ((string) this.Recurse()).Split(splitter, StringSplitOptions.None)
                                     : new[] {(string) this.Recurse()};
                foreach (string word in words)
                {
                    string newWord = Formatter(word);
                    if (needSpace) result.Append(splitter); else needSpace = true;
                    result.Append(newWord);                    
                }
            }
            return result.ToString();
        }
    }
}
