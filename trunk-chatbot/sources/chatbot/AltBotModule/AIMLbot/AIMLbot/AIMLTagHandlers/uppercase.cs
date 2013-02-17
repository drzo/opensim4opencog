using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The uppercase element tells the AIML interpreter to render the contents of the element
    /// in uppercase, as defined (if defined) by the locale indicated by the specified language
    /// if specified).
    /// 
    /// If no character in this Unifiable has a different uppercase version, based on the Unicode 
    /// standard, then the original Unifiable is returned. 
    /// </summary>
    public class uppercase : AIMLFormatingTagHandler
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
        public uppercase(AltBot bot,
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
            if (CheckNode("uppercase"))
            {
                return templateNodeInnerText.ToValue(query).ToUpper(Proc.Locale);
            }
            return Unifiable.Empty;
        }
    }
}
