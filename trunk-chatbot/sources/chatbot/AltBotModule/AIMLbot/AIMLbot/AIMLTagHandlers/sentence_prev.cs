using System;
using System.Linq;
using System.Xml;
using System.Text;
using System.Text.RegularExpressions;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The sentence element tells the AIML interpreter to render the contents of the element 
    /// such that the first letter of each sentence is in uppercase, as defined (if defined) by 
    /// the locale indicated by the specified language (if specified). Sentences are interpreted 
    /// as strings whose last character is the period or full-stop character .. If the string does 
    /// not contain a ., then the entire string is treated as a sentence.
    /// 
    /// If no character in this string has a different uppercase version, based on the Unicode 
    /// standard, then the original string is returned. 
    /// </summary>
    public class sentence_prev : Utils.AIMLTagHandler
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
        public sentence_prev(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        protected override Unifiable ProcessChangeU()
        {
            if(this.TemplateNodeName=="sentence")
            {
                if (this.TemplateNodeHasText)
                {
                    StringBuilder result = new StringBuilder();
                    char[] letters = ((string) this.Recurse()).Trim().ToCharArray();
                    bool doChange = true;
                    for (int i = 0; i < letters.Length; i++)
                    {
                        string letterAsString = Convert.ToString(letters[i]);
                        if (AltBot.Splitters.Contains(letterAsString))
                        {
                            doChange = true;
                        }

                        Regex lowercaseLetter = new Regex("[a-zA-Z]");

                        if (lowercaseLetter.IsMatch(letterAsString))
                        {
                            if (doChange)
                            {
                                result.Append(letterAsString.ToUpper(this.bot.Locale));
                                doChange = false;
                            }
                            else
                            {
                                result.Append(letterAsString.ToLower(this.bot.Locale));
                            }
                        }
                        else
                        {
                            result.Append(letterAsString);
                        }
                    }
                    return result.ToString();
                }
                else
                {
                    // atomic version of the node
                    // calls ProcessChange() one more time and should not get here again
                    return base.RecurseStar();
                }
            }

            return string.Empty;
        }
    }
}
