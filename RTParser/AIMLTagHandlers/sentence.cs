using System;
using System.Xml;
using System.Text;
using System.Text.RegularExpressions;

namespace RTParser.AIMLTagHandlers
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
    public class sentence : RTParser.Utils.AIMLTagHandler
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
        public sentence(RTParser.RTPBot bot,
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
            if(this.templateNode.Name.ToLower()=="sentence")
            {
                if (!templateNodeInnerText.IsEmpty)
                {
                    Unifiable result = Unifiable.CreateAppendable();
                    char[] letters = templateNodeInnerText.ToValue(query).Trim().ToCharArray();
                    bool doChange = true;
                    for (int i = 0; i < letters.Length; i++)
                    {
                        string letterAsString = Convert.ToString(letters[i]);
                        if (this.Proc.Splitters.Contains(letterAsString))
                        {
                            doChange = true;
                        }

                        Regex lowercaseLetter = new Regex("[a-zA-Z]");

                        if (lowercaseLetter.IsMatch(letterAsString))
                        {
                            if (doChange)
                            {
                                result.Append(letterAsString.ToUpper(this.Proc.Locale));
                                doChange = false;
                            }
                            else
                            {
                                result.Append(letterAsString.ToLower(this.Proc.Locale));
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
                    XmlNode starNode = Utils.AIMLTagHandler.getNode("<star/>", templateNode);
                    star recursiveStar = new star(this.Proc, this.user, this.query, this.request, this.result, starNode);
                    templateNodeInnerText = recursiveStar.Transform();
                    if (!templateNodeInnerText.IsEmpty)
                    {
                        return this.ProcessChange();
                    }
                    else
                    {
                        return Unifiable.Empty;
                    }
                }
            }

            return Unifiable.Empty;
        }
    }
}
