using System.Xml;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The atomic version of the person2 element is a shortcut for: 
    /// 
    /// <person2><star/></person2> 
    /// 
    /// The atomic person2 does not have any content.
    /// 
    /// The non-atomic person2 element instructs the AIML interpreter to: 
    /// 
    /// 1. replace words with first-person aspect in the result of Processing the contents of the 
    /// person2 element with words with the grammatically-corresponding second-person aspect; and,
    /// 
    /// 2. replace words with second-person aspect in the result of Processing the contents of the 
    /// person2 element with words with the grammatically-corresponding first-person aspect. 
    /// 
    /// The definition of "grammatically-corresponding" is left up to the implementation.
    /// 
    /// Historically, implementations of person2 have dealt with pronouns, likely due to the fact 
    /// that most AIML has been written in English. However, the decision about whether to transform 
    /// the person aspect of other words is left up to the implementation.
    /// </summary>
    public class person2 : AIMLDictSubstFormatingTagHandler
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
        public person2(AltBot bot,
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
            if (CheckNode("person2"))
            {
                if (!IsNullOrEmpty(templateNodeInnerText))
                {
                    // non atomic version of the node
                    return ApplySubstitutions.Substitute(substs.Person2Substitutions, templateNodeInnerText);
                }
                else
                {
                    writeToLogWarn("SHOULD NEVER GET HERE!");
                    // atomic version of the node
                    templateNodeInnerText = GetStarContent();
                    bool starFailed = IsNull(templateNodeInnerText);
                    if (starFailed)
                    {
                        QueryHasFailed = true;
                        return UnifiableEmpty;
                    }
                    if (!IsNullOrEmpty(templateNodeInnerText))
                    {
                        return ((AIMLTagHandler) this).Recurse();
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
