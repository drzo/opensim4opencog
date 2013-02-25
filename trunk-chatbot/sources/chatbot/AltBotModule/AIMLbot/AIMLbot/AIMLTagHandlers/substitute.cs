using System.Xml;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The atomic version of the person element is a shortcut for: 
    /// 
    /// <person><star/></person> 
    ///
    /// The atomic person does not have any content. 
    /// 
    /// The non-atomic person element instructs the AIML interpreter to: 
    /// 
    /// 1. replace words with first-person aspect in the result of Processing the contents of the 
    /// person element with words with the grammatically-corresponding third-person aspect; and 
    /// 
    /// 2. replace words with third-person aspect in the result of Processing the contents of the 
    /// person element with words with the grammatically-corresponding first-person aspect.
    /// 
    /// The definition of "grammatically-corresponding" is left up to the implementation. 
    /// 
    /// Historically, implementations of person have dealt with pronouns, likely due to the fact that 
    /// most AIML has been written in English. However, the decision about whether to transform the 
    /// person aspect of other words is left up to the implementation.
    /// </summary>
    public class substitute : AIMLDictSubstFormatingTagHandler
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
        public substitute(AltBot bot,
                          User user,
                          SubQuery query,
                          Request request,
                          Result result,
                          XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        #region Overrides of AIMLFormatingTagHandler

        protected override Unifiable Format(Unifiable text)
        {
            if (!IsNullOrEmpty(templateNodeInnerText))
            {
                // non atomic version of the node
                return ApplySubstitutions.Substitute(GetDictionary(), templateNodeInnerText);
            }
            else
            {
                //Need an atomic version
                // atomic version of the node
                XmlNode starNode = getNode("<star/>");
                star recursiveStar = new star(Proc, user, query, request, result, starNode);
                templateNode.InnerText = recursiveStar.Transform();
                if (templateNode.InnerText.Length > 0)
                {
                    return ProcessChangeU();
                }
                else
                {
                    return Unifiable.Empty;
                }

            }
            return Unifiable.Empty;
        }

        protected override ISettingsDictionary GetDictionary()
        {
            return request.GetSubstitutions(GetSubstutionName(), true);

        }

        private string GetSubstutionName()
        {
            return GetAttribValue("dict,substitutions,subst,use,file", templateNode.Name);
        }

        /// <summary>
        /// The subclass only needs to Process the non atomic inner text
        /// </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        /// <summary>
        /// The method that does the actual Processing of the text.
        /// </summary>
        /// <returns>The resulting Processed text</returns>
        protected override Unifiable ProcessChangeU()
        {
            if (isRecursive && !ReadOnly)
            {
                return  Format(TransformAtomically(null, true));
            }
            return TransformAtomically(Format, false);
        }

        #endregion
    }

    abstract public class AIMLDictSubstFormatingTagHandler : AIMLFormatingTagHandler
    {
        protected override bool ExpandingSearchWillYieldNoExtras { get { return true; } }
        protected ParentProvider Provider;
        public AIMLDictSubstFormatingTagHandler(AltBot bot,
                                                User user,
                                                SubQuery query,
                                                Request request,
                                                Result result,
                                                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            IsStarAtomically = true;
        }

        #region Overrides of AIMLFormatingTagHandler

        /// <summary>
        /// The subclass only needs to Process the non atomic inner text
        /// </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        protected override Unifiable Format(Unifiable text)
        {
            if (!IsNullOrEmpty(text))
            {
                // non atomic version of the node
                return ApplySubstitutions.Substitute(GetDictionary(), text);
            }
            return Unifiable.Empty;
        }


        #endregion

        protected virtual ISettingsDictionary GetDictionary()
        {
            ISettingsDictionary dict = request.GetSubstitutions(templateNode.Name.ToLower(), false);
            if (dict != null)
            {
                return dict;
            }
            return substs.PersonSubstitutions;
        }
    }
}