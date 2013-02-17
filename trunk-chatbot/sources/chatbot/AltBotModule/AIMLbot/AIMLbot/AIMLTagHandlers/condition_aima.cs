#if STANDALONE_TESTS
#else
using System;
using System.Xml;
using System.Text;
using System.Text.RegularExpressions;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using Aima.Core.Logic.Propositional.Visitors;
using AltAIMLParser;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;
using Unifiable = System.String;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The condition element instructs the AIML interpreter to return specified contents depending 
    /// upon the results of matching a predicate against a pattern. 
    /// 
    /// NB: The condition element has three different types. The three different types specified 
    /// here are distinguished by an xsi:type attribute, which permits a validating XML Schema 
    /// processor to validate them. Two of the types may contain li elements, of which there are 
    /// three different types, whose validity is determined by the type of enclosing condition. In 
    /// practice, an AIML interpreter may allow the omission of the xsi:type attribute and may instead 
    /// heuristically determine which type of condition (and hence li) is in use. 
    /// 
    /// Block Condition 
    /// ---------------
    /// 
    /// The blockCondition type of condition has a required attribute "name", which specifies an AIML 
    /// predicate, and a required attribute "value", which contains a simple pattern expression. 
    ///
    /// If the contents of the value attribute match the value of the predicate specified by name, then 
    /// the AIML interpreter should return the contents of the condition. If not, the empty string "" 
    /// should be returned.
    /// 
    /// Single-predicate Condition 
    /// --------------------------
    /// 
    /// The singlePredicateCondition type of condition has a required attribute "name", which specifies 
    /// an AIML predicate. This form of condition must contain at least one li element. Zero or more of 
    /// these li elements may be of the valueOnlyListItem type. Zero or one of these li elements may be 
    /// of the defaultListItem type.
    /// 
    /// The singlePredicateCondition type of condition is processed as follows: 
    ///
    /// Reading each contained li in order: 
    ///
    /// 1. If the li is a valueOnlyListItem type, then compare the contents of the value attribute of 
    /// the li with the value of the predicate specified by the name attribute of the enclosing 
    /// condition. 
    ///     a. If they match, then return the contents of the li and stop processing this condition. 
    ///     b. If they do not match, continue processing the condition. 
    /// 2. If the li is a defaultListItem type, then return the contents of the li and stop processing
    /// this condition.
    /// 
    /// Multi-predicate Condition 
    /// -------------------------
    /// 
    /// The multiPredicateCondition type of condition has no attributes. This form of condition must 
    /// contain at least one li element. Zero or more of these li elements may be of the 
    /// nameValueListItem type. Zero or one of these li elements may be of the defaultListItem type.
    /// 
    /// The multiPredicateCondition type of condition is processed as follows: 
    ///
    /// Reading each contained li in order: 
    ///
    /// 1. If the li is a nameValueListItem type, then compare the contents of the value attribute of 
    /// the li with the value of the predicate specified by the name attribute of the li. 
    ///     a. If they match, then return the contents of the li and stop processing this condition. 
    ///     b. If they do not match, continue processing the condition. 
    /// 2. If the li is a defaultListItem type, then return the contents of the li and stop processing 
    /// this condition. 
    /// 
    /// ****************
    /// 
    /// Condition List Items
    /// 
    /// As described above, two types of condition may contain li elements. There are three types of 
    /// li elements. The type of li element allowed in a given condition depends upon the type of that 
    /// condition, as described above. 
    /// 
    /// Default List Items 
    /// ------------------
    /// 
    /// An li element of the type defaultListItem has no attributes. It may contain any AIML template 
    /// elements. 
    ///
    /// Value-only List Items
    /// ---------------------
    /// 
    /// An li element of the type valueOnlyListItem has a required attribute value, which must contain 
    /// a simple pattern expression. The element may contain any AIML template elements.
    /// 
    /// Name and Value List Items
    /// -------------------------
    /// 
    /// An li element of the type nameValueListItem has a required attribute name, which specifies an 
    /// AIML predicate, and a required attribute value, which contains a simple pattern expression. The 
    /// element may contain any AIML template elements. 
    /// </summary>
    public class condition_aima : AIMLTagHandlerU
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
        public condition_aima(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            this.isRecursive = false;
        }
        protected int maxTrueConditions = 1;
        protected int currentTrueConditions = 0;
        protected override Unifiable ProcessChangeU()
        {
            if (this.TemplateNodeName == "condition")
            {
                // heuristically work out the type of condition being processed
                string name = GetAttribValue("name,var", null);
                string conditionValue = GetAttribValue("value", null);


                string type = GetAttribValue("type,dict", null);
                ISettingsDictionary dict = type == null ? null : request.GetDictionary(type);

                // old style name=value test
                if (name != null && conditionValue != null)
                {
                    dict = dict ?? request.TargetSettings;
                    string actualValue = dict.grabSetting(name);
                    if (IsPredMatch(conditionValue, actualValue))
                    {
                        return this.TemplateNodeInnerXml;
                    }
                    return string.Empty;
                }


                // multi-predicate
                foreach (XmlNode childLINode in this.templateNode.ChildNodes)
                {
                    if (childLINode.Name.ToLower() == "li")
                    {
                        // check the PEParser query for trueness
                        string isTrueQuery = GetAttribValue<string>(childLINode, "istrue", null);
                        if (isTrueQuery != null)
                        {
                            bool valid = false;
                            Sentence sen = (Sentence) new PEParser().Parse(isTrueQuery);
                            try
                            {
                                valid = bot.myActiveModel.IsTrue(sen);
                            }
                            catch
                            {
                                valid = false;
                            }
                            if (valid)
                            {
                                return childLINode.InnerXml;
                            }
                            continue;
                        }

                        // check for the <li> name and value
                        string cvalue = GetAttribValue<string>(childLINode, "value", null);
                        string cname = GetAttribValue<string>(childLINode, "name,var", null);
                        // if <li> name=""  pretend it doesnt exist
                        if (cname != null && cname.Trim() == "") cname = null;

                        // the fallthru (ussually the last <li> element)
                        if (cname == null && cvalue == null)
                        {
                            return childLINode.InnerXml;
                        }

                        // compare <condition> name to <li> value
                        if (cname == null && cvalue != null)
                        {
                            string ctype = GetAttribValue("type,dict", null);
                            ISettingsDictionary cdict = (ctype == null ? dict : request.GetDictionary(ctype)) ??
                                                       request.TargetSettings;
                            conditionValue = conditionValue ?? cdict.grabSetting(name) ?? String.Empty;
                            if (IsPredMatch(cvalue, conditionValue))
                            {
                                return childLINode.InnerXml;
                            }
                            continue;
                        }

                        // compare <li> name to <li> value
                        if (cname != null && cvalue != null)
                        {
                            string ctype = GetAttribValue("type,dict", null);
                            ISettingsDictionary cdict = (ctype == null ? dict : request.GetDictionary(ctype)) ??
                                                       request.TargetSettings;
                            string actualValue = cdict.grabSetting(cname);
                            if (IsPredMatch(cvalue, actualValue))
                            {
                                return childLINode.InnerXml;
                            }
                            continue;
                        }
                    }
                }
            }
            return string.Empty;
        }

        private bool IsPredMatch(string value, string actualValue)
        {
            value = "AAA " + value + " ZZZ";
            actualValue = "AAA " + actualValue + " ZZZ";
            Regex matcher = new Regex(value.Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+"),
                                      RegexOptions.IgnoreCase);
            return matcher.IsMatch(actualValue);
        }
    }
}
#endif //STANDALONE_TESTS
