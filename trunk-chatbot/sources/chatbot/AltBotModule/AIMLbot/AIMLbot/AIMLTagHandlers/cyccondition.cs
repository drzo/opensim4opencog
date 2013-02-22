using System;
using System.Xml;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The cyccondition element instructs the AIML interpreter to return specified contents depending 
    /// upon the results of matching a predicate against a pattern. 
    /// 
    /// NB: The cyccondition element has three different types. The three different types specified 
    /// here are distinguished by an xsi:type attribute, which permits a validating XML Schema 
    /// Processor to validate them. Two of the types may contain li elements, of which there are 
    /// three different types, whose validity is determined by the type of enclosing cyccondition. In 
    /// practice, an AIML interpreter may allow the omission of the xsi:type attribute and may instead 
    /// heuristically determine which type of cyccondition (and hence li) is in use. 
    /// 
    /// Block Condition 
    /// ---------------
    /// 
    /// The blockCondition type of condition has a required attribute "name", which specifies an AIML 
    /// predicate, and a required attribute "value", which contains a simple pattern expression. 
    ///
    /// If the contents of the value attribute match the value of the predicate specified by name, then 
    /// the AIML interpreter should return the contents of the cyccondition. If not, the empty Unifiable "" 
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
    /// The singlePredicateCondition type of condition is Processed as follows: 
    ///
    /// Reading each contained li in order: 
    ///
    /// 1. If the li is a valueOnlyListItem type, then compare the contents of the value attribute of 
    /// the li with the value of the predicate specified by the name attribute of the enclosing 
    /// condition. 
    ///     a. If they match, then return the contents of the li and stop Processing this condition. 
    ///     b. If they do not match, continue Processing the condition. 
    /// 2. If the li is a defaultListItem type, then return the contents of the li and stop Processing
    /// this condition.
    /// 
    /// Multi-predicate Condition 
    /// -------------------------
    /// 
    /// The multiPredicateCondition type of condition has no attributes. This form of condition must 
    /// contain at least one li element. Zero or more of these li elements may be of the 
    /// nameValueListItem type. Zero or one of these li elements may be of the defaultListItem type.
    /// 
    /// The multiPredicateCondition type of condition is Processed as follows: 
    ///
    /// Reading each contained li in order: 
    ///
    /// 1. If the li is a nameValueListItem type, then compare the contents of the value attribute of 
    /// the li with the value of the predicate specified by the name attribute of the li. 
    ///     a. If they match, then return the contents of the li and stop Processing this condition. 
    ///     b. If they do not match, continue Processing the condition. 
    /// 2. If the li is a defaultListItem type, then return the contents of the li and stop Processing 
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
    public class cyccondition : AIMLTagHandler
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
        public cyccondition(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isRecursive = false;
        }

        protected override Unifiable ProcessChangeU()
        {
            return InnerSource();
        }
        protected override string InnerSource()
        {
            if (templateNode.Name.ToLower() == "condition")
            {
                // heuristically work out the type of condition being Processed
                int tncount = AttributesCount(templateNode, "name,value");
                if (tncount == 2) // block
                {
                    Unifiable name = GetAttribValue("name", null);
                    Unifiable value = GetAttribValue("value", null);
                    if ((name != null) & (value != null))
                    {
                        Unifiable actualValue = query.grabSetting(name);
                        if (IsPredMatch(value, actualValue, query))
                        {
                            Succeed();
                            return Unifiable.InnerXmlText(templateNode);
                        }
                        return Unifiable.Empty;
                    }
                    UnknownCondition();
                }
                else if (tncount == 1) // single predicate
                {
                    if (templateNode.Attributes[0].Name == "name")
                    {
                        string name = GetAttribValue("name", String.Empty);

                        foreach (XmlNode childLINode in templateNode.ChildNodes)
                        {
                            int cac = AttributesCount(childLINode, "name,value");
                            if (childLINode.Name.ToLower() == "li")
                            {
                                if (cac == 1)
                                {
                                    if (childLINode.Attributes[0].Name.ToLower() == "value")
                                    {
                                        bool succeed;
                                        Unifiable actualValue = GetActualValue(childLINode, name, childLINode.Name, out succeed, query); ;
                                        Unifiable value = GetAttribValue<Unifiable>(childLINode, "value", NullUnifyFunct, ReduceStarAttribute<Unifiable>);
                                        if (IsPredMatch(value, actualValue, query))
                                        {
                                            Succeed();
                                            return Unifiable.InnerXmlText(childLINode);
                                        }
                                    }
                                }
                                else if (cac == 0)
                                {
                                    Succeed();
                                    return Unifiable.InnerXmlText(childLINode);
                                }
                            }
                        }
                    }
                }
                else if (tncount == 0) // multi-predicate
                {
                    foreach (XmlNode childLINode in templateNode.ChildNodes)
                    {
                        if (childLINode.Name.ToLower() == "li")
                        {
                            int cac = AttributesCount(childLINode, "name,value");

                            if (cac == 2)
                            {
                                string name = GetAttribValue<string>(childLINode, "name", NullStringFunct, ReduceStarAttribute<string>);
                                Unifiable value = GetAttribValue(childLINode, "value", NullUnifyFunct, ReduceStarAttribute);
                                if ((name.Length > 0) & (!IsNullOrEmpty(value)))
                                {
                                    bool succeed;
                                    Unifiable actualValue = base.GetActualValue(name, childLINode.Name, out succeed);
                                    if (IsPredMatch(value, actualValue, query))
                                    {
                                        Succeed();
                                        return Unifiable.InnerXmlText(childLINode);
                                    }
                                }
                            }
                            if (cac == 1)
                            {
                                string name = GetAttribValue<string>(childLINode, "name", NullStringFunct, ReduceStarAttribute<string>);
                                if ((name.Length > 0) && query.containsSettingCalled(name))
                                {
                                    Succeed();
                                    return Unifiable.InnerXmlText(childLINode);
                                }
                            }
                            else if (cac == 0)
                            {
                                Succeed();
                                return Unifiable.InnerXmlText(childLINode);
                            }
                        }
                    }
                }
            }
            return Unifiable.Empty;
        }

        public void UnknownCondition()
        {
            writeToLog("Unknown cyccondition(s) " + LineNumberTextInfo());
        }
    }
}
