using System;
using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The condition element instructs the AIML interpreter to return specified contents depending 
    /// upon the results of matching a predicate against a pattern. 
    /// 
    /// NB: The condition element has three different types. The three different types specified 
    /// here are distinguished by an xsi:type attribute, which permits a validating XML Schema 
    /// Processor to validate them. Two of the types may contain li elements, of which there are 
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
    /// the AIML interpreter should return the contents of the condition. If not, the empty Unifiable "" 
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
    public class liif : AIMLTagHandler
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
        public liif(AltBot bot,
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
            if (CheckNode("li,if"))
            {
                ResetValues(false);
                ISettingsDictionary dist = query;
                var vv = IfSucceed(templateNode, dist, query, ProcessSucceed, Failure);
                return vv;
            }
            return ProcessSucceed();
        }

        override protected Unifiable ProcessSucceed()
        {
            Unifiable before = InnerSource();
            string s = RecurseReal(templateNode, true);
            //var recurseResult1 = RecurseProcess();
            if (s == null)
            {
                Proc.TraceTest("NULL in FIIF " + ToString(), () =>
                                                                 {
                                                                     s = RecurseReal(templateNode, true);
                                                                 });
                return Failure("FIIF NULL");
            }
            Succeed();
            if (FinalResultValid) return FinalResult;
            return s;
        }

        static Unifiable IfSucceed(XmlNode templateNode, ISettingsDictionary dict, SubQuery query0, Func<Unifiable> Succeed, Func<string, Unifiable> Failure)
        {
            lock (templateNode)
            {
              //  ISettingsDictionary dict = query;
                //if (GetAttribValue(templateNode, "type", "") == "bot" || GetAttribValue(templateNode,"bot", "").ToLower() == "true")
                 //   dict = query0.TargetBot.GlobalSettings;

                bool yesInherit = true;
                bool noLocal = false;

                string locally = GetAttribValue(templateNode,"scope,type,exists", "");
                if (locally.Contains("local"))
                {
                    yesInherit = false;
                }
                if (locally.Contains("inher"))
                {
                    noLocal = true;
                    yesInherit = true;
                }

                Func<IConvertible, string> query = query0.ReduceStarAttribute<string>;

                string name = GetAttribValue<string>(templateNode, "name,var,if", NullStringFunct, query);
                string expression = GetAttribValue<string>(templateNode, "expr", NullStringFunct, query);
                string exists = GetAttribValue(templateNode, "exists", NullStringFunct, query);
                string contains = GetAttribValue(templateNode, "contains", NullStringFunct, query);
                Unifiable value = GetAttribValue(templateNode, "value", NullStringFunct, query);
                string scope = GetAttribValue(templateNode, "local,scope", NullStringFunct, query);
                string realName0;
                string type = GetAttribValue(templateNode, "type,user,bot", out realName0, NullStringFunct, query);
                if (type == null) type = realName0;

                string varname = name ?? exists;
                bool mustNotExist = false;
                if (contains != null)
                {
                    value = ".*" + contains + ".*";
                }

                if (exists != null)
                {
                    bool locallyContains = dict.containsLocalCalled(name);
                    bool anyContains = dict.containsSettingCalled(name);

                    if (exists.StartsWith("local"))
                    {
                        if (!locallyContains) return Unifiable.Empty;
                        return Succeed();
                    }

                    exists = exists.ToLower();
                    if (exists == "inherited")
                    {
                        if (!locallyContains && anyContains) return Succeed();
                        return Failure("" + name + "=" + anyContains + " !inherited ");
                    }
                    if (exists == "false")
                    {
                        if (!yesInherit)
                        {
                            if (!locallyContains)
                            {
                                return Succeed();
                            }
                        }
                        if (noLocal)
                        {
                            if (anyContains && !locallyContains)
                            {
                                return Succeed();
                            }
                        }
                        return Failure("" + name + "=" + anyContains + " !false ");
                    }
                    if (exists == "true")
                    {
                        if (!yesInherit)
                        {
                            if (locallyContains)
                            {
                                return Succeed();
                            }
                        }
                        if (noLocal)
                        {
                            if (!locallyContains && anyContains)
                            {
                                return Succeed();
                            }
                        }
                        return Failure("" + name + "=" + anyContains + "  " + templateNode.OuterXml);
                    }
                    string realName;
                    bool succeed;
                    //ReduceStar0 query = query0.ReduceStarAttribute;
                    Unifiable actualValue = GetActualValue(templateNode, name, type ?? dict.NameSpace, out succeed, query0);
                    if (IsPredMatch(value, actualValue, query0))
                    {
                        return Succeed();
                    }
                    else
                    {
                        return Failure("" + name + "=" + anyContains + "  " + templateNode.OuterXml);
                    }
                }
                return Succeed();
            }
        }

        public void UnknownCondition()
        {
            writeToLog("Unknown conditions " + LineNumberTextInfo());
        }
    }
}
