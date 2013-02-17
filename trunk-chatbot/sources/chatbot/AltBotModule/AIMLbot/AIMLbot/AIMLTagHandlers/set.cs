using System.Xml;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The set element instructs the AIML interpreter to set the value of a predicate to the result 
    /// of Processing the contents of the set element. The set element has a required attribute name, 
    /// which must be a valid AIML predicate name. If the predicate has not yet been defined, the AIML 
    /// interpreter should define it in memory. 
    /// 
    /// The AIML interpreter should, generically, return the result of Processing the contents of the 
    /// set element. The set element must not perform any text formatting or other "normalization" on 
    /// the predicate contents when returning them. 
    /// 
    /// The AIML interpreter implementation may optionally provide a mechanism that allows the AIML 
    /// author to designate certain predicates as "return-name-when-set", which means that a set 
    /// operation using such a predicate will return the name of the predicate, rather than its 
    /// captured value. (See [9.2].) 
    /// 
    /// A set element may contain any AIML template elements.
    /// </summary>
    public class set : AIMLTagHandlerU
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
        public set(AltBot bot,
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
            Unifiable defaultVal = GetAttribValue("default,defaultValue", null);
            if (CheckNode("set"))
            {
                var templateNodeInnerText = Recurse();
                Unifiable name = GetAttribValue("name,var", null);
                Unifiable value = GetAttribValue("value", null);
                string gName = GetAttribValue("global_name", null);
                ISettingsDictionary dict;
                string dictName = GetNameOfDict(query, "set", templateNode, out dict);
                Succeed();
                if (name == null)
                {
                    //recursive form like <set>name value Unifiable</set>
                    name = templateNodeInnerText.First;
                    var strV = templateNodeInnerText.Rest;
                    if (!IsNull(strV))
                    {
                        if (IsNull(value))
                        {
                            value = strV;
                        }
                        else
                        {
                            writeToLogWarn("ERROR extra stuff in ", strV);
                        }
                    }

                }
                else
                {
                    if (value == null)
                    {
                        var strV = templateNodeInnerText;
                        if (IsNull(strV))
                        {
                            writeToLogWarn("ERROR null stuff in SET ", strV);
                        }
                        value = strV;
                    }
                }
                string setReturn = GetAttribValue<string>(templateNode, "set-return",
                                                  () =>((string) Proc.GetRelationMetaProps().GetMeta(name, "set-return")),
                                                  ReduceStarAttribute<string>);
                if (value == null)
                {
                    value = templateNodeInnerText;
                }
                if (IsNull(value)) value = defaultVal;
                if (IsNull(value))
                {
                    if (QueryHasFailed) return FAIL;
                }
                var retVal = NamedValuesFromSettings.SetSettingForType(dictName, query, dict, name, gName, value, setReturn, templateNode);
                if (IsNull(retVal))
                {
                    writeToLogWarn("ERROR null stuff in SET ", retVal);                    
                } else if (IsIncomplete(retVal))
                {
                    writeToLogWarn("ERROR null stuff in SET ", retVal);
                }
                return retVal;
            }
            return defaultVal;
        }
    }
}
