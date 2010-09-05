using System.Xml;
using RTParser.Database;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The set element instructs the AIML interpreter to set the value of a predicate to the result 
    /// of processing the contents of the set element. The set element has a required attribute name, 
    /// which must be a valid AIML predicate name. If the predicate has not yet been defined, the AIML 
    /// interpreter should define it in memory. 
    /// 
    /// The AIML interpreter should, generically, return the result of processing the contents of the 
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
    public class set : RTParser.Utils.AIMLTagHandler
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
        public set(RTParser.RTPBot bot,
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
            Unifiable defaultVal = GetAttribValue("default", Unifiable.Empty);
            if (CheckNode("set"))
            {
                var templateNodeInnerText = Recurse();
                Unifiable name = GetAttribValue("name,var", null);
                Unifiable value = GetAttribValue("value", null);
                string gName = GetAttribValue("global_name", null);
                string dictName = GetDictName("type");

                if (name == null)
                {
                    //recursive form like <set>name value Unifiable</set>
                    name = templateNodeInnerText.First();
                    templateNodeInnerText = templateNodeInnerText.Rest();
                }
                string setReturn = GetAttribValue(templateNode, "set-return",
                                                  () =>
                                                  ((string) Proc.GetRelationMetaProps().GetMeta(name, "set-return")),
                                                  ReduceStarAttribute<string>);
                if (value == null)
                {
                    value = templateNodeInnerText;
                }
                if (value.IsEmpty) value = defaultVal;
                return NamedValuesFromSettings.SetSettingForType(dictName, query, query, name, gName, value, setReturn, templateNode);
            }
            return defaultVal;
        }
    }
}
