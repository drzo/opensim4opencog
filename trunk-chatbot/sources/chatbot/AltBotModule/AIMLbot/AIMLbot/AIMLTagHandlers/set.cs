using System;
using System.Xml;
using System.Text;
using AltAIMLbot.Utils;

namespace AltAIMLbot.AIMLTagHandlers
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
    public class set : AltAIMLbot.Utils.AIMLTagHandler
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
        public set(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            isStarWhenChildless = false;
        }

        protected override string ProcessChange()
        {
            if (this.TemplateNodeName == "set")
            {
                string name = GetAttribValue("name,var", null);
                string type0 = GetAttribValue("type,dict", "user");
                SettingsDictionary dict = request.GetDictionary(type0) ?? this.user.Predicates;
                string settingValue = TemplateNodeInnerText;
                this.bot.IsTraced(name);
                if (settingValue.Length > 0)
                {
                    dict.addSetting(name, settingValue);
                    if (name != "coins") bot.writeToLog("SET " + name + "=" + settingValue);
                    return dict.grabSetting(name);

                }
                else
                {
                    //dict.removeSetting(name);
                    dict.updateSetting(name, "");
                    bot.writeToLog("SET " + name + "=<BLANK>");
                    string res = dict.grabSetting(name);
                    return " , ";
                }
            }
            return string.Empty;
        }
    }
}
