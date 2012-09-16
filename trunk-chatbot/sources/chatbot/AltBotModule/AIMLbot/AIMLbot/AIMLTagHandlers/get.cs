using System;
using System.Xml;
using System.Text;
using AltAIMLbot.Utils;
using MushDLR223.Utilities;
using Unifiable = System.String;
namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The get element tells the AIML interpreter that it should substitute the contents of a 
    /// predicate, if that predicate has a value defined. If the predicate has no value defined, 
    /// the AIML interpreter should substitute the empty string "". 
    /// 
    /// The AIML interpreter implementation may optionally provide a mechanism that allows the 
    /// AIML author to designate default values for certain predicates (see [9.3.]). 
    /// 
    /// The get element must not perform any text formatting or other "normalization" on the predicate
    /// contents when returning them. 
    /// 
    /// The get element has a required name attribute that identifies the predicate with an AIML 
    /// predicate name. 
    /// 
    /// The get element does not have any content.
    /// </summary>
    public class get : AltAIMLbot.Utils.AIMLTagHandler
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
        public get(AltAIMLbot.AltBot bot,
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
            if (this.TemplateNodeName == "get")
            {
                string name = GetAttribValue("name,var", () => TemplateNodeInnerText);
                string type0 = GetAttribValue("type,dict", "user");
                SettingsDictionary dict = request.GetDictionary(type0) ?? this.user.Predicates;

                var r = dict.grabSetting(name);
                if (this.bot.IsTraced(name))
                {

                }
                if (r == null)
                {
                    return string.Empty;
                }
                return r;
            }
            return string.Empty;
        }
    }
}
