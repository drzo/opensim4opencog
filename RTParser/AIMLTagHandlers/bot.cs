using System;
using System.Xml;
using System.Text;
using System.IO;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// An element called bot, which may be considered a restricted version of get, is used to 
    /// tell the AIML interpreter that it should substitute the contents of a "bot predicate". The 
    /// value of a bot predicate is set at load-time, and cannot be changed at run-time. The AIML 
    /// interpreter may decide how to set the values of bot predicate at load-time. If the bot 
    /// predicate has no value defined, the AIML interpreter should substitute an empty Unifiable.
    /// 
    /// The bot element has a required name attribute that identifies the bot predicate. 
    /// 
    /// The bot element does not have any content. 
    /// </summary>
    public class bot : RTParser.Utils.AIMLTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="Proc">The Proc involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public bot(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }
        
        public StreamWriter chatTrace;

        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "bot")
            {
                if (RecurseResult != null) return RecurseResult;
                string name = GetAttribValue("name", templateNodeInnerText.Trim());
                Unifiable defaultVal = GetAttribValue("default", Unifiable.Empty);
                string realName;
                Unifiable value = SettingsDictionary.grabSettingDefualt(Proc.GlobalSettings, name ,out realName);
                
                if (Unifiable.IsNullOrEmpty(value))
                {
                    return defaultVal;
                }
                if (value.ToUpper() == "UNKNOWN")
                {
                    return ("unknown " + name);
                }
                if (name != "name") Succeed();
                if (Unifiable.IsNullOrEmpty(value))
                {
                    RecurseResult = value;
                }
                return value;
            }
            return Unifiable.Empty;
        }
    }
}
