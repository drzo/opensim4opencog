using System;
using System.Xml;
using System.Text;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// The version element tells the AIML interpreter that it should substitute the version number
    /// of the AIML interpreter.
    /// 
    /// The version element does not have any content. 
    /// </summary>
    public class botsetting : AltAIMLbot.Utils.AIMLTagHandler
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
        public botsetting(AltAIMLbot.AltBot bot,
                        AltAIMLbot.User user,
                        AltAIMLbot.Utils.SubQuery query,
                        AltAIMLbot.Request request,
                        AltAIMLbot.Result result,
                        XmlNode templateNode, string settingGrab)
            : base(bot, user, query, request, result, templateNode)
        {
            SettingToGrab = settingGrab;
        }

        private string SettingToGrab;
        protected override string ProcessChange()
        {
            return this.bot.GlobalSettings.grabSetting(SettingToGrab);
        }
    }
}
