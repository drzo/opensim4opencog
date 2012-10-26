using System;
using System.Xml;
using System.Text;
using AltAIMLParser;
using RTParser;

namespace AltAIMLbot.AIMLTagHandlers
{
    /// <summary>
    /// converts text into a prolog compatible list
    /// <prolist>What do you want ?</prolist>
    /// [what,do,you,want,questionmark]
    /// </summary>
    public class prolist : Utils.AIMLTagHandler
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
        public prolist(AltBot bot,
                        User user,
                        Utils.SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }
        public override bool isFormatter
        {
            get { return true; }
        }

        protected override string ProcessChange()
        {
            if (this.TemplateNodeName == "prolist")
            {
                string text = this.TemplateNodeInnerText.ToLower(this.bot.Locale);
                text = text.Replace("?", " questionmark ");
                text = text.Replace("!", " exclamationmark ");
                text = text.Replace(".", " periodmark ");
                while (text.Contains("  ")) text = text.Replace("  ", " ");
                text = "["+text.Replace(" ", ",").Trim()+"]";
                while (text.Contains("[,")) text = text.Replace("[,", "[");
                while (text.Contains(",]")) text = text.Replace(",]", "]");
                while (text.Contains(",,")) text = text.Replace(",,", ",");
                return text;
            }
            return string.Empty;
        }
    }
}