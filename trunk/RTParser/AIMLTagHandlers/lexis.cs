using System;
using System.Text.RegularExpressions;
using System.Xml;
using System.Text;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// IMPLEMENTED FOR COMPLETENESS REASONS
    /// </summary>
    public class lexis : UnifibleTagHandler
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
        public lexis(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public override float CanUnify(Unifiable with)
        {

            string re = "";
            if (templateNode.NodeType==XmlNodeType.Text)
            {
                re = templateNodeInnerText.AsString();
            }
            else if (templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in templateNode.ChildNodes)
                {
                    re += childNode.InnerText;
                }
            }
            else
            {
                templateNodeInnerText = Recurse();
            }
            
            //Lookup definition for current word we could unify with
            string wordAttributes = "";
            string key = (string)with.ToValue(query).Trim();
            if (this.user.bot.wordAttributeHash.Contains(key) )
            { 
                wordAttributes = (string)this.user.bot.wordAttributeHash[key];
            }
            else
            {
                if (this.user.bot.wordAttributeHash.Contains(key.ToLower()) )
                {
                    key = key.ToLower();
                    wordAttributes = (string)this.user.bot.wordAttributeHash[key];
                }
            }
            // Can you find a match inside ?
            var matcher = new Regex(re);
            if (matcher.IsMatch(wordAttributes)) return AND_TRUE;
            return AND_FALSE;
        }

        protected override Unifiable ProcessChange()
        {
            return templateNodeInnerText;
        }

        public override Unifiable CompleteProcess()
        {
            return base.CompleteProcess();
        }
    }
}
