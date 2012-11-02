using System;
using System.Xml;
using System.Text;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
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
    public class root : RTParser.Utils.LoadingTagHandler
    {
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="Proc">The Proc involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request inputted into the system</param>
        /// <param name="result">The result to be passed to the user</param>
        /// <param name="templateNode">The node to be Processed</param>
        public root(RTParser.AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode, ParentProvider pp)
            : base(bot, user, query, request, result, templateNode)
        {

            GetTargetSettings = pp ?? (() => this.query.TargetSettings);
        }

        protected override Unifiable ProcessLoad(LoaderOptions loaderOptions)
        {
            if (CheckNode("root,vars,substitutions,substitutions,bots,properties,predicates"))
            {
                // Process each of these child "settings"? nodes
                var prevDict = request.TargetSettings;
                try
                {
                    SettingsDictionaryReal.loadSettingNode((ISettingsDictionary)GetTargetSettings(), templateNode, SettingsPolicy.Default, request);
                }
                finally
                {
                    request.TargetSettings = prevDict;
                }
                return Unifiable.Empty;
                return ProcessSucceed();
            }
            return Unifiable.Empty;
        }

        public ParentProvider GetTargetSettings;
    }
}
