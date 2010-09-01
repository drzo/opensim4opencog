using System;
using System.Xml;
using System.Text;
using RTParser.Database;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// The get element tells the AIML interpreter that it should substitute the contents of a 
    /// predicate, if that predicate has a value defined. If the predicate has no value defined, 
    /// the AIML interpreter should substitute the empty Unifiable "". 
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
    public class get : RTParser.Utils.AIMLTagHandler
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
        public get(RTParser.RTPBot bot,
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
            Unifiable u = ProcessChange0();
            if (u.IsEmpty) return u;
            string s = u.ToValue(query);
            if (s.Contains(" ")) return s;
            if (s.ToLower().StartsWith("unknown"))
            {
                s = s.Substring(7);
                writeToLog("SETTINGS UNKNOWN " + s);
                return "unknown " + s;
            }
            return u;
        }

        protected Unifiable ProcessChange0()
        {
            if (CheckNode("get"))
            {
                string name = GetAttribValue(templateNode, "name,var", () => templateNodeInnerText, query);
                Unifiable defaultVal = GetAttribValue("default", Unifiable.Empty);
                ISettingsDictionary dict = query;
                string dictName = GetDictName("type,dict");
                Unifiable gName = GetAttribValue("global_name", name);
                bool succeed;
                var v = NamedValuesFromSettings.GetSettingForType(dictName, query, query, name, gName, defaultVal,
                                                                  out succeed, templateNode);
                if (succeed) Succeed();
                return v;
            }
            return Unifiable.Empty;
        }
    }
}
