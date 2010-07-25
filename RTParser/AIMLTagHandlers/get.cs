using System;
using System.Xml;
using System.Text;
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
            if (this.templateNode.Name.ToLower() == "get")
            {
                string name = GetAttribValue(templateNode, "name,var", () => templateNodeInnerText, query);
                Unifiable defaultVal = GetAttribValue("default", Unifiable.Empty);
                ISettingsDictionary dict = query;
                if (GetAttribValue("type", "") == "bot") dict = request.Proccessor.GlobalSettings;
                string realName;
                Unifiable resultGet = SettingsDictionary.grabSettingDefualt(dict, name, out realName);
 
                // if ((!String.IsNullOrEmpty(result)) && (!result.IsWildCard())) return result; // we have a local one
                
                // try to use a global blackboard predicate
                bool newlyCreated;
                RTParser.User gUser = this.user.bot.FindOrCreateUser("globalPreds", out newlyCreated);
                Unifiable gResult = SettingsDictionary.grabSettingDefualt(gUser.Predicates, name, out realName);

                if ((Unifiable.IsUnknown(resultGet)) && (!Unifiable.IsUnknown(gResult)))
                {
                    // result=nothing, gResult=something => return gResult
                    writeToLog("SETTINGS OVERRIDE " + gResult);
                    return gResult;
                }
                string sresultGet = resultGet.ToValue(query);
                if (sresultGet.ToUpper() == "UNKNOWN")
                {
                    return sresultGet + " " + name;
                }
                if (!String.IsNullOrEmpty(resultGet))
                {
                    if (!String.IsNullOrEmpty(gResult))
                    {
                        // result=*, gResult=something => return gResult
                        if (resultGet.IsWildCard()) return gResult;
                        Succeed();
                        // result=something, gResult=something => return result
                        return resultGet;
                    }
                    else
                    {
                        // result=something, gResult=nothing => return result
                        return resultGet;
                    }
                }
                // default => return defaultVal
                return defaultVal;
            }
            return Unifiable.Empty;
        }
    }
}
