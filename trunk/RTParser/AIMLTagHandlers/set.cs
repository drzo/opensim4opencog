using System;
using System.Xml;
using System.Text;

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
        //i been writeing a new language for AI trickery.. i am a little assumed but not enough to stop writting it.. and I handling cuts.. by the fact 
        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "set")
            {
                var templateNodeInnerText = Recurse();
                Unifiable name = GetAttribValue("name,var", null);
                Unifiable gName = GetAttribValue("global_name", null);
                // try to use a global blackboard predicate
                bool newlyCreated;
                RTParser.User gUser = this.user.bot.FindOrCreateUser("globalPreds", out newlyCreated);
                if (newlyCreated) gUser.IsRoleAcct = true;

                var thisRequestPredicates = this.request.Predicates;
                if (GetAttribValue("type", null) == "bot") thisRequestPredicates = request.TargetBot.GlobalSettings;
                if (!Unifiable.IsNull(name))
                {
                    if (!templateNodeInnerText.IsEmpty)
                    {
                        if (!Unifiable.IsNull(gName)) gUser.Predicates.addSetting(gName, templateNodeInnerText);
                        thisRequestPredicates.addSetting(name, templateNodeInnerText);
                        return thisRequestPredicates.grabSetting(name);
                    }
                    else
                    {
                        // remove the predicate
                        if (!Unifiable.IsNull(gName)) gUser.Predicates.removeSetting(gName);
                        thisRequestPredicates.removeSetting(name);
                        return Unifiable.Empty;
                    }
                }
                else  //recursive form like <set>name value Unifiable</set>
                {
                    Unifiable nv = Recurse();
                    //Unifiable fsp = nv.Split();//
                    //new char[] { ' ', '\n', '\t', '\r' }, StringSplitOptions.RemoveEmptyEntries);
                    name = nv.First();
                    Unifiable joined = nv.Rest();// Unifiable.Join(" ", fsp, 1, fsp.Length - 1);
                    if (joined.IsEmpty)
                    {
                        thisRequestPredicates.removeSetting(name);
                        return Unifiable.Empty;
                    }
                    if (!Unifiable.IsNull(gName)) gUser.Predicates.addSetting(gName, templateNodeInnerText);
                    thisRequestPredicates.addSetting(name, joined);
                    return thisRequestPredicates.grabSetting(name);
                }
            }
            return Unifiable.Empty;
        }
    }
}
