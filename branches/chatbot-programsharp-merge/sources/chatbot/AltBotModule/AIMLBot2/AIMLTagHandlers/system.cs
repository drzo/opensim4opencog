using System;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// NOT IMPLEMENTED FOR SECURITY REASONS
    /// </summary>
    public class system : RTParser.Utils.AIMLTagHandler
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
        public system(RTParser.RTPBot bot,
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
            if (!finalResult.IsValid)
            {
                var v = Recurse();
                var r  = this.Proc.SystemExecute(v, GetAttribValue("lang", "bot"), request);
                if (Unifiable.IsFalse(r))
                {
                    //finalResult = r;
                    return Unifiable.Empty;
                }
                else if (Unifiable.IsTrue(r))
                {
                    finalResult.Value = r;
                    //templateNodeInnerText = isValueSetStart + v;
                }
                return r;
            }
            return RecurseResult;
        }
    }
}
