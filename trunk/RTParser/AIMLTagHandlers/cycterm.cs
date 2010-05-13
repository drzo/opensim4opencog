using System;
using System.Collections.Generic;
using System.Xml;
using System.Text;

namespace RTParser.AIMLTagHandlers
{
    /// <summary>
    /// &lt;cycterm&gt; translates an English word/phrase into a Cyc symbol 
    /// </summary>
    public class cycterm : RTParser.Utils.AIMLTagHandler
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
        public cycterm(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
            //isRecursive = false;
        }

        public override Unifiable CompleteProcess()
        {
            return ProcessChange();
        }
        protected override Unifiable ProcessChange()
        {
            if (this.templateNode.Name.ToLower() == "cycterm")
            {
                Unifiable filter = base.GetAttribValue("filter", GetAttribValue("isa", "Thing"));
                Unifiable r = Recurse();
                Unifiable term;
                if (Proc.TheCyc.Lookup(r, filter, out term))
                {
                    return term;
                }
                return Unifiable.Empty;
            }
            return Unifiable.Empty;
        }
    }
}
