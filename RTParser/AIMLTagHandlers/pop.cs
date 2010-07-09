using System;
using System.Runtime;
using System.Text;
using System.Xml;
using System.Collections;
using System.Collections.Generic;
using System.IO;
//using System.Linq;
using System.Text.RegularExpressions;
using System.Diagnostics.CodeAnalysis;
using System.Diagnostics;
using RTParser;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    public class pop : RTParser.Utils.AIMLTagHandler
    {

        public pop(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "pop")
            {
                // If there is a conversation memo then pop it
                // otherwise take the tag content as a srai (to trip say a random reply)

                try
                {
                    if (this.user.bot.conversationStack.Count > 0)
                    {
                        Unifiable converseMemo = this.user.bot.conversationStack.Pop();
                        return converseMemo;
                    }
                    else
                    {
                        Unifiable starContent = Recurse();
                        XmlNode sraiNode = RTParser.Utils.AIMLTagHandler.getNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
                        srai sraiHandler = new srai(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
                        return sraiHandler.Transform();
                    }

                }
                catch
                {

                }

            }
            return Unifiable.Empty;

        }
    }
}