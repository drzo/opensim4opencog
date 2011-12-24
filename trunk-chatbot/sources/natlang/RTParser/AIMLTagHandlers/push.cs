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
    public class push : RTParser.Utils.AIMLTagHandler
    {

        public push(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "push")
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    Unifiable templateNodeInnerValue = Recurse();
                    this.user.bot.conversationStack.Push((string)templateNodeInnerValue);
                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }
}