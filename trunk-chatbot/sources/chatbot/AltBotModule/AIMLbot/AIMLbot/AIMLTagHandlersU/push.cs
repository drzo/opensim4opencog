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
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using RTParser;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    public class push : RTParser.Utils.AIMLTagHandlerU
    {

        public push(RTParser.AltBot bot,
                User user,
                SubQuery query,
                Request request,
                Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }



        protected override Unifiable ProcessChangeU()
        {
            if (this.templateNode.Name.ToLower() == "push")
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    Unifiable templateNodeInnerValue = Recurse();
                    Proc.conversationStack.Push((string)templateNodeInnerValue);
                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }
}