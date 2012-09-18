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
    public class pop : RTParser.Utils.AIMLTagHandlerU
    {

        public pop(RTParser.AltBot bot,
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
            if (this.templateNode.Name.ToLower() == "pop")
            {
                // If there is a conversation memo then pop it
                // otherwise take the tag content as a srai (to trip say a random reply)

                try
                {
                    if (Proc.conversationStack.Count > 0)
                    {
                        Unifiable converseMemo = Proc.conversationStack.Pop();
                        return converseMemo;
                    }
                    else
                    {
                        Unifiable starContent = Recurse();
                        return callSRAI(starContent);
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