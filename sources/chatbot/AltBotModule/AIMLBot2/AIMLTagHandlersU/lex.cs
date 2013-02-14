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
using AltAIMLParser;
using AltAIMLbot;
using RTParser;
using RTParser.Utils;

namespace RTParser.AIMLTagHandlers
{
    public class lex : RTParser.Utils.UnifibleTagHandler
    {

        public lex(RTParser.AltBot bot,
                RTParser.User user,
                RTParser.Utils.SubQuery query,
                Request request,
                Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }


        public override float CanUnify(Unifiable with)
        {
            throw new NotImplementedException();
        }


        protected override Unifiable ComputeInnerOrNull()
        {
            if (this.templateNode.Name.ToLower() == "lex")
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    Unifiable templateNodeInnerValue = Recurse();
                    string word = GetAttribValue("name", null);

                    this.user.rbot.wordAttributeHash.Add( word,(string)templateNodeInnerValue);
                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }
}