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
    public class lexset : RTParser.Utils.AIMLTagHandlerU
    {

        public lexset(RTParser.AltBot bot,
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
            if (this.templateNode.Name.ToLower() == "lexset")
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    Unifiable templateNodeInnerValue = Recurse();
                    string word = GetAttribValue("name", null);
                    string lexlist = (string)templateNodeInnerValue;
                    string [] lexset = lexlist.Split(' ');
                    string lexspec = word;
                    // Append the definition of any tokens including the original set and the token list
                    if (Proc.wordAttributeHash.Contains(word)) { lexspec +=" "+ (string)Proc.wordAttributeHash[word]; }
                    lexspec += " "+lexlist;
                    foreach (string lexcat in lexset)
                    {
                        try
                        {
                            lexspec += " " + ((string)Proc.wordAttributeHash[lexcat]);
                        }
                        catch { }
                    }
                    Proc.wordAttributeHash[word]=lexspec.Trim();
                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }
}