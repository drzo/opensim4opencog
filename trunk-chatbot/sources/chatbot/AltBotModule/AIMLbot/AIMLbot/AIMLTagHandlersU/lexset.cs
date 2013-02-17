using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class lexset : UnifibleTagHandler
    {

        public lexset(AltBot bot,
                User user,
                SubQuery query,
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
            if (templateNode.Name.ToLower() == "lexset")
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