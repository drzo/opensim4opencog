using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class lex : UnifibleTagHandler
    {

        public lex(AltBot bot,
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
            if (templateNode.Name.ToLower() == "lex")
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    Unifiable templateNodeInnerValue = Recurse();
                    string word = GetAttribValue("name", null);

                    Proc.wordAttributeHash.Add( word,(string)templateNodeInnerValue);
                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }
}