using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class pop : AIMLTagHandlerU
    {

        public pop(AltBot bot,
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
            if (templateNode.Name.ToLower() == "pop")
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