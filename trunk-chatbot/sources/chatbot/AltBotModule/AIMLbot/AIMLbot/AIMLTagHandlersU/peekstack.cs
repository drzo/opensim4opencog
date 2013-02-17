using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class peekstack : AIMLTagHandlerU
    {

        public peekstack(AltBot bot,
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
            if (templateNode.Name.ToLower() == "peekstack")
            {
                // If there are any conversation memos then return them all
                // otherwise return "Nothing."

                try
                {
                    if (Proc.conversationStack.Count > 0)
                    {
                        string [] memos = Proc.conversationStack.ToArray( );
                        string totalStack = "\n";
                        foreach (string s in memos)
                        {
                            totalStack += s + "\n";
                        }
                        Unifiable converseMemo = totalStack;
                        return converseMemo;
                    }
                    else
                    {
                        Unifiable emptyStack = "Nothing.";
                        return emptyStack;
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