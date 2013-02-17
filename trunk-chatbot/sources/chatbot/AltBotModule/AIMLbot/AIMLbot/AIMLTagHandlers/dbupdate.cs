using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class dbupdate : AIMLTagHandlerU
    {

        public dbupdate(AltBot bot,
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
            if (CheckNode("dbupdate"))
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    // Find and Replace
                    Unifiable templateNodeInnerValue = Recurse();
                    string myText = (string)templateNodeInnerValue;
                    myText = TargetBot.LuceneIndexer.FixPronouns(myText, request.Requester.grabSettingNoDebug);
                    string targetBotLuceneIndexerMayPush = TargetBot.LuceneIndexer.MayPush(myText, templateNode);
                    if (targetBotLuceneIndexerMayPush == null)
                    {
                        writeToLogWarn("WARNING: NO DBUPDATE " + myText);
                        QueryHasFailed = true;
                        return FAIL;
                    }
                    string msg = "BEGINUPDATE " + myText + " ENDUPDATE";
                    AddSideEffect(msg,
                                  () => TargetBot.LuceneIndexer.UpdateFactoid(myText, myText, templateNode));
                    return msg;
                }
                catch(Exception e)
                {
                    writeToLog("ERROR: {0}", e);
                }

            }
            return Unifiable.Empty;

        }
    }




}