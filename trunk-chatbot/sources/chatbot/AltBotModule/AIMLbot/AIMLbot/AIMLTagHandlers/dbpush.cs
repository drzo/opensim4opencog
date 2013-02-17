using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class dbpush : AIMLTagHandlerU
    {

        public dbpush(AltBot bot,
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
            if (CheckNode("dbpush"))
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    // what to remember
                    Unifiable templateNodeInnerValue = Recurse();
                    string myText0 = (string) templateNodeInnerValue;
                    var myText = TargetBot.LuceneIndexer.FixPronouns(myText0, request.Requester.grabSettingNoDebug);
                    writeToLog("FIXPRONOUNS: " + myText0 + " ->" + myText);
                    if (TargetBot.LuceneIndexer.MayPush(myText, templateNode) == null)
                    {
                        writeToLogWarn("WARNING: NO DBPUSH " + myText);
                        QueryHasFailed = true;
                        return FAIL;
                    }
                    AddSideEffect("DBPUSH " + myText,
                                  () => TargetBot.LuceneIndexer.InsertFactiod(myText, templateNode, null));
                    return "BEGINPUSH " + myText + " ENDPUSH";
                }
                catch (Exception e)
                {
                    writeToLog("ERROR: {0}", e);
                }

            }
            return Unifiable.Empty;

        }
    }




}