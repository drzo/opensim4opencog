using System;
using System.Xml;
using AltAIMLbot.Utils;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class dbdelete : AIMLTagHandlerU
    {

        public dbdelete(AltBot bot,
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
            if (CheckNode("dbdelete"))
            {
                // delete
                try
                {
                    // Find and Replace
                    Unifiable templateNodeInnerValue = Recurse();
                    string myText = (string)templateNodeInnerValue;
                    myText = TargetBot.LuceneIndexer.FixPronouns(myText, request.Requester.grabSettingNoDebug);
                    if (TargetBot.LuceneIndexer.MayPush(myText, templateNode) == null)
                    {
                        writeToLogWarn("WARNING: NO DBPUSH " + myText);
                        QueryHasFailed = true;
                        return FAIL;
                    }
                    string msg = "BEGINDELETE " + myText + " ENDDELETE";
                    AddSideEffect(msg,
                                  () => TargetBot.LuceneIndexer.DeleteTopScoring(myText, templateNode, false));
                    return Succeed(msg);
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