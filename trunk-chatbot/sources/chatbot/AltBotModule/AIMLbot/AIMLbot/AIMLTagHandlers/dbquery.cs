using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.Utilities;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{

    /// <summary>
    /// 
    /// </summary>
    public class dbquery : AIMLTagHandler
    {

        public dbquery(AltBot bot,
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
            if (CheckNode("dbquery"))
            {
                if (templateNode.ChildNodes.Count > 0 && templateNode.FirstChild.Name != "li")
                    return ProcessChangeLegacy();
                bool hasPassed = false;
                string majorPassed = null;
                foreach (var node in templateNode)
                {
                    // otherwise take the tag content as a srai (to trip say a random reply)
                    const bool expandOnNoHits = true; // actually WordNet
                    const float threshold = 0.0f;
                    Unifiable templateNodeInnerValue = ProcessChildNode(((XmlNode)node));
                    string failPrefix = GetAttribValue(((XmlNode)node), "failprefix", "").ToLower();
                    string passPrefix = GetAttribValue(((XmlNode)node), "passprefix", "").ToLower();
                    string resultPrefix = GetAttribValue(((XmlNode)node), "resultprefix", "").ToLower();
                    if (!string.IsNullOrEmpty(failPrefix))
                    {
                        //on <dbquery> failure, use a <srai> fallback
                        string sariCallStr = failPrefix + " " + (string) templateNodeInnerValue;
                        return callSRAI(sariCallStr);
                    }
                    if (!string.IsNullOrEmpty(passPrefix))
                    {
                        //on <dbquery> failure, use a <srai> fallback
                        majorPassed = passPrefix + " " + (string)templateNodeInnerValue;
                    }
                    if (IsNullOrEmpty(templateNodeInnerValue)) continue;
                    string searchTerm1 = TargetBot.LuceneIndexer.FixPronouns(templateNodeInnerValue,
                                                                             request.Requester.grabSettingNoDebug);
                    if (TargetBot.LuceneIndexer.MayAsk(searchTerm1, ((XmlNode)node)) == null)
                    {
                        writeToLogWarn("WARNING: NO DBASK " + searchTerm1);
                        continue;
                    }
                    float reliability;
                    Unifiable converseMemo = TargetBot.LuceneIndexer.AskQuery(
                        searchTerm1,
                        writeToLog,
                        () =>
                        {
                            if (string.IsNullOrEmpty(failPrefix)) return null;
                            //on <dbquery> failure, use a <srai> fallback
                            string sariCallStr = failPrefix + " " + (string)templateNodeInnerValue;
                            return callSRAI(sariCallStr);
                        },
                        templateNode,
                        threshold,
                        true, // use Wordnet
                        expandOnNoHits, out reliability);
                    if (!IsNullOrEmpty(converseMemo))
                    {
                        hasPassed = true;
                        QueryHasSuceeded = true;

                        if (!string.IsNullOrEmpty(passPrefix))
                        {
                            //on <dbquery> pass, use a <srai> for success
                            break;
                        }
                        if (!string.IsNullOrEmpty(resultPrefix))
                        {
                            //on <dbquery> failure, use a <srai> fallback
                            string sariCallStr = resultPrefix + " " + (string)converseMemo;
                            return callSRAI(sariCallStr);
                        }
                        return converseMemo;
                        //Unifiable converseMemo = Proc.conversationStack.Pop();
                    }
                }
                if (hasPassed)
                {
                    return callSRAI(majorPassed);
                }
                // if there is a high enough scoring record in Lucene, use up to max number of them?
                // otherwise there is a conversation memo then pop it??
            }
            return Unifiable.Empty;

        }

        protected Unifiable ProcessChangeLegacy()
        {
            if (CheckNode("dbquery"))
            {               
                // otherwise take the tag content as a srai (to trip say a random reply)
                const bool expandOnNoHits = true; // actually WordNet
                const float threshold = 0.0f;
                Unifiable templateNodeInnerValue = Recurse();
                string searchTermOrig = (string)templateNodeInnerValue;
                string searchTerm1 = TargetBot.LuceneIndexer.FixPronouns(searchTermOrig, request.Requester.grabSettingNoDebug);
                if (TargetBot.LuceneIndexer.MayAsk(searchTerm1, templateNode) == null)
                {
                    writeToLogWarn("WARNING: NO DBASK " + searchTerm1);
                    QueryHasFailed = true;
                    return FAIL;
                }
                float reliability;
                string failPrefix = GetAttribValue(templateNode, "failprefix", "").ToLower();
                Unifiable converseMemo = TargetBot.LuceneIndexer.AskQuery(searchTerm1, writeToLog,
                                                                          () =>
                                                                              {
                                                                                  //on <dbquery> failure, use a <srai> fallback
                                                                                  string sariCallStr = failPrefix + " " + request.rawInput;
                                                                                  return callSRAI(sariCallStr);

                                                                              },
                                                                             templateNode, 
                                                                             threshold, 
                                                                             true, // use Wordnet
                                                                             expandOnNoHits, out reliability);

                // if there is a high enough scoring record in Lucene, use up to max number of them?
                // otherwise there is a conversation memo then pop it??
                if (IsNullOrEmpty(converseMemo))
                {
                    //Unifiable converseMemo = Proc.conversationStack.Pop();
                }
                return converseMemo;
            }
            return Unifiable.Empty;

        }



        public override void writeToLog(string s, params object[] p)
        {
            //Proc.writeToLog("DBQUERY: " + s, p);
            //bool tempB = Proc.IsLogging;
            //Proc.IsLogging = true;
            // base.user.bot.writeToLog("DBQUERY: " + s, p);
            //Proc.IsLogging = tempB;
            DLRConsole.DebugWriteLine("DBQUERY: " + s, p);

        }

    }
}