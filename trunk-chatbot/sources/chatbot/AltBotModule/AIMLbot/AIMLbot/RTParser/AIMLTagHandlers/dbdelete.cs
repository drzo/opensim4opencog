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
using RTParser;
using RTParser.Utils;
using Lucene.Net.Store;
using Lucene.Net.Analysis;
using Lucene.Net.Analysis.Standard;
using Lucene.Net.Index;
using Lucene.Net.Documents;
using Lucene.Net.Search;
using Lucene.Net.QueryParsers;

namespace RTParser.AIMLTagHandlers
{
    public class dbdelete : RTParser.Utils.AIMLTagHandler
    {

        public dbdelete(RTParser.RTPBot bot,
                RTParser.User user,
                RTParser.Utils.SubQuery query,
                RTParser.Request request,
                RTParser.Result result,
                XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }



        protected override Unifiable ProcessChange()
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