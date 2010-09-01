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
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;

namespace RTParser.AIMLTagHandlers
{
    public class dbquery : RTParser.Utils.AIMLTagHandler
    {

        public dbquery(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "dbquery")
            {
                // If there is a conversation memo then pop it
                // otherwise take the tag content as a srai (to trip say a random reply)

                try
                {

                    // Searching:
                    ulong[] ids;
                    string[] results;
                    float[] scores;

                    int numHits;

                    Unifiable templateNodeInnerValue = Recurse();
                    string searchTerm1 = (string)templateNodeInnerValue;
                    string maxReplyStr = GetAttribValue("max", "1").ToLower();
                    int maxReply = Int16.Parse(maxReplyStr);
                    string failPrefix = GetAttribValue("failprefix", "").ToLower();
                    string thresholdStr = GetAttribValue("threshold", "0").ToLower();
                    float threshold = float.Parse(thresholdStr);

                    writeToLog("Searching for the term \"{0}\"...", searchTerm1);
                    this.user.bot.LuceneIndexer.Search(searchTerm1, out ids, out results, out scores);
                    numHits = ids.Length;
                    writeToLog("Number of hits == {0}.", numHits);
                    for (int i = 0; i < numHits; ++i)
                    {
                        writeToLog("{0}) Doc-id: {1}; Content: \"{2}\" with score {3}.", i + 1, ids[i], results[i], scores[i]);
                    }
                    
                    float topScore = 0;
                    if (numHits>0) topScore=scores[0];
                   // Console.WriteLine();




                    if ((numHits > 0)&&(topScore >= threshold))
                    {
                        // should be weighted but lets just use the highest scoring
                        string reply = "";
                        if (numHits<maxReply) maxReply =numHits;
                        for (int i = 0; i < maxReply; i++)
                        {
                            reply = reply + " " + results[i];
                        }
                        Unifiable converseMemo = reply.Trim();
                        writeToLog(" reply = {0}", reply);

                        //Unifiable converseMemo = this.user.bot.conversationStack.Pop();
                        return converseMemo;
                    }
                    else
                    {
                        Unifiable starContent = Recurse();
                        string sariCallStr  = failPrefix +" "+ (string)starContent;
                        XmlNode sraiNode = RTParser.Utils.AIMLTagHandler.getNode(String.Format("<srai>{0}</srai>", sariCallStr.Trim()), templateNode);
                        srai sraiHandler = new srai(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
                        return sraiHandler.Transform();
                    }

                }
                catch
                {

                }

            }
            return Unifiable.Empty;

        }

        private void writeToLog(string s, params object[] p)
        {
            //this.user.bot.writeToLog("DBQUERY: " + s, p);
            //bool tempB = this.user.bot.IsLogging;
            //this.user.bot.IsLogging = true;
            // base.user.bot.writeToLog("DBQUERY: " + s, p);
            //this.user.bot.IsLogging = tempB;
            DLRConsole.DebugWriteLine("DBQUERY: " + s, p);

        }

    }
}