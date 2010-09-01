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
                const bool useSynonyms = true; // actually WordNet
                Unifiable templateNodeInnerValue = Recurse();
                string searchTerm1 = (string) templateNodeInnerValue;
                return this.request.TargetBot.LuceneIndexer.callDBQuery(searchTerm1, this.writeToLog, this.OnFalure,
                                                                        this.templateNode, useSynonyms);
            }
            return Unifiable.Empty;

        }



        //on <dbquery> failure, use a <srai> fallback
        private Unifiable OnFalure(string failPrefix)
        {
            Unifiable starContent = Recurse();
            string sariCallStr  = failPrefix +" "+ (string)starContent;
            XmlNode sraiNode = RTParser.Utils.AIMLTagHandler.getNode(String.Format("<srai>{0}</srai>", sariCallStr.Trim()), templateNode);
            srai sraiHandler = new srai(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
            return sraiHandler.Transform();
        }

        public override void writeToLog(string s, params object[] p)
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