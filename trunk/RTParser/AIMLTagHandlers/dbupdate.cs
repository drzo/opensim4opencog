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
    public class dbupdate : RTParser.Utils.AIMLTagHandler
    {

        public dbupdate(RTParser.RTPBot bot,
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
            if (CheckNode("dbupdate"))
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    // Find and Replace
                    Unifiable templateNodeInnerValue = Recurse();
                    string myText = (string)templateNodeInnerValue;
                    AddSideEffect("DBUPDATE " + myText, () => TargetBot.LuceneIndexer.Update(myText, myText, templateNode));                     
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