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
    public class dbpush : RTParser.Utils.AIMLTagHandler
    {

        public dbpush(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "dbpush")
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    // what to remember
                    Unifiable templateNodeInnerValue = Recurse();
                    string myText = (string)templateNodeInnerValue;
                    ulong myDocID = this.user.bot.LuceneIndexer.IncDocId();
                    Dictionary<ulong, string> contentIdPairs = new Dictionary<ulong, string>();
                    contentIdPairs.Add(myDocID, myText);

                    // Indexing:
                    int numIndexed = this.user.bot.LuceneIndexer.Index(contentIdPairs);
                    Console.WriteLine("Indexed {0} docs.", numIndexed);
                    Console.WriteLine();
                }
                catch
                {
                }

            }
            return Unifiable.Empty;

        }
    }




}