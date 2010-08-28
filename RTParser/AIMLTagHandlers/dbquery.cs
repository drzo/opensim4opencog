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
                    Console.WriteLine("Searching for the term \"{0}\"...", searchTerm1);
                    this.user.bot.LuceneIndexer.Search(searchTerm1, out ids, out results, out scores);
                    numHits = ids.Length;
                    Console.WriteLine("Number of hits == {0}.", numHits);
                    for (int i = 0; i < numHits; ++i)
                    {
                        Console.WriteLine("{0}) Doc-id: {1}; Content: \"{2}\" with score {3}.", i + 1, ids[i], results[i], scores[i]);
                    }
                    Console.WriteLine();




                    if (numHits > 0)
                    {
                        // should be weighted but lets just use the highest scoring
                        Unifiable converseMemo = results[0];
                        //Unifiable converseMemo = this.user.bot.conversationStack.Pop();
                        return converseMemo;
                    }
                    else
                    {
                        Unifiable starContent = Recurse();
                        XmlNode sraiNode = RTParser.Utils.AIMLTagHandler.getNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
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
    }
}