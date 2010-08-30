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
    public class dbload : RTParser.Utils.AIMLTagHandler
    {

        public dbload(RTParser.RTPBot bot,
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
            if (this.templateNode.Name.ToLower() == "dbload")
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    // Find and Replace
                    Unifiable templateNodeInnerValue = Recurse();
                    string path = (string)templateNodeInnerValue;
                    path = path.Trim();
                    if (HostSystem.FileExists(path))
                    {
                        this.user.bot.LuceneIndexer.LoadFileByLines(path);
                    }
                    else
                    {
                        Console.WriteLine("WARNING: dbload cannot find file :{0}",path);
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