using System;
using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.Virtualization;

//using System.Linq;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class dbload : AIMLTagHandler
    {

        public dbload(AltBot bot,
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
            if (CheckNode("dbload"))
            {
                // Simply push the filled in tag contents onto the stack
                try
                {
                    // Find and Replace
                    Unifiable templateNodeInnerValue = Recurse();
                    string path = (string)templateNodeInnerValue;
                    path = path.Trim();
                    string[] files = HostSystem.GetFiles(path);
                    if (files != null && files.Length > 0)
                    {
                        AddSideEffect("DBLOAD " + path, () =>
                                                            {
                                                                foreach (string file in files)
                                                                {
                                                                    TargetBot.LuceneIndexer.LoadDocuments(file,
                                                                                                          templateNode);
                                                                }
                                                            });
                        return "@echo DBLOAD " + path;
                    }
                    else
                    {
                        Console.WriteLine("WARNING: dbload cannot find file :{0}", path);
                    }

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