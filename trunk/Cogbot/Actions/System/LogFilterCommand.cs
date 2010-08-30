using System;
using System.Collections.Generic;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class LogFilterCommand : Command, SystemApplicationCommand
    {
        public LogFilterCommand(BotClient testClient)
        {
            Name = "Log";
            Description = "Filters out console messages";
            Usage = "log ui -simmesh";
            Category = CommandCategory.BotClient;
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int start = 0;
            string fname = "UI";
            TextFilter filter = ClientManager.TheUILogFilter;
            if (args.Length>0)
            {
                string sf = args[0].ToLower();
                if (sf == "filter" || sf == "global")
                {
                    filter = DLRConsole.TheGlobalLogFilter;
                    fname = "Global";
                }
                else
                {
                    if (sf == "ui")
                    {
                        filter = DLRConsole.TheGlobalLogFilter;
                        fname = "UI";
                    }
                }
                start = 1;
            }
            string ss = Parser.Rejoin(args, start);
            filter.UpateLogging(ss, WriteLine);
            return Success("//log " + fname + " clear " + ss.Replace("\n", " "));
        }
    }
}
