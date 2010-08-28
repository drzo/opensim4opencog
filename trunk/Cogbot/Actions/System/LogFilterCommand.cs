using System;
using System.Collections.Generic;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class LogFilterCommand : Command, SystemApplicationCommand
    {
        public LogFilterCommand(BotClient testClient)
        {
            Name = "Log Filter";
            Description = "Filters out console messages";
            Usage = "Filters out console messages";
            Category = CommandCategory.BotClient;
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string ss = Parser.Rejoin(args, 0);
            ClientManager.TheGlobalLogFilter.UpateLogging(ss,WriteLine);
            return Success("filter: " + ss);
        }
    }
}
