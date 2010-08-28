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
            if (args.Length == 0)
                return Success("Logging is " + Settings.LOG_LEVEL);

            args[0] = args[0].ToLower();
            foreach (var s in typeof(Helpers.LogLevel).GetFields())
            {
                if (s.Name.ToLower().StartsWith(args[0]))
                {
                    Settings.LOG_LEVEL = (Helpers.LogLevel)s.GetValue(null);
                    return Success("Logging is set to " + Settings.LOG_LEVEL);
                }
            }
            return ShowUsage();// " debug [level] where level is one of None, Debug, Error, Info, Warn";
        }
    }
}
