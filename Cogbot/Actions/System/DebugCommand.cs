using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class DebugCommand : Command
    {
        public DebugCommand(BotClient testClient)
        {
            Name = "debug";
            Description = "Turn debug messages on or off. Usage: debug [level] where level is one of None, Debug, Error, Info, Warn";
            Category = CommandCategory.TestClient;
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
