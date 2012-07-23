using System;
using System.Collections.Generic;
using System.Reflection;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class DebugCommand : Command, SystemApplicationCommand
    {
        public DebugCommand(BotClient testClient)
        {
            Name = "debug";
            Description = "Turn debug messages on or off.";
            Details = AddUsage("debug <level>", "where level is one of None, Debug, Error, Info, Warn");
            Parameters = CreateParams(
                "level", typeof(string), "one of None, Debug, Error, Info, Warn");

            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
                return Success("Logging is " + Settings.LOG_LEVEL);

            string match = args[0].ToLower();
            int level = -2;
            foreach (var s in typeof(Helpers.LogLevel).GetFields(BindingFlags.Static | BindingFlags.Public))
            {
                level++;
                if (s.Name.ToLower().StartsWith(match))
                {
                    Settings.LOG_LEVEL = (Helpers.LogLevel) s.GetValue(null);
                    MushDLR223.Utilities.TaskQueueHandler.DebugLevel = level;
                    return Success("Logging is set to " + Settings.LOG_LEVEL);
                }
            }
            return ShowUsage();// " debug [level] where level is one of None, Debug, Error, Info, Warn";
        }
    }
}
