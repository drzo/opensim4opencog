using System.Collections;
using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using cogbot.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions
{
    public class BotVarCommand : cogbot.Actions.Command, BotSystemCommand
    {
        public BotVarCommand(BotClient client)
        {
            Name = "botvar";
            Description = "Maniputlates bot vars. Usage: botvar master";
            Category = cogbot.Actions.CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int used = 0;
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            else
            {
                var PS = WorldSystem.ResolveCollection(args[0], out used, null);
                int found = 0;
                foreach (var o in PS)
                {
                    found++;
                    WriteLine("" + o);
                }
                return Success("argsUsed " + used + " found " + found);
            }
        }
    }
}