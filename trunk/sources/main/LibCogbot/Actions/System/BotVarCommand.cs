

using System.Collections;
using System.Collections.Generic;
using Cogbot;
using Cogbot.World;
using Cogbot.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class BotVarCommand : Cogbot.Actions.Command, BotSystemCommand
    {
        public BotVarCommand(BotClient client)
        {
            Name = "botvar";
            Description = 
                "Display bot vars. Bot vars are an interface between botcmd and AIML. " +
                "For more information on bot vars see <a href='wiki/BotVars'>Bot Vars</a>";
            Details = AddUsage("botvar <varname>", "display a bot var") +
                Example("botvar master", "displays the name of the bot's master");

            Parameters = NamedParam.CreateParams(
                "varname", typeof(string), "bot var to display");

            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int used = 0;
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            else
            {
                var PS = WorldSystem.ResolveCollection(args[0], out used, null);
                if (PS == null) return Success("argsUsed " + used + " botvar was NULL");
                int found = 0;
                foreach (var o in PS)
                {
                    found++;
                    WriteLine("" + o);
                }
                return Success("argsUsed " + used + " found set with " + found);
            }
        }
    }
}
