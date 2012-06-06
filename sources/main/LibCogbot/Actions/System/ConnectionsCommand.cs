using System;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class ConnectionsCommand : Cogbot.Actions.Command, SystemApplicationCommand
    {
        public ConnectionsCommand(BotClient client)
        {
            Name = GetType().Name;
            Description = "shows simulator connections";
            Details = AddUsage("connections [name]", "if name is present, any simulator whose name includes the string name will be shown");
            Parameters = CreateParams(
                Optional("name", typeof(string), "substring of region name(s) to show"));
            Category = CommandCategory.Simulator;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                foreach (SimRegion R in SimRegion.CurrentRegions)
                {
                    WriteLine(R.NetworkInfo());
                }
            }
            else
            {
                foreach (SimRegion R in SimRegion.CurrentRegions)
                {
                    if (R.RegionName.Contains(String.Join(" ", args)))
                        WriteLine(R.NetworkInfo());
                }
            }
            return Success("Ran " + Name);
        }
    }
}