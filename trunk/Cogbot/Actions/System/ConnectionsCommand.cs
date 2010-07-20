using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class ConnectionsCommand : cogbot.Actions.Command, SystemApplicationCommand
    {
        public ConnectionsCommand(BotClient client)
        {
            Name = GetType().Name;
            Description = "shows simulator connections";
            Category = CommandCategory.Simulator;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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