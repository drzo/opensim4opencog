using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Land
{
    public class FindSimCommand : Command, GridMasterCommand
    {
        public FindSimCommand(BotClient testClient)
        {
            Name = "findsim";
            Description = "Searches for a simulator and returns information about it. Usage: findsim [Simulator Name]";
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " findsim [Simulator Name]";

            // Build the simulator name from the args list
            string simName = string.Empty;
            for (int i = 0; i < args.Length; i++)
                simName += args[i] + " ";
            simName = simName.TrimEnd().ToLower();

            //if (!GridDataCached[Client])
            //{
            //    Client.Grid.RequestAllSims(GridManager.MapLayerType.Objects);
            //    System.Threading.Thread.Sleep(5000);
            //    GridDataCached[Client] = true;
            //}

            GridRegion region;

            if (Client.Grid.GetGridRegion(simName, GridLayerType.Objects, out region))
                return
                    Success(string.Format("{0}: handle={1} ({2},{3})", region.Name, region.RegionHandle, region.X,
                                          region.Y));

            else
                return Failure("Lookup of " + simName + " failed");
        }
    }
}
