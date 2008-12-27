using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class FindSimCommand : Command
    {
        public FindSimCommand(cogbot.TextForm testClient)
        {
            Name = "findsim";
            Description = "Searches for a simulator and returns information about it. Usage: findsim [Simulator Name]";
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length < 1)
                return "Usage: findsim [Simulator Name]";

            // Build the simulator name from the args list
            string simName = string.Empty;
            for (int i = 0; i < args.Length; i++)
                simName += args[i] + " ";
            simName = simName.TrimEnd().ToLower();

            //if (!GridDataCached[client])
            //{
            //    client.Grid.RequestAllSims(GridManager.MapLayerType.Objects);
            //    System.Threading.Thread.Sleep(5000);
            //    GridDataCached[client] = true;
            //}

            GridRegion region;

            if (client.Grid.GetGridRegion(simName, GridLayerType.Objects, out region))
                return String.Format("{0}: handle={1} ({2},{3})", region.Name, region.RegionHandle, region.X, region.Y);
            else
                return "Lookup of " + simName + " failed";
        }
    }
}
