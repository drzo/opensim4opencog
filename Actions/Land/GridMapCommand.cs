using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class GridMapCommand : Command
    {
        public GridMapCommand(cogbot.TextForm testClient)
        {
            Name = "gridmap";
            Description = "Downloads all visible information about the grid map";
            Category = CommandCategory.Simulator;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            //if (args.Length < 1)
            //    return "";

            client.Grid.RequestMainlandSims(GridLayerType.Objects);
            
            return "Sent.";
        }
    }
}
