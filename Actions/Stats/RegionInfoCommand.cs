using System;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class RegionInfoCommand : Command
    {
        public RegionInfoCommand(cogbot.TextForm testClient)
		{
			Name = "regioninfo";
			Description = "Prints out info about all the current region";
            Category = CommandCategory.Simulator;
		}

        public override string Execute(string[] args, UUID fromAgentID)
        {
            StringBuilder output = new StringBuilder();
            output.AppendLine(client.Network.CurrentSim.ToString());
            output.Append("UUID: ");
            output.AppendLine(client.Network.CurrentSim.ID.ToString());
            uint x, y;
            Utils.LongToUInts(client.Network.CurrentSim.Handle, out x, out y);
            output.AppendLine(String.Format("Handle: {0} (X: {1} Y: {2})", client.Network.CurrentSim.Handle, x, y));
            output.Append("Access: ");
            output.AppendLine(client.Network.CurrentSim.Access.ToString());
            output.Append("Flags: ");
            output.AppendLine(client.Network.CurrentSim.Flags.ToString());
            output.Append("TerrainBase0: ");
            output.AppendLine(client.Network.CurrentSim.TerrainBase0.ToString());
            output.Append("TerrainBase1: ");
            output.AppendLine(client.Network.CurrentSim.TerrainBase1.ToString());
            output.Append("TerrainBase2: ");
            output.AppendLine(client.Network.CurrentSim.TerrainBase2.ToString());
            output.Append("TerrainBase3: ");
            output.AppendLine(client.Network.CurrentSim.TerrainBase3.ToString());
            output.Append("TerrainDetail0: ");
            output.AppendLine(client.Network.CurrentSim.TerrainDetail0.ToString());
            output.Append("TerrainDetail1: ");
            output.AppendLine(client.Network.CurrentSim.TerrainDetail1.ToString());
            output.Append("TerrainDetail2: ");
            output.AppendLine(client.Network.CurrentSim.TerrainDetail2.ToString());
            output.Append("TerrainDetail3: ");
            output.AppendLine(client.Network.CurrentSim.TerrainDetail3.ToString());
            output.Append("Water Height: ");
            output.AppendLine(client.Network.CurrentSim.WaterHeight.ToString());

            return output.ToString();
        }
    }
}
