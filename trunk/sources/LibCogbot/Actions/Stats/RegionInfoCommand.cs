using System;
using System.Text;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class RegionInfoCommand : Command, RegionMasterCommand
    {
        public RegionInfoCommand(BotClient testClient)
		{
			Name = "regioninfo";
			Description = "Prints out info about all the current region";
            Category = CommandCategory.Simulator;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            OpenMetaverse.Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;

            StringBuilder output = new StringBuilder();
            output.AppendLine(CurSim.ToString());
            output.Append("UUID: ");
            output.AppendLine(CurSim.ID.ToString());
            output.Append("RegionID: ");
            output.AppendLine(CurSim.RegionID.ToString());
            uint x, y;
            Utils.LongToUInts(CurSim.Handle, out x, out y);
            output.AppendLine(String.Format("Handle: {0} (X: {1} Y: {2})", CurSim.Handle, x, y));
            output.Append("Access: ");
            output.AppendLine(CurSim.Access.ToString());
            output.Append("Flags: ");
            output.AppendLine(CurSim.Flags.ToString());
            output.Append("TerrainBase0: ");
            output.AppendLine(CurSim.TerrainBase0.ToString());
            output.Append("TerrainBase1: ");
            output.AppendLine(CurSim.TerrainBase1.ToString());
            output.Append("TerrainBase2: ");
            output.AppendLine(CurSim.TerrainBase2.ToString());
            output.Append("TerrainBase3: ");
            output.AppendLine(CurSim.TerrainBase3.ToString());
            output.Append("TerrainDetail0: ");
            output.AppendLine(CurSim.TerrainDetail0.ToString());
            output.Append("TerrainDetail1: ");
            output.AppendLine(CurSim.TerrainDetail1.ToString());
            output.Append("TerrainDetail2: ");
            output.AppendLine(CurSim.TerrainDetail2.ToString());
            output.Append("TerrainDetail3: ");
            output.AppendLine(CurSim.TerrainDetail3.ToString());
            output.Append("Water Height: ");
            output.AppendLine(CurSim.WaterHeight.ToString());

            return Success(output.ToString());
        }
    }
}
