using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.System
{
    public class DilationCommand : Command, RegionMasterCommand
    {
		public DilationCommand(BotClient testClient)
        {
            Name = "dilation";
            Description = "Shows time dilation for current sim.";
            Category = CommandCategory.Simulator;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;
            return Success("Dilation is " + CurSim.Stats.Dilation.ToString());
        }
    }
}