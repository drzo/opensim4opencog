using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class DilationCommand : Command
    {
		public DilationCommand(cogbot.TextForm testClient)
        {
            Name = "dilation";
            Description = "Shows time dilation for current sim.";
            Category = CommandCategory.Simulator;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            return "Dilation is " + Client.Network.CurrentSim.Stats.Dilation.ToString();
        }
    }
}