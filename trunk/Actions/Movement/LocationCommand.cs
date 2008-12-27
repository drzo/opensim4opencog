using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class LocationCommand: Command
    {
        public LocationCommand(cogbot.TextForm testClient)
		{
			Name = "location";
			Description = "Show current location of avatar.";
            Category = CommandCategory.Movement;
		}

		public override string Execute(string[] args, UUID fromAgentID)
		{
            return "CurrentSim: '" + client.Network.CurrentSim.ToString() + "' Position: " + 
                client.Self.SimPosition.ToString();
		}
    }
}
