using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class GotoCommand: Command
    {
        public GotoCommand(cogbot.TextForm testClient)
		{
			Name = "goto";
			Description = "Teleport to a location (e.g. \"goto Hooper/100/100/30\")";
            Category = CommandCategory.Movement;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
			if (args.Length < 1)
                return "Usage: goto sim/x/y/z";

            string destination = String.Empty;

            // Handle multi-word sim names by combining the arguments
            foreach (string arg in args)
            {
                destination += arg + " ";
            }
            destination = destination.Trim();

            string[] tokens = destination.Split(new char[] { '/' });
            if (tokens.Length != 4)
                return "Usage: goto sim/x/y/z";

            string sim = tokens[0];
            float x, y, z;
            if (!float.TryParse(tokens[1], out x) ||
                !float.TryParse(tokens[2], out y) ||
                !float.TryParse(tokens[3], out z))
            {
                return "Usage: goto sim/x/y/z";
            }

            if (client.Self.Teleport(sim, new Vector3(x, y, z)))
                return "Teleported to " + client.Network.CurrentSim;
            else
                return "Teleport failed: " + client.Self.TeleportMessage;
		}
    }
}
