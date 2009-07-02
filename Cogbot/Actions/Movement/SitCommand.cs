using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SitCommand: Command
    {
        public SitCommand(BotClient testClient)
		{
			Name = "sit";
			Description = "Attempt to sit on the closest prim";
            Category = CommandCategory.Movement;
		}
			
        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            Primitive closest = null;
		    double closestDistance = Double.MaxValue;
            Simulator sim = Client.Network.CurrentSim;
            if (sim == null)
            {
                sim = Client.Network.Simulators[0];
                Client.Network.CurrentSim = sim;
            }
            sim.ObjectsPrimitives.ForEach(
                delegate(Primitive prim)
                {
                    float distance = Vector3.Distance(GetSimPosition(), prim.Position);

                    if (closest == null || distance < closestDistance)
                    {
                        closest = prim;
                        closestDistance = distance;
                    }
                }
            );

            if (closest != null)
            {
                Client.Self.RequestSit(closest.ID, Vector3.Zero);
                Client.Self.Sit();

                return "Sat on " + closest.ID + " (" + closest.LocalID + "). Distance: " + closestDistance;
            }
            else
            {
                return "Couldn't find a nearby prim to sit on";
            }
		}
    }
}
