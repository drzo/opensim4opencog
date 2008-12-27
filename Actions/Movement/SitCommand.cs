using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SitCommand: Command
    {
        public SitCommand(cogbot.TextForm testClient)
		{
			Name = "sit";
			Description = "Attempt to sit on the closest prim";
            Category = CommandCategory.Movement;
		}
			
        public override string Execute(string[] args, UUID fromAgentID)
		{
            Primitive closest = null;
		    double closestDistance = Double.MaxValue;

            client.Network.CurrentSim.ObjectsPrimitives.ForEach(
                delegate(Primitive prim)
                {
                    float distance = Vector3.Distance(client.Self.SimPosition, prim.Position);

                    if (closest == null || distance < closestDistance)
                    {
                        closest = prim;
                        closestDistance = distance;
                    }
                }
            );

            if (closest != null)
            {
                client.Self.RequestSit(closest.ID, Vector3.Zero);
                client.Self.Sit();

                return "Sat on " + closest.ID + " (" + closest.LocalID + "). Distance: " + closestDistance;
            }
            else
            {
                return "Couldn't find a nearby prim to sit on";
            }
		}
    }
}
