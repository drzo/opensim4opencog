using System;
using System.Collections.Generic;
using System.Text;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    public class SitCommand : Command, BotPersonalCommand
    {
        public SitCommand(BotClient testClient)
		{
			Name = "sitclose";
			Description = "Attempt to sit on the closest prim";
            AddVersion(CreateParams(), Description);
            Category = CommandCategory.Movement;
		}
			
        public override CmdResult ExecuteRequest(CmdRequest args)
		{
            Primitive closest = null;
		    double closestDistance = Double.MaxValue;
            Simulator sim = Client.Network.CurrentSim;
            if (sim == null)
            {
                WriteLine("Client Current Sim== null!");
                foreach (Simulator s in LockInfo.CopyOf(Client.Network.Simulators))
                {
                    sim = s;
                    break;                    
                }
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

                return Success("Sat on " + closest.ID + " (" + closest.LocalID + "). Distance: " + closestDistance);
            }
            else
            {
                return Failure("Couldn't find a nearby prim to sit on");
            }
		}
    }
}
