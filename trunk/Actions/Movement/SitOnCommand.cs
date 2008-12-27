using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SitOnCommand : Command
    {
        public SitOnCommand(cogbot.TextForm testClient)
        {
            Name = "siton";
            Description = "Attempt to sit on a particular prim, with specified UUID";
            Category = CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length != 1)
                return "Usage: siton UUID";

            UUID target;

            if (UUID.TryParse(args[0], out target))
            {
                Primitive targetPrim = client.Network.CurrentSim.ObjectsPrimitives.Find(
                    delegate(Primitive prim)
                    {
                        return prim.ID == target;
                    }
                );

                if (targetPrim != null)
                {
                    client.Self.RequestSit(targetPrim.ID, Vector3.Zero);
                    client.Self.Sit();
                    return "Requested to sit on prim " + targetPrim.ID.ToString() +
                        " (" + targetPrim.LocalID + ")";
                }
            }

            return "Couldn't find a prim to sit on with UUID " + args[0];
        }
    }
}
