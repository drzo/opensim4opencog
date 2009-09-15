using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SitOnCommand : Command
    {
        public SitOnCommand(BotClient testClient)
        {
            Name = "Sit On";
            Description = "Attempt to sit on a particular prim, with specified UUID";
            Category = CommandCategory.Movement;
            Parameters = new[] { typeof(Primitive), typeof(UUID) };  
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return "Usage: siton UUID";

            int argsUsed;
            Primitive targetPrim = WorldSystem.GetPrimitive(args, out argsUsed);

            if (targetPrim != null)
            {
                WorldSystem.TheSimAvatar.SitOn(WorldSystem.GetSimObject(targetPrim));
                return "Requested to sit on prim " + targetPrim.ID.ToString() +
                       " (" + targetPrim.LocalID + ")";
            }

            return "Couldn't find a prim to sit on with UUID " + args[0];
        }


    }
}
