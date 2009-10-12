using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
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
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };  
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return Failure(Usage);// " siton UUID";

            int argsUsed;
            List<Primitive> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            foreach (var targetPrim in PS)
            {
                WorldSystem.TheSimAvatar.SitOn(WorldSystem.GetSimObject(targetPrim));
                Success("Requested to sit on prim " + targetPrim.ID.ToString() +
                       " (" + targetPrim.LocalID + ")");
            }
            return SuccessOrFailure();
        }


    }
}
