using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Objects
{
    public class DeRezCommand : Command, RegionMasterCommand
    {
        public DeRezCommand(BotClient testClient)
        {
            Name = "derez";
            Description = "De-Rezes a specified prim. " + "Usage: derez [prim-uuid]";
            Category = CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            UUID primID = UUID.Zero;

            if (args.Length < 1)
            {
                return ShowUsage();
            }

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            foreach (var target in PS)
            {
                WorldSystem.DeletePrim(target.Prim);
                WriteLine("\n {0}", target);
                Success("Done.");
            }
            return SuccessOrFailure();
        }
    }
}
