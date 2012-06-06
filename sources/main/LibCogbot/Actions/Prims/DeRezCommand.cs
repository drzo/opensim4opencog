using System;
using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class DeRezCommand : Command, RegionMasterCommand
    {
        public DeRezCommand(BotClient testClient)
        {
            Name = "derez";
            Description = "De-Rezes a specified prim. " + "Usage: derez [prim-uuid]";
            Category = CommandCategory.Objects;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            UUID primID = UUID.Zero;

            if (args.Length < 1)
            {
                return ShowUsage();
            }

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + args.str);
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
