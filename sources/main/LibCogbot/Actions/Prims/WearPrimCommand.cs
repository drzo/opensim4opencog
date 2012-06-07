using System;
using System.Collections.Generic;
using System.Threading;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class WearPrimCommand : Cogbot.Actions.Command, BotPersonalCommand
    {

        public WearPrimCommand(BotClient client)
        {
            Name = "WearPrim";
            Description = "Takes (derez to inventory) and wears a prim. Usage: wearprim [prim]";
            Category = Cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }


        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + args.str);

            foreach (var currentPrim in PS)
            {
                AddSuccess(Name + " on " + currentPrim);
                if (!TheSimAvatar.AttachToSelf(currentPrim)) Failure("Cannot Attach " + currentPrim);

            }
            return SuccessOrFailure();
        }
    }
}