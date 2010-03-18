using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions.Objects
{
    public class WearPrimCommand : cogbot.Actions.Command, RegionMasterCommand
    {

        public WearPrimCommand(BotClient client)
        {
            Name = "WearPrim";
            Description = "Takes and wears a prim. Usage: wearprim [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }


        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));

            foreach (var currentPrim in PS)
            {
                Success(Name + " on " + currentPrim);
                if (!TheSimAvatar.AttachToSelf(currentPrim)) Failure("Cannot Attach " + currentPrim);

            }
            return SuccessOrFailure();
        }
    }
}