using System.Collections.Generic;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class TakeCommand : Cogbot.Actions.Command, RegionMasterCommand
    {
        public TakeCommand(BotClient client)
        {
            Name = "Take";
            Description = "Takes from a prim. Usage: Take [prim]";
            Category = Cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
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
            GridClient client = TheBotClient;
            foreach (var currentPrim in PS)
            {
                Success(Name + " on " + currentPrim);
                if (TheSimAvatar.TakeObject(currentPrim)==null) Failure("Cannot Take " + currentPrim);
            }
            return SuccessOrFailure();
        }
    }
}