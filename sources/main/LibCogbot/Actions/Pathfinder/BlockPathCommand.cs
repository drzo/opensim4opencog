using System;
using MushDLR223.ScriptEngines;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Pathfinder
{
    public class BlockPathCommand : Cogbot.Actions.Command, RegionMasterCommand
    {
        public BlockPathCommand(BotClient client)
        {
            Name = "Block Path";
            Description = "Puts one minute temp blocks toward objects";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return Failure("Cannot " + Name + " to " + String.Join(" ", args));
            }

            Vector3d v3d = pos.GlobalPosition;
            Vector3 v3 = pos.SimPosition;
            SimAbstractMover sam = SimCollisionPlaneMover.CreateSimPathMover(WorldSystem.TheSimAvatar, pos, pos.GetSizeDistance());
            sam.BlockTowardsVector(v3);
            return Success("SUCCESS ");
        }
    }
}