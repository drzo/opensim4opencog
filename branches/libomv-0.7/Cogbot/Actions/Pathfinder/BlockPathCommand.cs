using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Pathfinder
{
    public class BlockPathCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public BlockPathCommand(BotClient client)
        {
            Name = "Block Path";
            Description = "Puts one minute temp blocks toward objects";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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