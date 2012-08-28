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
        }

        public override void MakeInfo()
        {
            Description = "Puts one minute temp blocks toward objects";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams("pos", typeof (SimPosition), "the location you wish to " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argcount;
            SimPosition pos;
            if (!args.TryGetValue("pos", out pos)) pos = TheSimAvatar;

            Vector3d v3d = pos.GlobalPosition;
            Vector3 v3 = pos.SimPosition;
            SimAbstractMover sam = SimCollisionPlaneMover.CreateSimPathMover(WorldSystem.TheSimAvatar, pos,
                                                                             pos.GetSizeDistance());
            sam.BlockTowardsVector(v3);
            return Success("SUCCESS ");
        }
    }
}