using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;
using Cogbot.World;

namespace Cogbot.Actions.Pathfinder
{
    [Flags]
    public enum MovementProceedure
    {
        AutoPilot,
        AStar,
        TurnToAndWalk,
        FlyTo,
        Teleport,
        CogPusher
    }

    public class AStarGoto : Cogbot.Actions.Command, BotPersonalCommand
    {
        public AStarGoto(BotClient client)
        {
            Name = GetType().Name;
            Description = "Use A* Pathfinding to get to object";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams("position", typeof(SimPosition), "the location you wish to " + Name);

        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return Failure(String.Format("Cannot {0} to {1}", Name, String.Join(" ", args)));
            }
            int maxSeconds = 6;
            float maxDistance = 1f;
            if (argcount < args.Length)
            {
            }
            String str = "GotoTarget(" + pos + ")";
            WriteLine(str);
            ((SimAvatarClient) WorldSystem.TheSimAvatar).SalientMovementProceedure = MovementProceedure.AStar;
            bool MadIt = WorldSystem.TheSimAvatar.SalientGoto(pos);
            if (MadIt)
            {
                return Success(string.Format("SUCCESS {0}", str));

            }
            else
            {
                return Success("FAILED " + str);
            }
        }
    }
}