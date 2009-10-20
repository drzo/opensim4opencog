using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Pathfinder
{
    public enum MovementProceedure
    {
        AutoPilot,
        AStar,
        TurnToAndWalk,
        Teleport
    }

    public class AStarGoto : cogbot.Actions.Command, BotPersonalCommand
    {
        public AStarGoto(BotClient client)
        {
            Name = GetType().Name;
            Description = "Use A* Pathfinding to get to object";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] { new NamedParam( typeof(SimPosition), typeof(Vector3d)) };

        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
            bool MadIt = WorldSystem.TheSimAvatar.GotoTarget(pos);
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