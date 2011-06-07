using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class MovetoCommand : Command, BotPersonalCommand
    {
        public MovetoCommand(BotClient client)
        {
            Name = "moveto";
            Description = "Moves the avatar to the specified global position using simulator autopilot. Usage: moveto x y z";
            Category = CommandCategory.Movement;
            Parameters = new[] { new NamedParam(typeof(SimPosition), typeof(SimPosition)) };

        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            if (args.Length < 1)
                return ShowUsage();// " moveto x y z";
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position==null)
            {
                return Failure("Coulnd not resolve location: " + string.Join(" ", args));
            }
            Vector3d g = position.GlobalPosition;
            Client.Self.AutoPilot(g.X, g.Y, g.Z);

            return Success(string.Format("Attempting to move to (AutoPilot) <{0},{1},{2}>", g.X, g.Y, g.Z));
        }
    }
}
