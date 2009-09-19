using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Movement
{
    class MovetoCommand : Command
    {
        public MovetoCommand(BotClient client)
        {
            Name = "moveto";
            Description = "Moves the avatar to the specified global position using simulator autopilot. Usage: moveto x y z";
            Category = CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition), typeof(SimPosition)) };

        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            if (args.Length < 1)
                return "Usage: moveto x y z";
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            Vector3d g = position.GetWorldPosition();
            Client.Self.AutoPilot(g.X, g.Y, g.Z);

            return String.Format("Attempting to move to (AutoPilot) <{0},{1},{2}>", g.X, g.Y, g.Z);
        }
    }
}
