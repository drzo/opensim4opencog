using System;
using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    internal class AutoPilotCommand : Command, BotPersonalCommand
    {
        public AutoPilotCommand(BotClient client)
        {
            Name = "autopilot";
        }

        public override void MakeInfo()
        {
            Description =
                "Moves the avatar to the specified global position using simulator autopilot. Usage: autopilot x y z";
            Category = CommandCategory.Movement;
            Parameters = CreateParams("position", typeof (SimPosition), "the location you wish to " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argsUsed;
            if (args.Length < 1)
                return ShowUsage(); // " moveto x y z";
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position == null)
            {
                return Failure("Coulnd not resolve location: " + args.str);
            }
            Vector3d g = position.GlobalPosition;
            Client.Self.AutoPilot(g.X, g.Y, g.Z);

            return Success(string.Format("Attempting to move to (AutoPilot) <{0},{1},{2}>", g.X, g.Y, g.Z));
        }
    }
}