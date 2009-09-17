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
            Parameters = new[] {  new NamedParam(typeof(SimPosition), null) };

        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 3)
                return "Usage: moveto x y z";

            uint regionX, regionY;
            Utils.LongToUInts(Client.Network.CurrentSim.Handle, out regionX, out regionY);

            double x, y, z;
            if (!Double.TryParse(args[0], out x) ||
                !Double.TryParse(args[1], out y) ||
                !Double.TryParse(args[2], out z))
            {
                return "Usage: moveto x y z";
            }

            // Convert the local coordinates to global ones by adding the region handle parts to x and y
            x += (double)regionX;
            y += (double)regionY;

            Client.Self.AutoPilot(x, y, z);

            return String.Format("Attempting to move to <{0},{1},{2}>", x, y, z);
        }
    }
}
