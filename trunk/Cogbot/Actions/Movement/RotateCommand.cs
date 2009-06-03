using System;
using OpenMetaverse;
using cogbot.Listeners;

namespace cogbot.Actions.Movement
{

    class RotateCommand : Command
    {
        public RotateCommand(BotClient client)
        {
            Name = "rotate";
            Description = "The rotate command changes the BodyRotation on the server with a single packet. Usage: rotate [180]";
            Category = CommandCategory.Movement;
        }
        const float DEG_TO_RAD = 180f / (float)Math.PI;// 57.29577951f;
        /// <summary>
        /// Offsets a position by the Global position determined by the region handle
        /// </summary>
        /// <param name="regionHandle"></param>
        /// <param name="pos"></param>
        /// <returns></returns>
        public static Vector3 OffsetGobal(ulong regionHandle, Vector3 pos)
        {

            uint locationx = 0;
            uint locationy = 0;
            Utils.LongToUInts(regionHandle, out locationx, out locationy);
            pos.X = (int)locationx + pos.X;
            pos.Y = (int)locationy + pos.Y;

            return pos;
        }


        /**             
            textform> rotate
            Second Bot: Rotation is at 0
            textform> rotate 90
            Second Bot: Turned 90             
        */
        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length > 1)
                return "Usage: rotate [angle]";

            if (args.Length == 0)
            {
                return String.Format("ZHeading = {0}", (WorldSystem.TheSimAvatar.ZHeading + DEG_TO_RAD));
            }
            else
            {
                Vector3 cur = WorldSystem.TheSimAvatar.GetSimPosition();
                // Parse the number             
                float angle;
                if (!float.TryParse(args[0], out angle)) // rotate help
                    return "Usage: rotate [angle]";
                float newAngle = WorldSystem.TheSimAvatar.ZHeading + (angle / DEG_TO_RAD);
                cur.X += (float)Math.Cos(newAngle) * 2;
                cur.Y -= (float)Math.Sin(newAngle) * 2;            
                Client.Self.Movement.TurnToward(cur);
                Client.Self.Movement.SendUpdate(false);
                return string.Format("Turned To {0}", DEG_TO_RAD * newAngle);

            }
        }
    }
}
