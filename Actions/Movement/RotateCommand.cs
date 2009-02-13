using System;
using OpenMetaverse;

namespace cogbot.Actions.Movement
{
    class RotateCommand : Command
    {
        public RotateCommand(BotClient client)
        {
            Name = "rotate";
            Description = "Sends the RotateCommand command to the server for a single packet or a given number of seconds. Usage: rotate [180]";
            Category = CommandCategory.Movement;
        }
        readonly static float DEG_TO_RAD = 1 / 57.29577951f;
        /// <summary>
        /// Offsets a position by the Global position determined by the regionhandle
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

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length > 1)
                return "Usage: rotate [angle]";


            int angle;
            if (args.Length == 0)
            {
                float x, y, z;
                WorldSystem.TheSimAvatar.GetSimRotation().GetEulerAngles(out x, out y, out z);
                return "Rotation is at " + z * DEG_TO_RAD;
            }
            else
            {
                // Parse the number             
                if (!Int32.TryParse(args[0], out angle))
                    return "Usage: rotate [angle]";
                //float x, y, z;
                //Client.Self.Movement.BodyRotation.GetEulerAngles(out x, out y, out z);
                //double heading = (angle / 57.29577951);
                //Client.Self.Movement.UpdateFromHeading(heading, false);
                Quaternion x_angle = Quaternion.CreateFromEulers(new Vector3(0, 0, angle * DEG_TO_RAD));
                Quaternion current = Client.Self.Movement.BodyRotation;
                Quaternion next = current * x_angle;
                Client.Self.Movement.BodyRotation = next;
                Client.Self.Movement.SendUpdate();
                return "Turned " + angle;

            }
 
        }
    }
}
