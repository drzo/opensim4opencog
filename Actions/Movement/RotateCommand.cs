using System;
using OpenMetaverse;

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
        readonly static float DEG_TO_RAD = 180f/(float)Math.PI;// 57.29577951f;
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


        /**              
             
        textform> rotate
        Second Bot: Rotation is at 0
        textform> rotate 90
        Second Bot: Turned 90
        textform> rotate
        Second Bot: Rotation is at 0
            
        Why is the Client.Self.Movement.BodyRotation Z axis alway zero no mater what?

         * 

        textform> rotate 90
        Second Bot:   VerifyQuatTranspose: <0, 0, 0, 1>==<0, 0, 0, 1>
        Second Bot:   VerifyQuat: <0, 0, 0.7071068, 0.7071068>!=<0, 0.7071068, 0, 0.7071068>
        Second Bot:   VerifyQuatTranspose: <0, 0, 0.7071068, 0.7071068>==<0, 0, 0.7071068, 0.7071068>
        Second Bot:   VerifyQuat: <0, 0, 0.7071068, 0.7071068>!=<0, 0.7071068, 0, 0.7071068>
        Second Bot:   VerifyQuatTranspose: <0, 0, 0.7071068, 0.7071068>==<0, 0, 0.7071068, 0.7071068>
        Second Bot: Rotation is roll: 0  pitch: 90  yaw: 0  
        textform> rotate 90
        Second Bot:   VerifyQuat: <0, 0, 0.7071068, 0.7071068>!=<0, 0.7071068, 0, 0.7071068>
        Second Bot:   VerifyQuatTranspose: <0, 0, 0.7071068, 0.7071068>==<0, 0, 0.7071068, 0.7071068>
        Second Bot:   VerifyQuat: <0, 0, 0.7071068, 0.7071068>!=<0, 0.7071068, 0, 0.7071068>
        Second Bot:   VerifyQuatTranspose: <0, 0, 0.7071068, 0.7071068>==<0, 0, 0.7071068, 0.7071068>
        Second Bot:   VerifyQuat: <0, 0, 1, 0>!=<-4.371139E-08, -1, -4.371139E-08, 1.910686E-15>
        Second Bot: Rotation is roll: 180  pitch: 0  yaw: 180  
        textform> rotate 180
        Second Bot:   VerifyQuat: <0, 0, 1, 0>!=<-4.371139E-08, -1, -4.371139E-08, 1.910686E-15>
        Second Bot:   VerifyQuat: <0, 0, 1, -4.371139E-08>!=<-4.371139E-08, -1, -4.371139E-08, 4.371139E-08>
        Second Bot:   VerifyQuat: <0, 0, -4.371139E-08, -1>!=<0, 4.371139E-08, 0, 1>
        Second Bot: Rotation is roll: 0  pitch: 5.008956E-06  yaw: 0  

             
        */
        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length > 1)
                return "Usage: rotate [angle]";

            int angle;
            if (args.Length == 0)
            {             
                Quaternion current = Client.Self.Movement.BodyRotation;
                VerifyQuat(current);
                //float x, y, z;
                //current.GetEulerAngles(out x, out y, out z);
                //return "Rotation is at " + z * DEG_TO_RAD;
                float roll, pitch, yaw;
                current.GetEulerAngles(out roll, out pitch, out yaw);
                return String.Format("Rotation is roll: {0}  pitch: {1}  yaw: {2}  ", roll * DEG_TO_RAD, pitch * DEG_TO_RAD, yaw * DEG_TO_RAD);
            }
            else
            {
                // Parse the number             
                if (!Int32.TryParse(args[0], out angle)) // rotate help
                    return "Usage: rotate [angle]";

                Quaternion current = Client.Self.Movement.BodyRotation;
                Quaternion z_angle = Quaternion.CreateFromEulers(new Vector3(0, 0, angle / DEG_TO_RAD));
                Quaternion next = current * z_angle;
                VerifyQuat(current);
                VerifyQuat(z_angle);
                VerifyQuat(next);
                Client.Self.Movement.BodyRotation = next;
                Client.Self.Movement.SendUpdate();
                //return "Turned " + angle;
                float roll, pitch, yaw;
                next.GetEulerAngles(out roll, out pitch, out yaw);
                return String.Format("Rotation is roll: {0}  pitch: {1}  yaw: {2}  ", roll * DEG_TO_RAD, pitch * DEG_TO_RAD, yaw * DEG_TO_RAD);

            }
        }
        void VerifyQuat(Quaternion current)
        {
            current.Normalize();

            float roll, pitch, yaw;
            current.GetEulerAngles(out roll, out pitch, out yaw);
            Quaternion other = Quaternion.CreateFromEulers(new Vector3(roll, pitch, yaw));
            other.Normalize();
            // Always a problem!?!
            if (current != other) WriteLine(String.Format("  VerifyQuat: {0}!={1}", current, other));
            VerifyQuatTranspose(current);
        }

        void VerifyQuatTranspose(Quaternion current)
        {
            current.Normalize();

            float roll, pitch, yaw;
            current.GetEulerAngles(out roll, out yaw, out pitch);
            Quaternion other = Quaternion.CreateFromEulers(new Vector3(roll, pitch, yaw));
            other.Normalize();
            // This workarround it?
            if (current == other) WriteLine(String.Format("  VerifyQuatTranspose: {0}=={1}", current, other));
        }
    }
}
