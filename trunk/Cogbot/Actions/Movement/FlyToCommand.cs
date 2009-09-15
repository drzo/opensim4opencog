using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Movement
{
    class FlyToCommand : Command
    {

        Vector3 myPos = new Vector3();
        Vector2 myPos0 = new Vector2();
        Vector3 target = new Vector3();
        Vector2 target0 = new Vector2();
        float diff, olddiff, saveolddiff;
        int startTime = 0;
        int duration = 10000;
        ObjectManager.ObjectUpdatedCallback callback; 

        public FlyToCommand(BotClient testClient)
        {
            TheBotClient = testClient;

            Name = "Fly To";
            Description = "Fly the avatar toward the specified position for a maximum of seconds. Usage: FlyTo x y z [seconds]";
            Category = CommandCategory.Movement;
            Parameters = new[] { typeof(SimPosition), typeof(string) };
            callback = new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length > 4 || args.Length < 3)
                return "Usage: FlyTo x y z [seconds]";

            if (!float.TryParse(args[0], out target.X) ||
                !float.TryParse(args[1], out target.Y) ||
                !float.TryParse(args[2], out target.Z))
            {
                return "Usage: FlyTo x y z [seconds]";
            }
            target0.X = target.X;
            target0.Y = target.Y;

            if (args.Length == 4 && Int32.TryParse(args[3], out duration))
                duration *= 1000;

            Client.Objects.OnObjectUpdated += callback;
            startTime = Environment.TickCount;
            Client.Self.Movement.Fly = true;
            Client.Self.Movement.AtPos = true;
            Client.Self.Movement.AtNeg = false;
            ZMovement();
            Client.Self.Movement.TurnToward(target);
            //System.Threading.Thread.Sleep(100);

            //XYMovement();
            //ZMovement();
            //Client.Self.Movement.SendUpdate(false);

            return string.Format("flying to {0} in {1} seconds", target.ToString(), duration / 1000);
        }

        private void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (startTime == 0) return;
            if (update.LocalID == Client.Self.LocalID)
            {
                XYMovement();
                ZMovement();
                if (Client.Self.Movement.AtPos || Client.Self.Movement.AtNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    Debug("Flyxy ");
                }
                else if (Client.Self.Movement.UpPos || Client.Self.Movement.UpNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    //Client.Self.Movement.SendUpdate(false);
                    Debug("Fly z ");
                }
                else if (Vector3.Distance(target, GetSimPosition()) <= 2.0)
                {
                    EndFlyto();
                    Debug("At Target");
                }
            }
            if (Environment.TickCount - startTime > duration)
            {
                EndFlyto();
                Debug("End Flyto");
            }
        }

        private bool XYMovement()
        {
            bool res = false;

            myPos = GetSimPosition();
            myPos0.X = myPos.X;
            myPos0.Y = myPos.Y;
            diff = Vector2.Distance(target0, myPos0);
            Vector2 vvel = new Vector2(Client.Self.Velocity.X, Client.Self.Velocity.Y);
            float vel = vvel.Length();
            if (diff >= 10.0)
            {
                Client.Self.Movement.AtPos = true;
                //  Client.Self.Movement.AtNeg = false;
                //if (Math.Abs(diff - olddiff) > 1.5) {
                //  Client.Self.Movement.AtPos = diff < olddiff;
                //  Client.Self.Movement.AtNeg = diff > olddiff;
                //} else if (!Client.Self.Movement.AtPos && !Client.Self.Movement.AtNeg) {
                //  Client.Self.Movement.AtPos = true;
                //  Client.Self.Movement.AtNeg = false;
                //}
                res = true;
            }
            else if (diff >= 2 && vel < 5)
            {
                Client.Self.Movement.AtPos = true;
            }
            else
            {
                Client.Self.Movement.AtPos = false;
                Client.Self.Movement.AtNeg = false;
            }
            saveolddiff = olddiff;
            olddiff = diff;
            return res;
        }

        private void ZMovement()
        {
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            float diffz = (target.Z - GetSimPosition().Z);
            if (diffz >= 20.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -20.0)
                Client.Self.Movement.UpNeg = true;
            else if (diffz >= +5.0 && Client.Self.Velocity.Z < +4.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -5.0 && Client.Self.Velocity.Z > -4.0)
                Client.Self.Movement.UpNeg = true;
            else if (diffz >= +2.0 && Client.Self.Velocity.Z < +1.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -2.0 && Client.Self.Velocity.Z > -1.0)
                Client.Self.Movement.UpNeg = true;
        }

        private void EndFlyto()
        {
            startTime = 0;
            Client.Self.Movement.AtPos = false;
            Client.Self.Movement.AtNeg = false;
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            Client.Self.Movement.SendUpdate(false);
            Client.Objects.OnObjectUpdated -= callback;
        }

        private void Debug(string x)
        {
           // return; /* remove for debugging */
            WriteLine(x + " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
        myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
        Client.Self.Movement.AtPos, Client.Self.Movement.AtNeg, Client.Self.Movement.UpPos, Client.Self.Movement.UpNeg,
        Client.Self.Velocity.ToString(), Client.Self.AngularVelocity.ToString());
        }
    }
}
