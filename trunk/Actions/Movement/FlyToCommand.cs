using System;
using OpenMetaverse;

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

        public FlyToCommand(cogbot.TextForm testclient)
        {
            Name = "FlyTo";
            Description = "Fly the avatar toward the specified position for a maximum of seconds. Usage: FlyTo x y z [seconds]";
            Category = CommandCategory.Movement;
            client = testclient.client;
            client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
        }

        public override string Execute(string[] args, UUID fromAgentID)
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

            startTime = Environment.TickCount;
            client.Self.Movement.Fly = true;
            client.Self.Movement.AtPos = true;
            client.Self.Movement.AtNeg = false;
            ZMovement();
            client.Self.Movement.TurnToward(target);
            //System.Threading.Thread.Sleep(100);

            //XYMovement();
            //ZMovement();
            //client.Self.Movement.SendUpdate(false);

            return string.Format("flying to {0} in {1} seconds", target.ToString(), duration / 1000);
        }

        private void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (startTime == 0) return;
            if (update.LocalID == client.Self.LocalID)
            {
                XYMovement();
                ZMovement();
                if (client.Self.Movement.AtPos || client.Self.Movement.AtNeg)
                {
                    client.Self.Movement.TurnToward(target);
                    Debug("Flyxy ");
                }
                else if (client.Self.Movement.UpPos || client.Self.Movement.UpNeg)
                {
                    client.Self.Movement.TurnToward(target);
                    //client.Self.Movement.SendUpdate(false);
                    Debug("Fly z ");
                }
                else if (Vector3.Distance(target, client.Self.SimPosition) <= 2.0)
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

            myPos = client.Self.SimPosition;
            myPos0.X = myPos.X;
            myPos0.Y = myPos.Y;
            diff = Vector2.Distance(target0, myPos0);
            Vector2 vvel = new Vector2(client.Self.Velocity.X, client.Self.Velocity.Y);
            float vel = vvel.Length();
            if (diff >= 10.0)
            {
                client.Self.Movement.AtPos = true;
                //  client.Self.Movement.AtNeg = false;
                //if (Math.Abs(diff - olddiff) > 1.5) {
                //  client.Self.Movement.AtPos = diff < olddiff;
                //  client.Self.Movement.AtNeg = diff > olddiff;
                //} else if (!client.Self.Movement.AtPos && !client.Self.Movement.AtNeg) {
                //  client.Self.Movement.AtPos = true;
                //  client.Self.Movement.AtNeg = false;
                //}
                res = true;
            }
            else if (diff >= 2 && vel < 5)
            {
                client.Self.Movement.AtPos = true;
            }
            else
            {
                client.Self.Movement.AtPos = false;
                client.Self.Movement.AtNeg = false;
            }
            saveolddiff = olddiff;
            olddiff = diff;
            return res;
        }

        private void ZMovement()
        {
            client.Self.Movement.UpPos = false;
            client.Self.Movement.UpNeg = false;
            float diffz = (target.Z - client.Self.SimPosition.Z);
            if (diffz >= 20.0)
                client.Self.Movement.UpPos = true;
            else if (diffz <= -20.0)
                client.Self.Movement.UpNeg = true;
            else if (diffz >= +5.0 && client.Self.Velocity.Z < +4.0)
                client.Self.Movement.UpPos = true;
            else if (diffz <= -5.0 && client.Self.Velocity.Z > -4.0)
                client.Self.Movement.UpNeg = true;
            else if (diffz >= +2.0 && client.Self.Velocity.Z < +1.0)
                client.Self.Movement.UpPos = true;
            else if (diffz <= -2.0 && client.Self.Velocity.Z > -1.0)
                client.Self.Movement.UpNeg = true;
        }

        private void EndFlyto()
        {
            startTime = 0;
            client.Self.Movement.AtPos = false;
            client.Self.Movement.AtNeg = false;
            client.Self.Movement.UpPos = false;
            client.Self.Movement.UpNeg = false;
            client.Self.Movement.SendUpdate(false);
        }

        private void Debug(string x)
        {
            return; /* remove for debugging */
            WriteLine(x + " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
        myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
        client.Self.Movement.AtPos, client.Self.Movement.AtNeg, client.Self.Movement.UpPos, client.Self.Movement.UpNeg,
        client.Self.Velocity.ToString(), client.Self.AngularVelocity.ToString());
        }
    }
}
