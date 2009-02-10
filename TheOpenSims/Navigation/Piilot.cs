using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;
using System.Windows.Forms;

namespace cogbot.TheOpenSims.Navigation
{
     class Approacher
    {

        static public bool AutoGoto1(BotClient Client, Vector3 target3, float dist, long maxMs)
        {
            long endAt = Environment.TickCount + maxMs;
            Vector2 target = new Vector2(target3.X, target3.Y);
            float d = DistanceTo(Client, target);
            if (d < dist) return true;
            float ld = d;
            float traveled = 0.0f;
            uint x, y;
            // Vector2 P = Position();
            Utils.LongToUInts(Client.Network.CurrentSim.Handle, out x, out y);
            Client.Self.AutoPilot((ulong)(x + target.X), (ulong)(y + target.Y), target3.Z);
            bool AutoPilot = true;
            while (AutoPilot)
            {
                // float moved = Vector2.Distance(P, Position());
                // WriteLine("Moved=" + moved);
                if (d < dist)
                {
                    AutoPilot = false;
                }
                else
                    if (Environment.TickCount > endAt)
                    {
                        AutoPilot = false;
                    }
                    else
                    {
                        Application.DoEvents();
                        d = DistanceTo(Client, target);
                        traveled = ld - d;
                        if (traveled < 0)
                        {
                            AutoPilot = false;
                        }
                        Client.Self.Movement.TurnToward(target3);
                        ld = d;
                    }
                //    P = Position();
            }
            Client.Self.AutoPilotCancel();
            Client.WorldSystem.TheSimAvatar.StopMoving();
            Client.Self.Movement.TurnToward(target3);
            return true;
        }

        public static float DistanceTo(BotClient Client, Vector2 v2)
        {
            Vector2 cp = Position(Client);
            return Vector2.Distance(v2, cp);
        }

        public static Vector2 Position(BotClient Client)
        {
            return new Vector2(Client.Self.SimPosition.X, Client.Self.SimPosition.Y);
        }

    }

    class GotoVector
    {
        Vector3 myPos = new Vector3();
        Vector2 myPos0 = new Vector2();
        Vector3 target = new Vector3();
        Vector2 target0 = new Vector2();
        float diff, olddiff, saveolddiff;
        int startTime = 0;
        int duration = 10000;
        float maxDist = 2.0f;
        bool StillMoving = false;
        readonly BotClient Client;
        OpenMetaverse.ObjectManager.ObjectUpdatedCallback cb;

        public GotoVector(BotClient client, Vector3 targetXYZ, int maxTimeMS, float maxDistance)
        {
            Client = client;
            target = targetXYZ;
            duration = maxTimeMS;
            maxDist = maxDistance;
            cb = new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
        }

        //readonly OpenMetaverse.ObjectManager.ObjectUpdatedCallback cb = new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
        public void Goto()
        {
            //}

            //public override string Execute(string[] args, UUID fromAgentID)
            //{
            //    if (args.Length > 4 || args.Length < 3)
            //        return "Usage: FlyTo x y z [seconds]";

            //    if (!float.TryParse(args[0], out target.X) ||
            //        !float.TryParse(args[1], out target.Y) ||
            //        !float.TryParse(args[2], out target.Z))
            //    {
            //        return "Usage: FlyTo x y z [seconds]";
            //    }
            target0.X = target.X;
            target0.Y = target.Y;
            //target.Z = Client.Self.SimPosition.Z;

            //if (args.Length == 4 && Int32.TryParse(args[3], out duration))
            //    duration *= 1000;
            Client.Objects.OnObjectUpdated += cb;
            StillMoving = true;
            startTime = Environment.TickCount;
            //Client.Self.Movement.Fly = true;
            Client.Self.Movement.AtPos = true;
            Client.Self.Movement.AtNeg = false;
            ZMovement();
            Client.Self.Movement.TurnToward(target);
            while (StillMoving)
            {
                Thread.Sleep(7);
                DoTick();
                //Application.
                // StopMoving();
            }
            //ZMovement();
            //Client.Self.Movement.SendUpdate(false);

            Debug(string.Format("flying to {0} in {1} seconds", target.ToString(), duration / 1000));
        }


        bool InUpdate = false;
        object InUpdateLock = new Object();
        private void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (startTime == 0) return;
            if (update.LocalID == Client.Self.LocalID)
                DoTick();
        }

        private void DoTick()
        {
            {
                if (!StillMoving) return;
                lock (InUpdateLock)
                {
                    if (InUpdate) return;
                    InUpdate = true;
                }

                XYMovement();
                ZMovement();
                if (Client.Self.Movement.AtPos || Client.Self.Movement.AtNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    Debug("Fly xy ");
                }
                else if (Client.Self.Movement.UpPos || Client.Self.Movement.UpNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    //Client.Self.Movement.SendUpdate(false);
                    Debug("Fly z ");
                }
                else if (Vector3.Distance(target, Client.Self.SimPosition) <= maxDist)
                {
                    StopMoving();
                    Debug("At Target");
                }
                InUpdate = false;
            }
            if (Environment.TickCount - startTime > duration)
            {
                StopMoving();
                Debug("End Flyto");
            }
        }

        private bool XYMovement()
        {
            bool res = false;

            myPos = Client.Self.SimPosition;
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
            return;
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            float diffz = (target.Z - Client.Self.SimPosition.Z);
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

        private void StopMoving()
        {
            Client.Objects.OnObjectUpdated -= cb;
            startTime = 0;
            Client.Self.Movement.AtPos = false;
            Client.Self.Movement.AtNeg = false;
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            Client.Self.Movement.SendUpdate(false);
            StillMoving = false;
        }

        private void Debug(string x)
        {
            //return; /* remove for debugging */
            Console.WriteLine(x + " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
        myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
        Client.Self.Movement.AtPos, Client.Self.Movement.AtNeg, Client.Self.Movement.UpPos, Client.Self.Movement.UpNeg,
        Client.Self.Velocity.ToString(), Client.Self.AngularVelocity.ToString());
        }
    }


    public class MovementToVector
    {
        public static bool MoveTo(BotClient bc, Vector3 targ, float dist)
        {
            if (true)
            {
                Approacher.AutoGoto1(bc, targ, dist, 10000);
                return true;
            }

            MovementToVector mtv = new MovementToVector(bc, targ, dist);
            mtv.Goto();
            if (mtv.GetDistance() > dist) return false;
            return true;
        }
        // SimAvatar theAvatar;
        Vector3 Destination;
        Vector3 LastPosition;
        BotClient Client;
        //private AutoResetEvent Ready = new AutoResetEvent(false);
        Boolean justStopped = false;
        float lastDistance = Single.MaxValue;
        int autoPilotsRemaining = 6;

        float followDist = 2.0F;
        public MovementToVector(BotClient bc, Vector3 targ, float fd)
        {
            //theAvatar = bc;
            Client = bc;//.GetGridClient();
            Destination = targ;
            followDist = fd;
        }

        public void Goto()
        {
            float d = GetDistance();
            if (d < followDist)
            {
                followDist = d / 2;
            }
            //Client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
            tracker();
            StopMoving();
            Client.Self.Movement.TurnToward(Destination);
            if (madePhantom.Count > 0)
            {
                foreach (SimObject obj in madePhantom)
                {
                    obj.RestoreEnterable();
                }
                madePhantom.Clear();
            }
        }

        private float GetDistance()
        {
            return Vector3.Distance(Client.Self.SimPosition, Destination);
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            //{
            //    if (Vector3.Distance(Client.Self.BotPosition, Destination) > followDist)
            //    {
            //        //if (Vector3.Dist(LastTarget, Destination) > 1)
            //        //{
            //        //   LastTarget = Destination;
            //        //    Client.Self.Movement.TurnToward(Destination);
            //        //    Client.Self.Movement.AtPos = true;
            //        //    //Client.Self.AutoPilotCancel();
            //        //      Client.Self.Movement.UpdateInterval = 0;
            //        //    Client.Self.Movement.SendUpdate();
            //        //}
            //        //      Client.Self.AutoPilotLocal((int)Destination.X,
            //        //          (int)Destination.Y, Destination.Z);
            //    }
            //    else
            //    {
            //        //Client.Self.AutoPilotCancel();
            //    }
            //}
        }

        readonly ListAsSet<SimObject> madePhantom = new ListAsSet<SimObject>();

        void tracker()
        {
            float curDist = GetDistance();
            bool UseAutoPilot = false;
            float traveled = 10f;
            while (curDist > followDist && autoPilotsRemaining > 0)
            {
                LastPosition = Client.Self.SimPosition;
                if (UseAutoPilot)
                {
                    autoPilotsRemaining--;
                    if (autoPilotsRemaining > 0)
                    {
                        Console.WriteLine("AutoPilot due to traveled=" + traveled);
                        PhantomizeArea();
                        Client.Self.AutoPilotLocal((int)Destination.X, (int)Destination.Y, Destination.Z);
                        Thread.Sleep(2000);
                    }
                    else
                    {
                        UseAutoPilot = false;
                    }

                }
                if (!UseAutoPilot)
                {
                    Client.Self.AutoPilotCancel();
                    UpdateHeading();
                }
                Thread.Sleep(250);
                traveled = Vector3.Distance(LastPosition, Client.Self.SimPosition);
                if (traveled < 0.1)
                {
                    UseAutoPilot = true;
                }
                else
                {
                    UseAutoPilot = false;
                }

                curDist = GetDistance();

            }
            Client.Self.AutoPilotCancel();
        }

        private void PhantomizeArea()
        {
            foreach (SimObject obj in SimObject.GetNearByObjects(Client.Self.SimPosition, Client.WorldSystem, null, 2.0f, true)) //should be false
            {
                madePhantom.AddTo(obj);
                obj.MakeEnterable();
            }
        }

        private void UpdateHeading()
        {
            Random somthing = new Random(DateTime.Now.Millisecond);// We do stuff randomly here
            float curDist = GetDistance();

            if (lastDistance <= curDist)
            {
                //    StopMoving();
                //    followDist = curDist + 1.0F;
            }
            lastDistance = curDist;

            if (curDist > followDist)
            {

                //Client.Self.AnimationStop(Animations.WALK, true);
                //Client.Self.AnimationStart(Animations.WALK, true);
                //Client.Self.Movement.SendUpdate();
                if (curDist < (followDist * 1.25))
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(125);
                    Client.Self.Movement.Stop = true;
                    Client.Self.Movement.AtPos = false;
                    Client.Self.Movement.NudgeAtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(100);
                    Client.Self.Movement.NudgeAtPos = false;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(100);
                }
                else
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.UpdateInterval = 0; //100
                    Client.Self.Movement.SendUpdate(true);
                    //(int)(25 * (1 + (curDist / followDist)))
                    Thread.Sleep(somthing.Next(25, 100));
                }
                justStopped = true;
            }
            else
            {
                if (justStopped)
                {
                    StopMoving();

                    Thread.Sleep(25);
                    justStopped = false;
                }
                else
                {
                    Thread.Sleep(100);
                }


            }
        }

        private void StopMoving()
        {
            Client.Self.Movement.TurnToward(Destination);
            Client.Self.Movement.AtPos = false;
            //Client.Self.Movement.UpdateInterval = 0;
            Client.Self.Movement.StandUp = true;
            //Client.Self.Movement.SendUpdate();
            Client.Self.Movement.FinishAnim = true;
            Client.Self.Movement.Stop = true;
            Client.Self.Movement.SendUpdate(true);

        }
    }
    
}
