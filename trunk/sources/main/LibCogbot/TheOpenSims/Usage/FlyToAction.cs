using System;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    class FlyToAction : MoveToLocation
    {

        readonly private float maxDistance;
        readonly private Thread FollowThread;
        private bool KeepFollowing = true;
        public static bool UsePathfinder = true;
        bool CallbackReged = false;
        private int blockedTimes = 0;
        bool DoZ = false;
        

        public override void Abort()
        {
            var Client = GetGridClient();
            var ClientMovement = Client.Self.Movement;

            DeRegCallback();
            TheBot.StopMoving();
            EndFlyto();
            ClientMovement.Fly = true;
            if (FollowThread.IsAlive)
            {
                FollowThread.Abort();
            }

        }

        public override void InvokeReal()
        {
            target0.X = (float)target.X;
            target0.Y = (float)target.Y;

            RegCallback();
            SetMovement();
            //System.Threading.Thread.Sleep(100);

            //XYMovement();
            //ZMovement();
            //ClientMovement.SendUpdate(false);

            // start if not already started
            var bc = GetBotClient();
            if (!FollowThread.IsAlive)
            {
                try
                {
                    bc.AddThread(FollowThread);
                    FollowThread.Start();
                    FollowThread.Join();
                }
                finally
                {
                    bc.RemoveThread(FollowThread);
                    DeRegCallback();
                }
            }
        }


        private void SetMovement()
        {

            var Client = GetGridClient();
            var ClientMovement = Client.Self.Movement;

            RegCallback();
            startTime = DateTime.Now;
            ClientMovement.Fly = true;
            ClientMovement.AtPos = true;
            ClientMovement.AtNeg = false;
            ZMovement();
            // ClientMovement
            TheCBot.TurnToward(Target);

        }

        Vector3d myPos
        {
            get { return TheBot.GlobalPosition; }

        }
        Vector2 myPos0 = new Vector2();
        Vector3d target
        {
            get { return Target.GlobalPosition; }
        }
        Vector2 target0 = new Vector2();
        float diff, olddiff, saveolddiff;
        private DateTime startTime = DateTime.MinValue;
        //   int duration = 10000;
        readonly EventHandler<TerseObjectUpdateEventArgs> callback;

        public FlyToAction(SimAvatar impl, SimPosition position)
            : base(impl, position)
        {
            // set in base
            //TheBot = impl;
            //Target = position;
            maxDistance = position.GetSizeDistance();
            FollowThread = new Thread(FollowLoop);
            FollowThread.Name = ToString();
            callback = Objects_OnObjectUpdated;
        }

        public void FollowLoop()
        {
            try
            {
                while (KeepFollowing)
                {
                    if (!Target.IsRegionAttached)
                    {
                        EndFlyto();
                        DLRConsole.DebugWriteLine("" + this + " Not regions attached " + Target);
                        Thread.Sleep(2000);
                        return;
                    }
                    //DoZ = !DoZ;
                    //if (DoZ) SetMovement();
                    FlyToOnce();
                }
            }
            finally
            {
                EndFlyto();
            }
        }

        public void RegCallback()
        {
            if (!CallbackReged)
            {
                var Client = GetGridClient();
                Client.Objects.TerseObjectUpdate += callback;
                CallbackReged = true;
            }
        }

        public void DeRegCallback()
        {
            if (CallbackReged)
            {
                var Client = GetGridClient();
                Client.Objects.TerseObjectUpdate -= callback;
                CallbackReged = false;
            }
        }

        public bool FlyToOnce()
        {
            // startTime = 10000;
            try
            {


                var Client = GetGridClient();
                double dist = ((SimMover)TheBot).Distance(Target);
                Vector3d premoved = TheBot.GlobalPosition;
                if (dist > maxDistance)
                {
                    //ClientMovement.Fly = true;
                    //KeepFollowing = true;
                    //ClientMovement.AtPos = true;
                    //startTime = 10000;
                    if (DoZ)
                    {
                        Client.Self.Movement.AtPos = false;
                        //if (startTime + 30000 > Environment.TickCount)
                        {
                            DoZ = false;
                            XYMovement();
                            target0.X = (float) target.X;
                            target0.Y = (float) target.Y;
                            //Client.Objects.OnObjectUpdated -= callback;
                            RegCallback();
                            SetMovement();
                        }

                    }
                    else
                    {
                        if (startTime.AddMinutes(1) > DateTime.Now)
                        {
                            DoZ = true;
                        }
                    }
                    Thread.Sleep(2000);
                    double premovedDist = Vector3d.Distance(premoved, TheBot.GlobalPosition);
                    if (premovedDist < 1)
                    {
                        blockedTimes++;
                        if (blockedTimes < 5) return true;
                        blockedTimes = 0;
                        KeepFollowing = false;
                        TheBot.Debug("Blocked only moved " + premovedDist);
                        return false;
                    }
                    //DoZ = !DoZ;
                    //if (DoZ) ZMovement();
                    //ClientMovement.TurnToward(target);
                    //System.Threading.Thread.Sleep(100);

                    //if (!DoZ) XYMovement();
                    //  ClientMovement.AtPos = false;
                    //   ClientMovement.AtNeg = false;
                    //  ZMovement();
                    //   ClientMovement.SendUpdate(false);
                    return true;
                }
                else
                {
                    DoZ = true;
                    EndFlyto();
                    TheBot.TurnToward(Target.GlobalPosition);
                    //Thread.Sleep(1000); // total 3 seconds
                    return false;
                }
            }
            finally
            {
                if (false)
                {
                    DeRegCallback();
                }
            }
        }

        private void Objects_OnObjectUpdated(object s , TerseObjectUpdateEventArgs e)
        {
            var Client = GetGridClient();
            var ClientMovement = Client.Self.Movement;

            if (startTime == DateTime.MinValue)
            {
                DeRegCallback();
                return;
            }
            if (e.Update.LocalID == Client.Self.LocalID)
            {
                XYMovement();
                ZMovement();
                if (ClientMovement.AtPos || ClientMovement.AtNeg)
                {
                    TheBot.TurnToward(Target.GlobalPosition);
                    //Debug("Flyxy ");
                }
                else if (ClientMovement.UpPos || ClientMovement.UpNeg)
                {
                    TheBot.TurnToward(Target.GlobalPosition);
                    //ClientMovement.SendUpdate(false);
                    //Debug("Fly z ");
                }
                else if (Vector3d.Distance(Target.GlobalPosition, TheBot.GlobalPosition) <= 2.0)
                {
                    EndFlyto();
                    KeepFollowing = false;
                    Debug("At Target");
                }
            }
            if (!KeepFollowing)
            {
                EndFlyto();
                Debug("End Flyto");
            }
        }

        private bool XYMovement()
        {
            bool res = false;

            myPos0.X = (float)myPos.X;
            myPos0.Y = (float)myPos.Y;
            diff = Vector2.Distance(target0, myPos0);
            var Client = GetGridClient();
            var ClientMovement = Client.Self.Movement;

            Vector2 vvel = new Vector2(Client.Self.Velocity.X, Client.Self.Velocity.Y);
            float vel = vvel.Length();
            if (diff >= 10.0)
            {
                ClientMovement.AtPos = true;
                //  ClientMovement.AtNeg = false;
                //if (Math.Abs(diff - olddiff) > 1.5) {
                //  ClientMovement.AtPos = diff < olddiff;
                //  ClientMovement.AtNeg = diff > olddiff;
                //} else if (!ClientMovement.AtPos && !ClientMovement.AtNeg) {
                //  ClientMovement.AtPos = true;
                //  ClientMovement.AtNeg = false;
                //}
                res = true;
            }
            else if (diff >= 2 && vel < 5)
            {
                ClientMovement.AtPos = true;
            }
            else
            {
                ClientMovement.AtPos = false;
                ClientMovement.AtNeg = false;
            }
            saveolddiff = olddiff;
            olddiff = diff;
            return res;
        }

        private void ZMovement()
        {
            var Client = GetGridClient();
            var ClientMovement = Client.Self.Movement;


            ClientMovement.UpPos = false;
            ClientMovement.UpNeg = false;
            float diffz = (float)(target.Z - TheBot.SimPosition.Z);
            if (diffz >= 20.0)
                ClientMovement.UpPos = true;
            else if (diffz <= -20.0)
                ClientMovement.UpNeg = true;
            else if (diffz >= +5.0 && Client.Self.Velocity.Z < +4.0)
                ClientMovement.UpPos = true;
            else if (diffz <= -5.0 && Client.Self.Velocity.Z > -4.0)
                ClientMovement.UpNeg = true;
            else if (diffz >= +2.0 && Client.Self.Velocity.Z < +1.0)
                ClientMovement.UpPos = true;
            else if (diffz <= -2.0 && Client.Self.Velocity.Z > -1.0)
                ClientMovement.UpNeg = true;
        }

        public void EndFlyto()
        {
            startTime = DateTime.MinValue;
            var ClientMovement = GetGridClient().Self.Movement;
            ClientMovement.AtPos = false;
            ClientMovement.AtNeg = false;
            ClientMovement.UpPos = false;
            ClientMovement.UpNeg = false;
            ClientMovement.SendUpdate(false);
            DeRegCallback();
            DoZ = true;
        }

        private void Debug(string x)
        {
            var Client = GetGridClient();
            var ClientMovement = Client.Self.Movement;

            // return; /* remove for debugging */
            TheBot.Debug(x + " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
                    myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
                    ClientMovement.AtPos, ClientMovement.AtNeg, ClientMovement.UpPos, ClientMovement.UpNeg,
                    Client.Self.Velocity.ToString(), Client.Self.AngularVelocity.ToString());
        }

    }
}
