using System;
using System.Threading;
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
        bool DoZ = false;
        

        public override void Abort()
        {
            BotClient Client = TheBot.GetGridClient();

            if (CallbackReged)
            {
                Client.Objects.OnObjectUpdated -= callback;
                CallbackReged = false;
            }
            TheBot.StopMoving();
            EndFlyto();
            Client.Self.Movement.Fly = true;
            if (FollowThread.IsAlive)
            {
                FollowThread.Abort();
            }

        }

        public override void InvokeReal()
        {
            target0.X = (float)target.X;
            target0.Y = (float)target.Y;

            BotClient Client = TheBot.GetGridClient();
            //Client.Objects.OnObjectUpdated -= callback;
            if (!CallbackReged)
            {
                Client.Objects.OnObjectUpdated += callback;
                CallbackReged = true;
            }
            SetMovement();
            //System.Threading.Thread.Sleep(100);

            //XYMovement();
            //ZMovement();
            //Client.Self.Movement.SendUpdate(false);

            // start if not already started
            if (!FollowThread.IsAlive)
            {
                try
                {
                    FollowThread.Start();
                    FollowThread.Join();
                }
                finally
                {
                    if (CallbackReged)
                    {
                        Client.Objects.OnObjectUpdated -= callback;
                        CallbackReged = false;
                    }
                }
            }
        }

        private void SetMovement()
        {

            BotClient Client = TheBot.GetGridClient();
            if (!CallbackReged)
            {
                Client.Objects.OnObjectUpdated += callback;
                CallbackReged = true;
            }
            startTime = Environment.TickCount;
            Client.Self.Movement.Fly = true;
            Client.Self.Movement.AtPos = true;
            Client.Self.Movement.AtNeg = false;
            ZMovement();
            // Client.Self.Movement
            TheBot.TurnToward(Target);

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
        int startTime = 0;
        //   int duration = 10000;
        readonly ObjectManager.ObjectUpdatedCallback callback;

        public FlyToAction(SimAvatar impl, SimPosition position)
            : base(impl, position)
        {
            // set in base
            //TheBot = impl;
            //Target = position;
            maxDistance = position.GetSizeDistance();
            FollowThread = new Thread(FollowLoop);
            callback = Objects_OnObjectUpdated;
        }

        public void FollowLoop()
        {
            try
            {
                while (KeepFollowing)
                {
                    //DoZ = !DoZ;
                    //if (DoZ) SetMovement();
                    Thread.Sleep(2000);
                    // startTime = 10000;
                    if (!Target.IsRegionAttached)
                    {
                        EndFlyto();
                        Console.WriteLine("" + this + " Not regions attached " + Target);
                        Thread.Sleep(2000);
                        continue;
                    }
                    BotClient Client = TheBot.GetGridClient();
                    double dist = TheBot.Distance(Target);
                    if (dist > maxDistance)
                    {
                        //Client.Self.Movement.Fly = true;
                        //KeepFollowing = true;
                        //Client.Self.Movement.AtPos = true;
                        //startTime = 10000;
                        if (DoZ)
                        {
                            Client.Self.Movement.AtPos = false;
                            //if (startTime + 30000 > Environment.TickCount)
                            {
                                DoZ = false;
                                XYMovement();
                                target0.X = (float)target.X;
                                target0.Y = (float)target.Y;
                                //Client.Objects.OnObjectUpdated -= callback;
                                if (!CallbackReged)
                                {
                                    Client.Objects.OnObjectUpdated += callback;
                                    CallbackReged = true;
                                }
                                SetMovement();
                            }

                        }
                        else
                        {
                            if (startTime + 60000 > Environment.TickCount)
                            {
                                DoZ = true;
                            }
                        }
                       
                        //DoZ = !DoZ;
                        //if (DoZ) ZMovement();
                        //Client.Self.Movement.TurnToward(target);
                        //System.Threading.Thread.Sleep(100);

                        //if (!DoZ) XYMovement();
                        //  Client.Self.Movement.AtPos = false;
                        //   Client.Self.Movement.AtNeg = false;
                        //  ZMovement();
                        //   Client.Self.Movement.SendUpdate(false);

                    }
                    else
                    {
                        DoZ = true;
                        EndFlyto();
                        TheBot.TurnToward(Target);
                        //Thread.Sleep(1000); // total 3 seconds
                        //KeepFollowing = false;
                    }
                }
            }
            finally
            {
                EndFlyto();
                BotClient Client = TheBot.GetGridClient();
                if (CallbackReged)
                {
                    Client.Objects.OnObjectUpdated -= callback;
                    CallbackReged = false;
                }
            }
        }

        private void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            BotClient Client = TheBot.GetGridClient();
            if (startTime == 0)
            {
                if (CallbackReged)
                {
                    Client.Objects.OnObjectUpdated -= callback;
                    CallbackReged = false;
                }
                return;
            }
            if (update.LocalID == Client.Self.LocalID)
            {
                XYMovement();
                ZMovement();
                if (Client.Self.Movement.AtPos || Client.Self.Movement.AtNeg)
                {
                    TheBot.TurnToward(Target);
                    Debug("Flyxy ");
                }
                else if (Client.Self.Movement.UpPos || Client.Self.Movement.UpNeg)
                {
                    TheBot.TurnToward(Target);
                    //Client.Self.Movement.SendUpdate(false);
                    Debug("Fly z ");
                }
                else if (Vector3d.Distance(Target.GlobalPosition, TheBot.GlobalPosition) <= 2.0)
                {
                    EndFlyto();
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
            BotClient Client = TheBot.GetGridClient();
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
            BotClient Client = TheBot.GetGridClient();
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            float diffz = (float)(target.Z - TheBot.SimPosition.Z);
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
            BotClient Client = TheBot.GetGridClient();
            Client.Self.Movement.AtPos = false;
            Client.Self.Movement.AtNeg = false;
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            Client.Self.Movement.SendUpdate(false);
            if (CallbackReged)
            {
                Client.Objects.OnObjectUpdated -= callback;
                CallbackReged = false;
            }
            DoZ = true;
        }

        private void Debug(string x)
        {
            BotClient Client = TheBot.GetGridClient();
            // return; /* remove for debugging */
            TheBot.Debug(x + " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
                    myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
                    Client.Self.Movement.AtPos, Client.Self.Movement.AtNeg, Client.Self.Movement.UpPos, Client.Self.Movement.UpNeg,
                    Client.Self.Velocity.ToString(), Client.Self.AngularVelocity.ToString());
        }

    }
}
