using System;
using System.Threading;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public class FollowerAction : MoveToLocation
    {

        readonly private float maxDistance;
        readonly private Thread FollowThread;
        private bool KeepFollowing = true;
        public static bool UsePathfinder = true;
        public static bool UseFlight = true;

        public FollowerAction(SimAvatar impl, SimPosition position)
            : base(impl,position)
        {
            TheBot = impl;
            maxDistance = 3;// position.GetSizeDistance();
            Target = position;
            FollowThread = new Thread(FollowLoop);
        }

        public override string ToString()
        {
            return "" + TheBot.GetName() + ": Follow " + Target + " -> " + TheBot.DistanceVectorString(Target);
        }

        public override BotNeeds ProposedChange()
        {
            return base.ProposedChange();
        }

        public override void InvokeReal()
        {
            // start if not already started
            if (!FollowThread.IsAlive)
            {
                FollowThread.Start();
                FollowThread.Join();
            }
        }

        public void FollowLoop()
        {
            while (KeepFollowing)
            {
                Thread.Sleep(2000);
                if (!Target.IsRegionAttached)
                {
                    Console.WriteLine("" + this + " Not regions attached " + Target);
                    Thread.Sleep(2000);
                    continue;
                }
                double dist = TheBot.Distance(Target);
                if (dist > maxDistance)
                {
                    int useSimpleFollow = 2;
                    if (!TheBot.Flying)
                    {
                        Vector3 botpos = TheBot.SimPosition;
                        float theBotPathStoreGetGroundLevel = TheBot.PathStore.GetGroundLevel(botpos.X, botpos.Y);
                        if (botpos.Z + 2 > theBotPathStoreGetGroundLevel)
                        {
                            // avoid faling from heights
                            useSimpleFollow = 0;                            
                        }
                    }
                    while (useSimpleFollow-- > 0)
                    {
                        if (!Target.IsRegionAttached)
                        {
                            Console.WriteLine("" + this + " Not regions attached " + Target);
                            Thread.Sleep(2000);
                            continue;
                        }
                        // TheBot.TurnToward(Target);
                        dist = TheBot.Distance(Target);
                        TheBot.SetMoveTarget(Target, maxDistance);
                        Thread.Sleep(3000);
                        if (dist > (TheBot.Distance(Target) + 2))
                        {
                            // Simple Follow might have worked. try again
                            useSimpleFollow = 2;
                            continue;
                        }
                        TheBot.StopMoving();
                    }
                    //if (UsePathfinder)
                    if (!Target.IsRegionAttached || TheBot.Distance(Target) < maxDistance + 2) continue;

                    if (Target is SimAvatar)
                    {
                        SimAvatar av = (SimAvatar)Target;
                        if (UseFlight && av.Flying && av.RegionHandle == TheBot.RegionHandle)
                        {
                            TheBot.Flying = true;
                            TheBot.GetGridClient().ExecuteBotCommand("flyto " + av.ID, TheBot.Debug);
                        }
                    }
                    if (!Target.IsRegionAttached || TheBot.Distance(Target) < maxDistance + 2) continue;
                    if (UsePathfinder)
                        if (TheBot.GotoTarget(Target)) continue;
                }  
                else
                {
                    TheBot.TurnToward(Target);
                    Thread.Sleep(1000); // total 3 seconds
                }
            }
        }


        public override void Abort()
        {
            KeepFollowing = false;
            try
            {
                FollowThread.Abort();
            }
            catch (Exception)
            {
            }
        }

        public override Vector3 GetUsePostion()
        {
            return Target.SimPosition;
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

    }
}