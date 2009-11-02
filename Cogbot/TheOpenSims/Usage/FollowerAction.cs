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
        public static bool UseTeleport = true;
        public static double UseTeleportSteps = 3;
        public static bool UseSimpleTurnTo = true;

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
            int useSimpleFollow = 2;
            int FullPasses = 0;
            while (KeepFollowing)
            {
                if (!Target.IsRegionAttached)
                {
                    Console.WriteLine("" + this + " Not regions attached " + Target);
                    Thread.Sleep(2000);
                    continue;
                }
                double dist = TheBot.Distance(Target);

                if (dist > maxDistance || !CloseEnough())
                {
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
                    if (!Target.IsRegionAttached) continue;
                    if (UseSimpleTurnTo)
                    {
                        while (useSimpleFollow > 0)
                        {
                            useSimpleFollow--;
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
                    }
                    //if (UsePathfinder)
                    if (CloseEnough()) continue;

                    SimObject simO = Target as SimObject;
                    if (simO != null)
                    {
                        if (UseFlight || (simO.Flying || TheBot.IsFlying))
                        {
                            TheBot.Flying = true;
                            TheBot.GetGridClient().ExecuteBotCommand("flyto " + simO.ID, TheBot.Debug);
                        }
                        if (CloseEnough())
                        {
                            useSimpleFollow++;
                            continue;
                        }
                    }

                    if (UsePathfinder)
                        if (TheBot.GotoTarget(Target))
                        {
                            useSimpleFollow++;
                            continue;
                        }

                    if (CloseEnough())
                    {
                        FullPasses = 0;
                        continue;
                    }

                    FullPasses++;
                    if (FullPasses > 1)
                    {
                        if (UseTeleport)
                        {
                            Vector3d vto = Target.GlobalPosition - TheBot.GlobalPosition;
                            vto /= UseTeleportSteps;
                            vto += TheBot.GlobalPosition;
                            vto.Z = Target.GlobalPosition.Z;
                            TheBot.GetGridClient().ExecuteBotCommand(string.Format("teleport {0}", vto.ToRawString()), TheBot.Debug);
                            TheBot.TurnToward(Target);
                            FullPasses = 0;
                        }
                    }
                }  
                else
                {
                    TheBot.TurnToward(Target);
                    Thread.Sleep(1000); // total 3 seconds
                }
            }
        }

        private bool CloseEnough()
        {            
            if (!Target.IsRegionAttached) return true;
            double theBotGlobalPositionZ = TheBot.GlobalPosition.Z - Target.GlobalPosition.Z;
            if (Math.Abs(theBotGlobalPositionZ) > 0.7) return false;
            if (TheBot.Distance(Target) < maxDistance + 2) return true;
            return false;
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