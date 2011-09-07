using System;
using System.Threading;
using MushDLR223.Utilities;
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
            TheBot = (SimControllableAvatar)impl; 
            maxDistance = 3;// position.GetSizeDistance();
            Target = position;
            FollowThread = new Thread(FollowLoop);
            FollowThread.Name = ToString();
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
                Vector3 lastKnown;
                if (!Target.TryGetSimPosition(out lastKnown))
                {
                    DLRConsole.DebugWriteLine("" + this + " Not regions attached " + Target);
                    Thread.Sleep(2000);
                    continue;
                }
                double dist = TheBot.Distance(Target);

                if (dist > maxDistance || !CloseEnough())
                {
                    if (!TheBot.Flying)
                    {
                        Vector3 botpos = TheBot.SimPosition;
                        var pathStore = TheBot.PathStore;
                        if (pathStore == null)
                        {
                            pathStore = TheBot.PathStore;
                            DLRConsole.DebugWriteLine("" + TheBot + " No pathStore attached to self.. might fall!");
                            if (useSimpleFollow < 1) useSimpleFollow = 2;
                        }
                        else
                        {

                            float theBotPathStoreGetGroundLevel = pathStore.GetGroundLevel(botpos.X, botpos.Y);
                            if (botpos.Z + 2 > theBotPathStoreGetGroundLevel)
                            {
                                // avoid faling from heights
                                useSimpleFollow = 0;
                            }
                        }
                    }
                    if (!Target.IsRegionAttached) continue;
                    if (UseSimpleTurnTo)
                    {
                        if (useSimpleFollow > 0) Debug("UseSimpleTurnTo");
                        while (useSimpleFollow > 0)
                        {
                            useSimpleFollow--;
                            // TheBot.TurnToward(Target);
                            dist = TheBot.Distance(Target);
                            TheCBot.SetMoveTarget(Target, maxDistance);
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
                            Debug("UseFlight");
                            TheBot.Flying = true;
                            GetBotClient().ExecuteBotCommand("flyto " + simO.ID, TheBot, Debug);
                        }
                        if (CloseEnough())
                        {
                            useSimpleFollow++;
                            continue;
                        }
                    }

                    if (UsePathfinder)
                    {
                        Debug("UsePathfinder");
                        if (TheCBot.GotoTarget(Target))
                        {
                            UseFlight = false;
                            useSimpleFollow = 1;
                            continue;
                        }
                        UseFlight = !UseFlight;
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
                            Debug("UseTeleport");
                            Vector3d vto = Target.UsePosition.GlobalPosition - TheBot.GlobalPosition;
                            vto /= UseTeleportSteps;
                            vto += TheBot.GlobalPosition;
                            vto.Z = Target.GlobalPosition.Z;
                            var res =
                                GetBotClient().ExecuteBotCommand(
                                    string.Format("teleport {0}", vto.ToRawString()), TheBot, Debug);
                            if (!res.Success) UseTeleport = false; // cant teleport
                            TheBot.TurnToward(Target.GlobalPosition);
                            FullPasses = 0;
                        }
                    }
                    Debug("FullPasses=" + FullPasses);
                }  
                else
                {
                    TheBot.TurnToward(Target.GlobalPosition);
                    Thread.Sleep(1000); // total 3 seconds
                }
            }
        }

        private void Debug(string p, params object[] args)
        {
            TheBot.Debug(p, args);
        }

        private bool CloseEnough()
        {            
            if (!Target.IsRegionAttached) return true;
            double theBotGlobalPositionZ = TheBot.GlobalPosition.Z - Target.GlobalPosition.Z;
            if (Math.Abs(theBotGlobalPositionZ) > 0.7) return false;
            if (((SimMover)TheBot).Distance(Target) < maxDistance + 2) return true;
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