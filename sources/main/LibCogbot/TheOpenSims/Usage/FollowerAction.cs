using System;
using System.Threading;
using cogbot.Actions.Pathfinder;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{

    public class TrialProc
    {
        public MovementProceedure Proc;
        public int Times;
    }

    public class FollowerAction : MoveToLocation
    {

        readonly private float maxDistance;
        readonly private Thread FollowThread;
        private bool KeepFollowing = true;
        private TrialProc proposed = proceedure[0];
        private int procnum = 0;
        private int successes = 0;
        private bool setFlight = false;
        public static bool UseGotoTarget = true;
        public static bool UseAutoPilot = true;
        public static bool UseFlight = true;
        public static bool UseTeleport = true;
        public static bool UseSimpleTurnTo = true;
        [ConfigSetting(Description = "Use No Walking System")]
        public static bool AvoidFalls = true;
        public static bool AvoidFallsWithFlight = true;
        private static readonly TrialProc[] proceedure = {
                                                              UseTimes(MovementProceedure.AStar, 1),
                                                              UseTimes(MovementProceedure.AStar, 3),
                                                              UseTimes(MovementProceedure.TurnToAndWalk, 3),
                                                              UseTimes(MovementProceedure.FlyTo, 1),
                                                              UseTimes(MovementProceedure.Teleport, 1),
                                                              UseTimes(MovementProceedure.AutoPilot, 3),
                                                              UseTimes(MovementProceedure.FlyTo, 3),
                                                              UseTimes(MovementProceedure.TurnToAndWalk, 2),
                                                          };
        private static TrialProc UseTimes(MovementProceedure proc, int numTimes)
        {
            return new TrialProc { Proc = proc, Times = numTimes };
        }

        public FollowerAction(SimAvatar impl, SimPosition position)
            : base(impl,position)
        {
            TheBot = (SimControllableAvatar)impl; 
            maxDistance = 3;// position.GetSizeDistance();
            Target = position;
            FollowThread = new Thread(FollowLoop);
            FollowThread.Name = DebugName;
        }

        public override string ToString()
        {
            return DebugName;
        }

        protected string DebugName
        {
            get { return TheBot.GetName() + ": Follow " + Target + " -> " + TheBot.DistanceVectorString(Target); }
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
                Thread.Sleep(100);
                Vector3 lastKnown;
                if (!Target.TryGetSimPosition(out lastKnown))
                {
                    DLRConsole.DebugWriteLine("" + this + " Not regions attached " + Target);
                    Thread.Sleep(2000);
                    continue;
                }
                
                double dist = TheBot.Distance(Target);

                if (dist < maxDistance || CloseEnough()) continue;

                if (!TheCBot.SalientGoto(Target))
                {
                    SwitchSalientProc();
                    continue;
                }
                if (!TheCBot.WaitUntilPosSimple(Target.GlobalPosition, Target.GetSizeDistance(), 3, true))
                {
                    SwitchSalientProc();
                } else
                {
                    SucceedSalientProc();
                }
            }
        }

        private void SucceedSalientProc()
        {
            successes++;
            if (successes >= proposed.Times)
            {
                SwitchSalientProc();
                successes = 0;
            }
        }

        private void SwitchSalientProc()
        {
            procnum++;
            if (procnum >= proceedure.Length) procnum = 0;
            proposed = proceedure[procnum];
            SimAvatarClient TheCBot = (SimAvatarClient)this.TheCBot;
            if (setFlight)
            {
                setFlight = false;
                TheCBot.Flying = false;
            }
            switch (proposed.Proc)
            {
                case MovementProceedure.AutoPilot:
                    if (!UseAutoPilot)
                    {
                        SwitchSalientProc();
                        break;
                    }
                    CheckAvoidFalls(TheCBot);
                    break;
                case MovementProceedure.AStar:
                    if (!UseGotoTarget)
                    {
                        SwitchSalientProc();
                    }
                    break;
                case MovementProceedure.TurnToAndWalk:
                    if (!UseSimpleTurnTo)
                    {
                        SwitchSalientProc();
                        break;
                    }
                    CheckAvoidFalls(TheCBot);
                    break;
                case MovementProceedure.FlyTo:
                    if (!UseFlight)
                    {
                        SwitchSalientProc();
                    }
                    break;
                case MovementProceedure.Teleport:
                    if (!UseTeleport /*|| SimAvatarClient.GotoUseTeleportFallback || SimAvatarClient.MoveUseTeleportFallback*/)
                    {
                        SwitchSalientProc();
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException("proposed.Proc=" + proposed.Proc);
            }
            TheCBot.SalientMovementProceedure = proposed.Proc;
        }

        private void CheckAvoidFalls(SimAvatarClient TheCBot)
        {
            if (AvoidFalls && !TheCBot.Flying)
            {
                if (!AvoidFallsWithFlight)
                {
                    SwitchSalientProc();
                    return;
                }
                TheCBot.Flying = true;
                setFlight = true;
            }
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