using System;
using System.Threading;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public class FollowerAction : BotAction
    {
        public override SimPosition Target { get; set; }
        readonly static BotNeeds ProposedChanges = new BotNeeds(0.0f);
        readonly private float maxDistance;
        readonly private Thread FollowThread;
        private bool KeepFollowing = true;
        public static bool UsePathfinder = true;

        public FollowerAction(SimAvatar impl, SimPosition position)
            : base("" + impl.GetName() + ": Follow " + position + " -> " + impl.DistanceVectorString(position))
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
            return ProposedChanges;
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
                if (!Target.IsRegionAttached())
                {
                    Console.WriteLine("" + this + " Not regions attached " + Target);
                    Thread.Sleep(2000);
                    continue;
                }
                double dist = TheBot.Distance(Target);
                if (dist > maxDistance)
                {
                    int useSimpleFollow = 2;
                    while (useSimpleFollow-- > 0)
                    {
                        if (!Target.IsRegionAttached())
                        {
                            Console.WriteLine(""+this+" Not regions attached " + Target);                            
                            Thread.Sleep(2000);
                            continue;
                        }
                        // TheBot.TurnToward(Target);
                        dist = TheBot.Distance(Target);
                        TheBot.SetMoveTarget(Target, maxDistance);
                        Thread.Sleep(3000);
                        if (dist > (TheBot.Distance(Target)+1))
                        {
                            // Simple Follow might have worked. try again
                            useSimpleFollow = 2;
                            continue;                            
                        }
                        TheBot.StopMoving();
                    }
                    if (UsePathfinder && Target.IsRegionAttached() && TheBot.Distance(Target) > maxDistance + 2)
                        TheBot.GotoTarget(Target);
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
            return Target.GetSimPosition();
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

    }
}