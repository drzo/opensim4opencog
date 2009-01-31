using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using DotLisp;
using System.Reflection;
using cogbot.Listeners;
using System.Threading;
//Complex outcomes may be a result of simple causes, or they may just be complex by nature. 
//Those complexities that turn out to have simple causes can be simulated and studied, 
//thus increasing our knowledge without needing direct observation.
namespace cogbot.TheOpenSims
{

    public class SimAvatar : SimObject
    {

        public Thread avatarThinkerThread = null;
        public Thread avatarHeartbeatThread = null;

        public Avatar theAvatar
        {
            get { return (Avatar)thePrim; }
        }

        public SimAvatar InDialogWith = null;

        public BotNeeds CurrentNeeds;

        // things the bot cycles through mentally
        public ListAsSet<SimObject> KnowsAboutList = new ListAsSet<SimObject>();

        // which will result in 
        public ListAsSet<BotAction> KnowsActList = new ListAsSet<BotAction>();

        // which will be skewed with how much one bot like a Mental Aspect
        public Dictionary<BotMentalAspect, int> AspectEnjoyment = new Dictionary<BotMentalAspect, int>();

        //notice this also stores object types that pleases the bot as well as people
        // (so how much one bot likes another avatar is sotred here as well)

        // Actions tbe bot might do next cycle.
        ListAsSet<BotAction> AllPossibleActions = new ListAsSet<BotAction>();

        // Actions observed
        ListAsSet<BotAction> LearnedPossibleActions = new ListAsSet<BotAction>();

        // Action template stubs 
        ListAsSet<SimTypeUsage> LearnedPossibleUsages = new ListAsSet<SimTypeUsage>();

        // assuptions about stubs
        public Dictionary<SimObjectType, BotNeeds> Assumptions = new Dictionary<SimObjectType, BotNeeds>();

        // Current action 
        public BotAction CurrentAction = null;



        public SimAvatar(Avatar slAvatar, WorldObjects objectSystem)
            : base(slAvatar.Name, slAvatar, objectSystem)
        {
            //ObjectType = BotRegionModel.BotWorld.GetObjectType("Avatar");
            CurrentNeeds = new BotNeeds(90.0F);
            AspectName = slAvatar.Name;
            avatarHeartbeatThread = new Thread(new ThreadStart(Aging));
            avatarHeartbeatThread.Start();
            MakeEnterable();
        }

        public override bool RestoreEnterable()
        {
            return false;// base.RestoreEnterable();
        }

        public override bool IsRoot()
        {
            return true;
        }
        public override SimObject GetParent()
        {
            return null;
        }

        public bool IsSitting()
        {
            BotClient Client = base.WorldSystem.client;
            if (theAvatar.ID != Client.Self.AgentID)
            {
                if (Client.Self.Movement.SitOnGround) return true;
                return Client.Self.SittingOn != 0;
            }
            return theAvatar.ParentID != 0;
        }

     
        public override string DebugInfo()
        {
            String s = ToString();
            foreach (SimObject item in GetKnownObjects())
            {
                if (item is SimAvatar) continue;
                s += "\n   " + item.DebugInfo();
            }
            return "\n" + s;
        }

        public void StartThinking()
        {
            if (avatarThinkerThread == null)
            {
                avatarThinkerThread = new Thread(new ThreadStart(Think));
                if (theAvatar.LocalID == WorldSystem.client.Self.LocalID)
                {
                    // only think for ourselves
                    avatarThinkerThread.Start();
                }
            }
            if (!avatarThinkerThread.IsAlive) avatarThinkerThread.Resume();
        }

        public bool IsThinking()
        {
            return (avatarThinkerThread != null);
        }
        internal void PauseThinking()
        {
            if (avatarThinkerThread != null)
            {
                try
                {
                    // avatarThinkerThread.Suspend();
                    avatarThinkerThread.Abort();
                    avatarThinkerThread = null;
                }
                catch (Exception)
                {
                }
            }
        }

        public override Vector3 GetSimPosition()
        {
            if (theAvatar.LocalID == WorldSystem.client.Self.LocalID) return WorldSystem.client.Self.SimPosition;
            return base.GetSimPosition();
        }

        public void Think()
        {
            while (true)
            {
                try
                {
                    Thread.Sleep(3000);
                    ThinkOnce();
                }
                catch (Exception e)
                {
                    Debug(e.ToString());
                }
            }
        }
        public void Aging()
        {
            while (true)
            {
                CurrentNeeds.AddFrom(SimObjectType.GetObjectType("OnMinuteTimer").GetUsageActual("OnMinuteTimer"));
                CurrentNeeds.SetRange(0.0F, 100.0F);
                Thread.Sleep(60000); // one minute
                Debug(CurrentNeeds.ToString());
            }
        }


        public void ThinkOnce()
        {
            ScanNewObjects();

            Thread.Sleep(2000);
            CurrentAction = GetNextAction();
            if (CurrentAction != null)
            {
                UseAspect(CurrentAction);
            }
        }

        private BotAction GetNextAction()
        {
            BotAction act = CurrentAction;

            List<BotAction> acts = GetPossibleActions();

            if (acts.Count > 0)
            {
                act = BestAct(acts);
                acts.Remove(act);
            }
            return act;
        }

        public BotAction BestAct(List<BotAction> acts)
        {
            if (acts.Count == 0) return null;
            BotAction bestAct = acts[0];
            if (acts.Count == 1) return bestAct;
            float bestRate = bestAct.RateIt();
            foreach (BotAction b in acts)
            {
                float brate = b.RateIt();
                if (brate > bestRate)
                {
                    bestAct = b;
                    bestRate = brate;
                }
            }
            return bestAct;
        }

        public ListAsSet<BotAction> GetPossibleActions()
        {
            if (AllPossibleActions.Count < 2)
            {
                AllPossibleActions = NewPossibleActions();
            }
            return AllPossibleActions;
        }

        private ListAsSet<BotAction> NewPossibleActions()
        {
            List<SimObject> knowns = GetKnownObjects();

            ListAsSet<BotAction> acts = new ListAsSet<BotAction>();
            foreach (BotAction obj in LearnedPossibleActions)
            {
                acts.Add(obj);
            }

            foreach (SimObject obj in knowns)
            {
                foreach (SimObjectUsage objuse in obj.GetUsages())
                {
                    acts.Add(new BotObjectAction(this, objuse));
                    foreach (SimTypeUsage puse in LearnedPossibleUsages)
                    {
                        //acts.Add( new BotObjectAction(this, puse, obj));
                    }

                }
            }
            return acts;
        }

        public List<SimObject> GetKnownObjects()
        {
            return KnowsAboutList;
        }

        public void UseAspect(BotMentalAspect someAspect)
        {
            if (someAspect is BotAction)
            {
                BotAction act = (BotAction)someAspect;
                act.InvokeReal();
                return;
            }
            if (InDialogWith != null)
            {
                TalkTo(InDialogWith, someAspect);
                return;
            }

            if (someAspect is SimAvatar)
            {
                // SocialTo("talk",(SimAvatar)someAspect);
                return;
            }
            //UseObject((SimObject)someAspect);

        }

        public SimObject GetNextInterestingObject()
        {
            SimObject mostInteresting = null;
            KnowsAboutList.Remove(this);
            int count = KnowsAboutList.Count - 2;
            foreach (BotMentalAspect cAspect in KnowsAboutList)
            {
                if (cAspect is SimObject)
                {
                    if (mostInteresting == null)
                    {
                        mostInteresting = (SimObject)cAspect;
                        // break;
                    }
                    else
                    {
                        mostInteresting = (SimObject)CompareTwo(mostInteresting, cAspect);
                    }
                    count--;
                    if (count < 0) break;
                }
            }
            KnowsAboutList.Remove(mostInteresting);
            KnowsAboutList.AddTo(mostInteresting);
            return mostInteresting;
        }
        readonly Random MyRandom = new Random();
        // TODO Real Eval routine
        public BotMentalAspect CompareTwo(BotMentalAspect mostInteresting, BotMentalAspect cAspect)
        {
            if ((mostInteresting is SimObject) && (cAspect is SimObject))
            {
                return CompareObjects((SimObject)mostInteresting, (SimObject)cAspect);
            }
            return (MyRandom.Next(1, 2) == 1) ? mostInteresting : cAspect;
        }

        private BotMentalAspect CompareObjects(SimObject simObject, SimObject simObject_2)
        {
            if (simObject == simObject_2) return simObject;
            float a1 = simObject.RateIt(CurrentNeeds, this);
            float a2 = simObject_2.RateIt(CurrentNeeds, this);
            if (a2 > a1) return simObject_2;
            return simObject;
        }

        public void ScanNewObjects()
        {
            ListAsSet<SimObject> objects = GetNearByObjects(20);
            lock (objects) foreach (SimObject obj in objects)
                {
                    if (obj.IsRoot() && obj!=this)
                        lock (KnowsAboutList) if (!KnowsAboutList.Contains(obj))
                            {
                                if (KnowsAboutList.Count < 2) KnowsAboutList.AddTo(obj);
                                else
                                    KnowsAboutList.Insert(1, obj);
                            }
                }

        }

        // Avatars approach distance
        public override float GetSizeDistance()
        {
            return 3f;
        }

        public float Approach(SimObject obj, float maxDistance)
        {
            SimObject UnPhantom = null;
            BotClient Client = GetGridClient();
            // stand up first
            if (Client.Self.Movement.SitOnGround)
            {
                Client.Self.Stand();
            }
            else
            {
                uint sit = Client.Self.SittingOn;
                if (sit != 0)
                {
                    UnPhantom = GetBotWorld().GetSimObject(WorldSystem.GetPrimitive(sit));
                    UnPhantom.MakeEnterable();
                    Client.Self.Stand();
                }
            }

            float dist = obj.GetSizeDistance();

            Vector3 vector3 = obj.GetUsePosition();

            Debug("Approaching " + vector3 + " dist=" + dist + " " + obj);
            obj.MakeEnterable();

            MovementToVector.MoveTo(this, vector3, dist);
            Client.Self.Movement.TurnToward(obj.GetSimPosition());

            Thread.Sleep(2000);

            if (UnPhantom != null)
            {
                UnPhantom.RestoreEnterable();
            }
            return dist;
        }

        private BotRegionModel GetBotWorld()
        {
            return BotRegionModel.BotWorld;
        }

        public BotClient GetGridClient()
        {
            BotClient Client = base.WorldSystem.client;
            if (theAvatar.ID != Client.Self.AgentID)
            {
                throw new Exception("This avatar " + theAvatar + " has no GridClient");
            }
            return Client;
        }

        public void TalkTo(SimAvatar avatar, String talkAbout)
        {
            SimAvatar avatarWasInDialogWith = avatar.InDialogWith;
            SimAvatar wasInDialogWith = InDialogWith;
            try
            {
                InDialogWith = avatar;
                BotClient Client = GetGridClient();
                Client.Self.Movement.TurnToward(InDialogWith.GetSimPosition());
                Client.Talk(InDialogWith + ": " + talkAbout);
                Thread.Sleep(3000);
            }
            finally
            {
                InDialogWith = wasInDialogWith;
                avatar.InDialogWith = avatarWasInDialogWith;
            }
        }

        public void TalkTo(SimAvatar avatar, BotMentalAspect talkAbout)
        {
            // TODO find a better text represantation (a thought bubble maybe?)
            TalkTo(avatar, "" + talkAbout);
        }

        public void Debug(string p)
        {
            Console.WriteLine("++" + theAvatar.Name
                + ": " + p);
        }

        public void Eat(SimObject target)
        {
            Debug("!!! EAT " + target);
        }

        public ThreadStart WithSitOn(SimObject obj, ThreadStart closure)
        {
            return new ThreadStart(delegate()
            {
                Primitive targetPrim = obj.thePrim;
                BotClient Client = GetGridClient();
                Client.Self.RequestSit(targetPrim.ID, Vector3.Zero);
                Client.Self.Sit();
                closure.Invoke();
                Client.Self.Stand();
            });
        }

        public ThreadStart WithGrabAt(SimObject obj, ThreadStart closure)
        {
            return new ThreadStart(delegate()
            {
                Primitive targetPrim = obj.thePrim;
                uint objectLocalID = targetPrim.LocalID;
                BotClient Client = GetGridClient();

                try
                {
                    Client.Self.Grab(objectLocalID);
                    closure.Invoke();
                }
                finally
                {
                    Client.Self.DeGrab(objectLocalID);
                }
            });
        }

        public ThreadStart WithAnim(UUID anim, ThreadStart closure)
        {
            BotClient Client = GetGridClient();
            return new ThreadStart(delegate()
            {
                AnimThread animThread = new AnimThread(Client, anim);
                try
                {
                    animThread.Start();
                    closure.Invoke();
                }
                finally
                {
                    animThread.Stop();
                }
            });
        }

        public UUID FindAnimUUID(string use)
        {
            return WorldObjects.GetAnimationUUID(use);
        }

        public void ExecuteLisp(SimObjectUsage botObjectAction, String lisp)
        {
            if (lisp == null) return;
            BotClient Client = GetGridClient();
            if (!String.IsNullOrEmpty(lisp))
            {
                Client.lispTaskInterperter.Intern("TheBot", this);
                Client.lispTaskInterperter.Intern("Target", botObjectAction.Target);
                Client.lispTaskInterperter.Intern("botObjectAction", botObjectAction);
                Client.evalLispString((String)lisp);
            }
        }


        public string GetName()
        {
            return theAvatar.Name;
        }

        public override string ToString()
        {
            return GetName();
        }
    }


    public class MovementToVector
    {
        public static bool MoveTo(SimAvatar bc, Vector3 targ, float dist)
        {
            MovementToVector mtv = new MovementToVector(bc, targ,dist);
            mtv.Goto();
            if (mtv.GetDistance() > dist) return false;
            return true;
        }
        SimAvatar theAvatar;
        Vector3 Destination;
        Vector3 LastPosition;
        BotClient Client;
        //private AutoResetEvent Ready = new AutoResetEvent(false);
        Boolean justStopped = false;
        float lastDistance = Single.MaxValue;
        int autoPilotsRemaining = 6;

        float followDist = 2.0F;
        public MovementToVector(SimAvatar bc, Vector3 targ,float fd)
        {
            theAvatar = bc;
            Client = bc.GetGridClient();
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


        void tracker()
        {
            float curDist = GetDistance();
            bool UseAutoPilot = false;
            float traveled = 10f;
            List<SimObject> madePhantom = new List<SimObject>();
            while (curDist > followDist && autoPilotsRemaining > 0)
            {
                LastPosition = Client.Self.SimPosition;
                if (UseAutoPilot)
                {
                    autoPilotsRemaining--;
                    if (autoPilotsRemaining > 0)
                    {
                        Console.WriteLine("AutoPilot due to traveled=" + traveled);
                        Client.Self.AutoPilot(Destination.X, Destination.Y, Destination.Z);
                        madePhantom = theAvatar.GetNearByObjects(2);
                        if (madePhantom.Count > 0)
                        {
                            foreach (SimObject obj in madePhantom)
                            {
                                obj.MakeEnterable();
                            }
                        }
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
                if (madePhantom.Count > 0)
                {
                    foreach (SimObject obj in madePhantom)
                    {
                        obj.RestoreEnterable();
                    }
                    madePhantom.Clear();
                }
            }
            Client.Self.AutoPilotCancel();
        }

        private void UpdateHeading()
        {
            Random somthing = new Random(Environment.TickCount);// We do stuff randomly here
            float curDist = GetDistance();
            if (lastDistance <= curDist)
            {
                //    StopMoving();
                //    followDist = curDist + 1.0F;
            }
            lastDistance = curDist;

            if (curDist > followDist)
            {

                //Client.Self.Movement.SendUpdate();
                if (curDist < (followDist * 1.25))
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(125);
                    Client.Self.Movement.Stop = true;
                    Client.Self.Movement.AtPos = false;
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
