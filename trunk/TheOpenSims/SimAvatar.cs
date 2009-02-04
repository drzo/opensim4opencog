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
            ObjectType.SuperTypes.Add(SimObjectType.GetObjectType("Avatar"));
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
            return this;
        }

        public bool IsSitting()
        {
            //BotClient Client = base.WorldSystem.client;
            if (IsLocal())
            {
                if (Client.Self.Movement.SitOnGround) return true;
                return Client.Self.SittingOn != 0;
            }
            return theAvatar.ParentID != 0;
        }

        public bool IsLocal()
        {
            return Client.Self.AgentID == theAvatar.ID || Client.Self.LocalID == theAvatar.LocalID;
        }

     
        public override string DebugInfo()
        {
            String s = ToString();
            List<SimObject> KnowsAboutList = GetKnownObjects();
            KnowsAboutList.Sort(CompareObjects);
            foreach (SimObject item in KnowsAboutList)
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
                if (IsLocal())
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
            if (Client != null) if (IsLocal()) return Client.Self.SimPosition;
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
               // Debug(CurrentNeeds.ToString());
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

        public void SortActs(List<BotAction> acts)
        {
            acts.Sort(CompareActs);
        }

        int CompareActs(BotAction act1, BotAction act2)
        {
            return (int)(act2.RateIt() - act1.RateIt());
        }
        int CompareObjects(SimObject act1, SimObject act2)
        {
            return (int)(act2.RateIt(this) - act1.RateIt(this));
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
        readonly Random MyRandom = new Random(DateTime.Now.Millisecond);
        // TODO Real Eval routine
        public BotMentalAspect CompareTwo(BotMentalAspect mostInteresting, BotMentalAspect cAspect)
        {
            if ((mostInteresting is SimObject) && (cAspect is SimObject))
            {
                int rate = CompareObjects((SimObject)mostInteresting, (SimObject)cAspect);
                if (rate > 0) return cAspect;
                if (rate < 0) return mostInteresting;
            }
            return (MyRandom.Next(1, 2) == 1) ? mostInteresting : cAspect;
        }

        public void ScanNewObjects()
        {
            ListAsSet<SimObject> objects = GetNearByObjects(100,true);
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
                    UnPhantom = WorldSystem.GetSimObject(WorldSystem.GetPrimitive(sit));
                    UnPhantom.MakeEnterable();
                    Client.Self.Stand();
                }
            }

            float dist = obj.GetSizeDistance();
            if (dist > maxDistance)
            {
                dist = maxDistance;
            }

            Vector3 vector3 = obj.GetUsePosition();

            Debug("Approaching " + vector3 + " dist=" + dist + " " + obj);
            obj.MakeEnterable();


            Thread mover = new Thread(new ThreadStart(delegate()
            {
                try
                {
                    MovementToVector.MoveTo(Client, vector3, dist);
                } catch (Exception) {}
            }));
            mover.Start();
            for (int i = 0; i < 10; i++)
            {
                if (mover.IsAlive)
                {
                    Thread.Sleep(1000);
                    continue;
                }
                else
                {
                    break;
                }
            }
            if (mover.IsAlive)
            {                
                mover.Abort();
            }

            Client.Self.Movement.TurnToward(obj.GetSimPosition());

            if (UnPhantom != null)            
                UnPhantom.RestoreEnterable();

            return Vector3.Distance(GetSimPosition(), obj.GetSimPosition());
        }


        public BotClient GetGridClient()
        {
            //if (Client != null) return Client;
            //BotClient Client = WorldSystem.client;
            //if (theAvatar.ID != Client.Self.AgentID)
            //{
            //    throw new Exception("This avatar " + theAvatar + " has no GridClient");
            //}
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
                Client.Self.AnimationStop(Animations.TALK, true);
                Client.Self.AnimationStart(Animations.TALK, true);
                Client.Talk(InDialogWith + ": " + talkAbout);
                Thread.Sleep(3000);
                Client.Self.AnimationStop(Animations.TALK, true);
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
        BotClient Client;
        internal void SetClient(BotClient Client)
        {
            this.Client = Client;
            WorldSystem = Client.WorldSystem;
            //WorldSystem.AddTracking(this,Client);
        }
    }

}
