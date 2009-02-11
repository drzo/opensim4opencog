using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using DotLisp;
using System.Reflection;
using cogbot.Listeners;
using System.Threading;
using System.Windows.Forms;
using cogbot.TheOpenSims.Navigation;
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

        readonly public BotNeeds CurrentNeeds;
        public float SightRange = 30.0f;


        // things the bot cycles through mentally
        public List<SimObject> KnownSimObjects = new List<SimObject>();

        public List<SimObject> GetKnownObjects()
        {
            ScanNewObjects(3, SightRange);
            SortByDistance(KnownSimObjects);
            return KnownSimObjects;
        }

        // which will result in 
        public List<BotAction> KnownBotActions = new List<BotAction>();

        // which will be skewed with how much one bot like a Mental Aspect
        public Dictionary<BotMentalAspect, int> AspectEnjoyment = new Dictionary<BotMentalAspect, int>();

        //notice this also stores object types that pleases the bot as well as people
        // (so how much one bot likes another avatar is sotred here as well)

        // Actions tbe bot might do next cycle.
        List<BotAction> TodoBotActions = new List<BotAction>();

        // Actions observed
        List<BotAction> ObservedBotActions = new List<BotAction>();

        // Action template stubs 
        List<SimTypeUsage> KnownTypeUsages = new List<SimTypeUsage>();


        // assuptions about stubs
        public Dictionary<SimObjectType, BotNeeds> Assumptions = new Dictionary<SimObjectType, BotNeeds>();

        // Current action 
        public BotAction CurrentAction = null;



        public SimAvatar(Avatar slAvatar, WorldObjects objectSystem)
            : base(slAvatar.Name, slAvatar, objectSystem)
        {
            ObjectType.SuperType.Add(SimTypeSystem.GetObjectType("Avatar"));
            CurrentNeeds = new BotNeeds(90.0F);
            AspectName = slAvatar.Name;
            avatarHeartbeatThread = new Thread(new ThreadStart(Aging));
            avatarHeartbeatThread.Name = "AvatarHeartbeatThread for " + Client;
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
                AgentManager ClientSelf = Client.Self;
                AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                if (ClientMovement.SitOnGround) return true;
                return ClientSelf.SittingOn != 0;
            }
            return theAvatar.ParentID != 0;
        }

        public bool IsLocal()
        {
            AgentManager ClientSelf = Client.Self;
            return ClientSelf.AgentID == theAvatar.ID || ClientSelf.LocalID == theAvatar.LocalID;
        }


        public override string DebugInfo()
        {
            String s = ToString();
            List<SimObject> KnowsAboutList = GetKnownObjects();
            KnowsAboutList.Sort(CompareObjects);
            int show = 10;
            s += "\nKnowsAboutList: " + KnowsAboutList.Count;
            foreach (SimObject item in KnowsAboutList)
            {
                show--;
                if (show < 0) break;
                //if (item is SimAvatar) continue;
                s += "\n   " + item + " " + DistanceVectorString(item);
            }
            show = 10;
            KnownTypeUsages.Sort(CompareUsage);
            s += "\nKnownTypeUsages: " + KnownTypeUsages.Count;
            foreach (SimTypeUsage item in KnownTypeUsages)
            {
                show--;
                if (show < 0) break;
                //if (item is SimAvatar) continue;
                s += "\n   " + item + " " + item.RateIt(CurrentNeeds);
            }
            return "\n" + s;
        }

        public void StartThinking()
        {
            if (avatarThinkerThread == null)
            {
                avatarThinkerThread = new Thread(new ThreadStart(Think));
                avatarThinkerThread.Name = "AvatarThinkerThread for " + Client;
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
                CurrentNeeds.AddFrom(SimTypeSystem.GetObjectType("OnMinuteTimer").GetUsageActual("OnMinuteTimer"));
                CurrentNeeds.SetRange(0.0F, 100.0F);
                Thread.Sleep(60000); // one minute
                // Debug(CurrentNeeds.ToString());
            }
        }


        public void ThinkOnce()
        {
            ScanNewObjects(2, SightRange);

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

            IList<SimUsage> acts = (IList<SimUsage>)GetPossibleActions();

            if (acts.Count > 0)
            {
                act = (BotAction)FindBestUsage(acts);
                acts.Remove(act);
            }
            return act;
        }

        public SimUsage FindBestUsage(IEnumerable<SimUsage> acts)
        {
            SimUsage bestAct = null;
            if (acts != null)
            {
                IEnumerator<SimUsage> enumer = acts.GetEnumerator();
                float bestRate = float.MinValue;
                while (enumer.MoveNext())
                {
                    SimUsage b = enumer.Current;
                    float brate = b.RateIt(CurrentNeeds);
                    if (brate > bestRate)
                    {
                        bestAct = b;
                        bestRate = brate;
                    }
                }
            }
            return bestAct;
        }

        //public void AddGrass(Simulator simulator, Vector3 scale, Quaternion rotation, Vector3 position, Grass grassType, UUID groupOwner)
        //{
        //}
        //public void AddPrim(Simulator simulator, Primitive.ConstructionData prim, UUID groupID, Vector3 position, Vector3 scale, Quaternion rotation)
        //{
        //}
        //public void AddTree(Simulator simulator, Vector3 scale, Quaternion rotation, Vector3 position, Tree treeType, UUID groupOwner, bool newTree)
        //{
        //}
        //public void AttachObject(Simulator simulator, uint localID, AttachmentPoint attachPoint, Quaternion rotation)
        //{
        //}

        //public static Primitive.ConstructionData BuildBasicShape(PrimType type)
        //{
        //}

        //public SimObject RezObjectType(SimObject copyOf)
        //{
        //    string treeName = args[0].Trim(new char[] { ' ' });
        //    Tree tree = (Tree)Enum.Parse(typeof(Tree), treeName);

        //    Vector3 treePosition = ClientSelf.SimPosition;
        //    treePosition.Z += 3.0f;

        //    Client.Objects.AddTree(Client.Network.CurrentSim, new Vector3(0.5f, 0.5f, 0.5f),
        //        Quaternion.Identity, treePosition, tree, Client.GroupID, false);

        //    //ClientSelf.
        //    return copyOf;
        //}

        //public void SortActs(List<SimUsage> acts)
        //{
        //    acts.Sort(CompareUsage);
        //}

        public int CompareUsage(SimUsage act1, SimUsage act2)
        {
            return (int)(act2.RateIt(CurrentNeeds) - act1.RateIt(CurrentNeeds));
        }

        public int CompareObjects(SimObject act1, SimObject act2)
        {
            return (int)(act2.RateIt(CurrentNeeds) - act1.RateIt(CurrentNeeds));
        }

        public IList<BotAction> GetPossibleActions()
        {
            if (TodoBotActions.Count < 2)
            {
                TodoBotActions = NewPossibleActions();
            }
            return TodoBotActions;
        }

        private List<BotAction> NewPossibleActions()
        {
            List<SimObject> knowns = GetKnownObjects();
            List<BotAction> acts = new List<BotAction>();
            foreach (BotAction obj in ObservedBotActions)
            {
                acts.Add(obj);
            }

            foreach (SimObject obj in knowns)
            {
                foreach (SimObjectUsage objuse in obj.GetUsages())
                {
                    acts.Add(new BotObjectAction(this, objuse));
                    foreach (SimTypeUsage puse in KnownTypeUsages)
                    {
                        //acts.Add( new BotObjectAction(this, puse, obj));
                    }

                }
            }
            return acts;
        }

        internal void DoBestUse(SimObject someObject)
        {
            SimTypeUsage use = someObject.GetBestUse(CurrentNeeds);
            if (use == null)
            {
                float closeness = Approach(someObject, 2);
                AgentManager ClientSelf = Client.Self;
                ClientSelf.Touch(someObject.thePrim.LocalID);
                if (closeness < 3)
                {
                    ClientSelf.RequestSit(someObject.thePrim.ID, Vector3.Zero);
                    ClientSelf.Sit();
                }
                return;
            }
            UseAspect(new BotObjectAction(this, new SimObjectUsage(use, someObject)));
            return;
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

            if (someAspect is SimObject)
            {
                SimObject someObject = (SimObject)someAspect;
                DoBestUse(someObject);
            }

        }

        List<SimObject> InterestingObjects = new List<SimObject>();

        public SimObject GetNextInterestingObject()
        {
            SimObject mostInteresting = null;
            if (InterestingObjects.Count < 2)
            {
                InterestingObjects = GetKnownObjects();
                InterestingObjects.Remove(this);
            }
            int count = InterestingObjects.Count - 2;
            foreach (BotMentalAspect cAspect in InterestingObjects)
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
            InterestingObjects.Remove(mostInteresting);
            InterestingObjects.Add(mostInteresting);
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

        public void ScanNewObjects(int minimum, float sightRange)
        {
            List<SimObject> objects = GetNearByObjects(sightRange, true);
            lock (objects)
            {
                foreach (SimObject obj in objects)
                {
                    if (obj != this)
                        if (obj.IsRoot() || obj.IsTyped())
                        {
                            if (!KnownSimObjects.Contains(obj))
                            {
                                KnownSimObjects.Add(obj);
                                IList<SimTypeUsage> uses = obj.GetTypeUsages();
                                foreach (SimTypeUsage use in uses)
                                {
                                    if (!KnownTypeUsages.Contains(use))
                                    {
                                        KnownTypeUsages.Add(use);
                                    }
                                }
                            }
                        }
                }
            }
            if (KnownSimObjects.Count < minimum)
            {
                if (sightRange < 255)
                    ScanNewObjects(minimum, sightRange + 10);
            }
        }

        // Avatars approach distance
        public override float GetSizeDistance()
        {
            return 2f;
        }

        float ApproachDistance;
        public SimPosition ApproachTarget;
        Thread ApproachThread = null;

        public float Approach(SimObject obj, float maxDistance)
        {
            BotClient Client = GetGridClient();
            // stand up first
            SimObject UnPhantom = StandUp();
            // make sure it not going somewhere
            // set the new target

            Vector3 vector3 = obj.GetUsePosition();
            string str = "Approaching " + obj + " " + DistanceVectorString(obj) + " to get " + ApproachDistance;
            Debug(str);
            obj.MakeEnterable();
            SetFollow(obj);
            //ApproachDistance = obj.GetSizeDistance() +maxDistance;
            // 16 seconds of travel permitted
            for (int i = 0; i < 15; i++)
            {

                if (Distance(obj) > ApproachDistance)
                {
                    Thread.Sleep(1000);
                    continue;
                }
                else
                {
                    break;
                }
            }

            // StopMoving();
            if (UnPhantom != null)
                UnPhantom.RestoreEnterable();
            AgentManager.AgentMovement ClientMovement = Client.Self.Movement;
            ClientMovement.TurnToward(obj.GetUsePosition());
            return Distance(obj);
        }

        public SimObject StandUp()
        {
            SimObject UnPhantom = null;
            AgentManager ClientSelf = Client.Self;
            AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
            if (ClientMovement.SitOnGround)
            {
                ClientSelf.Stand();
            }
            else
            {
                uint sit = ClientSelf.SittingOn;
                if (sit != 0)
                {
                    UnPhantom = WorldSystem.GetSimObject(WorldSystem.GetPrimitive(sit));
                    UnPhantom.MakeEnterable();
                    ClientSelf.Stand();
                }
            }
            return UnPhantom;
        }


        public BotClient GetGridClient()
        {
            //if (Client != null) return Client;
            //BotClient Client = WorldSystem.client;
            //if (theAvatar.ID != ClientSelf.AgentID)
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
                AgentManager ClientSelf = Client.Self;
                AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                ClientMovement.TurnToward(InDialogWith.GetSimPosition());
                ClientSelf.AnimationStop(Animations.TALK, true);
                ClientSelf.AnimationStart(Animations.TALK, true);
                Client.Talk(InDialogWith + ": " + talkAbout);
                Thread.Sleep(3000);
                ClientSelf.AnimationStop(Animations.TALK, true);
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

        public override void Debug(string p)
        {
            WorldSystem.output(p);
        }

        public void Eat(SimObject target)
        {
            Debug("!!! EAT " + target);
        }

        public ThreadStart WithSitOn(SimObject obj, ThreadStart closure)
        {
            BotClient Client = GetGridClient();
            AgentManager ClientSelf = Client.Self;
            return new ThreadStart(delegate()
            {
                Primitive targetPrim = obj.thePrim;
                ClientSelf.RequestSit(targetPrim.ID, Vector3.Zero);
                ClientSelf.Sit();
                try
                {
                    closure.Invoke();
                }
                finally
                {
                    ClientSelf.Stand();
                }
            });
        }

        public ThreadStart WithGrabAt(SimObject obj, ThreadStart closure)
        {
            BotClient Client = GetGridClient();
            return new ThreadStart(delegate()
            {
                Primitive targetPrim = obj.thePrim;
                uint objectLocalID = targetPrim.LocalID;
                AgentManager ClientSelf = Client.Self;
                try
                {
                    ClientSelf.Grab(objectLocalID);
                    closure.Invoke();
                }
                finally
                {
                    ClientSelf.DeGrab(objectLocalID);
                }
            });
        }

        public ThreadStart WithAnim(UUID anim, ThreadStart closure)
        {
            BotClient Client = GetGridClient();
            AnimThread animThread = new AnimThread(Client.Self, anim);
            return new ThreadStart(delegate()
            {
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
            return cogbot.TheOpenSims.SimAnimation.GetAnimationUUID(use);
        }

        public void ExecuteLisp(SimObjectUsage botObjectAction, String lisp)
        {
            BotClient Client = GetGridClient();
            if (!String.IsNullOrEmpty(lisp))
            {
                Client.lispTaskInterperter.Intern("TheBot", this);
                Client.lispTaskInterperter.Intern("Target", botObjectAction.Target);
                Client.lispTaskInterperter.Intern("botObjectAction", botObjectAction);
                Client.evalLispString((String)lisp);
            }
        }


        public override bool IsFloating
        {
            get
            {
                AgentManager ClientSelf = Client.Self;
                AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                return ClientMovement.Fly;
            }
            set
            {
                if (IsFloating != value)
                {
                    AgentManager ClientSelf = Client.Self;
                    ClientSelf.Fly(value);
                }
            }
        }

        public override string GetName()
        {
            return theAvatar.Name;
        }

        public override string ToString()
        {
            return GetName();
        }

        BotClient Client;
        public void SetClient(BotClient Client)
        {
            this.Client = Client;
            WorldSystem = Client.WorldSystem;
            WorldSystem.SetSimAvatar(this);
            //WorldSystem.AddTracking(this,Client);
        }

        public SimObject FindSimObject(SimObjectType pUse)
        {
            IList<SimObject> objects = GetKnownObjects();
            foreach (SimObject obj in objects)
            {
                if (obj.IsTypeOf(pUse) != null) return obj;
            }
            return null;
        }

        public override bool Matches(string name)
        {
            return SimTypeSystem.MatchString(base.ToString(), name)
                || SimTypeSystem.MatchString(ToString(), name);
        }


        object TrackerLoopLock = new object();

        void TrackerLoop()
        {
            AgentManager ClientSelf = Client.Self;
            AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
            bool StartedFlying = false;// !IsFloating;
            try
            {
                Random somthing = new Random(Environment.TickCount);// We do stuff randomly here
                Boolean justStopped = false;
                while (true)
                {
                    lock (TrackerLoopLock)
                    if (ApproachTarget != null)
                    {
                        ApproachDistance = ApproachTarget.GetSizeDistance();
                        Vector3 targetPosition = new Vector3(ApproachTarget.GetSimPosition());
                        float ZDist = Math.Abs(targetPosition.Z - ClientSelf.SimPosition.Z);
                        targetPosition.Z = GetSimPosition().Z;
                        // allow flight
                        if (ZDist > ApproachDistance)
                        {
                            if (!ClientMovement.Fly)
                            {
                                if (!StartedFlying)
                                {
                                    // ClientSelf.Fly(true);                                 
                                    StartedFlying = true;
                                }
                            }
                        }
                        else
                        {
                            if (StartedFlying)
                            {
                                // ClientSelf.Fly(false);
                                StartedFlying = false;
                            }
                        }

                        if (StartedFlying)
                        {
                            // targetPosition.Z = ApproachTarget.GetSimPosition().Z;
                        }
                        float curDist = Vector3.Distance(ClientSelf.SimPosition, targetPosition);
                        if (curDist > ApproachDistance)
                        {

                            //ClientMovement.SendUpdate();
                            if (curDist < (ApproachDistance * 1.25))
                            {
                                ClientMovement.TurnToward(targetPosition);
                                ClientMovement.AtPos = true;
                                Thread.Sleep(25);
                                ClientMovement.Stop = true;
                                ClientMovement.AtPos = false;
                                ClientMovement.NudgeAtPos = false;
                                ClientMovement.SendUpdate(false);

                                Thread.Sleep(100);
                            }
                            else
                            {
                                ClientMovement.TurnToward(targetPosition);
                                ClientMovement.AtPos = true;
                                ClientMovement.UpdateInterval = 0; //100
                                ClientMovement.SendUpdate(false);
                                Application.DoEvents();
                                //(int)(25 * (1 + (curDist / ApproachDistance)))
                                Thread.Sleep(somthing.Next(25, 100));
                            }
                            justStopped = true;
                        }
                        else
                        {
                            if (justStopped)
                            {
                                ClientMovement.TurnToward(targetPosition);
                                ClientMovement.AtPos = false;
                                //ClientMovement.UpdateInterval = 0;
                                ClientMovement.StandUp = true;
                                //ClientMovement.SendUpdate();
                                ClientMovement.FinishAnim = true;
                                ClientMovement.Stop = true;
                                ClientMovement.SendUpdate(false);
                                Thread.Sleep(25);
                                // WorldSystem.TheSimAvatar.StopMoving();
                                justStopped = false;
                            }
                            else
                            {
                                Thread.Sleep(100);
                            }


                        }

                    }
                    else
                    {
                        Thread.Sleep(1000);
                        return; // if ApproachTarget is null then we're not interested anymore 
                    }
                }
            }
            finally
            {
                try
                {
                    // WorldSystem.TheSimAvatar.StopMoving();
                }
                catch (Exception e) { }
            }
        }

        internal void StopMoving()
        {
            lock (TrackerLoopLock)
            {
                ApproachTarget = null;
            }
            AgentManager ClientSelf = Client.Self;
            ClientSelf.AutoPilotCancel();
            AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
            //  ClientMovement. AlwaysRun = false;
            ClientMovement.AtNeg = false;
            ClientMovement.AtPos = false;
            //ClientMovement.AutoResetControls = true;
            //   ClientMovement. Away = false;
            ClientMovement.FastAt = false;
            ClientMovement.FastLeft = false;
            ClientMovement.FastUp = false;
            // ClientMovement.FinishAnim = true;
            //  ClientMovement. Fly = false;
            ClientMovement.LButtonDown = false;
            ClientMovement.LButtonUp = false;
            ClientMovement.LeftNeg = false;
            ClientMovement.LeftPos = false;
            ClientMovement.MLButtonDown = false;
            ClientMovement.MLButtonUp = false;
            // ClientMovement. Mouselook = false;
            ClientMovement.NudgeAtNeg = false;
            ClientMovement.NudgeAtPos = false;
            ClientMovement.NudgeLeftNeg = false;
            ClientMovement.NudgeLeftPos = false;
            ClientMovement.NudgeUpNeg = false;
            ClientMovement.NudgeUpPos = false;
            ClientMovement.PitchNeg = false;
            ClientMovement.PitchPos = false;
            //ClientMovement. SitOnGround = false;
            //ClientMovement. StandUp = false;
            ClientMovement.Stop = true;
            ClientMovement.TurnLeft = false;
            ClientMovement.TurnRight = false;
            ClientMovement.UpdateInterval = 0;
            ClientMovement.UpNeg = false;
            ClientMovement.UpPos = false;
            ClientMovement.YawNeg = false;
            ClientMovement.YawPos = false;

            ClientMovement.SendUpdate();
        }

        public void SetFollow(SimPosition followAvatar)
        {
            lock (TrackerLoopLock)
            {
                if (followAvatar != ApproachTarget)
                {
                    StopMoving();
                }
                if (ApproachThread == null)
                {
                    ApproachThread = new Thread(TrackerLoop);
                    ApproachThread.Name = "SetFollow: " + ApproachTarget;
                    ApproachThread.Start();
                }
                ApproachTarget = followAvatar;
            }
        }
    }

}
