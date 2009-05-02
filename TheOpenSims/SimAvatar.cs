using System;
using System.Collections.Generic;
using OpenMetaverse;
using cogbot.Listeners;
using System.Threading;
using PathSystem3D.Navigation;
using System.Collections;
using PathSystem3D;
//Complex outcomes may be a result of simple causes, or they may just be complex by nature. 
//Those complexities that turn out to have simple causes can be simulated and studied, 
//thus increasing our knowledge without needing direct observation.
namespace cogbot.TheOpenSims
{

    public class SimAvatarImpl : SimObjectImpl, SimMover, cogbot.TheOpenSims.SimAvatar
    {

        public override void AddPossibleAction(string typeUse, params object[] args)
        {
            KnownTypeUsages.AddTo(SimTypeSystem.CreateTypeUsage(typeUse));
            base.AddPossibleAction(typeUse, args);
        }

        public override void AddCanBeTargetOf(string textualActionName, params object[] args)
        {
            base.AddCanBeTargetOf(textualActionName, args);
        }

        public Thread avatarThinkerThread;
        public Thread avatarHeartbeatThread;

        public Avatar theAvatar
        {
            get { return (Avatar)Prim; }
        }

        public SimAvatar InDialogWith { get; set; }

        readonly BotNeeds _CurrentNeeds;

        public BotNeeds CurrentNeeds
        {
            get { return _CurrentNeeds; }
        }

        double _SightRange = 260.0;
        public double SightRange
        {
            get
            {
                return _SightRange;
            }
            set
            {
                if (value > _SightRange)
                {
                    GetNearByObjects(value, false);
                }
                _SightRange = value;
            }
        }
        // things the bot cycles through mentally
        readonly public ListAsSet<SimObject> KnownSimObjects = new ListAsSet<SimObject>();

        public List<SimObject> GetKnownObjects()
        {
            ScanNewObjects(3, SightRange);
            SortByDistance(KnownSimObjects);
            return KnownSimObjects;
        }

        public override List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly)
        {
            List<SimObject> near = base.GetNearByObjects(maxDistance, rootOnly);
            AddKnowns(near);
            return near;
        }

        // which will result in 
        public List<BotAction> KnownBotActions = new List<BotAction>();

        // which will be skewed with how much one bot like a Mental Aspect
        readonly public Dictionary<BotMentalAspect, int> AspectEnjoyment = new Dictionary<BotMentalAspect, int>();

        //notice this also stores object types that pleases the bot as well as people
        // (so how much one bot likes another avatar is stored here as well)

        // Actions the bot might do next cycle.
        List<BotAction> TodoBotActions = new List<BotAction>();

        // Actions observed
        readonly List<BotAction> ObservedBotActions = new List<BotAction>();

        // Action template stubs 
        readonly ListAsSet<SimTypeUsage> KnownTypeUsages = new ListAsSet<SimTypeUsage>();

        // Assumptions about stubs
        readonly public Dictionary<SimObjectType, BotNeeds> Assumptions = new Dictionary<SimObjectType, BotNeeds>();

        // Current action 
        public BotAction CurrentAction;

        // When seeking out objects for use
        // the whole region at least - this is different than the sight distance
        public double MaxThinkAboutDistance = 256d;

        // When seeking out objects for use -  
        // this is only limited due to the pathfinder demo for the moment
        public double MaxSupportedZChange = 2d;


        public override sealed bool MakeEnterable(SimMover actor)
        {
            return false;
        }

        readonly string AspectName;
        public SimAvatarImpl(Avatar slAvatar, WorldObjects objectSystem, Simulator reg)
            : base(slAvatar, objectSystem, reg)
        {
            WorldObjects.SimAvatars.Add(this);
            ObjectType.SuperType.Add(SimTypeSystem.GetObjectType("Avatar"));
            _CurrentNeeds = new BotNeeds(90.0F);
            try
            {

                AspectName = slAvatar.Name;
            }
            catch (Exception)
            {
                AspectName += objectSystem.client + "_Avatar_" + slAvatar.LocalID;
            }
            avatarHeartbeatThread = new Thread(Aging);
            avatarHeartbeatThread.Name = String.Format("AvatarHeartbeatThread for {0}", AspectName);
            avatarHeartbeatThread.Priority = ThreadPriority.Lowest;
            avatarHeartbeatThread.Start();
            MakeEnterable(this);
        }


        public override bool RestoreEnterable(SimMover agent)
        {
            return false;// base.RestoreEnterable(this);
        }

        public override bool IsRoot
        {
            get { return theAvatar.ParentID == 0; }
        }

        // public override ISimObject Parent {  get { return this; }   }

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

        public override bool IsLocal()
        {
            if (Client == null) return false;
            AgentManager ClientSelf = Client.Self;
            return ClientSelf.AgentID == theAvatar.ID || ClientSelf.LocalID == theAvatar.LocalID;
        }

        public override string DebugInfo()
        {
            String s = String.Format("\n{0}" ,ToString());
            List<SimObject> KnowsAboutList = GetKnownObjects();
            KnowsAboutList.Sort(CompareObjects);
            s += String.Format("\nCurrentAction: {0}", CurrentAction);
            int show = 10;
            s += String.Format("\nKnowsAboutList: {0}", KnowsAboutList.Count);
            foreach (SimObject item in KnowsAboutList)
            {
                show--;
                if (show < 0) break;
                //if (item is ISimAvatar) continue;
                s += String.Format("\n   {0} {1}", item, DistanceVectorString(item));
            }
            show = 10;
            KnownTypeUsages.Sort(CompareUsage);
            s += String.Format("\nKnownTypeUsages: {0}", KnownTypeUsages.Count);
            foreach (SimTypeUsage item in KnownTypeUsages)
            {
                show--;
                if (show < 0) break;
                //if (item is ISimAvatar) continue;
                s += String.Format("\n   {0} {1}", item, item.RateIt(CurrentNeeds));
            }
            s += String.Format("\nCurrentNeeds: {0}", CurrentNeeds);
            return s;
        }

        public void StartThinking()
        {
            if (avatarThinkerThread == null)
            {
                avatarThinkerThread = new Thread(Think) { Name = "AvatarThinkerThread for " + Client };
                if (IsLocal())
                {
                    // only think for ourselves
                    avatarThinkerThread.Priority = ThreadPriority.Normal;
                    avatarThinkerThread.Start();
                }
            }
            if (!avatarThinkerThread.IsAlive) avatarThinkerThread.Resume();
        }

        public bool IsThinking()
        {
            return (avatarThinkerThread != null && avatarThinkerThread.IsAlive);
        }
        public void PauseThinking()
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

        public override Simulator GetSimulator()
        {
            if (IsLocal()) return Client.Network.CurrentSim;
            return GetSimRegion().TheSimulator;
        }

        public override Vector3 GetSimPosition()
        {
            if (IsLocal())
            {
                if (Client.Settings.OBJECT_TRACKING)
                    return Client.Self.SimPosition;
                if (theAvatar.ParentID == 0) return theAvatar.Position;
            }

            Vector3 local = base.GetSimPosition();
            if (SimPathStore.OutOfRegion(local))
            {
                Debug(" OutOfRegion " + local);
            }
            return local;
        }

        public override Vector3d GetWorldPosition()
        {
            //if (IsLocal())
            //{
            //    if (Client.Settings.OBJECT_TRACKING)
            //        return Client.Self.GlobalPosition;

            //}
            return GetSimRegion().LocalToGlobal(GetSimPosition());
        }

        public override SimRegion GetSimRegion()
        {
            if (Prim == null)
            {
                if (_CurrentRegion == null) if (IsLocal())
                    {
                        _CurrentRegion = SimRegion.GetRegion(Client.Network.CurrentSim);
                    }
                return _CurrentRegion;
            }

            if (_CurrentRegion == null)
            {
                _CurrentRegion = SimRegion.GetRegion(Prim.RegionHandle);
                Debug("out of date _CurrentRegion ");
            }
            if (theAvatar.RegionHandle != _CurrentRegion.RegionHandle)
            {
                //Debug("out of date RegionHandle ");
                //_CurrentRegion = null;
            }
            return _CurrentRegion;
        }


        public override Quaternion GetSimRotation()
        {
            if (IsLocal())
            {
                if (Client.Settings.OBJECT_TRACKING)
                    return Client.Self.SimRotation;
            }
            lock (Prim)
            {
                if (theAvatar.RegionHandle != _CurrentRegion.RegionHandle)
                {
                    Debug("out of date RegionHandle ");
                }
                return base.GetSimRotation();
            }
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
            BotNeeds OneMinute = SimTypeSystem.GetObjectType("OnMinuteTimer").GetUsageActual("OnMinuteTimer");
            while (true)
            {
                CurrentNeeds.AddFrom(OneMinute);
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

        public BotAction GetNextAction()
        {
            BotAction act = CurrentAction;

            IList<BotAction> acts = GetPossibleActions(MaxThinkAboutDistance, MaxSupportedZChange);

            if (acts.Count > 0)
            {
                act = (BotAction)FindBestUsage(acts);
                acts.Remove(act);
            }
            return act;
        }

        public SimUsage FindBestUsage(IEnumerable acts)
        {
            SimUsage bestAct = null;
            if (acts != null)
            {
                IEnumerator enumer = acts.GetEnumerator();
                double bestRate = double.MinValue;
                while (enumer.MoveNext())
                {
                    SimUsage b = (SimUsage)enumer.Current;
                    double brate = b.RateIt(CurrentNeeds);
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

        //public ISimObject RezObjectType(ISimObject copyOf)
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
            if (act1 == act2) return 0;
            if (act1 == null) return -1;
            if (act2 == null) return 1;
            return (int)(act2.RateIt(CurrentNeeds) - act1.RateIt(CurrentNeeds));
        }

        public IList<BotAction> GetPossibleActions(double maxXYDistance, double maxZDist)
        {
            if (TodoBotActions.Count < 2)
            {
                TodoBotActions = NewPossibleActions(maxXYDistance, maxZDist);
            }
            return TodoBotActions;
        }

        public List<BotAction> NewPossibleActions(double maxXYDistance, double maxZDist)
        {
            double myZ = GetWorldPosition().Z;
            List<SimObject> useObjects = new List<SimObject>();
            foreach (SimObject O in GetKnownObjects())
            {
                if (O.Distance(this) > maxXYDistance) continue;
                if (Math.Abs(O.GetWorldPosition().Z - myZ) > maxZDist) continue;
                useObjects.Add(O);
            }


            List<BotAction> acts = new List<BotAction>();
            foreach (BotAction obj in ObservedBotActions)
            {
                acts.Add(obj);
            }

            foreach (SimObject obj in useObjects)
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

        public void DoBestUse(SimObject someObject)
        {
            SimTypeUsage use = someObject.GetBestUse(CurrentNeeds);
            if (use == null)
            {
                double closeness = Approach(someObject, 2);
                AgentManager ClientSelf = Client.Self;
                ClientSelf.Touch(someObject.Prim.LocalID);
                if (closeness < 3)
                {
                    ClientSelf.RequestSit(someObject.Prim.ID, Vector3.Zero);
                    ClientSelf.Sit();
                }
                return;
            }
            Do(use, someObject);
            return;
        }

        public void Do(SimTypeUsage use, SimObject someObject)
        {
            UseAspect(new BotObjectAction(this, new SimObjectUsage(use, someObject)));
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

        /// <summary>
        ///  TODO Real Eval routine
        /// </summary>
        /// <param name="mostInteresting"></param>
        /// <param name="cAspect"></param>
        /// <returns></returns>
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

        public void ScanNewObjects(int minimum, double sightRange)
        {
            List<SimObject> objects = GetNearByObjects(sightRange, true);
            // ill do this for us: AddKnowns(objects);
            if (KnownSimObjects.Count < minimum)
            {
                if (sightRange < 255)
                    ScanNewObjects(minimum, sightRange + 10);
            }
        }

        private void AddKnowns(IEnumerable<SimObject> objects)
        {
            lock (objects)
            {
                foreach (SimObject obj in objects)
                {
                    if (obj != this)
                        if (obj.IsRoot || obj.IsTyped)
                        {
                            lock (KnownSimObjects) if (!KnownSimObjects.Contains(obj))
                                {
                                    KnownSimObjects.Add(obj);
                                    IList<SimTypeUsage> uses = obj.GetTypeUsages();
                                    foreach (SimTypeUsage use in uses)
                                    {
                                        lock (KnownTypeUsages) if (!KnownTypeUsages.Contains(use))
                                            {
                                                KnownTypeUsages.Add(use);
                                            }
                                    }
                                }
                        }
                }
            }
        }

        // Avatars approach distance
        public override float GetSizeDistance()
        {
            return 2f;
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
            SimAvatar avatarWasInDialogWith = ((SimAvatarImpl)avatar).InDialogWith;
            SimAvatar wasInDialogWith = InDialogWith;
            try
            {
                InDialogWith = avatar;
                BotClient Client = GetGridClient();
                AgentManager ClientSelf = Client.Self;
                AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                TurnToward(InDialogWith);
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

        public override void Debug(string p, params object[] args)
        {
            WorldSystem.output(String.Format(p, args));
        }

        public void Eat(SimObject target)
        {
            Debug("!!! EAT " + target);
        }

        public ThreadStart WithSitOn(SimObject obj, ThreadStart closure)
        {
            bool CanUseSit = WorldObjects.CanUseSit;
            BotClient Client = GetGridClient();
            AgentManager ClientSelf = Client.Self;
            return () =>
            {
                if (CanUseSit)
                {
                    Primitive targetPrim = obj.Prim;

                    ClientSelf.RequestSit(targetPrim.ID, Vector3.Zero);
                    ClientSelf.Sit();
                }

                try
                {
                    closure.Invoke();
                }
                finally
                {
                    bool SAU = Client.Settings.SEND_AGENT_UPDATES;
                    try
                    {
                        Client.Settings.SEND_AGENT_UPDATES = true;
                        ClientSelf.AnimationStart(Animations.STANDUP, true);
                        if (CanUseSit) ClientSelf.Stand();
                    }
                    finally
                    {
                        Client.Settings.SEND_AGENT_UPDATES = SAU;
                    }
                }
            };
        }

        public ThreadStart WithGrabAt(SimObject obj, ThreadStart closure)
        {
            BotClient Client = GetGridClient();
            return () =>
            {
                Primitive targetPrim = obj.Prim;
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
            };
        }

        public ThreadStart WithAnim(UUID anim, ThreadStart closure)
        {
            BotClient Client = GetGridClient();
            AnimThread animThread = new AnimThread(Client.Self, anim);
            return () =>
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
                       };
        }

        public UUID FindAnimUUID(string use)
        {
            return cogbot.TheOpenSims.SimAnimation.GetAnimationUUID(use);
        }

        public void ExecuteLisp(SimObjectUsage botObjectAction, Object lisp)
        {
            BotClient Client = GetGridClient();
            if (lisp != null)
            {
                Client.Intern("TheBot", this);
                Client.Intern("TheTarget", botObjectAction.Target);
                Client.Intern("TheAction", botObjectAction);
                Client.evalLispCode(lisp);
            }
        }


        public override bool IsFloating
        {
            get
            {
                if (IsLocal())
                    return Client.Self.Movement.Fly;
                return false;
            }
            set
            {
                if (IsFloating != value)
                {
                    if (IsLocal()) Client.Self.Fly(value);
                }
            }
        }

        public override string GetName()
        {
            try
            {
                return theAvatar.Name;
            }
            catch (Exception)
            {
                return AspectName;
            }
        }

        public override string ToString()
        {
            return GetName();
        }

        BotClient Client;
        public void SetClient(BotClient Client)
        {
            lock (Client)
            {
                this.Client = Client;
                Client.Intern("TheBot", this);
                //  WorldSystem = Client.WorldSystem;
                //if (IsLocal())
                {
                    //                   EnsureTrackerRunning();
                }
            }
            //WorldSystem.AddTracking(this,Client);
        }

        public SimObject FindSimObject(SimObjectType pUse, double maxXYDistance, double maxZDist)
        {
            double myZ = GetWorldPosition().Z;
            IList<SimObject> objects = GetKnownObjects();
            foreach (SimObject O in objects)
            {
                if (O.Distance(this) > maxXYDistance) continue;
                if (Math.Abs(O.GetWorldPosition().Z - myZ) > maxZDist) continue;
                if (O.IsTypeOf(pUse) != null) return O;
            }
            return null;
        }

        public override bool Matches(string name)
        {
            return SimTypeSystem.MatchString(base.ToString(), name)
                || SimTypeSystem.MatchString(ToString(), name);
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
                    Simulator simu = GetSimulator();
                    UnPhantom = WorldSystem.GetSimObject(WorldSystem.GetPrimitive(sit, simu));
                    UnPhantom.MakeEnterable(this);
                    ClientSelf.Stand();
                }
            }
            return UnPhantom;
        }


        public override void UpdateObject(ObjectUpdate objectUpdate, ObjectUpdate objectUpdateDiff)
        {
        }

        public override void Touch(SimObject simObject)
        {
            if (IsLocal())
            {
                Client.Self.Touch(simObject.Prim.LocalID);
            }
            else
            {
                Debug("Cant touch !IsLocal() " + simObject);
            }
        }

        public void RemoveObject(SimObject O)
        {
            KnownSimObjects.Remove(O);
        }

        //public override SimWaypoint GetWaypoint()
        //{
        //    Vector3 v3 = GetWorldPosition();
        //    SimRegion PathStore = GetSimRegion();
        //    SimWaypoint swp = PathStore.CreateClosestWaypoint(v3);
        //    double dist = Vector3.Distance(v3, swp.GetWorldPosition());
        //    if (!swp.Passable)
        //    {
        //        WorldSystem.output("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
        //    }
        //    return swp;
        //}

        public override void StopMoving()
        {
            lock (TrackerLoopLock)
            {
                ApproachVector3D = Vector3d.Zero;
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


        /// <summary>
        /// 
        /// </summary>
        /// <param name="obj"></param>
        /// <param name="maxDistance"></param>
        /// <returns></returns>
        public double Approach(SimObject obj, double maxDistance)
        {
            BotClient Client = GetGridClient();
            // stand up first
            SimObject UnPhantom = StandUp();
            // make sure it not going somewhere
            // set the new target
            ApproachDistance = obj.GetSizeDistance() + 0.5f;
            string str = "Approaching " + obj + " " + DistanceVectorString(obj) + " to get " + ApproachDistance;
            Debug(str);
            obj.MakeEnterable(this);
            // if (!MoveTo(obj.GetWorldPosition(), obj.GetSizeDistance() + 0.5f, 12))
            {
                GotoTarget(obj);
            }
            if (UnPhantom != null)
                UnPhantom.RestoreEnterable(this);

            return (double)Distance(obj);
        }

        readonly object TrackerLoopLock = new object();

        void TrackerLoop()
        {
            Boolean stopNext = false;
            while (true)
            {
                Vector3d targetPosition = ApproachVector3D;
                lock (TrackerLoopLock)
                {
                    // Debug("TrackerLoop: " + Thread.CurrentThread);
                    if (ApproachVector3D == Vector3d.Zero)
                    {
                        if (ApproachPosition == null)
                        {
                            Thread.Sleep(100);
                            continue;
                        }
                       
                    }
                    if (ApproachPosition!=null)
                    {
                        targetPosition = ApproachPosition.GetWorldPosition();
                    }
                    
                }
                double realTargetZ = targetPosition.Z;
                Vector3d worldPosition = GetWorldPosition();
                //ApproachDistance = ApproachPosition.GetSizeDistance();
                try
                {
                    AgentManager ClientSelf = Client.Self;
                    AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                    ClientMovement.AutoResetControls = false;
                    ClientMovement.UpdateInterval = 0; //100
                    SimRegion R = GetSimRegion();
                    float WaterHeight = R.WaterHeight();
                    float selfZ = ClientSelf.SimPosition.Z;
                    double UpDown = realTargetZ - selfZ;
                    double curDist = Vector3d.Distance(worldPosition, targetPosition);

                    //if (curDist < ApproachDistance)
                    //{
                    //    ClientMovement.Stop = true;
                    //    TurnToward(targetPosition);
                    //    ClientMovement.SendUpdate();
                    //    continue;
                    //}
                    double ZDist = Math.Abs(UpDown);
                    if (UpDown > 1)
                    {
                        targetPosition.Z = selfZ + 0.2f; // incline upward
                    }
                    else
                    {
                        targetPosition.Z = selfZ;
                    }

                    // Like water areas
                    bool swimming = GetPathStore().IsUnderWater(worldPosition);

                    // Reset previous Z 
                    ClientMovement.FastUp = false;
                    ClientMovement.UpPos = false;
                    ClientMovement.UpNeg = false;
                    ClientMovement.NudgeUpPos = false;
                    ClientMovement.NudgeUpNeg = false;

                    double curXYDist = Vector3d.Distance(worldPosition, new Vector3d(targetPosition.X, targetPosition.Y, selfZ));

                    curDist = Vector3d.Distance(worldPosition, targetPosition);

                    if (swimming)
                    {
                        // WaterHeight = WaterHeight - 1f;
                        if (!ClientMovement.Fly)
                        {
                            ClientMovement.Fly = false;
                        }

                        bool nudgeUpDownMoves = true;

                        if (selfZ > WaterHeight - 0.5)
                        {
                            // Bob downward
                            if (nudgeUpDownMoves)
                                ClientMovement.NudgeUpNeg = true;
                            else
                                ClientMovement.UpNeg = true;
                            SendUpdate(10);
                            //
                            //  continue; //to keep going up
                        }
                        else
                        {
                            //  nudge = !nudge;
                            if (selfZ < WaterHeight - 2.5)
                            {
                                // Bob upward
                                if (nudgeUpDownMoves)
                                    ClientMovement.NudgeUpPos = true;
                                else
                                    ClientMovement.UpPos = true;
                                SendUpdate(10);
                                //   continue; //to keep going up
                            }
                        }
                        targetPosition.Z = WaterHeight - 0.25f;
                    }

                    ClientMovement.Fly = swimming || GetPathStore().IsFlyZone(SimPathStore.GlobalToLocal(worldPosition));

                    if (swimming)
                    {
                        // Reset previous Z 
                        ClientMovement.FastUp = false;
                        ClientMovement.UpPos = false;
                        ClientMovement.UpNeg = false;
                        ClientMovement.NudgeUpPos = false;
                        ClientMovement.NudgeUpNeg = false;
                        SendUpdate(10);
                    }

                    //// Little Jumps
                    if (ZDist*2 > curDist)
                    {
                        if (!ClientMovement.Fly)
                        {
                            if (UpDown > 0)
                            {
                                ClientMovement.NudgeUpPos = true;
                                SendUpdate(10);
                                ClientMovement.NudgeUpPos = false;
                            }
                        }
                    }
                    //else
                    //{
                    //    if (ClientMovement.Fly)
                    //    {
                    //        ClientMovement.NudgeUpPos = false;
                    //        // ClientSelf.Fly(false);
                    //        ClientMovement.Fly = false;
                    //    }
                    //}


                    if (ApproachVector3D == Vector3d.Zero)
                    {
                        if (ApproachPosition == null)
                        {                         
                            continue;
                        }
                    }

                    TurnToward(targetPosition);
                    // Far away
                    if (curXYDist > ApproachDistance)
                    {
                        ClientMovement.Stop = false;
                        ClientMovement.FinishAnim = false;
                        TurnToward(targetPosition);
                        // getting close though
                        if (curDist < (ApproachDistance * 1.25))
                        {
                            //ClientMovement.AtPos = true;
                            //SendUpdate(125);
                            //ClientMovement.Stop = true;
                            ClientMovement.AtPos = false;
                            ClientMovement.NudgeAtPos = true;
                            SendUpdate(100);
                            ClientMovement.NudgeAtPos = false;
                            SendUpdate(100);
                            stopNext = true;
                            continue;
                        }
                        else
                        {
                            ClientMovement.AtPos = true;
                            ClientMovement.UpdateInterval = 0;
                            SendUpdate(MyRandom.Next(25, 100));
                            // TurnToward(targetPosition);
                            //(int)(25 * (1 + (curDist / followDist)))
                            //   MoveFast(ApproachPosition);
                            //    if (ApproachPosition!=null) MoveSlow(ApproachPosition);
                            stopNext = true;
                            continue;
                        }
                    }
                    else
                    {
                        if (stopNext)
                        {
                            //                            TurnToward(targetPosition);
                            ClientMovement.AtPos = false;
                            ClientMovement.UpdateInterval = 0;
                            //ClientMovement.StandUp = true;
                            //ClientMovement.SendUpdate();
                            ClientMovement.FinishAnim = true;
                            ClientMovement.Stop = true;
                            SendUpdate(25);
                            // WorldSystem.TheSimAvatar.StopMoving();
                            stopNext = false;
                            continue;

                        }
                        else
                        {
                            Thread.Sleep(100);
                            continue;
                        }


                    }

                }
                catch (Exception e)
                {
                    Debug("" + e);
                }

            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="finalTarget"></param>
        /// <param name="maxDistance"></param>
        /// <param name="maxSeconds"></param>
        /// <returns></returns>
        public override bool MoveTo(Vector3d finalTarget, double maxDistance, int maxSeconds)
        {
            double currentDist = Vector3d.Distance(finalTarget, GetWorldPosition());
            if (currentDist < maxDistance) return true;
            lock (TrackerLoopLock)
            {
              //  SimWaypoint P = SimWaypointImpl.CreateGlobal(finalTarget);
                SetMoveTarget(finalTarget);
                ApproachDistance = maxDistance;
            }
            double lastDistance = currentDist;
            long endTick = Environment.TickCount + maxSeconds * 1000;
            while (Environment.TickCount < endTick)
            {
                currentDist = Vector3d.Distance(finalTarget, GetWorldPosition());
                if (currentDist > lastDistance + 0.1)
                {
                    Console.Write("=");
                    StopMoving();
                    return true;
                }
                if (currentDist > maxDistance)
                {
                    Thread.Sleep(200);
                    //Application.DoEvents();
                    lastDistance = currentDist;
                    continue;
                }
                else
                {
                    Console.Write("+");
                    // StopMoving();
                    return true;
                }
            }
            StopMoving();
            Console.Write("-");
            return false;
        }

        public override void SendUpdate(int ms)
        {
            //Client.Self.Movement.AutoResetControls = true;
            Client.Self.Movement.SendUpdate(true);
            if (ms>0) Thread.Sleep(ms);
        }

        public override void TeleportTo(SimRegion R, Vector3 local)
        {
            if (!IsLocal())
            {
                throw Error("GotoTarget !IsLocal()");
            }
            Client.Self.Teleport(R.RegionName, local);
        }

        public void SetMoveTarget(SimPosition target)
        {
            if (target==null)
            {
                ApproachVector3D = Vector3d.Zero;
                ApproachPosition = null;
                return;
            }
            if (ApproachPosition != target)
            {
                SetMoveTarget(target.GetWorldPosition());
            }
        }


        public override void SetMoveTarget(Vector3d target)
        {
            lock (TrackerLoopLock)
            {
                if (target != ApproachVector3D)
                {
                    //ApproachPosition = target;
                    ApproachVector3D = target;
                    if (target != Vector3d.Zero)
                    {
                       
                        EnsureTrackerRunning();
                    }
                    else
                    {
                        StopMoving();
                    }
                }
            }
        }

        private void EnsureTrackerRunning()
        {
            lock (TrackerLoopLock)
            {
                if (ApproachThread == null)
                {

                    WorldSystem.SetSimAvatar(this);
                    ApproachThread = new Thread(TrackerLoop);
                    ApproachThread.Name = "TrackerLoop for " + Client;
                    ApproachThread.Priority = ThreadPriority.Normal;
                    ApproachThread.Start();
                }
            }
        }

        double ApproachDistance = 2f;

        public SimPosition ApproachPosition { get; set; }
        public Vector3d ApproachVector3D { get; set; }

        Thread ApproachThread;//= new Thread(TrackerLoop);

        public override bool TurnToward(Vector3 target)
        {
            bool changed = false;
            AgentManager.AgentMovement ClientMovement = Client.Self.Movement;
            Quaternion parentRot = Quaternion.Identity;

            if (Client.Self.SittingOn > 0)
            {
                if (!Client.Network.CurrentSim.ObjectsPrimitives.ContainsKey(Client.Self.SittingOn))
                {
                    Logger.Log("Attempted TurnToward but parent prim is not in dictionary", Helpers.LogLevel.Warning, Client);
                    return false;
                }

                else
                {
                    Primitive parent = WorldSystem.GetPrimitive(Prim.ParentID, Client.Network.CurrentSim);
                    parentRot = parent.Rotation;
                }
            }

            Quaternion between = Vector3.RotationBetween(Vector3.UnitX, Vector3.Normalize(target - Client.Self.SimPosition));
            Quaternion rot = between * (Quaternion.Identity / parentRot);

            Quaternion br = ClientMovement.BodyRotation;
            Quaternion hr = ClientMovement.HeadRotation;
            //if (br != rot || hr != rot)
            {
                changed = true;
                ClientMovement.BodyRotation = rot;
                ClientMovement.HeadRotation = rot;
                ClientMovement.Camera.LookAt(Client.Self.SimPosition, target);

                bool prev = Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK;
                try
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                    SendUpdate(0);
                }
                finally
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = prev;
                }
            }
            return changed;
        }
        public override void UpdateOccupied()
        {
            // Vector3 pos = GetSimPosition();
            // if (SimPathStore.OutOfRegion(pos)) return;
            //don't change this spot
            //GetPathStore().SetPassable(pos.X, pos.Y, pos.Z);
        }

        #region SimAvatar Members


        #endregion
    }

    public interface SimAvatar : SimObject, SimMover
    {
        void Aging();
        SimPosition ApproachPosition { get; set; }
        void RemoveObject(SimObject O);
        double Approach(SimObject obj, double maxDistance);
        int CompareObjects(SimObject act1, SimObject act2);
        BotMentalAspect CompareTwo(BotMentalAspect mostInteresting, BotMentalAspect cAspect);
        int CompareUsage(SimUsage act1, SimUsage act2);
        void Do(SimTypeUsage use, SimObject someObject);
        void DoBestUse(SimObject someObject);
        void Eat(SimObject target);
        void ExecuteLisp(SimObjectUsage botObjectAction, object lisp);
        OpenMetaverse.UUID FindAnimUUID(string use);
        SimUsage FindBestUsage(System.Collections.IEnumerable acts);
        SimObject FindSimObject(SimObjectType pUse, double maxXYDistance, double maxZDist);
        cogbot.BotClient GetGridClient();
        System.Collections.Generic.List<SimObject> GetKnownObjects();
        BotAction GetNextAction();
        SimObject GetNextInterestingObject();
        IList<BotAction> GetPossibleActions(double maxXYDistance, double maxZDist);
        bool IsSitting();
        bool IsThinking();
        System.Collections.Generic.List<BotAction> NewPossibleActions(double maxXYDistance, double maxZDist);
        void PauseThinking();
        void ScanNewObjects(int minimum, double sightRange);
        void SetClient(cogbot.BotClient Client);
        double SightRange { get; set; }
        SimAvatar InDialogWith { get; set; }
       // SimPosition ApproachPosition { get; set; }
        BotNeeds CurrentNeeds { get; }
        SimObject StandUp();
        void StartThinking();
        void TalkTo(SimAvatar avatar, BotMentalAspect talkAbout);
        void TalkTo(SimAvatar avatar, string talkAbout);
        OpenMetaverse.Avatar theAvatar { get; }
        void Think();
        void ThinkOnce();
        void UseAspect(BotMentalAspect someAspect);
        System.Threading.ThreadStart WithAnim(OpenMetaverse.UUID anim, System.Threading.ThreadStart closure);
        System.Threading.ThreadStart WithGrabAt(SimObject obj, System.Threading.ThreadStart closure);
        System.Threading.ThreadStart WithSitOn(SimObject obj, System.Threading.ThreadStart closure);

        //List<SimObject> GetNearByObjects(double p, bool p_2);

        void SetMoveTarget(SimPosition followAvatar);
    }
}
