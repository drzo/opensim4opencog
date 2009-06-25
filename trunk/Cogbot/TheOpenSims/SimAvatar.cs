using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using PathSystem3D.Navigation;
using Random=System.Random;
using UUID=OpenMetaverse.UUID;

/// Complex outcomes may be a result of simple causes, or they may just be complex by nature. 
/// Those complexities that turn out to have simple causes can be simulated and studied, 
/// thus increasing our knowledge without needing direct observation.

namespace cogbot.TheOpenSims
{
    public partial class SimAvatarImpl : SimObjectImpl, SimMover, SimAvatar, SimActor
    {
        public override bool IsKilled
        {
            ///  get { return WasKilled; }
            set
            {
                if (!WasKilled) /// already
                {
                    List<SimObject> AttachedChildren0 = GetChildren();
                    lock (AttachedChildren0)
                        foreach (SimObject C in AttachedChildren0)
                        {
                            C.IsKilled = true;
                        }
                    ///  RemoveCollisions();
                }
                WasKilled = value;
            }
        }


        public override void ThreadJump()
        {
            ///  all the anims here are mainly so we can see what the bot is doing
            (new Thread(() =>
                        WithAnim(Animations.SHRUG, () =>
                                                       {
                                                           //Client.Self.Fly(false);
                                                           Client.Self.AnimationStart(Animations.WORRY, true);
                                                           Thread.Sleep(10);
                                                           Client.Self.Jump(true);
                                                           Thread.Sleep(500);
                                                           Client.Self.Jump(false);
                                                           Client.Self.AnimationStop(Animations.WORRY, true);
                                                       }))).Start();
        }

        public ThreadStart WithAnim(UUID uUID, ThreadStart threadStart)
        {
            return WithAnim(SimAssetStore.FindOrCreateAsset(uUID, AssetType.Animation), threadStart);
        }

        public override void OpenNearbyClosedPassages()
        {
            WithAnim(Animations.AFRAID, base.OpenNearbyClosedPassages);
        }

        public float ZHeading
        {
            get
            {
                if (!IsRegionAttached())
                {
                    WorldSystem.ReSelectObject(Prim);
                    WorldSystem.RequestMissingObject(Prim.LocalID, WorldSystem.GetSimulator(RegionHandle));
                }
                Vector3 v3 = Vector3.Transform(Vector3.UnitX, Matrix4.CreateFromQuaternion(GetSimRotation()));
                return (float)(Math.Atan2(-v3.X, -v3.Y) + Math.PI); // 2Pi= N, 1/2Pi = E
            }
        }

        public SimHeading GetHeading()
        {
            if (!IsRegionAttached())
            {
                WorldSystem.ReSelectObject(Prim);
                WorldSystem.RequestMissingObject(Prim.LocalID, WorldSystem.GetSimulator(RegionHandle));
            }
            return new SimHeading(this);
        }


        public override string DebugInfo()
        {

            string s = String.Format("{0} {1}", GetName(), GetHeading());
            lock (ActionEventQueue) foreach (SimObjectEvent s1 in ActionEventQueue)
            {
                s += "\n " + s1;
                
            }
            return s;
                
        }                    
        public override bool OnEffect(string effectType, object t, object p, float duration, UUID id)
        {
            bool noteable = LogEvent(new SimObjectEvent(effectType, SimEventType.EFFECT, SimEventStatus.Once, this, t, p, duration, id));
            //todo
            if (noteable) WorldSystem.SendNewEvent("on-effect", effectType, this, t, p, duration, id);
            return noteable;
            //throw new NotImplementedException();
        }

        public override bool LogEvent(SimObjectEvent SE)
        {
            string typeUse = SE.Verb;
            object[] args1_N = SE.Parameters;
            // does this backwards to the first argument is the most reliavant object
            for (int i = args1_N.Length-1; i >= 0 ; i--)
            {
                object o = args1_N[i];
                if (o == this) continue; //skip self
                if (o is SimObject) KnownSimObjects.AddFirst((SimObject)o);
            }
            _knownTypeUsages.AddTo(SimTypeSystem.CreateTypeUsage(typeUse));
            bool noteable = base.LogEvent(SE);
            if (noteable)
                //if (theAvatar.Name.Contains("rael"))
                {
                    Console.WriteLine(SE);
                }
            return noteable;
        }

        public override void AddCanBeTargetOf(int argN, SimObjectEvent evt)
        {
            base.AddCanBeTargetOf(argN, evt);
        }

        public Avatar theAvatar
        {
            get { return (Avatar) Prim; }
        }

        //public SimAvatar InDialogWith { get; set; }


        private double _SightRange = 260.0;

        public double SightRange
        {
            get { return _SightRange; }
            set
            {
                if (value > _SightRange)
                {
                    GetNearByObjects(value, false);
                }
                _SightRange = value;
            }
        }

       
        /// <summary>
        /// things the bot cycles through mentally
        /// </summary>
        public readonly ListAsSet<SimObject> KnownSimObjects = new ListAsSet<SimObject>();

        /// <summary>
        /// Returns hopefully at least three objects sorted by distance
        /// </summary>
        /// <returns></returns>
        public List<SimObject> GetKnownObjects()
        {
            ScanNewObjects(3, SightRange, false);
            lock (KnownSimObjects) SortByDistance(KnownSimObjects);
            return KnownSimObjects;
        }

        public override List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly)
        {
            List<SimObject> near = base.GetNearByObjects(maxDistance, rootOnly);
            AddKnowns(near);
            return near;
        }

        /// <summary>
        ///  Action template stubs 
        /// </summary>
        private readonly ListAsSet<SimTypeUsage> _knownTypeUsages;

        private BotAction _currentAction;
        private Thread actionThread = null;
        private readonly object actionLock = new object();
        public BotMentalAspect LastAction { get; set;}
        
        /// <summary>
        ///  Current action 
        /// </summary>       
        public BotAction CurrentAction
        {
            get
            {
                lock (actionLock)
                {
                    if (_currentAction == null) return null;
                    return _currentAction;
                }
            }
            set
            {
                try
                {
                    lock (actionLock)
                    {
                        if (Object.ReferenceEquals(_currentAction, value)) return;
                        if (_currentAction != null)
                        {
                            LastAction = _currentAction;
                            try
                            {
                                _currentAction.Abort();

                            }
                            catch (Exception)
                            {
                            }
                            _currentAction = value;
                        }
                        _currentAction = value;
                        if (actionThread != null)
                        {
                            try
                            {
                                actionThread.Abort();
                            }
                            catch (Exception)
                            {
                            }
                            finally
                            {
                                actionThread = null;
                            }
                        }
                        if (value != null)
                        {
                            actionThread = new Thread(() =>
                                                          {
                                                              try
                                                              {
                                                                  value.InvokeReal();
                                                              }
                                                              catch (Exception e)
                                                              {
                                                                  Debug("InvokeReal: " + e);
                                                                  throw e;
                                                              }
                                                              finally
                                                              {
                                                                  if (false)
                                                                      lock (actionLock)
                                                                      {
                                                                          if (_currentAction == value)
                                                                          {
                                                                              try
                                                                              {
                                                                                  _currentAction.Abort();

                                                                              }
                                                                              catch (Exception)
                                                                              {
                                                                              }
                                                                              _currentAction = null;
                                                                          }
                                                                      }
                                                              }
                                                          });
                            actionThread.Name = _currentAction.ToString();
                            actionThread.Start();
                        }
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine("" + e);
                }
            }
        }

        public override sealed bool MakeEnterable(SimMover actor)
        {
            return false;
        }

        private readonly string AspectName;

        public SimAvatarImpl(Avatar slAvatar, WorldObjects objectSystem, Simulator reg)
            : base(slAvatar, objectSystem, reg)
        {
            _knownTypeUsages = new ListAsSet<SimTypeUsage>();
            WorldObjects.SimAvatars.Add(this);
            ObjectType.SuperType.Add(SimTypeSystem.GetObjectType("Avatar"));
            try
            {
                AspectName = slAvatar.Name;
            }
            catch (Exception)
            {
                AspectName += objectSystem.client + "_Avatar_" + slAvatar.LocalID;
            }

            MakeEnterable(this);
        }

        public override bool RestoreEnterable(SimMover agent)
        {
            return false; ///  base.RestoreEnterable(this);
        }

        public override bool IsRoot
        {
            get { return theAvatar.ParentID == 0; }
        }

        ///  public override ISimObject Parent {  get { return this; }   }

        public bool IsSitting
        {
            get
            {
                if (IsControllable)
                {
                    if (Client.Self.SittingOn != 0) return true;
                    if (Client.Self.Movement.SitOnGround) return true;
                }
                Dictionary<UUID, int> anims = ExpectedCurrentAnims.Dictionary;
                if (SimAssetStore.Matches(anims.Keys,"sit").Count>0) return true;
                return theAvatar.ParentID != 0;
            }
            set
            {
                if (IsSitting == value) return;
                if (IsControllable)
                {
                    if (value)
                    {
                        SitOnGround();
                    }
                    else
                    {
                        StandUp();
                    }
                }
                Debug("Uncontroled IsStting=" + value);
            }
        }

        public override bool IsControllable
        {
            get
            {
                if (Client == null) return false;
                AgentManager ClientSelf = Client.Self;
                return ClientSelf.AgentID == theAvatar.ID; //|| ClientSelf.LocalID == theAvatar.LocalID;
            }
        }


        public override Simulator GetSimulator()
        {
           // if (Client!=null && Client.Self.AgentID == Prim.ID) return Client.Network.CurrentSim;
            return GetSimRegion().TheSimulator;
        }

        public override Vector3 GetSimPosition()
        {
            //if (Client!=null && Client.Self.AgentID == Prim.ID)
            //{
            //    if (Client.Settings.OBJECT_TRACKING)
            //        return Client.Self.SimPosition;
            //}
            if (theAvatar.ParentID == 0)
            {
                return LastKnownPos = theAvatar.Position;
            }
            return base.GetSimPosition();
        }

        public override Vector3d GetWorldPosition()
        {
            /// if (Client.Self.AgentID == Prim.ID)
            /// {
            ///     if (Client.Settings.OBJECT_TRACKING)
            ///         return Client.Self.GlobalPosition;

            /// }
            return base.GetWorldPosition();
            //return GetSimRegion().LocalToGlobal(GetSimPosition());
        }

        public override SimRegion GetSimRegion()
        {
            return base.GetSimRegion();            
            //if (Prim == null)
            //{
            //    if (_CurrentRegion == null)
            //        if (IsControllable)
            //        {
            //            _CurrentRegion = SimRegion.GetRegion(Client.Network.CurrentSim);
            //        }
            //    return _CurrentRegion;
            //}

            //if (Prim.RegionHandle == 0)
            //{
            //    Console.WriteLine("Don't know the region for " + this);
            //}

            //if (_CurrentRegion == null)
            //{
            //    if (Prim.RegionHandle==0)
            //    {
            //        Console.WriteLine("Dont know the region for " + this);
            //    }
            //    _CurrentRegion = SimRegion.GetRegion(Prim.RegionHandle);
            //    Debug("out of date _CurrentRegion ");
            //}
            //if (theAvatar.RegionHandle != _CurrentRegion.RegionHandle)
            //{
            //    /// Debug("out of date RegionHandle ");
            //    /// _CurrentRegion = null;
            //}
            //return _CurrentRegion;
        }


        public override Quaternion GetSimRotation()
        {
            if (IsControllable)
            {
                if (Client.Settings.OBJECT_TRACKING)
                    return Client.Self.SimRotation;
            }
            /// lock (Prim)
            {
                //if (theAvatar.RegionHandle != _CurrentRegion.RegionHandle)
                //{
                //    Debug("out of date RegionHandle ");
                //}
                return base.GetSimRotation();
            }
        }
        /// public void AddGrass(Simulator simulator, Vector3 scale, Quaternion rotation, Vector3 position, Grass grassType, UUID groupOwner)
        /// {
        /// }
        /// public void AddPrim(Simulator simulator, Primitive.ConstructionData prim, UUID groupID, Vector3 position, Vector3 scale, Quaternion rotation)
        /// {
        /// }
        /// public void AddTree(Simulator simulator, Vector3 scale, Quaternion rotation, Vector3 position, Tree treeType, UUID groupOwner, bool newTree)
        /// {
        /// }
        /// public void AttachObject(Simulator simulator, uint localID, AttachmentPoint attachPoint, Quaternion rotation)
        /// {
        /// }

        /// public static Primitive.ConstructionData BuildBasicShape(PrimType type)
        /// {
        /// }

        /// public ISimObject RezObjectType(ISimObject copyOf)
        /// {
        ///     string treeName = args[0].Trim(new char[] { ' ' });
        ///     Tree tree = (Tree)Enum.Parse(typeof(Tree), treeName);

        ///     Vector3 treePosition = ClientSelf.SimPosition;
        ///     treePosition.Z += 3.0f;

        ///     Client.Objects.AddTree(Client.Network.CurrentSim, new Vector3(0.5f, 0.5f, 0.5f),
        ///         Quaternion.Identity, treePosition, tree, Client.GroupID, false);

        ///     /// ClientSelf.
        ///     return copyOf;
        /// }

        /// public void SortActs(List<SimUsage> acts)
        /// {
        ///     acts.Sort(CompareUsage);
        /// }

      
        public void Do(SimTypeUsage use, SimObject someObject)
        {
            CurrentAction = new BotObjectAction(this, new SimObjectUsage(use, someObject));
        }


        /// <summary>
        ///  
        /// </summary>
        /// <param name="minimum"></param>
        /// <param name="sightRange"></param>
        public void ScanNewObjects(int minimum, double sightRange, bool rootOnly)
        {
            List<SimObject> objects = GetNearByObjects(sightRange, rootOnly);
            ///  ill do this for us: AddKnowns(objects);
            if (KnownSimObjects.Count < minimum)
            {
                if (sightRange < 255)
                    ScanNewObjects(minimum, sightRange + 10, false);
            }
        }

        /// <summary>
        ///  
        /// </summary>
        /// <param name="objects"></param>
        private void AddKnowns(IEnumerable<SimObject> objects)
        {
            lock (objects)
            {
                foreach (SimObject obj in objects)
                {
                    if (obj != this)
                        if (obj.IsRoot || obj.IsTyped)
                        {
                            lock (KnownSimObjects)
                                if (!KnownSimObjects.Contains(obj))
                                {
                                    KnownSimObjects.Add(obj);
                                    IList<SimTypeUsage> uses = obj.GetTypeUsages();
                                    foreach (SimTypeUsage use in uses)
                                    {
                                        lock (_knownTypeUsages)
                                            if (!_knownTypeUsages.Contains(use))
                                            {
                                                _knownTypeUsages.Add(use);
                                            }
                                    }
                                }
                        }
                }
            }
        }

        /// <summary>
        ///  
        /// </summary>
        /// <param name="regionHandle"></param>
        public override void ResetRegion(ulong regionHandle)
        {
            bool changed = (regionHandle != RegionHandle);
            base.ResetRegion(regionHandle);
            if (changed)
            {
                lock (KnownSimObjects) KnownSimObjects.Clear();
                GetKnownObjects();
            }
        }


        /// <summary>
        ///   Avatars approach distance
        /// </summary>
        /// <returns></returns>
        public override float GetSizeDistance()
        {
            return 2f;
        }


        public BotClient GetGridClient()
        {
            /// if (Client != null) return Client;
            /// BotClient Client = WorldSystem.client;
            /// if (theAvatar.ID != ClientSelf.AgentID)
            /// {
            ///     throw new Exception("This avatar " + theAvatar + " has no GridClient");
            /// }
            return Client;
        }

        public void TalkTo(SimAvatar avatar, String talkAbout)
        {
            //SimAvatar avatarWasInDialogWith = ((SimAvatarImpl) avatar).InDialogWith;
            //SimAvatar wasInDialogWith = InDialogWith;
            //try
            {
                SimObject InDialogWith = avatar;
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
            //finally
            //{
            //    InDialogWith = wasInDialogWith;
            //    //avatar.InDialogWith = avatarWasInDialogWith;
            //}
        }

        public void TalkTo(SimAvatar avatar, BotMentalAspect talkAbout)
        {
            ///  TODO find a better text representation (a thought bubble maybe?)
            TalkTo(avatar, "" + talkAbout);
        }

        public override void Debug(string p, params object[] args)
        {
            if (Client != null)
            {
                Client.WorldSystem.WriteLine(String.Format(p, args));
            }
            else
            {
                WorldSystem.WriteLine(String.Format(p, args));                
            }
        }

        public void Eat(SimObject target)
        {
            Debug("!!! EAT " + target);
        }

        public ThreadStart WithSitOn(SimObject obj, ThreadStart closure)
        {
            bool CanUseSit = WorldObjects.CanUseSit;
            return () =>
                       {
                           bool SattedUpon = false;
                           if (CanUseSit)
                           {
                              SattedUpon = SitOn(obj);
                           }

                           try
                           {
                               closure.Invoke();
                           }
                           finally
                           {
                               if (CanUseSit)
                               {
                                   if (SattedUpon) StandUp();
                               }
                           }
                       };
        }

        private void StopAllAnimations()
        {
            Dictionary<UUID, bool> animations = new Dictionary<UUID, bool>();
            foreach (UUID animation in GetCurrentAnims())
            {
                animations[animation] = false;
            }
            //foreach (UUID animation in AddedAnims.Keys)
            //{
            //    animations[animation] = false;
            //}
            //foreach (UUID animation in RemovedAnims.Keys)
            //{
            //    animations[animation] = false;
            //}
            Client.Self.Animate(animations, true);
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

        public ThreadStart WithAnim(SimAsset anim, ThreadStart closure)
        {
            BotClient Client = GetGridClient();
            AssetThread assetThread = new AssetThread(Client.Self, anim);
            return () =>
                       {
                           try
                           {
                               assetThread.Start();
                               closure.Invoke();
                           }
                           finally
                           {
                               assetThread.Stop();
                           }
                       };
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
                if (IsControllable)
                    return Client.Self.Movement.Fly;
                return false;
            }
            set
            {
                if (IsFloating != value)
                {
                    if (IsControllable) Client.Self.Fly(value);
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

        private BotClient Client;

        public void SetClient(BotClient Client)
        {
            lock (Client)
            {
                this.Client = Client;
                Client.Intern("TheBot", this);
                ///   WorldSystem = Client.WorldSystem;
                /// if (Client.Self.AgentID == Prim.ID)
                {
                    ///                    EnsureTrackerRunning();
                }
            }
            /// WorldSystem.AddTracking(this,Client);
        }

        public SimObject FindSimObject(SimObjectType pUse, double maxXYDistance, double maxZDist)
        {
            double myZ = GetWorldPosition().Z;
            IList<SimObject> objects = GetKnownObjects();
            lock (objects) foreach (SimObject O in objects)
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
            BotClient Client = GetGridClient();
            AgentManager ClientSelf = Client.Self;
            bool SAU = Client.Settings.SEND_AGENT_UPDATES;
            try
            {
                Client.Settings.SEND_AGENT_UPDATES = true;
                SimObject UnPhantom = null;
                AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                uint sit = ClientSelf.SittingOn;
                if (sit != 0)
                {
                    Simulator simu = GetSimulator();
                    UnPhantom = WorldSystem.GetSimObject(WorldSystem.GetPrimitive(sit, simu), simu);
                    UnPhantom.MakeEnterable(this);
                }
                Client.Self.Crouch(false);
                ClientSelf.AnimationStart(Animations.STANDUP, true);
                ClientSelf.Stand();
                StopAllAnimations();
                return UnPhantom;
            }
            finally
            {
                Client.Settings.SEND_AGENT_UPDATES = SAU;
            }
        }


        public override void UpdateObject(ObjectUpdate objectUpdate, ObjectUpdate objectUpdateDiff)
        {
        }

        public override void Touch(SimObject simObject)
        {
            if (IsControllable)
            {
                Client.Self.Touch(simObject.Prim.LocalID);
            }
            else
            {
                Debug("Cant touch !Client.Self.AgentID == Prim.ID " + simObject);
            }
        }

        public void RemoveObject(SimObject O)
        {
            KnownSimObjects.Remove(O);
        }

        /// public override SimWaypoint GetWaypoint()
        /// {
        ///     Vector3 v3 = GetWorldPosition();
        ///     SimRegion PathStore = GetSimRegion();
        ///     SimWaypoint swp = PathStore.CreateClosestWaypoint(v3);
        ///     double dist = Vector3.Distance(v3, swp.GetWorldPosition());
        ///     if (!swp.Passable)
        ///     {
        ///         WorldSystem.WriteLine("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
        ///     }
        ///     return swp;
        /// }

        public override void StopMoving()
        {
            lock (TrackerLoopLock)
            {
                ApproachPosition = null;
                lastDistance = float.MaxValue;
                ApproachVector3D = Vector3d.Zero;
            }
            AgentManager ClientSelf = Client.Self;
            ClientSelf.AutoPilotCancel();
            AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
            ///   ClientMovement. AlwaysRun = false;
            ClientMovement.AtNeg = false;
            ClientMovement.AtPos = false;
            /// ClientMovement.AutoResetControls = true;
            ///    ClientMovement. Away = false;
            ClientMovement.FastAt = false;
            ClientMovement.FastLeft = false;
            ClientMovement.FastUp = false;
            ClientMovement.FinishAnim = true;
            ///   ClientMovement. Fly = false;
            ClientMovement.LButtonDown = false;
            ClientMovement.LButtonUp = false;
            ClientMovement.LeftNeg = false;
            ClientMovement.LeftPos = false;
            ClientMovement.MLButtonDown = false;
            ClientMovement.MLButtonUp = false;
            ///  ClientMovement. Mouselook = false;
            ClientMovement.NudgeAtNeg = false;
            ClientMovement.NudgeAtPos = false;
            ClientMovement.NudgeLeftNeg = false;
            ClientMovement.NudgeLeftPos = false;
            ClientMovement.NudgeUpNeg = false;
            ClientMovement.NudgeUpPos = false;
            ClientMovement.PitchNeg = false;
            ClientMovement.PitchPos = false;
            /// ClientMovement. SitOnGround = false;
            /// ClientMovement. StandUp = false;
            ClientMovement.Stop = true;
            ClientMovement.TurnLeft = false;
            ClientMovement.TurnRight = false;
            //ClientMovement.UpdateInterval = 0;
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
            ///  stand up first
            SimObject UnPhantom = StandUp();
            ///  make sure it not going somewhere
            ///  set the new target
            ApproachDistance = obj.GetSizeDistance() + 0.5f;
            string str = "Approaching " + obj + " " + DistanceVectorString(obj) + " to get " + ApproachDistance;
            Debug(str);
            obj.MakeEnterable(this);
            ///  if (!MoveTo(obj.GetWorldPosition(), obj.GetSizeDistance() + 0.5f, 12))
            {
                GotoTarget(obj);
                TurnToward(obj);
            }
            if (UnPhantom != null)
                UnPhantom.RestoreEnterable(this);

            return (double) Distance(obj);
        }

        private readonly object TrackerLoopLock = new object();

        private bool IsBlocked = false;
        private double lastDistance = float.MaxValue;

        private void TrackerLoop()
        {
            Random MyRandom = new Random(DateTime.Now.Millisecond);
            Boolean stopNext = false;
            Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = false;
            Client.Self.Movement.AutoResetControls = true;
            Client.Self.Movement.UpdateInterval = 10;

            while (true)
            {
                Vector3d targetPosition = ApproachVector3D;
                lock (TrackerLoopLock)
                {
                    ///  Debug("TrackerLoop: " + Thread.CurrentThread);
                    if (ApproachVector3D == Vector3d.Zero)
                    {
                        if (ApproachPosition == null)
                        {
                            lastDistance = float.MaxValue;
                            Thread.Sleep(100);
                            continue;
                        }
                    }
                    if (ApproachPosition != null)
                    {
                        targetPosition = ApproachPosition.GetWorldPosition();
                    }
                }
                double realTargetZ = targetPosition.Z;
                Vector3d worldPosition = GetWorldPosition();
                /// ApproachDistance = ApproachPosition.GetSizeDistance();
                try
                {
                    AgentManager ClientSelf = Client.Self;
                    AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                    ClientMovement.AutoResetControls = false;
                    //ClientMovement.UpdateInterval = 0; /// 100
                    SimRegion R = GetSimRegion();
                    float WaterHeight = R.WaterHeight();
                    float selfZ = ClientSelf.SimPosition.Z;
                    double UpDown = realTargetZ - selfZ;

                    double ZDist = Math.Abs(UpDown);
                    ///  Like water areas
                    bool swimming = WaterHeight > selfZ;

                    if (UpDown > 5f)
                    {
                        targetPosition.Z = realTargetZ; ///  selfZ + 0.2f; ///  incline upward
                        if (!ClientMovement.Fly)
                        {
                            Debug("Starting to fly");
                            ClientMovement.Fly = true;
                            SendUpdate(10);
                            ///   selfZ = realTargetZ;
                            TurnToward(targetPosition);
                            ClientMovement.AtPos = true;
                            SendUpdate(10);
                            ApproachVector3D = targetPosition;
///                             continue;
                        }
                    }
                    else
                    {
                        ClientMovement.Fly = false;
                        targetPosition.Z = selfZ;
                    }


                    ///  Reset previous Z 
                    ClientMovement.FastUp = false;
                    ClientMovement.UpPos = false;
                    ClientMovement.UpNeg = false;
                    ClientMovement.NudgeUpPos = false;
                    ClientMovement.NudgeUpNeg = false;

                    double curXYDist = Vector3d.Distance(worldPosition,
                                                         new Vector3d(targetPosition.X, targetPosition.Y, selfZ));

                    double curDist = Vector3d.Distance(worldPosition, targetPosition);
                    IsBlocked = false;
                    if (lastDistance <= curDist)
                    {
                        if (Prim.Velocity == Vector3.Zero)
                            IsBlocked = true;
                        if (ApproachPosition!=null)
                        {
                            // follower should pass by.. but on way point navigation would be ok
                            ClientMovement.Stop = true;
                            ClientMovement.FinishAnim = true;
                            SendUpdate(100);
                            ClientMovement.Stop = false;
                            lastDistance = curDist;
                            continue;
                        }
                    }
                    lastDistance = curDist;


                    if (swimming)
                    {
                        ///  WaterHeight = WaterHeight - 1f;
                        /// if (!ClientMovement.Fly)
                        /// {
                        ///     ClientMovement.Fly = false;
                        /// }

                        bool nudgeUpDownMoves = true;

                        if (selfZ > WaterHeight - 0.5)
                        {
                            ///  Bob downward
                            if (nudgeUpDownMoves)
                                ClientMovement.NudgeUpNeg = true;
                            else
                                ClientMovement.UpNeg = true;
                            SendUpdate(10);
                            /// 
                            ///   continue; /// to keep going up
                        }
                        else
                        {
                            ///   nudge = !nudge;
                            if (selfZ < WaterHeight - 2.5)
                            {
                                ///  Bob upward
                                if (nudgeUpDownMoves)
                                    ClientMovement.NudgeUpPos = true;
                                else
                                    ClientMovement.UpPos = true;
                                SendUpdate(10);
                                ///    continue; /// to keep going up
                            }
                        }
                        targetPosition.Z = WaterHeight - 0.25f;
                    }

                    /// if ()/// ClientMovement.Fly = swimming;///  todo ||  GetPathStore().IsFlyZone(SimPathStore.GlobalToLocal(worldPosition));

                    if (swimming)
                    {
                        ClientMovement.Fly = true;
                        ///  Reset previous Z 
                        ClientMovement.FastUp = false;
                        ClientMovement.UpPos = false;
                        ClientMovement.UpNeg = false;
                        ClientMovement.NudgeUpPos = false;
                        ClientMovement.NudgeUpNeg = false;
                        SendUpdate(10);
                    }

                    /// //  Little Jumps
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
                    /// else
                    /// {
                    ///     if (ClientMovement.Fly)
                    ///     {
                    ///         ClientMovement.NudgeUpPos = false;
                    ///         ///  ClientSelf.Fly(false);
                    ///         ClientMovement.Fly = false;
                    ///     }
                    /// }

                    ///  Far away
                    if (curXYDist > ApproachDistance)
                    {
                        ClientMovement.Stop = false;
                        ClientMovement.FinishAnim = false;
                        TurnToward(targetPosition);
                        ///  getting close though
                        if (curDist < (ApproachDistance*1.25))
                        {
                            /// ClientMovement.AtPos = true;
                            /// SendUpdate(125);
                            /// ClientMovement.Stop = true;
                            ClientMovement.AtPos = false;
                            ClientMovement.NudgeAtPos = true;
                            SendUpdate(100);
                            ClientMovement.NudgeAtPos = false;
                            SendUpdate(100);
                            TurnToward(targetPosition);
                            stopNext = true;
                            continue;
                        }
                        else
                        {
                            TurnToward(targetPosition);
                            ClientMovement.AtPos = true;
                           // ClientMovement.UpdateInterval = 0;
                            SendUpdate(MyRandom.Next(25, 100));
                            ///   TurnToward(targetPosition);
                            /// (int)(25 * (1 + (curDist / followDist)))
                            ///    MoveFast(ApproachPosition);
                            ///     if (ApproachPosition!=null) MoveSlow(ApproachPosition);
                            stopNext = true;
                            continue;
                        }
                    }
                    else
                    {
                        if (stopNext)
                        {
                            ///                             TurnToward(targetPosition);
                            ///  ClientMovement.ResetControlFlags();
                            ClientMovement.AtPos = false;
                            //ClientMovement.UpdateInterval = 0;
                            /// ClientMovement.StandUp = true;
                            /// ClientMovement.SendUpdate();
                            ClientMovement.FinishAnim = true;
                            ClientMovement.Stop = true;
                            SendUpdate(25);
                            ///  WorldSystem.TheSimAvatar.StopMoving();
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
        public override bool MoveTo(Vector3d finalTarget, double maxDistance, float maxSeconds)
        {
            int blockCount = 0;
            IsBlocked = false;
            double currentDist = Vector3d.Distance(finalTarget, GetWorldPosition());
            ///  if (currentDist < maxDistance) return true;
            lock (TrackerLoopLock)
            {
                ///   SimWaypoint P = SimWaypointImpl.CreateGlobal(finalTarget);
                SetMoveTarget(finalTarget);
                ApproachDistance = maxDistance;
            }
            bool IsKnownMoving = false;
            double lastDistance = currentDist;
            long endTick = Environment.TickCount + (int) (maxSeconds*1000);
            while (Environment.TickCount < endTick)
            {
                currentDist = Vector3d.Distance(finalTarget, GetWorldPosition());
                if (!IsKnownMoving)
                {
                    if (Prim.Velocity != Vector3.Zero) IsKnownMoving = true;
                }
                else
                {
                    if (currentDist < maxDistance) return true;
                    if (Prim.Velocity == Vector3.Zero)
                    {
                        Console.Write("!");
                        if (IsBlocked)
                        {
                            blockCount++;
                            if (blockCount > 2)
                            {
                                StopMoving();
                                Debug("BLOCKED!");
                                return false;
                            }
                        }
                    }
                }
                if (currentDist > lastDistance)
                {
                    ///  Console.Write("=");
                    if (currentDist < maxDistance) return true;
                    StopMoving();
                    if (IsBlocked)
                    {
                        Console.Write("=");
                        return false;
                    }
                    return true;
                }
                if (currentDist > maxDistance)
                {
                    Thread.Sleep(40);
                    /// Application.DoEvents();
                    lastDistance = currentDist;
                    continue;
                }
                else
                {
                    Console.Write("+");
                    ///  StopMoving();
                    return true;
                }
            }
            StopMoving();
            Console.Write("-");
            return false;
        }

        public override void SendUpdate(int ms)
        {
            /// Client.Self.Movement.AutoResetControls = true;
            Client.Self.Movement.SendUpdate(true);
            if (ms > 0) Thread.Sleep(ms);
        }

        public override void TeleportTo(SimRegion R, Vector3 local)
        {
            if (!IsControllable)
            {
                throw Error("GotoTarget !Client.Self.AgentID == Prim.ID");
            }
            Client.ExecuteCommand("teleport " + R.RegionName + "/" + local.X + "/" + local.Y + "/" + local.Z);
          //  Client.Self.Teleport(R.RegionName, local);
        }

        public override void SetMoveTarget(SimPosition target, double maxDist)
        {
            if (target == null)
            {
                ApproachVector3D = Vector3d.Zero;
                ApproachPosition = null;
                lastDistance = float.MaxValue;
                return;
            }
            if (ApproachPosition != target)
            {
                lastDistance = float.MaxValue;
                SetMoveTarget(target.GetWorldPosition());
                ApproachDistance = maxDist;
            }
        }


        public void SetMoveTarget(Vector3d target)
        {
            lock (TrackerLoopLock)
            {
                if (target != ApproachVector3D)
                {
                    /// ApproachPosition = target;
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

        private double ApproachDistance = 2f;

        public SimPosition ApproachPosition { get; set; }
        public Vector3d ApproachVector3D { get; set; }

        /// <summary>
        ///  Action template stubs 
        /// </summary>
        public IEnumerable<SimTypeUsage> KnownTypeUsages
        {
            get { return _knownTypeUsages; }
        }

        public bool SitOn(SimObject someObject)
        {
            if (someObject == null) return SitOnGround();

            AgentManager ClientSelf = Client.Self;
            uint local = someObject.Prim.LocalID;

            //already sitting on 
            if (theAvatar.ParentID == local || Client.Self.SittingOn == local)
            {
                Debug("todo should we stand once first? " + someObject);
                return true;
            }

            AutoResetEvent are = new AutoResetEvent(false);
            ObjectManager.AvatarSitChanged OnSitChanged =
                (simulator, avatar, sittingon, oldseat) =>
                {
                    if (avatar == theAvatar)
                    {
                        are.Set();
                    }
                };
            Client.Objects.OnAvatarSitChanged += OnSitChanged;
            try
            {
                ClientSelf.RequestSit(someObject.Prim.ID, Vector3.Zero);
                ClientSelf.Sit();
                if (!are.WaitOne(10000))
                {
                   // return false;
                }
                return local == ClientSelf.SittingOn;
            }
            finally
            {
                Client.Objects.OnAvatarSitChanged -= OnSitChanged;
            }
        }

        public bool SitOnGround()
        {
            Client.Self.Movement.AutoResetControls = false;
            Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
            Client.Self.Movement.UpdateInterval = 0;

            AgentManager ClientSelf = Client.Self;
            AutoResetEvent are = new AutoResetEvent(false);
            ObjectManager.AvatarSitChanged OnSitChanged =
                (simulator, avatar, sittingon, oldseat) =>
                {
                    if (avatar == theAvatar)
                    {
                        are.Set();
                    }
                };
            AvatarManager.AvatarAnimationCallback OnAnimsChanged =
                (UUID avatarID, InternalDictionary<UUID, int> anims) =>
                    {
                        if (avatarID == theAvatar.ID)
                            if (SimAssetStore.IsSitAnim(anims.Dictionary.Keys))
                        {
                            //int seq = anims[Animations.SIT_GROUND];
                            are.Set();
                        }
                    };
            Client.Objects.OnAvatarSitChanged += OnSitChanged;
            Client.Avatars.OnAvatarAnimation += OnAnimsChanged;
            try
            {
                ClientSelf.SitOnGround();
                if (!are.WaitOne(10000))
                {
                    return false;
                }
                return 0 == ClientSelf.SittingOn;
            }
            finally
            {
                Client.Objects.OnAvatarSitChanged -= OnSitChanged;
                Client.Avatars.OnAvatarAnimation -= OnAnimsChanged;
            }
        }

        private Thread ApproachThread; /// = new Thread(TrackerLoop);

        public override bool SetObjectRotation(Quaternion localPos)
        {
            if (!IsRoot)
            {
                Quaternion start = GetSimRotation();
                Quaternion offset = localPos / start;
                SimObject p = Parent;
                return p.SetObjectRotation(p.GetSimRotation() * offset);
            }
            WorldSystem.SetObjectRotation(Prim, localPos);
            return true;
        }

        private static int InTurn = 0;
        public override bool TurnToward(Vector3 target)
        {
            bool prev = Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK;
            int time = Client.Self.Movement.UpdateInterval;
            bool uen = Client.Self.Movement.AutoResetControls;
            if (InTurn>0)
            {
               //throw new InvalidActivityException("two TurnTowards?"); 
            }
            try
            {
                Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
               // Client.Self.Movement.UpdateInterval = 0;
                Client.Self.Movement.AutoResetControls = false;
                InTurn++;
                return TurnToward0(target);
            }
            finally
            {
                Client.Self.Movement.UpdateInterval = time;
                Client.Self.Movement.AutoResetControls = uen;
                Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = prev;
                InTurn--;

            }
        }

        public bool TurnToward0(Vector3 target)
        {
            if (!IsControllable)
            {
                Debug("Cannot COntrol TurnToward " + target);
                return false;
            }

            bool changed = false;
            AgentManager.AgentMovement ClientMovement = Client.Self.Movement;
            Quaternion parentRot = Quaternion.Identity;

            if (Client.Self.SittingOn > 0)
            {
                if (!Client.Network.CurrentSim.ObjectsPrimitives.ContainsKey(Client.Self.SittingOn))
                {
                    Logger.Log("Attempted TurnToward but parent prim is not in dictionary", Helpers.LogLevel.Warning,
                               Client);
                    return false;
                }

                else
                {
                    Primitive parent = WorldSystem.GetPrimitive(Prim.ParentID, Client.Network.CurrentSim);
                    if (parent == null) Debug("cant get parrent ");
                    else
                        parentRot = parent.Rotation;
                }
            }

            {
                int tries = 1;// int.MaxValue;
                Vector3 lp = GetSimPosition();
                bool needsTurn = true;
                while (needsTurn && tries-- > 0)
                {
//                    ClientMovement.ResetControlFlags();
                    double ZDir = ZHeading;
                    Vector3 dif = target - lp;
                    double Wanted = (Math.Atan2(-dif.X, -dif.Y) + Math.PI); // 2Pi= N, 1/2Pi = E
                    double lr = (ZDir - Wanted)*SimPathStore.RAD2DEG;
                    while (lr > 180) lr -= 360;
                    while (lr < -180) lr += 360;
                    if (lr < -20)
                    {
                        //ClientMovement.ResetControlFlags();
                        ClientMovement.TurnRight = true;
                        ClientMovement.YawNeg = true;
                        ClientMovement.SendUpdate(true);
                        Thread.Sleep(200);
                        ClientMovement.TurnRight = false;
                        ClientMovement.YawNeg = false;
                        ClientMovement.SendUpdate(true);
                        //ClientMovement.YawNeg = true;
                        //ClientMovement.SendUpdate(true);
                        //ClientMovement.TurnRight = true;
                       // Thread.Sleep(10);
                        double az = (ZHeading * SimPathStore.RAD2DEG + 30) / SimPathStore.RAD2DEG;
                        float xmul = (float)Math.Cos(az);
                        float ymul = (float)Math.Sin(az);
                        //target = new Vector3(lp.X + xmul, lp.Y - ymul, lp.Z);
                       // Debug("Need to turn " + lr + " for " + target);
                    }
                    else if ( lr > 20)
                    {
                        //ClientMovement.ResetControlFlags();
                        ClientMovement.YawPos = true;
                        ClientMovement.TurnLeft = true;
                        ClientMovement.SendUpdate(true);
                        Thread.Sleep(200);
                        ClientMovement.YawPos = false;
                        ClientMovement.TurnLeft = false;
                        ClientMovement.SendUpdate(true);
                        // ClientMovement.YawPos = true;
                       //// ClientMovement.SendUpdate(true);
                       // ClientMovement.TurnLeft = true;
                       //// Thread.Sleep(10);
                        double az = (ZHeading * SimPathStore.RAD2DEG - 30) / SimPathStore.RAD2DEG;
                        float xmul = (float)Math.Cos(az);
                        float ymul = (float)Math.Sin(az);
                       // target = new Vector3(lp.X + xmul, lp.Y - ymul, lp.Z);
                       // Debug("Need to turn " + lr + " for " + target);
                    } else 
                    {
                        needsTurn = false;
                    }
                    if (lr < -170 || lr > 170)
                    {
                        if(false && IsDrivingVehical)
                        {
                            bool atPos = ClientMovement.AtPos;
                            bool nudgeAtPos = ClientMovement.NudgeAtPos;

                            // hit reverse for a moment
                            ClientMovement.AtPos = false;
                            ClientMovement.NudgeAtPos = false;
                            ClientMovement.AtNeg = true;
                            ClientMovement.SendUpdate(true);
                            Thread.Sleep(200);
                            ClientMovement.AtNeg = false;
                            ClientMovement.SendUpdate(true);

                            ClientMovement.AtPos = atPos;
                            ClientMovement.NudgeAtPos = nudgeAtPos;
                        }

                    }
                }

                Quaternion between = Vector3.RotationBetween(Vector3.UnitX,
                                             Vector3.Normalize(target - Client.Self.SimPosition));
                Quaternion rot = between * (Quaternion.Identity / parentRot);

                Quaternion br = ClientMovement.BodyRotation;
                Quaternion hr = ClientMovement.HeadRotation;

                changed = true;
                ClientMovement.BodyRotation = rot;
                ClientMovement.HeadRotation = rot;
                ClientMovement.Camera.LookAt(Client.Self.SimPosition, target);

                bool prev = Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK;
                try
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                    ClientMovement.SendUpdate(true);
                }
                finally
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = prev;
                }
            }

            return changed;
        }

        protected bool IsDrivingVehical
        {
            get { return Prim.ParentID != 0; }        
        }

        public override bool UpdateOccupied()
        {
            ///  Vector3 pos = GetSimPosition();
            ///  if (SimPathStore.OutOfRegion(pos)) return;
            /// don't change this spot
            /// GetPathStore().SetPassable(pos.X, pos.Y, pos.Z);
            return false;
        }

        #region SimAvatar Members

        public bool IsWalking
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("Walking"));
            }

        }
        public bool IsFlying
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("Flying"));
            }
        }

        public bool IsStanding
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("Standing"));
            }
        }

        public bool IsSleeping
        {
            get
            {
                return Overlaps(GetCurrentAnims(), SimAssetStore.MeaningUUIDs("Sleeping"));
            }
        }

        public ICollection<UUID> GetCurrentAnims()
        {
            return ExpectedCurrentAnims.Dictionary.Keys;
        }

        public IDictionary<UUID,int> GetCurrentAnimDict()
        {
            return ExpectedCurrentAnims.Dictionary;
        }

        private readonly InternalDictionary<UUID, int> ExpectedCurrentAnims = new InternalDictionary<UUID, int>();


        private int AnimSequenceNumber = 0;
        /// public UUID CurrentAmin = UUID.Zero;
        /// <summary>
        ///  Nephrael Rae: [on-object-animation '(avatar "Candie Brooks") "TALK"][on-object-animation '(avatar "Candie Brooks") "STAND_1"][on-object-animation '(avatar "Candie Brooks") "e45fbdc9-af8f-9408-f742-fcb8c341d2c8"]
        /// </summary>
        /// <param name="anims"></param>
        public void OnAvatarAnimations(InternalDictionary<UUID, int> anims)
        {
            Dictionary<UUID, int> RemovedAnims = new Dictionary<UUID, int>();
            Dictionary<UUID, int> AddedAnims = new Dictionary<UUID, int>();
            bool SendAnimEvent = !WorldObjects.UseNewEventSystem;
            //if (!theAvatar.Name.Contains("rael")) return;
            lock (ExpectedCurrentAnims)
            {
                int mostCurrentSequence = int.MinValue;
                int leastCurrentSequence = int.MaxValue;
                GetSequenceNumbers(ExpectedCurrentAnims.Dictionary, ref leastCurrentSequence, ref mostCurrentSequence);

                ///  first time so find the lowest number
                int mostCurrentSequence1 = int.MinValue;
                int leastCurrentSequence1 = int.MaxValue;
                GetSequenceNumbers(anims.Dictionary, ref leastCurrentSequence1, ref mostCurrentSequence1);



                //UUID mostCurrentAnim;// = UUID.Zero;
                ///  List<String> names = new List<String>();
                List<UUID> RemovedThisEvent = new List<UUID>(GetCurrentAnims());
                anims.ForEach(delegate(UUID key)
                                  {
                                      RemovedThisEvent.Remove(key);
                                      int newAnimNumber;
                                      anims.TryGetValue(key, out newAnimNumber);
                                      if (newAnimNumber >= mostCurrentSequence)
                                      {
                                          mostCurrentSequence = newAnimNumber;
                                          WorldObjects.Master.RequestAsset(key, AssetType.Animation, true);
                                          //mostCurrentAnim = key;
                                      }
                                      if (ExpectedCurrentAnims.ContainsKey(key))
                                      {
                                          int oldAnimNumber;
                                          ExpectedCurrentAnims.TryGetValue(key, out oldAnimNumber);
                                          if (oldAnimNumber == newAnimNumber)
                                              ///  still the same
                                          {
                                              AddedAnims.Remove(key);
                                              RemovedAnims.Remove(key);
                                              return;
                                          }
                                          if (oldAnimNumber > newAnimNumber)
                                          {
                                            //  Debug("error oldAnimNumber > newAnimNumber");
                                          }
                                          ///      else
                                          {
                                              if (oldAnimNumber + 1 != newAnimNumber)
                                              {
                                                  RemovedAnims[key] = oldAnimNumber + 1;
                                                  AddedAnims[key] = newAnimNumber;
                                              }
                                          }
                                          return;
                                      }
                                      AddedAnims[key] = newAnimNumber; /// AddedAnims.Add(key, newAnimNumber);
                                      RemovedAnims.Remove(key);
                                      /// int whenSeq = newAnimNumber + 1;
                                      /// if (!RemovedAnimsWhen.ContainsKey(whenSeq))
                                      /// {
                                      ///     RemovedAnimsWhen[whenSeq] = new List<UUID>();
                                      /// }
                                      /// RemovedAnimsWhen[whenSeq].Add(key);
                                  });
                List<UUID> shownRemoved = new List<UUID>();
                List<UUID> showAdded = new List<UUID>();

                ICollection<UUID> AddedThisEvent = new List<UUID>(AddedAnims.Keys);

                foreach (UUID key in RemovedThisEvent)
                {
                    ExpectedCurrentAnims.Dictionary.Remove(key);
                }

                foreach (UUID list in RemovedAnims.Keys)
                {
                    if (RemovedThisEvent.Contains(list))
                    {
                        RemovedThisEvent.Remove(list);
                    }
                }


                List<SimObjectEvent> startStops = new List<SimObjectEvent>();

                //if (SimAnimationStore.IsSitAnim(RemovedThisEvent)) {
                //    if (!SimAnimationStore.IsSitAnim(AddedThisEvent))
                //    {
                //        LogEvent(new SimObjectEvent("StandUp", SimEventType.ANIM, this, ZHeading * SimPathStore.RAD2DEG, GetSimulator().Name, GetSimPosition()));
                //    }
                //}
                //start or stop moving
                List<UUID> RemovedThisEvent0 = new List<UUID>(RemovedThisEvent);
                List<UUID> AddedThisEvent0 = new List<UUID>(AddedThisEvent);
                StartOrStopAnimEvent(RemovedThisEvent0, AddedThisEvent0, "Moving", startStops);
                
                StartOrStopAnimEvent(RemovedThisEvent0, AddedThisEvent0,  "Jumping", startStops);

                StartOrStopAnimEvent(RemovedThisEvent0, AddedThisEvent0, "Sitting", startStops);

                StartOrStopAnimEvent(RemovedThisEvent0, AddedThisEvent0, "Standing", startStops);
                // start or stop flying
                StartOrStopAnimEvent(RemovedThisEvent0, AddedThisEvent0, "Flying", startStops);
                //start or stop sleeping
                StartOrStopAnimEvent(RemovedThisEvent0, AddedThisEvent0, "Laying", startStops);


                //start or stop talking
                //StartOrStopAnimEvent(RemovedThisEvent, AddedThisEvent, SimAnimationStore.IsCommunationAnim, "Commuincation", SimEventType.ANIM, startStops);

               // StartOrStopAnimEvent(RemovedThisEvent, AddedThisEvent, "OtherAnim", startStops);

                foreach (SimObjectEvent evt in startStops)
                {
                    if (evt.Verb == "Flying")
                    {
                        LastEventByName[evt.EventName] = evt;
                        if (evt.EventStatus!=SimEventStatus.Start) continue;
                    }
                    if (evt.Verb == "Standing")
                    {
                        LastEventByName[evt.EventName] = evt;
                        //continue;
                    }
                    if (evt.Verb == "Sitting")
                    {
                        LastEventByName[evt.EventName] = evt;
                        //continue;
                    }
                    if (evt.EventName == "Moving-Start")
                    {
                        LastEventByName[evt.EventName] = evt;
                        //lastEvent = evt;
                        //continue;
                    }
                    //if (evt.EventName == "MovingStop")
                    //{
                    //    object old = GetLastEvent("MovingStart", 2);
                    //    evt.Verb = "MoveTo";
                    //}
                    if (SimEventStatus.Start==evt.EventStatus)
                    {
                        SetPosture(evt);                        
                    }
                  //  LogEvent(evt);
                }

                for (int seq = leastCurrentSequence; seq <= mostCurrentSequence; seq++)
                {
                    if (RemovedAnims.Count > 0)
                    {
                        foreach (KeyValuePair<UUID, int> uuid in RemovedAnims)
                        {
                            if (seq == uuid.Value)
                            {
                                SimAsset a = SimAssetStore.FindOrCreateAsset(uuid.Key, AssetType.Animation);
                                if (RemovedThisEvent0.Contains(uuid.Key))
                                {
                                    LogEvent(new SimObjectEvent("OnAnim", SimEventType.ANIM, SimEventStatus.Stop, this,
                                                                a, GetHeading()));
                                    shownRemoved.Add(uuid.Key);
                                }
                            }
                        }
                    }
                    if (AddedAnims.Count > 0)
                    {
                        foreach (KeyValuePair<UUID, int> uuid in AddedAnims)
                        {
                            if (seq == uuid.Value)
                            {
                                SimAsset a = SimAssetStore.FindOrCreateAsset(uuid.Key, AssetType.Animation);
                                if (AddedThisEvent0.Contains(uuid.Key))
                                {
                                    LogEvent(new SimObjectEvent("OnAnim", SimEventType.ANIM, SimEventStatus.Start, this,
                                                                a, GetHeading()));
                                    showAdded.Add(uuid.Key);
                                }
                            }
                        }
                    }
                }
                leastCurrentSequence = mostCurrentSequence;

                foreach (UUID key in shownRemoved)
                {
                    RemovedThisEvent.Remove(key);
                    RemovedAnims.Remove(key);
                }
                if (SimAssetStore.IsSitAnim(showAdded))
                {

                }
                foreach (UUID key in showAdded)
                {
                    AddedAnims.Remove(key);
                }
            }

            //ExpectedCurrentAnims.Dictionary.Clear();
            ExpectedCurrentAnims.Dictionary = anims.Dictionary;
            /// String newName = WorldSystem.GetAnimationName(mostCurrentAnim);
            /// {
            ///     if (oldAnim != mostCurrentAnim)
            ///     {
            ///         String oldName = WorldSystem.GetAnimationName(oldAnim);
            ///         if (oldName.Length > 4 && newName.Length > 4 &&
            ///             oldName.Substring(0, 5) == newName.Substring(0, 5))
            ///         {
            ///         }
            ///         else
            ///             WorldSystem.SendNewEvent("On-Object-Start-Animation", this, newName);
            ///     }
            /// }
            /// else
            /// {
            ///     WorldSystem.SendNewEvent("On-Object-Start-Animation", this, newName);
            /// }

            /// CurrentAmin = mostCurrentAnim;
            /// SendNewEvent("On-Avatar-Animation", avatar, names);
        }

        private string PostureType;
        private SimObjectEvent LastPostureEvent;
        readonly private object postureLock  = new object();
        private void SetPosture(SimObjectEvent evt)
        {
            lock (postureLock)
            {
                if (PostureType != null)
                {
                    // was the same 
                    if (PostureType == evt.Verb) return;
                    LogEvent(new SimObjectEvent(PostureType + (IsFlying ? "-Flying" : ""), SimEventType.ANIM, SimEventStatus.Stop,
                                                evt.Parameters));
                    PostureType = evt.Verb;
                    LogEvent(new SimObjectEvent(PostureType + (IsFlying ? "-Flying" : ""), SimEventType.ANIM, SimEventStatus.Start,
                                                evt.Parameters));
                }
                PostureType = evt.Verb;
                LastPostureEvent = evt;
            }
        }

        private static void GetSequenceNumbers(IEnumerable<KeyValuePair<UUID, int>> anims, ref int leastCurrentSequence,ref int mostCurrentSequence)
        {
            lock (anims)
            {
                int mostCurrentSequence0 = mostCurrentSequence;
                int leastCurrentSequence0 = leastCurrentSequence;
                foreach (KeyValuePair<UUID, int> i in anims)
                {
                    int newAnimNumber = i.Value;
                    if (newAnimNumber < leastCurrentSequence0)
                    {
                        leastCurrentSequence0 = newAnimNumber;
                    }
                    if (newAnimNumber > mostCurrentSequence0)
                    {
                        mostCurrentSequence0 = newAnimNumber;
                    }
                    WorldObjects.Master.RequestAsset(i.Key, AssetType.Animation, true);

                }
                leastCurrentSequence = leastCurrentSequence0;
                mostCurrentSequence = mostCurrentSequence0;
            }
        }

        //private delegate bool AnimationTest(ICollection<UUID> thisEvent);

        private void StartOrStopAnimEvent(ICollection<UUID> RemovedThisEvent, ICollection<UUID> AddedThisEvent, string name, IList<SimObjectEvent> startStops)
        {
            bool wasStarted = false;
            bool wasStopped = false;
            List<UUID> e = SimAssetStore.MeaningUUIDs(name);
          //  if (e.Count==0) throw new NoSuchElementException(name);
            foreach (UUID list in e)
            {
                if (AddedThisEvent.Contains(list))
                {
                    AddedThisEvent.Remove(list);
                    wasStarted = true;
                }
            }
            foreach (UUID list in e)
            {
                if (RemovedThisEvent.Contains(list))
                {
                    RemovedThisEvent.Remove(list);
                    wasStopped = true;
                }
            }
            if (wasStarted && wasStopped) return;
            if (wasStarted) startStops.Add(new SimObjectEvent(name, SimEventType.ANIM, SimEventStatus.Start, this, GetHeading()));
            if (wasStopped) startStops.Insert(0, new SimObjectEvent(name, SimEventType.ANIM, SimEventStatus.Stop, this, GetHeading()));

        }

        static bool Overlaps(IEnumerable<UUID> c1, IEnumerable<UUID> c2)
        {
            foreach (var uuid in c2)
            {
                foreach (var uuid1 in c1)
                {
                    if (uuid == uuid1) return true;
                }
            }
            return false;
        }


        public object GetLastEvent(String name, int arg)
        {
            return LastEventByName.ContainsKey(name) ? LastEventByName[name].GetArg(arg) : null;
        }

        #endregion
    }


    public interface SimActor : SimAvatar, SimMover
    {
        new SimPosition ApproachPosition { get; set; }
        double Approach(SimObject obj, double maxDistance);
        cogbot.TheOpenSims.BotAction CurrentAction { get; set; }
        void Do(SimTypeUsage use, SimObject someObject);
        void Eat(SimObject target);
        void ExecuteLisp(SimObjectUsage botObjectAction, object lisp);
        SimRegion GetSimRegion();
        bool SitOnGround();
        SimObject StandUp();
        //void StopMoving();
        void TalkTo(SimAvatar avatar, BotMentalAspect talkAbout);
        void TalkTo(SimAvatar avatar, string talkAbout);
        ThreadStart WithAnim(SimAsset anim, ThreadStart closure);
        ThreadStart WithAnim(UUID animID, ThreadStart closure);
        ThreadStart WithGrabAt(SimObject obj, ThreadStart closure);
        ThreadStart WithSitOn(SimObject obj, ThreadStart closure);
        //ICollection<BotAction> GetPossibleActions(double maxXYDistance, double maxZDist);
//        List<BotAction> ScanNewPossibleActions(double maxXYDistance, double maxZDist);
        void SetClient(BotClient Client);
        //BotClient GetGridClient();
        new bool IsSitting { get; set; }
        BotMentalAspect LastAction { get; set; }
        //IEnumerable<SimTypeUsage> KnownTypeUsages { get; }
        bool SitOn(SimObject o);

        BotMentalAspect GetObject(string name);

    }


    public interface SimAvatar : SimObject, PathSystem3D.Navigation.SimMover
    {
        void RemoveObject(SimObject O);
        SimObject FindSimObject(SimObjectType pUse, double maxXYDistance, double maxZDist);
        /// <summary>
        /// Returns hopefully at least three objects sorted by distance
        /// </summary>
        /// <returns></returns>
        List<SimObject> GetKnownObjects();
        void ScanNewObjects(int minimum, double sightRange,bool rootOnly);
        double SightRange { get; set; }
        SimPosition ApproachPosition { get; }
        //BotNeeds CurrentNeeds { get; }
        Avatar theAvatar { get; }
        bool IsSitting { get; }
        float ZHeading { get; }
        IEnumerable<SimTypeUsage> KnownTypeUsages { get; }
        BotMentalAspect LastAction { get; }
        void OnAvatarAnimations(InternalDictionary<UUID, int> anims);

        ICollection<UUID> GetCurrentAnims();
        IDictionary<UUID, int> GetCurrentAnimDict();

        BotClient GetGridClient();
    }
}