using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Actions.Pathfinder;
using cogbot.Listeners;
using java.lang;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;
using Boolean=System.Boolean;
using Exception=System.Exception;
using Math=System.Math;
using Object=System.Object;
using String=System.String;
using Thread=System.Threading.Thread;
using System.Drawing;

namespace cogbot.TheOpenSims
{
    public partial class SimAvatarClient : SimAvatarImpl, SimMover, SimAvatar, SimActor, SimControllableAvatar
    {

        ListAsSet<EffectBeamInfo> BeamInfos = new ListAsSet<EffectBeamInfo>();
        ListAsSet<SimPosition> SelectedObjects = new ListAsSet<SimPosition>();
        private bool _SelectedBeam;
        public bool SelectedBeam
        {
            get
            {
                return _SelectedBeam;
            }
            set
            {
                if (_SelectedBeam == value) return;
                foreach (var set in BeamInfos)
                {
                    set.UnSetPointing();
                }
                BeamInfos.Clear();
                if (value)
                {
                    GridClient grc = GetGridClient();
                    foreach (var o in SelectedObjects)
                    {
                        EffectBeamInfo info = new EffectBeamInfo(grc);
                        info.SetPointing(o, 3);
                        BeamInfos.AddTo(info);
                    }
                }
                _SelectedBeam = value;
            }
        }
        ListAsSet<SimPosition> SimActor.GetSelectedObjects()
        {
            return SelectedObjects;
        }

        public void SelectedRemove(SimPosition o)
        {
            if (!SelectedObjects.Remove(o) || !_SelectedBeam) return;
            _SelectedBeam = false;
            SelectedBeam = true;
        }

        public void SelectedAdd(SimPosition o)
        {
            if (!SelectedObjects.AddTo(o) || !_SelectedBeam) return;
            if (true)
            {
                _SelectedBeam = false;
                SelectedBeam = true;
                return;
            }
            EffectBeamInfo info = new EffectBeamInfo(GetGridClient());
            info.SetPointing(o, 3);
            BeamInfos.AddTo(info);
        }


        public override void ThreadJump()
        {
            ///  all the anims here are mainly so we can see what the bot is doing
            (new Thread(
                WithAnim(Animations.SHRUG,
                         WithAnim(Animations.WORRY, () =>
                                                        {
                                                            //Client.Self.Fly(false);
                                                            Thread.Sleep(10);
                                                            Client.Self.Jump(true);
                                                            Thread.Sleep(500);
                                                            Client.Self.Jump(false);
                                                        })))).Start();
        }

        public ThreadStart WithAnim(UUID uUID, ThreadStart threadStart)
        {
            return WithAnim(SimAssetStore.FindOrCreateAsset(uUID, AssetType.Animation), threadStart);
        }

        private DateTime ThisUpdateShown;
        public static int PipesAlive = 0;
        public static int PipesNumberNext = 0;
        public override void IndicateRoute(IEnumerable<Vector3d> list, Color color)
        {
            var PS = GetSimRegion();
            var llist = new List<Vector3>();
            double Z = GlobalPosition.Z;
            foreach (var v3d in list)
            {
                Z = v3d.Z;
                llist.Add(PS.GlobalToLocal(v3d));
            }
            bool haveFirst = false;
            bool haveSecond = false;
            Vector3 prev = Vector3.Zero;
            double atan = 999;

            var CP = PathStore.GetCollisionPlane((float) Z);
            if (CP.lastUpdate != ThisUpdateShown)
            {
                ThisUpdateShown = CP.lastUpdate;
                Client.Self.Chat("http://logicmoo.dyndns.org:5580/cogpath/path.gif", 100, ChatType.Normal);
                //Client.Self.Chat("hi " + Client.Self.SimPosition.Z, 100, ChatType.Normal);
            }
            
            bool throttle = llist.Count < 200;
            foreach (var next in llist)
            {
                if (!haveFirst)
                {
                    haveFirst = true;
                    prev = next;
                    continue;
                }

                Vector3 diff = prev - next;
                double thisAtan = Math.Atan2(diff.X, diff.Y);
                if (Math.Abs(atan-thisAtan)<0.10)
                {
                    atan = thisAtan;
                    continue;
                }
                if (throttle) Thread.Sleep(3);
                                
                if (true)
                {
                    Client.Self.Chat(
                        String.Format("{0},{1},{2},{3},{4},{5}", //color.R, color.G, color.B,
                                      prev.X, prev.Y, prev.Z, next.X, next.Y, next.Z),
                        100, ChatType.Normal);
                }
                else if (true)
                {
                    Client.Self.Chat(
                        String.Format("{0},{1},{2},{3},{4},{5},{6},{7},{8}", color.R, color.G, color.B,
                                      prev.X, prev.Y, prev.Z, next.X, next.Y, next.Z),
                        100, ChatType.Normal);
                }
                else
                {
                    Client.Self.Chat(String.Format("{0},255,0,0,128,{1},{2},{3},{4},{5},{6}",
                                                   PipesNumberNext, prev.X, prev.Y, prev.Z, next.X,
                                                   next.Y, next.Z),
                                     100, ChatType.Normal);
                }

                PipesNumberNext++;
                PipesAlive++;
                prev = next;
            }
        }

        public void KillPipes()
        {
            Client.Self.Chat("die", 100, ChatType.Normal);
            PipesAlive = 0;
            PipesNumberNext = 0;
        }

        public override bool OpenNearbyClosedPassages()
        {
            bool changed = false;
            foreach (var s in GetNearByObjects(20, false))
            {
                if (s.PathFinding.AddCollisions()) changed = true;
            }
            if (!changed)
                foreach (var s in GetNearByObjects(40, false))
                {
                    if (s.PathFinding.AddCollisions()) changed = true;
                }
            //WithAnim(Animations.AFRAID,()=> base.OpenNearbyClosedPassages()).Invoke();
            return OpenNearbyClosedPassages2() || changed;
        }
        public bool OpenNearbyClosedPassages2()
        {
            bool changed = false;
            SimObjectType DOOR = SimTypeSystem.DOOR;
            // look for closed doors

            var UnEnterables = new List<SimObjectPathFinding>();
            foreach (SimObject O in GetNearByObjects(3, false))
            {
                var P = O.PathFinding;
                if (P.AddCollisions()) changed = true;
                if (!O.IsPhantom)
                {
                    if (O.Affordances.IsTypeOf(DOOR) != null)
                    {
                        P.MakeEnterable(this);
                        UnEnterables.Add(P);
                        continue;
                    }
                    if (O.IsSculpted)
                    {
                        P.MakeEnterable(this);
                        UnEnterables.Add(P);
                        continue;
                    }
                }
            }
            if (UnEnterables.Count > 0)
            {
                changed = true;
                new Thread(delegate()
                               {
                                   Thread.Sleep(90000); // 90 seconds
                                   foreach (var O in UnEnterables)
                                   {
                                       O.RestoreEnterable(this);
                                   }
                               }).Start();
            }
            return changed;
        }

        private SimMoverState old;
        private readonly static Dictionary<SimMoverState, UUID> State2Anim = new Dictionary<SimMoverState, UUID>();
        protected override void OnMoverStateChange(SimMoverState obj)
        {

            //return; // todo make sure it doesnt mess up turning animations while moving
            if (State2Anim.Count == 0)
            {
                State2Anim[SimMoverState.THINKING] = Animations.EXPRESS_TOOTHSMILE;
                State2Anim[SimMoverState.TRYAGAIN] = Animations.SURPRISE;
                State2Anim[SimMoverState.BLOCKED] = Animations.WORRY;
            }

            if (old != obj)
            {
                UUID oldAnim;
                if (State2Anim.TryGetValue(old, out oldAnim))
                {
                    if (oldAnim != UUID.Zero) Client.Self.AnimationStop(oldAnim, true);
                }
            }
            UUID newAnim;
            if (State2Anim.TryGetValue(obj, out newAnim))
            {
                if (newAnim != UUID.Zero)
                {
                    Client.Self.AnimationStart(newAnim, true);
                    Thread.Sleep(100);
                }
            } 
            old = obj;
        }

        private Thread actionThread = null;
        private readonly object actionLock = new object();

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
                                _currentAction = null;
                                LastAction.Abort();
                            }
                            catch (Exception ex)
                            {
                                Debug("While aborting last action: " + ex);
                                // already terminated (not abortable)
                            }
                            _currentAction = value;
                        }
                        _currentAction = value;
                        Thread lastActionthread = actionThread;
                        if (value != null)
                        {
                            actionThread = makeActionThread(value);
                        }
                        else
                        {
                            actionThread = null;
                        }
                        try
                        {

                            if (lastActionthread != null)
                            {
                                try
                                {
                                    lastActionthread.Abort();
                                }
                                catch (Exception ex)
                                {
                                    Debug("While aborting last thread: " + ex);
                                    // already terminated (not abortable)
                                }
                            }
                        }
                        finally
                        {
                            if (actionThread != null && value != null)
                            {
                                actionThread.Name = value.ToString();
                                actionThread.IsBackground = false;
                                actionThread.Start();
                            }
                        }
                    }
                }
                catch (Exception ex)
                {
                    Logger.Log(GetName() + " exception " + ex, Helpers.LogLevel.Error, ex);
                }
            }
        }

        private Thread makeActionThread(BotAction value)
        {
            return new Thread(() =>
                                  {
                                      try
                                      {
                                          value.InvokeReal();
                                      }
                                      catch (Exception e)
                                      {
                                          Debug("InvokeReal: " + e);
                                          //throw e;
                                      }
                                      finally
                                      {
                                          //lock (actionLock)
                                          {
                                              if (_currentAction == value)
                                              {
                                                  LastAction = value;
                                                  _currentAction = null;
                                              }
                                          }
                                      }
                                  });

        }

        public SimAvatarClient(UUID id, WorldObjects objectSystem, Simulator reg)
            : base(id, objectSystem, reg)
        {
            WorldObjects.SimAvatars.Add(this);
            Affordances.ObjectType.SuperType.Add(SimTypeSystem.GetObjectType("Avatar"));
            //try
            //{
            //    AspectName = slAvatar.Name;
            //}
            //catch (Exception)
            //{
            //    AspectName += objectSystem.client + "_Avatar_" + slAvatar.LocalID;
            //}
            //MakeEnterable(this);
        }


        public override bool IsRoot
        {
            get { Avatar theAvatar = this.theAvatar; return theAvatar == null || theAvatar.ParentID == 0; }
        }

        ///  public override ISimObject Parent {  get { return this; }   }

        public override bool IsSitting
        {
            get
            {
                if (IsControllable)
                {
                    if (Client.Self.SittingOn != 0) return true;
                    if (Client.Self.Movement.SitOnGround) return true;
                }
                if (SimAssetStore.Matches(GetCurrentAnims(), "sit").Count > 0) return true;
                var theAvatar = this.theAvatar;
                if (theAvatar == null) return false;
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
                var Client = GetGridClient();
                if (Client == null)
                {
                    return false;
                }
                AgentManager ClientSelf = Client.Self;
                lock (HasPrimLock)
                {
                    if (!HasPrim)
                    {
                        // return false;
                    }

                } //|| ClientSelf.LocalID == theAvatar.LocalID;
                return ClientSelf.AgentID == ID;
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

        public InventoryItem TakeObject(SimObject currentPrim)
        {
            if (!IsControllable) return null;

            InventoryItem iitem = null;
            ManualResetEvent ItemsRecieved = new ManualResetEvent(false);

            EventHandler<ItemReceivedEventArgs> onItemReceived = (sender, e) =>
                                                                     {
                                                                         var item = e.Item;
                                                                         if (currentPrim.ID == item.AssetUUID)
                                                                         {
                                                                             iitem = item;
                                                                             ItemsRecieved.Set();
                                                                         }
                                                                     };
            Client.Inventory.ItemReceived += onItemReceived;
            Client.Inventory.RequestDeRezToInventory(currentPrim.LocalID, DeRezDestination.AgentInventoryTake,
                                                     Client.Inventory.FindFolderForType(AssetType.Object), UUID.Zero);

            try
            {
                //30 secs
                if (!ItemsRecieved.WaitOne(30000))
                {
                    return null;
                }
                return iitem;
            }
            finally
            {
                Client.Inventory.ItemReceived -= onItemReceived;
            }
        }


        public bool AttachToSelf(SimObject currentPrim)
        {
            if (!IsControllable) return false;
            return WearItem(TakeObject(currentPrim));
        }

        public bool WearItem(InventoryItem item)
        {
            if (!IsControllable) return false;
            if (item == null) return false;
            Client.Appearance.AddToOutfit(new List<InventoryItem> { item });
            return true;
        }

        public BotClient GetBotClient()
        {
            /// if (Client != null) return Client;
            /// BotClient Client = WorldSystem.client;
            /// if (theAvatar.ID != ClientSelf.AgentID)
            /// {
            ///     throw new Exception("This avatar " + theAvatar + " has no GridClient");
            /// }
            return Client0;
        }

        public GridClient GetGridClient()
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
                var Client = GetGridClient();
                AgentManager ClientSelf = Client.Self;
                AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                TurnToward(InDialogWith);
                ClientSelf.AnimationStop(Animations.TALK, true);
                ClientSelf.AnimationStart(Animations.TALK, true);
                GetBotClient().Talk(InDialogWith + ": " + talkAbout);
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
            string str = DLRConsole.SafeFormat(p, args) + " -'" + GetName() + "'-";
            if (Client0 != null)
            {
                Client0.WorldSystem.WriteLine(str);
                if (Client0.TheRadegastInstance != null)
                {
                    Client0.DisplayNotificationInChat(str);
                }
            }
            else
            {
                WorldSystem.WriteLine(str);
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
            var Client = GetGridClient();
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
            var Client = GetGridClient();
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
            if (lisp == null) return;
            if (lisp is SimTypeUsage)
            {
                SimTypeUsage u = (SimTypeUsage)lisp;
                if (u.LispScript != null)
                {
                    ExecuteLisp(botObjectAction, u.LispScript);
                }
                foreach (SimObjectType ot in botObjectAction.Target.Affordances.ObjectType.SuperType)
                {

                }
                return;
            }

            BotClient Client = GetBotClient();
            {
                Client.Intern("TheBot", this);
                Client.Intern("TheTarget", botObjectAction.Target);
                Client.Intern("TheAction", botObjectAction);
                Client.evalLispCode(lisp);
            }
        }


        public override bool Flying
        {
            get
            {
                if (IsControllable)
                    return Client.Self.Movement.Fly;
                if (base.Flying) return true;
                return IsFlying;
            }
            set
            {
                if (Flying != value)
                {
                    if (IsControllable) Client.Self.Fly(value);
                }
            }
        }


        private BotClient Client0;
        private GridClient Client
        {
            get
            {
                return Client0;
            }
        }

        public void SetClient(BotClient Client)
        {
            lock (Client)
            {
                this.Client0 = Client;
                Client0.Intern("TheBot", this);
                ///   WorldSystem = Client.WorldSystem;
                /// if (Client.Self.AgentID == Prim.ID)
                {
                    ///                    EnsureTrackerRunning();
                }
            }
            /// WorldSystem.AddTracking(this,Client);
        }




        public SimObject StandUp()
        {
            var Client = GetGridClient();
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
                    UnPhantom.PathFinding.MakeEnterable(this);
                }
                Client.Self.Crouch(false);
                ClientSelf.AnimationStart(Animations.STANDUP, true);
                ClientSelf.Stand();
                ClientSelf.AnimationStop(Animations.STANDUP, true);
                // StopAllAnimations();
                return UnPhantom;
            }
            finally
            {
                Client.Settings.SEND_AGENT_UPDATES = SAU;
            }
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


        /// public override SimWaypoint GetWaypoint()
        /// {
        ///     Vector3 v3 = GlobalPosition();
        ///     SimRegion PathStore = GetSimRegion();
        ///     SimWaypoint swp = PathStore.CreateClosestWaypoint(v3);
        ///     double dist = Vector3.Distance(v3, swp.GlobalPosition());
        ///     if (!swp.Passable)
        ///     {
        ///         WorldSystem.WriteLine("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
        ///     }
        ///     return swp;
        /// }
        public override void StopMoving()
        {
            StopMovingReal();
        }
        public override void StopMovingIsProblem()
        {
            StopMovingReal();
        }
        public void StopMovingReal()
        {
            lock (TrackerLoopLock)
            {
                ApproachPosition = null;
                lastDistance = float.MaxValue;
                ApproachVector3D = Vector3d.Zero;
            }
            AgentManager ClientSelf = Client.Self;
            
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
            ClientSelf.AutoPilotCancel();
        }


        /// <summary>
        ///  
        /// </summary>
        /// <param name="obj"></param>
        /// <param name="maxDistance"></param>
        /// <returns></returns>
        public double Approach(SimObject obj, double maxDistance)
        {
            OnlyMoveOnThisThread();
            ///  stand up first
            SimObject UnPhantom = StandUp();
            ///  make sure it not going somewhere
            ///  set the new target
            ApproachDistance = maxDistance;
            string str = "Approaching " + obj + " " + DistanceVectorString(obj) + " to get " + ApproachDistance;
            Debug(str);
            try
            {
                if (DebugLevel > 1) IndicateTarget(obj, true);
                obj.PathFinding.MakeEnterable(this);
                ///  if (!MoveTo(obj.GlobalPosition(), obj.GetSizeDistance() + 0.5f, 12))
                SalientGoto(obj);
                TurnToward(obj);
                SimpleMoveTo(obj.GlobalPosition, maxDistance, 1);
            }
            finally
            {
                if (UnPhantom != null)
                    UnPhantom.PathFinding.RestoreEnterable(this);
                if (DebugLevel > 1) IndicateTarget(obj, false);
            }
            return (double)Distance(obj);
        }



        private readonly object TrackerLoopLock = new object();

        private bool IsBlocked = false;
        private double lastDistance = float.MaxValue;

        private void TrackerLoop()
        {
            Random MyRandom = new Random(DateTime.Now.Millisecond);
            Boolean stopNext = false;
            //Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = false;
            //Client.Self.Movement.AutoResetControls = true;
            //Client.Self.Movement.UpdateInterval = 10;
            DateTime lastDateTime = DateTime.Now;
            DateTime currentDateTime = DateTime.Now;
            while (true)
            {
                lastDateTime = currentDateTime;
                currentDateTime = DateTime.Now;

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
                        targetPosition = ApproachPosition.GlobalPosition;
                    } else
                    {
                        targetPosition = ApproachVector3D;
                    }
                }
                double realTargetZ = targetPosition.Z;
                Vector3d worldPosition = GlobalPosition;
                /// ApproachDistance = ApproachPosition.GetSizeDistance();
                try
                {
                    AgentManager ClientSelf = Client.Self;
                    AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                    //ClientMovement.AutoResetControls = false;
                    //ClientMovement.UpdateInterval = 0; /// 100
                    SimRegion R = GetSimRegion();
                    float WaterHeight = R.WaterHeight();
                    double selfZ = worldPosition.Z;
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
                        if (HasPrim && Prim.Velocity == Vector3.Zero)
                        {
                            var span = currentDateTime.Subtract(lastDateTime);
                            IsBlocked = true;                            
                        }
                        if (ApproachPosition != null)
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
                    if (ZDist * 2 > curDist)
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
                        if (SimpleMoveToMovementProceedure == MovementProceedure.AutoPilot)
                        {
                            ClientSelf.AutoPilot(targetPosition.X, targetPosition.Y, targetPosition.Z);
                            SendUpdate(1000);
                            continue;
                        }
                        if (SimpleMoveToMovementProceedure == MovementProceedure.Teleport)
                        {
                            //ClientSelf.AutoPilot(targetPosition.X, targetPosition.Y, targetPosition.Z);
                            SendUpdate(1000);
                            continue;
                        }
                        ClientMovement.Stop = false;
                        ClientMovement.FinishAnim = false;
                        TurnToward(targetPosition);
                        ///  getting close though
                        if (curDist < (ApproachDistance * 1.25))
                        {
                            /// ClientMovement.AtPos = true;
                            /// SendUpdate(125);
                            /// ClientMovement.Stop = true;
                            ClientMovement.AtPos = false;
                            ClientMovement.NudgeAtPos = true;
                            SendUpdate(100);
                            ClientMovement.NudgeAtPos = false;
                            SendUpdate(100);
                            //TurnToward(targetPosition);
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



        public MovementProceedure SimpleMoveToMovementProceedure = MovementProceedure.TurnToAndWalk;
        public MovementProceedure SalientMovementProceedure = MovementProceedure.AStar;

        /// <summary>
        ///  
        /// </summary>
        /// <param name="finalTarget"></param>
        /// <param name="maxDistance"></param>
        /// <param name="maxSeconds"></param>
        /// <returns></returns>
        public override bool SimpleMoveTo(Vector3d finalTarget, double maxDistance, float maxSeconds)
        {
            if (false)
            {
                Random MyRand = new Random();
                if (MyRand.Next(5) < 2)
                    Client.Self.LookAtEffect(ID, UUID.Zero, finalTarget, (LookAtType) MyRand.Next(11), ID);
            }
            OnlyMoveOnThisThread();
            TurnToward(finalTarget);
            IsBlocked = false;
            double currentDist = DistanceNoZ(finalTarget, GlobalPosition);
            bool adjustCourse;
            ///  if (currentDist < maxDistance) return true;
            switch (SimpleMoveToMovementProceedure)
            {
                case MovementProceedure.Teleport:
                    if (currentDist < maxDistance) return true;
                    bool tp = this.TeleportTo(SimRegion.GetWaypoint(finalTarget));
                    if (currentDist < maxDistance) return true;
                    adjustCourse = false;
                    if (!tp)
                    {
                        Debug("TP failed => MoveToMovementProceedure = MovementProceedure.TurnToAndWalk;");
                        SimpleMoveToMovementProceedure = MovementProceedure.TurnToAndWalk;
                        adjustCourse = true;
                    }
                    TurnToward(finalTarget);
                    lock (TrackerLoopLock)
                    {
                        ///   SimWaypoint P = SimWaypointImpl.CreateGlobal(finalTarget);
                        SetMoveTarget(finalTarget);
                        ApproachDistance = maxDistance;
                    }
                    break;
                case MovementProceedure.AStar:
                    if (currentDist < maxDistance) return true;
                    throw new UnsupportedOperationException("SimpleMoveToMovementProceedure=" +
                                                            SimpleMoveToMovementProceedure);
                    GotoTargetAStar(SimRegion.GetWaypoint(finalTarget));
                    break;
                case MovementProceedure.AutoPilot:
                case MovementProceedure.TurnToAndWalk:
                default:
                    adjustCourse = true;
                    lock (TrackerLoopLock)
                    {
                        ///   SimWaypoint P = SimWaypointImpl.CreateGlobal(finalTarget);
                        SetMoveTarget(finalTarget);
                        ApproachDistance = maxDistance;
                    }
                    break;
            }
            return WaitUntilPosSimple(finalTarget, maxDistance, maxSeconds, true);
        }


        public bool WaitUntilPosSimple(Vector3d finalTarget, double maxDistance, float maxSeconds, bool adjustCourse){
            int blockCount = 0;
            maxDistance += 0.2;
            double currentDist = DistanceNoZ(finalTarget, GlobalPosition);
            if (currentDist < maxDistance) return true;
            bool IsKnownMoving = false;
            double lastDistance = currentDist;
            long endTick = Environment.TickCount + (int)(maxSeconds * 1000);
            while (Environment.TickCount < endTick)
            {
                currentDist = DistanceNoZ(finalTarget, GlobalPosition);
                if (Prim == null)
                {
                    Debug("Where is my body? " + ToString());
                    Thread.Sleep(100);
                    continue;
                }
                var PrimVelocity = Prim.Velocity;
                if (!IsKnownMoving)
                {
                    if (PrimVelocity != Vector3.Zero) IsKnownMoving = true;
                }
                else
                {
                    if (currentDist < maxDistance) return true;
                    if (adjustCourse) if (Prim != null && PrimVelocity == Vector3.Zero && lastDistance == currentDist)
                    {
                        Write("!");
                        if (IsBlocked)
                        {
                            blockCount++;
                            if (blockCount > 3)
                            {
                                if (SimAvatarClient.UseTeleportFallback)
                                {
                                    StopMoving();
                                    Debug("Blocked so using TP to " + (finalTarget - GlobalPosition));
                                    return this.TeleportTo(finalTarget);
                                }
                                Debug("BLOCKED!");
                                //return true;
                                return false;
                            }
                        }
                    }
                }
                if (currentDist > lastDistance + 0.1)
                {
                    if (adjustCourse) StopMoving();
                    ///  Console.Write("=");
                    if (currentDist < maxDistance) return true;
                    if (adjustCourse) StopMoving();
                    if (IsBlocked)
                    {
                        Write("=");
                        Debug("SLIPPING!");
                        return false;
                    }
                    return true;
                }
                if (currentDist <= maxDistance)
                {
                    Write("+");
                    ///  StopMoving();
                    return true;
                }
                Thread.Sleep(40);
                lastDistance = currentDist;
                //continue;
            }
            if (adjustCourse) StopMoving();
            Write("-");
            return false;
        }

        private void Write(string s)
        {
            DLRConsole.DebugWrite(s);
        }

        public override bool SalientGoto(SimPosition pos)
        {
            double maxDistance = pos.GetSizeDistance() + 1;
            switch (SalientMovementProceedure)
            {
                case MovementProceedure.Teleport:
                    StopMoving();
                    return this.TeleportTo(pos);
                case MovementProceedure.AutoPilot:
                case MovementProceedure.TurnToAndWalk:
                    return SimpleMoveTo(pos.UsePosition.GlobalPosition, pos.GetSizeDistance(), 3);

                    // TODO 
                case MovementProceedure.AStar:
                    bool res = GotoTargetAStar(pos);
                    if (res) return res;
                    if (SimAvatarImpl.UseTeleportFallback)
                    {
                        StopMoving();
                        Debug("Goto sneaking in TP to " + pos);
                        res = this.TeleportTo(pos.UsePosition);
                        StopMoving();
                        TurnToward(pos);
                        return res;
                    }
                    SetMoveTarget(pos, maxDistance - 1);
                    Thread.Sleep(1000);
                    StopMoving();
                    if (maxDistance > this.Distance(pos))
                    {
                        return true;
                    }
                    return false;
                default:
                    {
                        throw new UnsupportedOperationException("" + SalientMovementProceedure); 
                    }
            }
        }
        public override void SendUpdate(int ms)
        {
            /// Client.Self.Movement.AutoResetControls = true;
            Client.Self.Movement.SendUpdate(true);
            if (ms > 0) Thread.Sleep(ms);
        }

        public override bool TeleportTo(SimRegion R, Vector3 local)
        {
            if (!IsControllable)
            {
                throw Error("GotoTarget !Client.Self.AgentID == Prim.ID");
            }
            SimPosition pos = R.GetWaypointOf(local);
            Vector3d global = pos.GlobalPosition;
            StopMoving();
            return Client.Self.Teleport(R.RegionHandle, local, local);

            //CmdResult s = Client.ExecuteCommand("teleport " + R.RegionName + "/" + local.X + "/" + local.Y + "/" + local.Z, Debug);
            //return s.Success;
            //  Client.Self.Teleport(R.RegionName, local);
        }

        public override void SetMoveTarget(SimPosition target, double maxDist)
        {
            OnlyMoveOnThisThread();
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
                SetMoveTarget(target.GlobalPosition);
                ApproachDistance = maxDist;
            }
        }

        public Thread MovementConsumer;
        public void OnlyMoveOnThisThread()
        {
            if (MovementConsumer != null)
            {
                if (MovementConsumer != Thread.CurrentThread && MovementConsumer.IsAlive)
                {
                    if (!MovementConsumer.IsBackground)
                    {
                        MovementConsumer.Abort();
                    }
                }
            }
            //throw new NotImplementedException();
            MovementConsumer = Thread.CurrentThread;
        }

        public void SetMoveTarget(Vector3d target)
        {
            OnlyMoveOnThisThread();
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
                    ApproachThread.IsBackground = true;
                    ApproachThread.Start();
                }
            }
        }


        public bool SitOn(SimObject someObject)
        {
            if (someObject == null) return SitOnGround();

            AgentManager ClientSelf = Client.Self;
            uint local = someObject.LocalID;

            //already sitting on 
            Avatar theAvatar = this.theAvatar;
            if (theAvatar == null) return false;
            if (theAvatar.ParentID == local || Client.Self.SittingOn == local)
            {
                Debug("todo should we stand once first? " + someObject);
                return true;
            }

            AutoResetEvent are = new AutoResetEvent(false);
            EventHandler<AvatarSitChangedEventArgs> OnSitChanged =
                (s, e) =>
                    {
                        if (e.Avatar == theAvatar)
                        {
                            are.Set();
                        }
                    };
            Client.Objects.AvatarSitChanged += OnSitChanged;
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
                Client.Objects.AvatarSitChanged -= OnSitChanged;
            }
        }

        public bool SitOnGround()
        {
            //Client.Self.Movement.AutoResetControls = false;
            //Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
            //Client.Self.Movement.UpdateInterval = 0;

            AgentManager ClientSelf = Client.Self;
            AutoResetEvent are = new AutoResetEvent(false);
            Avatar theAvatar = this.theAvatar;
            if (theAvatar == null) return false;
            EventHandler<AvatarSitChangedEventArgs> OnSitChanged =
                (s, e) =>
                    {
                        if (e.Avatar == theAvatar)
                        {
                            are.Set();
                        }
                    };
            EventHandler<AvatarAnimationEventArgs> OnAnimsChanged =
                (s, e) =>
                    {
                        if (e.AvatarID == theAvatar.ID)
                            if (SimAssetStore.IsSitAnim(GetAnimUUIDs(e.Animations)))
                            {
                                //int seq = anims[Animations.SIT_GROUND];
                                are.Set();
                            }
                    };
            Client.Objects.AvatarSitChanged += OnSitChanged;
            Client.Avatars.AvatarAnimation += OnAnimsChanged;
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
                Client.Objects.AvatarSitChanged -= OnSitChanged;
                Client.Avatars.AvatarAnimation -= OnAnimsChanged;
            }
        }

        private Thread ApproachThread; /// = new Thread(TrackerLoop);

        public override bool SetObjectRotation(Quaternion localPos)
        {
            if (!IsRoot)
            {
                Quaternion start = SimRotation;
                Quaternion offset = localPos / start;
                SimObject p = Parent;
                return p.SetObjectRotation(p.SimRotation * offset);
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
            if (InTurn > 0)
            {
                //throw new InvalidActivityException("two TurnTowards?"); 
            }
            try
            {
                Client.Settings.SEND_AGENT_UPDATES = true;
                Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                Client.Self.Movement.TurnToward(target);
                Client.Self.Movement.UpdateInterval = 10;
                Client.Self.Movement.AutoResetControls = true;
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
            //my philosophy is if the body and Head roation is facing hood ornimant.. the controls right/left are applicable .. is someone screws up and makes my avatar stand on it head backwards.. thats still fine i can still locate the hood ornimant.. i just have to hit the opposite directions left/right.. there is no guesswork.. just math.. sometimes whne i want my avatar to ratote left.. and hes in a car.. the only choice i have is the buttons.. the head roation is useless.. so basically libomv declares.. if your sitting on something.. dont expect stuff to work. what bothrs me is why does it do what i want and probably anyone else who wants a bot to dirve a prim wants.. .. whats all this trouble about sitting ona  prim and gettign world position
            if (!IsControllable)
            {
                Debug("Cannot COntrol TurnToward " + target);
                return false;
            }

            bool changed = false;
            AgentManager.AgentMovement ClientMovement = Client.Self.Movement;

            if (!IsDrivingVehical)
            {
                bool prev = Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK;
                try
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                    ClientMovement.AutoResetControls = false;
                    ClientMovement.TurnToward(target);
                    ClientMovement.SendUpdate(true, Client.Network.CurrentSim);
                    //ClientMovement.SendUpdate(true);
                }
                finally
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = prev;
                }
                return true;
            }

            Quaternion parentRot = Quaternion.Identity;

            Avatar Prim = theAvatar;
            if (Prim.ParentID != 0)
            {
                Primitive parent = null;
                if (_Parent != null)
                {
                    parent = _Parent.Prim;
                }
                if (parent == null)
                {
                    parent = WorldSystem.GetPrimitive(Prim.ParentID, Client.Network.CurrentSim);
                }
                if (parent == null)
                {
                    Logger.Log("Attempted TurnToward but parent prim is not in dictionary", Helpers.LogLevel.Warning,
                               Client);
                    parent = WorldSystem.GetPrimitive(Prim.ParentID, Client.Network.CurrentSim);
                }
                if (parent == null) Debug("cant get parrent ");
                else
                {
                    parentRot = parent.Rotation;
                    RequestedParent = true;
                }
            }

            {
                int tries = 1;// int.MaxValue;
                Vector3 lp = SimPosition;
                bool needsTurn = true;
                while (needsTurn && tries-- > 0)
                {

                    // ClientMovement.ResetControlFlags();
                    double ZDir = ZHeading;
                    Vector3 dif = target - lp;
                    double Wanted = (Math.Atan2(-dif.X, -dif.Y) + Math.PI); // 2Pi= N, 1/2Pi = E
                    double lr = (ZDir - Wanted) * SimPathStore.RAD2DEG;
                    while (lr > 180) lr -= 360;
                    while (lr < -180) lr += 360;
                    if (lr < -20)
                    {
                        //ClientMovement.ResetControlFlags();
                        ClientMovement.TurnRight = true;
                        ClientMovement.YawNeg = true;
                        ClientMovement.SendUpdate(true);
                        Thread.Sleep(800);
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
                    else if (lr > 20)
                    {
                        //ClientMovement.ResetControlFlags();
                        ClientMovement.YawPos = true;
                        ClientMovement.TurnLeft = true;
                        ClientMovement.SendUpdate(true);
                        Thread.Sleep(800);
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
                    }
                    else
                    {
                        needsTurn = false;
                    }

                    // use reverse?
                    if (lr < -170 || lr > 170)
                    {
                        if (IsDrivingVehical)
                        {
                            bool atPos = ClientMovement.AtPos;
                            bool nudgeAtPos = ClientMovement.NudgeAtPos;

                            // hit reverse for a moment
                            ClientMovement.AtPos = false;
                            ClientMovement.NudgeAtPos = false;
                            ClientMovement.AtNeg = true;
                            ClientMovement.SendUpdate(true);
                            Thread.Sleep(800);
                            ClientMovement.AtNeg = false;
                            ClientMovement.SendUpdate(true);

                            ClientMovement.AtPos = atPos;
                            ClientMovement.NudgeAtPos = nudgeAtPos;
                        }

                    }
                }

                Quaternion between = Vector3.RotationBetween(Vector3.UnitX,
                                                             Vector3.Normalize(target - SimPosition));
                Quaternion rot = between * (Quaternion.Identity / parentRot);

                Quaternion br = ClientMovement.BodyRotation;
                Quaternion hr = ClientMovement.HeadRotation;

                changed = true;
                ClientMovement.BodyRotation = rot;
                ClientMovement.HeadRotation = rot;
                ClientMovement.Camera.LookAt(SimPosition, target);

                bool prev = Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK;
                try
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                    ClientMovement.TurnToward(target);
                    //ClientMovement.SendUpdate(true);
                }
                finally
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = prev;
                }
            }

            return changed;
        }

    }
    public interface SimActor : SimControllableAvatar, SimMover, SimObjectPathMover
    {
        //new SimPosition ApproachPosition { get; set; }
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
        BotAction LastAction { get; set; }
        bool SelectedBeam { get; set; }
        //IEnumerable<SimTypeUsage> KnownTypeUsages { get; }
        bool SitOn(SimObject o);

        BotMentalAspect GetObject(string name);
        ListAsSet<SimPosition> GetSelectedObjects();
        void SelectedRemove(SimPosition position);
        void SelectedAdd(SimPosition position);
        bool AttachToSelf(SimObject prim);
        InventoryItem TakeObject(SimObject prim);
        bool WearItem(InventoryItem item);
    }

    public interface SimControllableAvatar : SimAvatar, SimAvatarSight, SimObjectPathMover
    {
        GridClient GetGridClient();
        BotClient GetBotClient();

        SimPosition ApproachPosition { get; }
    }
}