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

        public static SimAvatarClient SingleInstance;

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
            SingleInstance = this;
            Affordances.ObjectType.SuperType.Add(SimTypeSystem.GetObjectType("PlayerAvatar"));
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



        //private BotClient Client0;
        public override BotClient Client
        {
            get
            {
                return Client0 ?? base.Client;
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
        Vector3d ApproachVector3D { get; set; }
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

      //  SimPosition ApproachPosition { get; }
    }
}