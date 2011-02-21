using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Text;
using System.Threading;
using cogbot.Actions;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Packets;
using OpenMetaverse.StructuredData;
using PathSystem3D.Navigation;
using cogbot.Utilities;

namespace cogbot.Listeners
{

    public partial class WorldObjects
    {
        public static readonly Dictionary<ulong, WorldObjects> SimMaster = new Dictionary<ulong, WorldObjects>();
        /// <summary>
        /// This is all bot simulator references: the Count would be Bots*Regions
        /// </summary>
        private static readonly ListAsSet<Simulator> _AllSimulators = new ListAsSet<Simulator>();
        public static WorldObjects GridMaster;
        private static bool RequestedGridInfos = false;
        private bool IsConnected = false;
        private bool RegisterAllOnce = false;
        private readonly ListAsSet<ulong> MasteringRegions = new ListAsSet<ulong>();
        private bool RequestParcelObjects;

        internal static IEnumerable<Simulator> AllSimulators
        {
            get
            {
                List<Simulator> sims = null;

                lock (_AllSimulators)
                {
                    sims = new List<Simulator>(_AllSimulators);
                }

                return sims;
            }
        }

        static readonly List<PacketType> PacketTypeRegional = new List<PacketType>
                                                                  {
                                                                      PacketType.ViewerEffect,
                                                                      PacketType.SoundTrigger,
                                                                      PacketType.AvatarAnimation,
                                                                      PacketType.AgentAnimation
                                                                  };

        private bool Network_SkipEvent(PacketType type, Simulator sim)
        {
            if (PacketTypeRegional.Contains(type))
            {
                if (!IsMaster(sim)) return true;
            }
            return false;
        }

        public bool IsMaster(Simulator simulator)
        {
            lock (MasteringRegions)
            {
                if (MasteringRegions.Contains(simulator.Handle)) return true;
                lock (SimMaster)
                {
                    if (!SimMaster.ContainsKey(simulator.Handle))
                    {
                        SimMaster[simulator.Handle] = this;
                        MasteringRegions.AddTo(simulator.Handle);
                        return true;
                    }
                }
                return false;
            }
        }

        public bool IsRegionMaster
        {
            get { return IsMaster(client.Network.CurrentSim); }
        }

        public bool IsGridMaster
        {
            get { return GridMaster == this; }
        }

        public IEnumerable<string> GroupNames
        {
            get { return _defaultProvider.GroupNames; }
        } 

        public override void Parcels_OnSimParcelsDownloaded(object sender,
                                                            SimParcelsDownloadedEventArgs e)
        {
            EnsureSimulator(e.Simulator);
            //base.Parcels_OnSimParcelsDownloaded(simulator, simParcels, parcelMap);
        }

        public override void Network_OnConnected(object sender)
        {
            //Network_OnSimConnectedHook( (Simulator)sender);
            base.Network_OnConnected(sender);
            if (sender != client.gridClient)
            {
                Logger.DebugLog("wrong client " + sender);
               // client =(BotClient)(GridClient)sender;
              // throw new ArgumentException("wrong client " + sender);
            }
            //            RequestGridInfos();
        }

        private void RequestGridInfos(ulong regionHandle)
        {
            if (!RequestedGridInfos)
            {
                int Range = 0;
                RequestedGridInfos = true;
                uint X;
                uint Y;

                Utils.LongToUInts(regionHandle, out X, out Y);
                X /= 256;
                Y /= 256;
                if (X < 2) X = 2;
                else if (X > 65533) X = 65533;
                if (Y < 2) Y = 2;
                else if (Y > 65533) Y = 65533;
                client.Grid.RequestMapBlocks(GridLayerType.Objects, (ushort)(X - Range), (ushort)(Y - Range),
                                             (ushort)(X + Range), (ushort)(Y + Range), false);
                //client.Grid.RequestMainlandSims(GridLayerType.Objects);
                //  client.Grid.RequestMainlandSims(GridLayerType.Terrain);
                //   client.Grid.RequestMapLayer(GridLayerType.Objects);
                // client.Grid.RequestMapLayer(GridLayerType.Terrain);
            }
        }

        public void Network_OnSimConnectedHook(object sender, SimConnectedEventArgs e)
        {            
            Simulator simulator = e.Simulator;
            ///base.Network_OnSimConnected(simulator);
            lock (WorldObjectsMasterLock)
            {
                if (simulator.Handle == 0)
                {
                    Debug("Simulator Handle==0 for " + simulator);
                    return;
                }
                EnsureSimulator(simulator);
                IsConnected = true;
                if (SimRegion.IsMaster(simulator, client.gridClient))
                {
                    Debug("---SIMMASTER---------" + client + " region: " + simulator);
                    SetWorldMaster(true);
                    SimMaster[simulator.Handle] = this;
                    //client.Grid.RequestMapRegion(simulator.Name, GridLayerType.Objects);
                    client.Grid.RequestMapRegion(simulator.Name, GridLayerType.Terrain);
                    client.Estate.RequestInfo();
                    //client.Grid.RequestMapRegion(simulator.Name, GridLayerType.LandForSale);
                    //client.Grid.RequestMapItems(simulator.Handle,OpenMetaverse.GridItemType.Classified,GridLayerType.Terrain);
                    MasteringRegions.Add(simulator.Handle);
                    if (simulator == client.Network.CurrentSim)
                    {
                        lock (MaintainSimCollisionsList)
                        {
                            if (!MaintainSimCollisionsList.Contains(simulator.Handle))
                            {
                                MaintainSimCollisionsList.Add(simulator.Handle);
                            }
                        }
                    }
                    //RequestGridInfos(simulator.Handle);
                }
                else
                {
                    Debug("-----NOT SIMMASTER-------" + client + " region: " + simulator);
                    MasteringRegions.Remove(simulator.Handle);
                    if (MasteringRegions.Count == 0)
                    {
                        SetWorldMaster(false);
                        Debug("------UNREGISTERING------" + client);
                    }
                }
            }
            if (simulator == client.Network.CurrentSim)
            {
                StartupPostLoginQueues();
                // new Thread(() => client.Appearance.SetPreviousAppearance(true)).Start();
            }
            //if (simulator == client.Network.CurrentSim) { new Thread(() => { Thread.Sleep(30000); client.Appearance.SetPreviousAppearance(true); }).Start(); }
        }

        public void CheckConnected(Simulator simulator)
        {
            if (!IsConnected)
            {
                Network_OnSimConnectedHook(this,new SimConnectedEventArgs(simulator));
            }
        }


        public override void Network_OnSimConnecting(object sender, SimConnectingEventArgs e)
        {
            //LeaveSimulator(simulator);
            e.Cancel = false;// base.Network_OnSimConnecting(simulator);
        }
                                          
        public override void Network_OnEventQueueRunning(object sender, EventQueueRunningEventArgs e)
        {
            var simulator = e.Simulator;
            //if (simulator == client.Network.CurrentSim) { new Thread(() => client.Appearance.WearOutfit(new string[] { "Clothing", "Default" })).Start(); }
            if (string.IsNullOrEmpty(simulator.Name))
            {
           //    simulator.Client.Grid.RequestMapItems(simulator.Handle,GridItemType.AgentLocations,GridLayerType.Terrain); 
            }
           // base.Network_OnEventQueueRunning(simulator);
            if (simulator == client.Network.CurrentSim)
            {
                StartupPostLoginQueues();
                // new Thread(() => client.Appearance.SetPreviousAppearance(true)).Start();
            }
            EnsureSimulator(simulator);
        }

        static bool DidStartupPostLoginQueues = false;
        static readonly object DidStartupPostLoginQueuesLock = new object();
        
        private void StartupPostLoginQueues()
        {
            lock (DidStartupPostLoginQueuesLock)
            {
                if (DidStartupPostLoginQueues) return;
                DidStartupPostLoginQueues = true;
            }
            ParentGrabber.Start();
            CatchUpQueue.Start();
            MetaDataQueue.Start();
            OnConnectedQueue.Start();
            SimAssetStore.TaskQueueStart();
            SlowConnectedQueue.Enqueue(() => SlowConnectedQueue.DebugQueue = false);
            SlowConnectedQueue.Start();
        }

        public override void Network_OnCurrentSimChanged(object sender, SimChangedEventArgs e)
        {
            var PreviousSimulator = e.PreviousSimulator;
            if (TheSimAvatar.GetSimulator() == PreviousSimulator)
            {
                Debug("TheSimAvatar._CurrentRegion.TheSimulator == PreviousSimulator " + PreviousSimulator);
            }
            On_ChangeSims(PreviousSimulator, client.Network.CurrentSim);
        }

        private void LeaveSimulator(Simulator simulator)
        {
            if (IsMaster(simulator))
            {
                Debug("SIM LOOSING ITS MASTER!" + this + " " + simulator);
                MasteringRegions.Remove(simulator.Handle);
                FindNewMaster(simulator.Handle);
            }
            if (TheSimAvatar.GetSimulator() == simulator)
            {
                Debug("TheSimAvatar._CurrentRegion.TheSimulator == simulator " + simulator);
            }
        }

        private void FindNewMaster(ulong handle)
        {
            IEnumerable<Simulator> _AllSimulators = AllSimulators;
            SimRegion R = SimRegion.GetRegion(handle);
            lock (_AllSimulators)
            {
                foreach (var simulator in _AllSimulators)
                {
                    if (simulator.Client == client.gridClient) continue;
                    if (simulator.Handle != handle) continue;
                    GridClient cl = simulator.Client;
                    R.SetMaster(cl);
                    BotClient bc = BotClientFor(cl);
                    if (SimRegion.IsMaster(simulator, bc.gridClient))
                    {
                        bc.WorldSystem.MasteringRegions.Add(handle);
                    }
                    else
                    {
                        bc.WorldSystem.MasteringRegions.Add(handle);
                    }
                    Debug("Found a strong new client for region " + R + " as " + cl);
                    return;
                }
                foreach (var simulator in _AllSimulators)
                {
                 //   if (simulator.Client == client.gridClient) continue;
                    if (simulator.Handle != handle) continue;
                    GridClient cl = simulator.Client;
                    BotClient bc = BotClientFor(cl);
                    if (SimRegion.IsMaster(simulator, bc.gridClient))
                    {
                        bc.WorldSystem.MasteringRegions.Add(handle);
                    }
                    if (!cl.Network.Simulators.Contains(simulator)) continue;
                    R.SetMaster(cl);
                    Debug("Found a new client for region " + R + " as " + cl);
                    return;
                }
            }
            Debug("UHT OH, No client is Mastering for region " + R);
        }

        public override void Self_OnTeleport(object sender, TeleportEventArgs e)
        {
            base.Self_OnTeleport(sender, e);
        }

        public override void Self_OnRegionCrossed(object sender, RegionCrossedEventArgs e)
        {
            Simulator oldSim = e.OldSimulator;
            Simulator newSim = e.NewSimulator;
            On_ChangeSims(oldSim, newSim);
            base.Self_OnRegionCrossed(sender,e);
        }

        private void On_ChangeSims(Simulator PreviousSimulator, Simulator newSim)
        {

            if (PreviousSimulator != null)
            {
                LeaveSimulator(PreviousSimulator);
                //  new Thread(() => client.Appearance.SetPreviousAppearance(false)).Start();
            }
            else
            {
                //                new Thread(() => client.Appearance.WearOutfit(new string[] { "Clothing", "Default", "IRobot" })).Start();
            }
            EnsureSimulator(newSim);
            TheSimAvatar.ResetRegion(newSim.Handle);
        }

        public override void Network_OnSimDisconnected(object sender, SimDisconnectedEventArgs e)
        {
            var simulator = e.Simulator;
            var reason = e.Reason;
            base.Network_OnSimDisconnected(sender, e);
            RemoveSim(simulator);
            SimRegion.GetRegion(simulator).RemoveSim(simulator);            
            LeaveSimulator(simulator);
            
            if (simulator == client.Network.CurrentSim)
                PropertyQueue.AddFirst(() =>
                                           {
                                               Debug("CLOSE for region " + simulator);
                                               //client.Login();
                                           }
                    );
        }

        private static Exception WhoIsIn;



        private void RemoveSim(Simulator simulator)
        {
            lock (_AllSimulators)
            {
                _AllSimulators.Remove(simulator);

            }
        }

        public override void Network_OnDisconnected(object sender, DisconnectedEventArgs e)
        {            
            {
                foreach (var simulator in AllSimulators)
                {
                    if (simulator.Client!=client.gridClient)
                    {
                        if (GridMaster==this && simulator.Connected)
                        {
                            GridMaster = BotClientFor(simulator.Client).WorldSystem;
                            Debug("Changed GridMaster to " + GridMaster);
                        }
                        continue;
                    }
                    RemoveSim(simulator);
                    SimRegion.GetRegion(simulator).RemoveSim(simulator);
                    LeaveSimulator(simulator);
                }

            }
            base.Network_OnDisconnected(sender, e);
        }

        public override void Grid_OnGridRegion(object sender, GridRegionEventArgs e)
        {
            var region = e.Region;
            SimRegion R = SimRegion.GetRegion(region.RegionHandle, client);
            if (R != null)
            {
                R.GridInfo = region;
                R.TheWorldSystem = this;
                R.RegionMaster = client;
            }

            // base.Grid_OnGridRegion(region);
        }


        public override void Grid_OnRegionHandleReply(object sender, RegionHandleReplyEventArgs e)
        {
            var regionHandle = e.RegionHandle;
            var regionID = e.RegionID;
            if (regionHandle==0) return;
            RegisterUUID(regionID, GetRegion(regionHandle));
            base.Grid_OnRegionHandleReply(sender, e);
        }


        public override void RegisterAll()
        {
            lock (WorldObjectsMasterLock)
            {
                if (!RegisterAllOnce)
                {
                    RegisterAllOnce = true;
                    base.RegisterAll();
                    /* client.Sound.OnAttachSound -= Sound_OnAttachSound;
                     client.Sound.OnAttachSoundGainChange -= Sound_OnAttachSoundGainChange;
                     client.Sound.OnSoundTrigger -= Sound_OnSoundTrigger;
                     client.Sound.OnPreloadSound -= Sound_OnPreloadSound;
                     */

                    // Sound manager
                    client.Network.RegisterCallback(PacketType.AttachedSound, new EventHandler<PacketReceivedEventArgs>(AttachedSoundHandler));
                    client.Network.RegisterCallback(PacketType.AttachedSoundGainChange, new EventHandler<PacketReceivedEventArgs>(AttachedSoundGainChangeHandler));
                    client.Network.RegisterCallback(PacketType.PreloadSound, new EventHandler<PacketReceivedEventArgs>(PreloadSoundHandler));
                    client.Network.RegisterCallback(PacketType.SoundTrigger, new EventHandler<PacketReceivedEventArgs>(SoundTriggerHandler));

                    client.Sound.AttachedSound -= Sound_OnAttachSound;
                    client.Sound.AttachedSoundGainChange -= Sound_OnAttachSoundGainChange;
                    client.Sound.SoundTrigger -= Sound_OnSoundTrigger;
                    client.Sound.PreloadSound -= Sound_OnPreloadSound;


                    // Mean collision
                    client.Network.RegisterCallback(PacketType.MeanCollisionAlert,
                                                    new EventHandler<PacketReceivedEventArgs>(MeanCollisionAlertHandler));
                    client.Self.MeanCollision -= Self_OnMeanCollision;



                    // Viewer effect callback
                    client.Network.RegisterCallback(PacketType.ViewerEffect,
                                                    new EventHandler<PacketReceivedEventArgs>(ViewerEffectHandler));
                    client.Avatars.ViewerEffectPointAt -= Avatars_OnPointAt;
                    client.Avatars.ViewerEffectLookAt -= Avatars_OnLookAt;
                    client.Avatars.ViewerEffect -= Avatars_OnEffect;


                    // Avatar appearance
                    client.Network.RegisterCallback(PacketType.AvatarAppearance,
                                                    new EventHandler<PacketReceivedEventArgs>(AvatarAppearanceHandler));


                    client.Network.RegisterCallback(PacketType.AvatarAnimation, new EventHandler<PacketReceivedEventArgs>(AvatarAnimationHandler));
                    client.Avatars.AvatarAnimation -= Avatars_OnAvatarAnimation;

                    
                    // raises these events already
                    client.Assets.UploadProgress -= Assets_OnUploadProgress; // On-Upload-Progress
                    client.Self.CameraConstraint -= Self_OnCameraConstraint;


                    client.Settings.PIPELINE_REQUEST_TIMEOUT = 60000;


                    client.Objects.ObjectPropertiesUpdated += Objects_OnPrimitiveProperties;
                    client.Objects.TerseObjectUpdate += Objects_OnObjectUpdated;
                    client.Objects.ObjectProperties += Objects_OnObjectProperties;
                    client.Objects.ObjectDataBlockUpdate += Objects_OnObjectDataBlockUpdate;

                    client.Network.RegisterEventCallback("AgentGroupDataUpdate", new Caps.EventQueueCallback(AgentGroupDataUpdateHandler));
                    // deprecated in simulator v1.27
                    client.Network.RegisterCallback(PacketType.AgentGroupDataUpdate, new EventHandler<PacketReceivedEventArgs>(AgentGroupDataUpdatePT));

                }
            }
        }

        public override void UnregisterAll()
        {
            lock (WorldObjectsMasterLock)
            {
                if (RegisterAllOnce)
                {
                    RegisterAllOnce = false;
                    base.UnregisterAll();
                    client.Network.UnregisterCallback(PacketType.ViewerEffect,
                                new EventHandler<PacketReceivedEventArgs>(ViewerEffectHandler));

                    client.Objects.ObjectPropertiesUpdated -= Objects_OnPrimitiveProperties;
                    client.Objects.TerseObjectUpdate -= Objects_OnObjectUpdated;
                    client.Objects.ObjectProperties -= Objects_OnObjectProperties;
                    client.Objects.ObjectDataBlockUpdate -= Objects_OnObjectDataBlockUpdate;
                    RegisterThinClient();
                }
            }
        }

        public override void Network_OnSimConnected(object sender, SimConnectedEventArgs e)
        {
            Simulator simulator = e.Simulator;
            EnsureSimulator(simulator);
        }

        private void RegisterThinClient()
        {
            client.Network.LoginProgress += Network_OnLogin;
            //client.Network.OnConnected += Network_OnConnected;
            client.Network.LoggedOut += Network_OnLogoutReply;
            client.Network.SimConnecting += Network_OnSimConnecting;
            client.Network.SimConnected += Network_OnSimConnected;
            client.Network.SimDisconnected += Network_OnSimDisconnected;
            client.Network.Disconnected += Network_OnDisconnected;
            client.Network.SimChanged += Network_OnCurrentSimChanged;
            client.Network.EventQueueRunning += Network_OnEventQueueRunning;

            client.Self.ChatFromSimulator += Self_OnChat;
            //throw new NotImplementedException();
            
            client.Self.ScriptDialog += Self_OnScriptDialog;
            client.Self.ScriptQuestion += Self_OnScriptQuestion;
            client.Self.LoadURL += Self_OnLoadURL;
            client.Self.IM += Self_OnInstantMessage;
            client.Self.TeleportProgress += Self_OnTeleport;
            client.Self.MoneyBalance += Self_OnBalanceUpdated;
            client.Self.MoneyBalanceReply += Self_OnMoneyBalanceReplyReceived;
            client.Self.AgentDataReply += Self_OnAgentDataUpdated;
            client.Self.AnimationsChanged += Self_OnAnimationsChanged;
            client.Self.MeanCollision -= Self_OnMeanCollision;
            client.Self.RegionCrossed += Self_OnRegionCrossed;
            client.Self.GroupChatJoined += Self_OnGroupChatJoin;
            client.Self.GroupChatLeft += Self_OnGroupChatLeft;
            client.Self.AlertMessage += Self_OnAlertMessage;
            client.Self.ScriptControlChange += Self_OnScriptControlChange;
            client.Self.CameraConstraint -= Self_OnCameraConstraint;
            client.Self.ScriptSensorReply += Self_OnScriptSensorReply;
            client.Self.AvatarSitResponse += Self_OnAvatarSitResponse;
            client.Self.ChatSessionMemberAdded += Self_OnChatSessionMemberAdded;
            client.Self.ChatSessionMemberLeft += Self_OnChatSessionMemberLeft;
            client.Appearance.AgentWearablesReply += Appearance_OnAgentWearables;
            //client.Appearance.OnAppearanceUpdated += Appearance_OnAppearanceUpdated;
            client.Friends.FriendNames += Friends_OnFriendNamesReceived;
            client.Friends.FriendOnline += Friends_OnFriendOnline;
            client.Friends.FriendOffline += Friends_OnFriendOffline;
            client.Friends.FriendRightsUpdate += Friends_OnFriendRights;
            client.Friends.FriendshipOffered += Friends_OnFriendshipOffered;
            client.Friends.FriendshipResponse += Friends_OnFriendshipResponse;
            client.Friends.FriendshipTerminated += Friends_OnFriendshipTerminated;
        
            client.Inventory.ItemReceived += Inventory_OnItemReceived;
            client.Inventory.FolderUpdated += Inventory_OnFolderUpdated;
            client.Inventory.InventoryObjectOffered += Inventory_OnObjectOffered;
            client.Inventory.FindObjectByPathReply += Inventory_OnFindObjectByPath;
            client.Inventory.TaskItemReceived += Inventory_OnTaskItemReceived;


            // so we can find ourselves
            client.Objects.AvatarUpdate += Objects_OnNewAvatar;

            // just in case
            client.Network.SimConnected -= Network_OnSimConnectedHook;
            client.Inventory.ScriptRunningReply -= Inventory_OnScriptRunning;
            // just in case twice
            client.Network.SimConnected -= Network_OnSimConnectedHook;
            client.Inventory.ScriptRunningReply -= Inventory_OnScriptRunning;
            client.Network.SimConnected += Network_OnSimConnectedHook;
            client.Inventory.ScriptRunningReply += Inventory_OnScriptRunning;
        }

        private EventHandler<ScriptDialogEventArgs> senderFrom0(Delegate o)
        {
            throw new NotImplementedException();
        }

        private EventHandler senderFrom(object o)
        {
            throw new NotImplementedException();
        }

        //public volatile static WorldObjects Master;
        public void SetWorldMaster(bool isMaster)
        {
            lock (WorldObjectsMasterLock)
            {
                if (isMaster) GridMaster = this;
                if (MasteringRegions.Count > 0 && !isMaster) throw new ArgumentException("Cant un-master!");

                isMaster = true;
                client.Settings.ALWAYS_DECODE_OBJECTS = isMaster;
                client.Settings.ALWAYS_REQUEST_OBJECTS = isMaster;
                client.Settings.ALWAYS_REQUEST_PARCEL_ACL = isMaster;
                client.Settings.ALWAYS_REQUEST_PARCEL_DWELL = isMaster;
                client.Settings.STORE_LAND_PATCHES = isMaster;
                client.Settings.OBJECT_TRACKING = isMaster;
                client.Settings.PARCEL_TRACKING = isMaster;

                // client.Settings.OBJECT_TRACKING = isMaster;

                if (isMaster) RegisterAll();
                else UnregisterAll();
            }
        }
        public void EnsureSimulator(Simulator simulator)
        {
            if (simulator == null || simulator.Handle==0) return;
            if (!Monitor.TryEnter(_AllSimulators,10000))
            {
                WriteLine("Cant lock _AllSimulators");
                return;
            }
            lock (SimMaster)
            {
                if (!SimMaster.ContainsKey(simulator.Handle))
                {
                    SimMaster[simulator.Handle] = this;
                    MasteringRegions.AddTo(simulator.Handle);
                }
            }
            try
            {
                {

                    foreach (Simulator set in _AllSimulators)
                    {
                        if (set.Handle == simulator.Handle && set.Client == simulator.Client) return;
                    }
                    _AllSimulators.Add(simulator);
                }
                SimRegion.GetRegion(simulator);
            }
            finally
            {
                Monitor.Exit(_AllSimulators);
            }
        }

        internal Simulator GetSimulator(ulong handle)
        {
            if (handle == 0)
            {
                return client.Network.CurrentSim;
            }
            lock (client.Network.Simulators)
            {
                foreach (Simulator sim in client.Network.Simulators)
                {
                    if (sim.Handle == handle && sim.Connected) return sim;
                }
            }
            //  lock (AllSimulators)
            {
                foreach (Simulator sim in AllSimulators)
                {
                    if (sim.Handle == handle && sim.Connected) return sim;
                }
            }
            return GetRegion(handle).TheSimulator;
        }

        public Simulator GetSimulator(Primitive Prim)
        {
            if (Prim != null)
            {
                ulong handle = Prim.RegionHandle;
                if (handle != 0) return GetSimulator(handle);
            }
            Debug("GetSimulator returning current sim for " + Prim);
            return client.Network.CurrentSim;
        }


        public override void Terrain_OnLandPatch(object sender, LandPatchReceivedEventArgs e) 
        {
            //client.Terrain.LandPatchReceived -= Terrain_OnLandPatch;
            //Simulator simulator, int x, int y, int width, float[] data
            //Console.Write(",");
            //SimRegion R = SimRegion.GetRegion(simulator);
            //base.Terrain_OnLandPatch(simulator, x, y, width, null);

            //throw new NotImplementedException();
            //   SendNewEvent("On-Land-Patch", x, y, width, BVHData);
            //            WriteLine("ClientManager Terrain_OnLandPatch: "+simulator.ToString()+"/"+x.ToString()+"/"+y.ToString()+" w="+width.ToString());
        }

        public override void Parcels_OnParcelInfo(object sender, ParcelInfoReplyEventArgs e)
        {
            var parcel = e.Parcel;
            SimRegion r = SimRegion.GetRegion(parcel.SimName, client);
            if (r != null) r.Parcels_OnParcelInfo(parcel);
            else base.Parcels_OnParcelInfo(sender, e);
        }

        public override void Parcels_OnAccessListReply(object sender, ParcelAccessListReplyEventArgs e)
        {
            //base.Parcels_OnAccessListReply(simulator, sequenceID, localID, flags, accessEntries);
        }

        public override void Parcels_OnParcelProperties(object sender, ParcelPropertiesEventArgs e)
        {
            Simulator simulator = e.Simulator;
            var parcel = e.Parcel;
            SimRegion r = SimRegion.GetRegion(simulator);
            r.Parcels_OnParcelProperties(simulator, parcel, e.Result, e.SelectedPrims, e.SequenceID, e.SnapSelection);
            //base.Parcels_OnParcelProperties(simulator, parcel, result, selectedPrims, sequenceID, snapSelection);
        }
        public override void Parcels_OnParcelSelectedObjects(object sender, ForceSelectObjectsReplyEventArgs e)
        {
            Simulator simulator = e.Simulator;
            var objectIDs = e.ObjectIDs;
            var resetList = e.ResetList;

            SimRegion r = SimRegion.GetRegion(simulator);
            r.Parcels_OnParcelSelectedObjects(simulator, objectIDs, resetList);
            base.Parcels_OnParcelSelectedObjects(sender,e);
        }

        static readonly List<UUID> parcelInfoRequests = new List<UUID>();
        public override void Parcels_OnParcelDwell(object sender, ParcelDwellReplyEventArgs e)
        {

            lock (parcelInfoRequests)
            {
                var parcelID = e.ParcelID;
                if (parcelInfoRequests.Contains(parcelID)) return;
                parcelInfoRequests.Add(parcelID);
                client.Parcels.RequestParcelInfo(parcelID);
            }

            //base.Parcels_OnParcelDwell(parcelID, localID, dwell);
        }


        public SimObject AsObject(string fromName, UUID id, PCode isAvatar)
        {
            SimObject p = null;
            if (id != UUID.Zero)
            {
                SimObject obj = GetSimObjectFromUUID(id);
                if (obj != null) return obj;
            }
            else
            {
                if (string.IsNullOrEmpty(fromName)) return null;
                if (IsSystemName(fromName)) return null;
                int au;
                List<SimObject> ps = GetPrimitives(new string[] { fromName }, out au);
                if (ps.Count==1) p = ps[0];
            }
            if (p != null) return p;
            Object o = null;
            if (isAvatar==PCode.None && !string.IsNullOrEmpty(fromName))
            {
                if (!fromName.Contains(" "))
                {
                    isAvatar = PCode.Prim;
                }  else
                {
                    isAvatar = PCode.Avatar;  
                }
            }
            return GetSource(client.Network.CurrentSim, id, null, ref o, isAvatar);
        }

        private static bool IsSystemName(string fromName)
        {
            if (string.IsNullOrEmpty(fromName)) return false;
            fromName = fromName.ToLower();
            return fromName == "second life" || fromName == "system" || fromName == "opensim";
        }

        private SimObject GetSource(Simulator sim, UUID sourceID, SimObject source, ref object s, PCode isAvatar)
        {
            if (source != null)
            {
                s = source;
                return source;
            }
            source = GetSimObjectFromUUID(sourceID);
            if (source != null)
            {
                s = source;
                return source;
            }
            Primitive sp = GetPrimitive(sourceID, null);
            if (sp != null)
            {
                source = GetSimObject(sp);
            }
            else
            {
                if (sim != null && !RequestParcelObjects)
                {
                    RequestParcelObjects = true;
                    client.Parcels.RequestAllSimParcels(sim, false, 250);
                    client.Grid.RequestMapItems(sim.Handle, GridItemType.AgentLocations, GridLayerType.Objects);
                }
                SourceDetect(sourceID);
                // client.Self.RequestSit(sourceID,Vector3.Zero);
                //  client.Directory.
            }

            if (source != null)
            {
                s = source;
            }
            else
            {
                lock (AvatarRegion)
                    if (AvatarRegion.ContainsKey(sourceID))
                    {
                        Debug("found it!");
                    }
                    else {lock (Name2Key)
                        {
                            foreach (KeyValuePair<string, UUID> key in Name2Key)
                            {
                                string name = key.Key;
                                if (name.StartsWith("("))
                                {
                                    
                                }
                                if (key.Value == sourceID)
                                {
                                    var list = SimAvatars.CopyOf();
                                    foreach (SimAvatar set in list)
                                    {
                                        if (set.ID == sourceID)
                                        {
                                            s = source = set;
                                            return source;
                                        }
                                    }
                                    foreach (SimAvatar set in list)
                                    {
                                        string n = set.GetName();
                                        if (n != null && n.Equals(name))
                                        {
                                            s = source = set;
                                            return source;
                                        }
                                    }
                                    Debug("No avatar object for " + name);
                                    SimAvatarImpl impl = CreateSimAvatar(key.Value, this, sim);
                                    impl.AspectName = name;
                                    s = source = impl;
                                    return source;
                                }
                            }
                        }}
            }
            if (isAvatar==PCode.Prim)
            {

                SimObject impl = CreateSimObject(sourceID, this, sim);
                s = source = impl;
                return source;

            }
            if (s==null)
            {
                s = sourceID;
            }
            return source;
        }

        private void SourceDetect(UUID sourceID)
        {
            client.Avatars.RequestAvatarName(sourceID);
            client.Friends.MapFriend(sourceID);
           /*
            UUID trans = UUID.Random();
            client.Self.LookAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, LookAtType.Select, trans);
            client.Self.PointAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, PointAtType.Select, trans);
            client.Self.BeamEffect(client.Self.AgentID, sourceID, Vector3d.Zero, new Color4(255, 0, 0, 255), 1f, trans);
            client.Self.PointAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, PointAtType.None, trans);
            client.Self.LookAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, LookAtType.None, trans);
            client.Self.BeamEffect(UUID.Zero, UUID.Zero, Vector3d.Zero, new Color4(255, 255, 255, 255), 0, trans);
            */
        }

        public static WorldObjects MasterFor(ulong handle)
        {
            return GridMaster;
        }

        static object GetSimLock(Simulator simulator)
        {
            if (simulator == null)
            {
                return GetSimObjectLock;
            }
            lock (GetSimObjectLock)
            {
                if (!GetSimObjectLock.ContainsKey(simulator.Handle))
                    GetSimObjectLock[simulator.Handle] = new object();
            }
            return GetSimObjectLock[simulator.Handle];
        }

        private static readonly List<ulong> MaintainSimCollisionsList = new List<ulong>();
        public static bool MaintainSimCollisions(ulong handle)
        {
            lock (MaintainSimCollisionsList) return MaintainSimCollisionsList.Contains(handle);
        }

        public bool IsWorthMeshing(SimObjectImpl impl)
        {
            double d = Vector3d.Distance(impl.GlobalPosition, client.Self.GlobalPosition);
            if (d < 50)
            {
                return true;
            }
            return false;
        }
    }
}