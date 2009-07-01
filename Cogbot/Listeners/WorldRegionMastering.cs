using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Packets;
using OpenMetaverse.StructuredData;
using PathSystem3D.Navigation;

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
                    sims = new List<Simulator>(_AllSimulators);

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
            lock (MasteringRegions) return MasteringRegions.Contains(simulator.Handle);
        }

        public bool IsRegionMaster
        {
            get { return IsMaster(client.Network.CurrentSim); }
        }

        public bool IsGridMaster
        {
            get { return GridMaster == this; }
        }

        public override void Parcels_OnSimParcelsDownloaded(Simulator simulator,
                                                            InternalDictionary<int, Parcel> simParcels, int[,] parcelMap)
        {
            //base.Parcels_OnSimParcelsDownloaded(simulator, simParcels, parcelMap);
        }

        public override void Network_OnConnected(object sender)
        {
            //Network_OnSimConnectedHook( (Simulator)sender);
            base.Network_OnConnected(sender);
            if (sender != client.gridClient)
            {
                throw new ArgumentException("wrong client " + sender);
            }
            //            RequestGridInfos();
        }

        private void RequestGridInfos(ulong regionHandle)
        {
            if (!RequestedGridInfos)
            {
                int Range = 1;
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

        List<Simulator> ConnectedHook = new List<Simulator>();
        public void Network_OnSimConnectedHook(Simulator simulator)
        {
            ConnectedHook.Add(simulator);
            if (RunningHook.Contains(simulator))
            {
                Debug("RUNNING ALREADY");
            }
            base.Network_OnSimConnected(simulator);
            lock (WorldObjectsMasterLock)
            {
                EnsureSimulator(simulator);
                IsConnected = true;
                if (SimRegion.IsMaster(simulator, client.gridClient))
                {
                    Debug("---SIMMASTER---------" + client + " region: " + simulator);
                    SetWorldMaster(true);
                    //client.Grid.RequestMapRegion(simulator.Name, GridLayerType.Objects);
                    client.Grid.RequestMapRegion(simulator.Name, GridLayerType.Terrain);
                    //client.Grid.RequestMapRegion(simulator.Name, GridLayerType.LandForSale);
                    //client.Grid.RequestMapItems(simulator.Handle,OpenMetaverse.GridItemType.Classified,GridLayerType.Terrain);
                    MasteringRegions.Add(simulator.Handle);
                    RequestGridInfos(simulator.Handle);
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
            //if (simulator == client.Network.CurrentSim) { new Thread(() => { Thread.Sleep(30000); client.Appearance.SetPreviousAppearance(true); }).Start(); }
        }

        public void CheckConnected(Simulator simulator)
        {
            if (!IsConnected)
            {
                Network_OnSimConnectedHook(simulator);
            }
        }

        List<Simulator> RunningHook = new List<Simulator>();
        public override void Network_OnEventQueueRunning(Simulator simulator)
        {
            RunningHook.Add(simulator);
            if (ConnectedHook.Contains(simulator))
            {
                Debug("CONNECTED ALREADY");
            }

            //if (simulator == client.Network.CurrentSim) { new Thread(() => client.Appearance.WearOutfit(new string[] { "Clothing", "Default" })).Start(); }
            if (string.IsNullOrEmpty(simulator.Name))
            {
           //    simulator.Client.Grid.RequestMapItems(simulator.Handle,GridItemType.AgentLocations,GridLayerType.Terrain); 
            }
            base.Network_OnEventQueueRunning(simulator);
            if (simulator == client.Network.CurrentSim)
            {
                new Thread(() => client.Appearance.SetPreviousAppearance(true)).Start();
            }
        }

        public override void Network_OnCurrentSimChanged(Simulator PreviousSimulator)
        {
            base.Network_OnCurrentSimChanged(PreviousSimulator);
            if (TheSimAvatar.GetSimulator() == PreviousSimulator)
            {
                Debug("TheSimAvatar._CurrentRegion.TheSimulator == PreviousSimulator " + PreviousSimulator);
            }

            if (PreviousSimulator != null)
            {
                LeaveSimulator(PreviousSimulator);
                new Thread(() => client.Appearance.SetPreviousAppearance(false)).Start();
            }
            else
            {
//                new Thread(() => client.Appearance.WearOutfit(new string[] { "Clothing", "Default", "IRobot" })).Start();
            }
            EnsureSimulator(client.Network.CurrentSim);
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
                    Debug("Found a new client for region " + R + " as " + cl);
                    return;
                }
                foreach (var simulator in _AllSimulators)
                {
                    if (simulator.Client == client.gridClient) continue;
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
            Debug("Now client for region " + R);
        }

        public override void Self_OnRegionCrossed(Simulator oldSim, Simulator newSim)
        {
            if (oldSim != null)
            {
                LeaveSimulator(oldSim);
            }
            EnsureSimulator(newSim);
            base.Self_OnRegionCrossed(oldSim, newSim);
        }

        public override void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason)
        {
            base.Network_OnSimDisconnected(simulator, reason);
            lock (_AllSimulators)
            {
                _AllSimulators.Remove(simulator);
                SimRegion.GetRegion(simulator).RemoveSim(simulator);
                LeaveSimulator(simulator);
            }
        }

        public override void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message)
        {
            lock (_AllSimulators)
            {
                foreach (var simulator in _AllSimulators)
                {
                    if (simulator.Client!=client.gridClient) continue;
                    _AllSimulators.Remove(simulator);
                    SimRegion.GetRegion(simulator).RemoveSim(simulator);
                    LeaveSimulator(simulator);
                }

            }
            base.Network_OnDisconnected(reason, message);
        }

        public override void Grid_OnGridRegion(GridRegion region)
        {
            SimRegion R = SimRegion.GetRegion(region.RegionHandle, client);
            if (R != null)
                R.GridInfo = region;
            // base.Grid_OnGridRegion(region);
        }


        public override void Grid_OnRegionHandleReply(UUID regionID, ulong regionHandle)
        {
            RegisterUUID(regionID, GetRegion(regionHandle));
            base.Grid_OnRegionHandleReply(regionID, regionHandle);
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
                    client.Network.RegisterCallback(PacketType.AttachedSound, new NetworkManager.PacketCallback(AttachedSoundHandler));
                    client.Network.RegisterCallback(PacketType.AttachedSoundGainChange, new NetworkManager.PacketCallback(AttachedSoundGainChangeHandler));
                    client.Network.RegisterCallback(PacketType.PreloadSound, new NetworkManager.PacketCallback(PreloadSoundHandler));
                    client.Network.RegisterCallback(PacketType.SoundTrigger, new NetworkManager.PacketCallback(SoundTriggerHandler));

                    client.Sound.OnAttachSound -= Sound_OnAttachSound;
                    client.Sound.OnAttachSoundGainChange -= Sound_OnAttachSoundGainChange;
                    client.Sound.OnSoundTrigger -= Sound_OnSoundTrigger;
                    client.Sound.OnPreloadSound -= Sound_OnPreloadSound;


                    // Mean collision
                    client.Network.RegisterCallback(PacketType.MeanCollisionAlert,
                                                    new NetworkManager.PacketCallback(MeanCollisionAlertHandler));
                    client.Self.OnMeanCollision -= Self_OnMeanCollision;



                    // Viewer effect callback
                    client.Network.RegisterCallback(PacketType.ViewerEffect,
                                                    new NetworkManager.PacketCallback(ViewerEffectHandler));
                    client.Avatars.OnPointAt -= Avatars_OnPointAt;
                    client.Avatars.OnLookAt -= Avatars_OnLookAt;
                    client.Avatars.OnEffect -= Avatars_OnEffect;


                    // Avatar appearance
                    client.Network.RegisterCallback(PacketType.AvatarAppearance,
                                                    new NetworkManager.PacketCallback(AvatarAppearanceHandler));


                    client.Network.RegisterCallback(PacketType.AvatarAnimation, new NetworkManager.PacketCallback(AvatarAnimationHandler));
                    client.Avatars.OnAvatarAnimation -= Avatars_OnAvatarAnimation;

                    
                    // raises these events already
                    client.Assets.OnUploadProgress -= Assets_OnUploadProgress; // On-Upload-Progress
                    client.Self.OnCameraConstraint -= Self_OnCameraConstraint;


                    client.Settings.PIPELINE_REQUEST_TIMEOUT = 60000;

                    client.Objects.OnObjectPropertiesUpdated += Objects_OnPrimitiveProperties;
                    client.Objects.OnObjectTerseUpdate += Objects_OnPrimitiveUpdate;
                    client.Objects.OnObjectUpdated -= Objects_OnObjectUpdated;
                    client.Objects.OnObjectDataBlockUpdate += Objects_OnObjectDataBlockUpdate;


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
                                new NetworkManager.PacketCallback(ViewerEffectHandler));

                    client.Objects.OnObjectPropertiesUpdated -= Objects_OnPrimitiveProperties;
                    client.Objects.OnObjectTerseUpdate -= Objects_OnPrimitiveUpdate;
                    client.Objects.OnObjectUpdated -= Objects_OnObjectUpdated;
                    client.Objects.OnObjectDataBlockUpdate -= Objects_OnObjectDataBlockUpdate;
                    RegisterThinClient();
                }
            }
        }

        private void RegisterThinClient()
        {
            client.Network.OnLogin += Network_OnLogin;
            client.Network.OnConnected += Network_OnConnected;
            client.Network.OnLogoutReply += Network_OnLogoutReply;
            client.Network.OnSimConnecting += Network_OnSimConnecting;
            client.Network.OnSimConnected += Network_OnSimConnected;
            client.Network.OnSimDisconnected += Network_OnSimDisconnected;
            client.Network.OnDisconnected += Network_OnDisconnected;
            client.Network.OnCurrentSimChanged += Network_OnCurrentSimChanged;
            client.Network.OnEventQueueRunning += Network_OnEventQueueRunning;

            client.Self.OnChat += Self_OnChat;
            client.Self.OnScriptDialog += Self_OnScriptDialog;
            client.Self.OnScriptQuestion += Self_OnScriptQuestion;
            client.Self.OnLoadURL += Self_OnLoadURL;
            client.Self.OnInstantMessage += Self_OnInstantMessage;
            client.Self.OnTeleport += Self_OnTeleport;
            client.Self.OnBalanceUpdated += Self_OnBalanceUpdated;
            client.Self.OnMoneyBalanceReplyReceived += Self_OnMoneyBalanceReplyReceived;
            client.Self.OnAgentDataUpdated += Self_OnAgentDataUpdated;
            client.Self.OnAnimationsChanged += Self_OnAnimationsChanged;
            client.Self.OnMeanCollision -= Self_OnMeanCollision;
            client.Self.OnRegionCrossed += Self_OnRegionCrossed;
            client.Self.OnGroupChatJoin += Self_OnGroupChatJoin;
            client.Self.OnGroupChatLeft += Self_OnGroupChatLeft;
            client.Self.OnAlertMessage += Self_OnAlertMessage;
            client.Self.OnScriptControlChange += Self_OnScriptControlChange;
            client.Self.OnCameraConstraint -= Self_OnCameraConstraint;
            client.Self.OnScriptSensorReply += Self_OnScriptSensorReply;
            client.Self.OnAvatarSitResponse += Self_OnAvatarSitResponse;
            client.Self.OnChatSessionMemberAdded += Self_OnChatSessionMemberAdded;
            client.Self.OnChatSessionMemberLeft += Self_OnChatSessionMemberLeft;
            client.Appearance.OnAgentWearables += Appearance_OnAgentWearables;
            client.Appearance.OnAppearanceUpdated += Appearance_OnAppearanceUpdated;
            client.Friends.OnFriendNamesReceived += Friends_OnFriendNamesReceived;
            client.Friends.OnFriendOnline += Friends_OnFriendOnline;
            client.Friends.OnFriendOffline += Friends_OnFriendOffline;
            client.Friends.OnFriendRights += Friends_OnFriendRights;
            client.Friends.OnFriendshipOffered += Friends_OnFriendshipOffered;
            client.Friends.OnFriendshipResponse += Friends_OnFriendshipResponse;
            client.Friends.OnFriendshipTerminated += Friends_OnFriendshipTerminated;
            client.Inventory.OnItemReceived += Inventory_OnItemReceived;
            client.Inventory.OnFolderUpdated += Inventory_OnFolderUpdated;
            client.Inventory.OnObjectOffered += Inventory_OnObjectOffered;
            client.Inventory.OnFindObjectByPath += Inventory_OnFindObjectByPath;
            client.Inventory.OnTaskItemReceived += Inventory_OnTaskItemReceived;


            // so we can find ourselves
            client.Objects.OnNewAvatar += Objects_OnNewAvatar;

            // just in case
            client.Network.OnSimConnected -= Network_OnSimConnectedHook;
            client.Inventory.OnScriptRunning -= Inventory_OnScriptRunning;
            // just in case twice
            client.Network.OnSimConnected -= Network_OnSimConnectedHook;
            client.Inventory.OnScriptRunning -= Inventory_OnScriptRunning;
            client.Network.OnSimConnected += Network_OnSimConnectedHook;
            client.Inventory.OnScriptRunning += Inventory_OnScriptRunning;
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
            if (simulator == null) return;
            lock (_AllSimulators)
            {

                foreach (Simulator set in _AllSimulators)
                {
                    if (set.Handle==simulator.Handle && set.Client==simulator.Client) return;
                }
                _AllSimulators.Add(simulator);
                SimRegion.GetRegion(simulator);
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


        public override void Terrain_OnLandPatch(Simulator simulator, int x, int y, int width, float[] data)
        {
            Console.Write(",");
            //SimRegion R = SimRegion.GetRegion(simulator);
            //base.Terrain_OnLandPatch(simulator, x, y, width, null);

            //throw new NotImplementedException();
            //   SendNewEvent("On-Land-Patch", x, y, width, BVHData);
            //            WriteLine("TextForm Terrain_OnLandPatch: "+simulator.ToString()+"/"+x.ToString()+"/"+y.ToString()+" w="+width.ToString());
        }

        public override void Parcels_OnParcelInfo(ParcelInfo parcel)
        {
            SimRegion r = SimRegion.GetRegion(parcel.SimName);
            r.Parcels_OnParcelInfo(parcel);
           // base.Parcels_OnParcelInfo(parcel);
        }

        public override void Parcels_OnAccessListReply(Simulator simulator, int sequenceID, int localID, uint flags, List<ParcelManager.ParcelAccessEntry> accessEntries)
        {
            //base.Parcels_OnAccessListReply(simulator, sequenceID, localID, flags, accessEntries);
        }

        public override void Parcels_OnParcelProperties(Simulator simulator, Parcel parcel, ParcelResult result, int selectedPrims, int sequenceID, bool snapSelection)
        {
            SimRegion r = SimRegion.GetRegion(simulator);
            r.Parcels_OnParcelProperties(simulator, parcel, result, selectedPrims, sequenceID, snapSelection);
            //base.Parcels_OnParcelProperties(simulator, parcel, result, selectedPrims, sequenceID, snapSelection);
        }
        public override void Parcels_OnParcelSelectedObjects(Simulator simulator, List<uint> objectIDs, bool resetList)
        {
            SimRegion r = SimRegion.GetRegion(simulator);
            r.Parcels_OnParcelSelectedObjects(simulator, objectIDs, resetList);
            base.Parcels_OnParcelSelectedObjects(simulator, objectIDs, resetList);
        }

        static readonly List<UUID> parcelInfoRequests = new List<UUID>();
        public override void Parcels_OnParcelDwell(UUID parcelID, int localID, float dwell)
        {
            lock (parcelInfoRequests)
            {
                if (parcelInfoRequests.Contains(parcelID)) return;
                parcelInfoRequests.Add(parcelID);
            }
            client.Parcels.InfoRequest(parcelID);
            //base.Parcels_OnParcelDwell(parcelID, localID, dwell);
        }


        public SimObject AsObject(string fromName, UUID id, PCode isAvatar)
        {
            Primitive p;
            if (id != UUID.Zero)
            {
                SimObject obj = GetSimObjectFromUUID(id);
                if (obj != null) return obj;
                p = GetPrimitive(id, null);
            }
            else
            {
                if (string.IsNullOrEmpty(fromName)) return null;
                int au;
                p = GetPrimitive(new string[] { fromName }, out au);
            }
            if (p != null) return GetSimObject(p);
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
                if (sim!=null && !RequestParcelObjects)
                {
                    RequestParcelObjects = true;
                    client.Parcels.RequestAllSimParcels(sim, false, 250);
                    client.Grid.RequestMapItems(sim.Handle, GridItemType.AgentLocations, GridLayerType.Objects);
                }
                client.Avatars.RequestAvatarName(sourceID);
                client.Friends.MapFriend(sourceID);
                UUID trans = UUID.Random();
                client.Self.LookAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, LookAtType.Select, trans);
                client.Self.PointAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, PointAtType.Select, trans);
                client.Self.BeamEffect(client.Self.AgentID, sourceID, Vector3d.Zero, new Color4(255, 0, 0, 255), 1f, trans);
                client.Self.PointAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, PointAtType.None, trans);
                client.Self.LookAtEffect(client.Self.AgentID, sourceID, Vector3d.Zero, LookAtType.None, trans);
                client.Self.BeamEffect(UUID.Zero, UUID.Zero, Vector3d.Zero, new Color4(255, 255, 255, 255), 0, trans); 
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
    }
}