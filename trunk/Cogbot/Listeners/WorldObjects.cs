using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;

namespace cogbot.Listeners
{
    public delegate float SimObjectHeuristic(SimObject prim);

    public partial class WorldObjects : AllEvents
    {

        public static bool CanPhantomize = false;
        public static bool CanUseSit = true;
        public static bool DoSimulatorsCatchUp = false; //GridMaster will turn this on/off only if it needed
        public static bool MaintainAnims = true;
        public static bool MaintainAssetsInFolders = true;
        public static bool GleanAssetsFromInventory = true;
        public static bool MaintainAnimsInFolders = true;
        public static bool GleanAssetsFromFolders = true;
        public static bool MaintainAttachments = true;
        public static bool MaintainCollisions = false; // keep false so the bot only meshes what it needs
        public static bool MaintainEffects = true;
        public static bool MaintainOnlyMasterEffects = false;
        public static float MaintainEffectsDistance = 80;
        public static bool MaintainActions = true;
        public static bool MaintainPropertiesFromQueue = true;
        public static bool MaintainObjectUpdates = true;
        public static bool MaintainObjectProperties = true;
        public static bool MaintainSounds = true;
        static public bool MaintainAvatarMetaData = true;
        static public bool MaintainGroupMetaData = true;
        static public bool RequestGroupMetaData = false;
        public static bool MaintainSimObjectInfoMap = false;
		public static bool SendSimObjectInfoMap = true;
        public static bool SendOnDataAspectUpdate = true;
        public static bool ZeroOutUselessUUIDs = true;
        /// <summary>
        /// False since currently broken this is where FromTaskIDs are guessed as to being object or Avatars
        /// </summary>
        public static bool DiscoverTaskUUIDs = false; 
		
        public static bool UseNewEventSystem = true;
        public static bool SimplifyBoxes = true; // true takes longer startup but speeds up runtime path finding
        public static bool SendAllEvents = MaintainObjectUpdates;




        private static readonly Dictionary<ulong, object> GetSimObjectLock = new Dictionary<ulong, object>();

        private static readonly Dictionary<ulong, HashSet<uint>> primsSelected = new Dictionary<ulong, HashSet<uint>>();
        private static readonly Dictionary<ulong, List<uint>> primsSelectedOutbox = new Dictionary<ulong, List<uint>>();

        private static readonly TaskQueueHandler PropertyQueue = new TaskQueueHandler("NewObjectQueue", TimeSpan.Zero, true);
        public static readonly TaskQueueHandler UpdateObjectData = new TaskQueueHandler("UpdateObjectData");
        public static readonly TaskQueueHandler ParentGrabber = new TaskQueueHandler("ParentGrabber", TimeSpan.FromSeconds(1), false);

        private static readonly object SelectObjectsTimerLock = new object();
        private static readonly List<ThreadStart> ShutdownHooks = new List<ThreadStart>();
        private static readonly TaskQueueHandler EventQueue = new TaskQueueHandler("World EventQueue");
        private static readonly TaskQueueHandler CatchUpQueue = new TaskQueueHandler("Simulator catchup", TimeSpan.FromSeconds(60), false);
        private static readonly TaskQueueHandler MetaDataQueue = PropertyQueue;//new TaskQueueHandler("MetaData Getter", TimeSpan.FromSeconds(0), false);
        public static readonly TaskQueueHandler OnConnectedQueue = new TaskQueueHandler("OnConnectedQueue", TimeSpan.FromMilliseconds(20), false);
        public static readonly TaskQueueHandler SlowConnectedQueue = SimAssetStore.SlowConnectedQueue;
        internal static readonly Dictionary<UUID, object> uuidTypeObject = new Dictionary<UUID, object>();
        private static readonly object WorldObjectsMasterLock = new object();

        private static int CountnumAvatars;

        public static readonly ListAsSet<SimAvatar> SimAvatars = new ListAsSet<SimAvatar>();
        public static readonly ListAsSet<SimObject> SimObjects = new ListAsSet<SimObject>();

        private static Timer EnsureSelectedTimer;
        private static bool inTimer = false;
        public static float buildingSize = 5;
        public static TimeSpan burstInterval;
        public static int burstSize = 100;
        public DateTime burstStartTime;
        public static float burstTime = 1;
        public SimActor m_TheSimAvatar;
        public List<string> numberedAvatars;
        public List<SimObjectHeuristic> objectHeuristics;
        public Dictionary<UUID, List<Primitive>> primGroups;
        public int searchStep;
        public bool IsDisposed = false;

        public override string GetModuleName()
        {
            return "WorldSystem";
        }

        public override void StartupListener()
        {
            RegisterAll();
        }

        public override void Dispose()
        {
            if (IsDisposed) return;
            IsDisposed = true;
            foreach (var h in ShutdownHooks)
            {
                try
                {
                    h();
                }
                catch (Exception)
                {
                    
                }
            }
            UnregisterAll();
            base.UnregisterAll(); //becasue of "thin client"
            if (IsGridMaster)
            {
                WriteLine("GridMaster Disposing!");
                EventQueue.Dispose();
                CatchUpQueue.Dispose();
                PropertyQueue.Dispose();
                MaintainCollisions = false;
                MaintainActions = false;
                SimAssetSystem.Dispose();
                ParentGrabber.Dispose();
                SimPaths.Dispose();
                MetaDataQueue.Dispose();
                OnConnectedQueue.Dispose();
                SlowConnectedQueue.Dispose();
            }
        }


        public override bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {

            if (eventName.EndsWith("On-Image-Receive-Progress")) return true;
            if (eventName.EndsWith("Look-At")) return true;
            var parms = new NamedParam[paramNames.Length];
            for (int i = 0; i < paramNames.Length; i++)
            {
                parms[i] = new NamedParam(paramNames[i], paramTypes[i],parameters[i]);
            }
            SimObjectEvent evt = new SimObjectEvent(SimEventStatus.Once, eventName, SimEventType.UNKNOWN, SimEventClass.REGIONAL, parms);
            client.SendPipelineEvent(evt);
            return true;
        }

        public WorldObjects(BotClient client)
            : base(client)
        {
            client.WorldSystem = this;
            RegisterAll();
            DLRConsole.TransparentCallers.Add(typeof (WorldObjects));
            if (Utils.GetRunningRuntime() == Utils.Runtime.Mono)
            {
                // client.Settings.USE_LLSD_LOGIN = true;
            } //else
            ///client.Settings.USE_LLSD_LOGIN = true;

            // TODO client.Network.PacketEvents.SkipEvent += Network_SkipEvent;
            _defaultProvider = new DefaultWorldGroupProvider(this);
            AddGroupProvider(_defaultProvider);
            lock (WorldObjectsMasterLock)
            {
                if (GridMaster == null)
                {
                    GridMaster = this;
                    if (client.Network.CurrentSim != null) DoSimulatorsCatchUp = true;
                    if (DoSimulatorsCatchUp)
                    {
                        DoSimulatorsCatchUp = false;
                        CatchUpQueue.AddFirst(DoCatchup);
                    }
                    client.Settings.USE_LLSD_LOGIN = false;
                }
                else
                {
                    //only one rpc at a time  (btw broken with OpenSim.. works with Linden)
                    client.Settings.USE_LLSD_LOGIN = true;
                }
                DoSimulatorsCatchUp = false;
                //new DebugAllEvents(client);

                primGroups = new Dictionary<UUID, List<Primitive>>();

                objectHeuristics = new List<SimObjectHeuristic>();
                objectHeuristics.Add(new SimObjectHeuristic(distanceHeuristic));
                //objectHeuristics.Add(new SimObjectHeuristic(nameLengthHeuristic));
                objectHeuristics.Add(new SimObjectHeuristic(boringNamesHeuristic));

                client.Settings.ENABLE_CAPS = true;
                client.Settings.ENABLE_SIMSTATS = true;
                client.Settings.AVATAR_TRACKING = true;
                client.Settings.THROTTLE_OUTGOING_PACKETS = false;
                client.Settings.MULTIPLE_SIMS = true;
                client.Settings.SIMULATOR_TIMEOUT = int.MaxValue;
                client.Settings.LOGIN_TIMEOUT = 120 * 1000;

                client.Settings.SEND_AGENT_UPDATES = true;

                client.Settings.SEND_PINGS = false;

                //client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                //client.Self.Movement.AutoResetControls = false;
                //client.Self.Movement.UpdateInterval = 0;

                client.Network.SimConnected += Network_OnSimConnectedHook;
                client.Inventory.ScriptRunningReply += Inventory_OnScriptRunning;


                burstStartTime = DateTime.Now;
                burstInterval = new TimeSpan(0, 0, 0, 0, (int)(burstTime * 1000));
                searchStep = 1;

                numberedAvatars = new List<string>();

                if (RegionMasterTexturePipeline == null)
                {
                    RegionMasterTexturePipeline = client.Assets;
                    //RegionMasterTexturePipeline.OnDownloadFinished += new TexturePipeline.DownloadFinishedCallback(RegionMasterTexturePipeline_OnDownloadFinished);
                    client.Settings.USE_ASSET_CACHE = true;
                }
                else
                {
                    //client.Settings.USE_TEXTURE_CACHE = false;
                }
                // must be after the pipeline is made
                _simAssetSystem = new SimAssetStore(client);
                if (GridMaster == this)
                {

                    {
                        //BotWorld = this;
                        SimTypeSystem.LoadDefaultTypes();
                    }
                    EnsureSelectedTimer = new Timer(ReallyEnsureSelected_Thread, null, 1000, 1000);
                    _SimPaths = new WorldPathSystem(this);
                }
                //SetWorldMaster(false);
                //RegisterAll();
            }
        }

        static void DoCatchup()
        {
            foreach (Simulator S in AllSimulators)
            {
                GridMaster.CatchUp(S);
            }
            if (DoSimulatorsCatchUp)
            {
                DoSimulatorsCatchUp = false;
                CatchUpQueue.Enqueue(DoCatchup);
            }
            
        }

        public SimActor TheSimAvatar
        {
            get
            {
                if (m_TheSimAvatar == null)
                {
                    UUID id = client.Self.AgentID;
                    if (id == UUID.Zero)
                    {
                        throw new ArgumentException("" + client);
                    }
                    TheSimAvatar = (SimActor)GetSimObjectFromUUID(id);
                    if (m_TheSimAvatar == null)
                    {
                        Avatar av = GetAvatar(id, client.Network.CurrentSim);
                        if (av != null) TheSimAvatar = (SimActor)GetSimObject(av, client.Network.CurrentSim);
                        if (m_TheSimAvatar == null)
                        {
                            SimAvatarImpl impl;
                            TheSimAvatar = impl = CreateSimAvatar(id, this, client.Network.CurrentSim);
                            impl.AspectName = client.GetName();
                        }
                    }
                    if (m_TheSimAvatar == null)
                    {
                        return null;
                    } else
                    {
                        m_TheSimAvatar.SetClient(client);                        
                    }
                }
                return m_TheSimAvatar;
            }
            set
            {
                if (value == null) return;
                if (value == m_TheSimAvatar) return;                
            	m_TheSimAvatar = value;
            }

        }


        public static implicit operator GridClient(WorldObjects m)
        {
            return m.client.gridClient;
        }

        internal static BotClient BotClientFor(GridClient client)
        {

            foreach (BotClient bc in ClientManager.SingleInstance.BotClients)            
            {
            
                if (bc.gridClient == client) return bc;                
            }
            return null;
        }

        public override void Self_OnCameraConstraint(object sender, CameraConstraintEventArgs e)
        {
            //base.Self_OnCameraConstraint(collidePlane);
        }

        public void SetSimAvatar(SimActor simAvatar)
        {
            m_TheSimAvatar = simAvatar;
        }

        private static void Debug(string p)
        {
            if (p.Contains("ERROR"))
            {
                DLRConsole.DebugWriteLine(p);
                return;
            }
            if (Settings.LOG_LEVEL != Helpers.LogLevel.None)
            {
                DLRConsole.DebugWriteLine(p);
            }
        }

        // these will be shared between Clients and regions

        //public static BotRegionModel BotWorld = null;
        //        TheBotsInspector inspector = new TheBotsInspector();

        ///  inspector.Show();
        public void CatchUp(Simulator simulator)
        {
            List<Primitive> primsCatchup;
            object simLock = GetSimLock(simulator);
            //Thread.Sleep(3000);
            if (!Monitor.TryEnter(simLock)) return; else Monitor.Exit(simLock);
            primsCatchup = new List<Primitive>(simulator.ObjectsPrimitives.Count + simulator.ObjectsAvatars.Count);
            simulator.ObjectsPrimitives.ForEach(a => primsCatchup.Add(a));
            simulator.ObjectsAvatars.ForEach(a => primsCatchup.Add(a));
            bool known = false;
            foreach (Primitive item in primsCatchup)
            {
                if (item.ID != UUID.Zero)
                {
                    //       lock (uuidTypeObject)
                    //         known = uuidTypeObject.ContainsKey(item.ID);
                    //   if (!known)
                    if (item.ParentID == 0 && SimRegion.OutOfRegion(item.Position)) continue;
                    if (!Monitor.TryEnter(simLock)) return; else Monitor.Exit(simLock);
                    GetSimObject(item, simulator);
                }
            }
        }


        public override string ToString()
        {
            return "(.WorldSystem " + client + ")";
        }

        static int waiters = 0;

        private void OfferPrimToSimObject(Primitive prim, SimObject obj0, Simulator simulator)
        {
            if (simulator != null && prim.Properties == null)
            {
                EnsureSelected(prim.LocalID, simulator);
            }
            // Ensure at least one prim
            if (obj0.Prim == null)
            {
                obj0.SetFirstPrim(prim);
                obj0.ConfirmedObject = true;
            }
            if (!obj0.IsRegionAttached)
            {
                obj0.ResetPrim(prim, client, simulator);
            }
        }

        public SimObject GetSimObject(Primitive prim, Simulator simulator)
        {
            if (prim == null)
            {
                return null;
            }
            //if (prim.ID == null) return null;
            SimObject obj0 = GetSimObjectFromUUID(prim.ID);
            if (obj0 != null)
            {
                OfferPrimToSimObject(prim, obj0, simulator);
                return obj0;
            }

            if (simulator == null)
            {
                simulator = GetSimulator(prim);
            }

            object olock = GetSimLock(simulator);
            //waiters++;
            //while (!Monitor.TryEnter(olock))
            //{
            //    if (waiters > 2)
            //    {
            //        Debug("waiters=" + waiters);
            //    }
            //    Thread.Sleep(1000);
            //    Debug("Held Lock too long");
            //}
            //waiters--;
            lock (olock)
            {
                obj0 = GetSimObjectFromUUID(prim.ID);
                if (obj0 != null)
                {
                    OfferPrimToSimObject(prim, obj0, simulator);
                    return obj0;
                }
                // not found
                if (prim is Avatar)
                {
                    CountnumAvatars++;
                    Debug("+++++++++++++++Making {0} {1}", prim, ((Avatar)prim).Name);
                    if (prim.ID == UUID.Zero)
                    {
                        Debug("  - - -#$%#$%#$%% - ------- - Weird Avatar " + prim);
                        BlockUntilPrimValid(prim, simulator);
                        Debug("  - - -#$%#$%#$%% - ------- - Unweird Avatar " + prim);
                    }
                    obj0 = CreateSimAvatar(prim.ID, this, simulator);
                }
                else
                {
                    if (prim.ID==UUID.Zero)
                    {
                        Debug("  - - -#$%#$%#$%% - ------- - Weird Prim " + prim);
                        return null;
                    }
                    obj0 = CreateSimObject(prim.ID, this, simulator);
                    obj0.ConfirmedObject = true;
                }
            }
            if (prim.RegionHandle == 0)
                prim.RegionHandle = simulator.Handle;
            obj0.SetFirstPrim(prim);
            SendOnAddSimObject(obj0);
            return (SimObject)obj0;
        }

        public static SimObject GetSimObjectFromUUID(UUID id)
        {
            if (id == UUID.Zero) return null;
            Object obj0;
            //lock (uuidTypeObject)
            if (uuidTypeObject.TryGetValue(id, out obj0))
            {
                if (obj0 is SimObject)
                {
                    return (SimObject)obj0;
                }
            }
            return null; // todo
            //WorldObjects WO = Master;

            //Primitive p = WO.GetPrimitive(id, null);
            //if (p == null) return null;
            //return WO.GetSimObject(p);
            //Avatar av = null;
            //if (false /*todo deadlocker*/ && Master.tryGetAvatarById(id, out av))
            //{
            //    Debug("Slow get for avatar " + av);
            //    return Master.GetSimObject(av, null);
            //}
            //return null;
        }

        public static SimObject GetSimObjectFromPrimUUID(Primitive prim)
        {
            if (prim == null || prim.ID == UUID.Zero) return null;
            Object obj0;
            //lock (uuidTypeObject)
            if (uuidTypeObject.TryGetValue(prim.ID, out obj0))
            {
                if (obj0 is SimObject)
                {
                    return (SimObject)obj0;
                }
            }
            return null;
        }


        public virtual void Debug(string p, params object[] args)
        {
            Debug(DLRConsole.SafeFormat(p, args));
        }

        public void WriteLine(string p, params object[] args)
        {
            Debug(p, args);
            client.WriteLine(p, args);
        }


        public static bool TryGetSimObject(UUID victim, out SimObject victimAv)
        {
            victimAv = GetSimObjectFromUUID(victim);
            return victimAv != null;
        }

        public static Primitive BlockUntilProperties(Primitive prim, Simulator simulator)
        {
            if (prim.Properties != null) return prim;
            EnsureSelected(prim.LocalID, simulator);
            while (prim.Properties == null)
            {
                // TODO maybe add a timer
                Thread.Sleep(1000);
                Debug("BlockUntilProperties " + prim);
            }
            return prim;
        }

        public static Primitive BlockUntilPrimValid(Primitive prim, Simulator simulator)
        {
            if (prim.Properties != null) return prim;
            EnsureSelected(prim.LocalID, simulator);
            int maxTimes = 4;
            while (prim.ID == UUID.Zero)
            {
                if (maxTimes-- <= 0)
                {
                    throw new AbandonedMutexException("cant get perent!");
                }
                // TODO maybe add a timer
                Thread.Sleep(1000);
                Debug("BlockUntilPrimValid " + prim);
            }
            return prim;
        }


        public override void Objects_OnObjectKilled(object sender, KillObjectEventArgs e)
        {
            Simulator simulator = e.Simulator;
            // had to move this out of the closure because the Primitive is gone later
            Primitive p = GetPrimitive(e.ObjectLocalID, simulator);
            if (p == null)
            {
                //   base.Objects_OnObjectKilled(simulator, objectID);
                return;
            }
            SimObject O = GetSimObjectFromUUID(p.ID);
            if (O == null)
            {
                return;
            }
            EventQueue.Enqueue(() =>
                                    {
                                        //if (O == null)
                                        //    O = GetSimObjectFromUUID(p.ID);
                                        //if (O == null)
                                        //    O = GetSimObject(p, simulator);
                                        //if (O == null)
                                        //{
                                        //    SendNewEvent("on-prim-killed", p);
                                        //    return;
                                        //}
                                        //if (Settings.LOG_LEVEL != Helpers.LogLevel.Info)
                                            //Debug("Killing object: " + O);
                                        {
                                            {
                                                SendOnRemoveSimObject(O);
                                                if (O.KilledPrim(p, simulator))
                                                {
                                                   // lock (SimAvatars)
                                                        foreach (SimAvatar A in SimAvatars)
                                                        {
                                                            A.RemoveObject(O);
                                                        }
                                                    if (O is SimAvatar)
                                                    {
                                                        //lock (SimAvatars)
                                                        {
                                                            //  SimAvatars.Remove((SimAvatar)O);
                                                            Debug("Killing Avatar: " + O);
                                                        }
                                                    }
                                                    //lock (SimObjects) SimObjects.Remove(O);                                     
                                                }

                                            }
                                        }
                                    });
        }

        public static void RegisterUUIDMaybe(UUID id, object type)
        {
            object before;
            if (uuidTypeObject.TryGetValue(id, out before)) return;
            lock (uuidTypeObject)
            {
                if (!uuidTypeObject.TryGetValue(id, out before))
                {
                    uuidTypeObject[id] = type;
                }
            }
        }

        public static void RegisterUUID(UUID id, object type)
        {
            if (type is Primitive)
            {
                Debug("cant register " + type);
            }
            if (type is SimObject)
                lock (uuidTypeObject) uuidTypeObject[id] = type;
            else
            {
                lock (uuidTypeObject)
                {
                    object before;
                    if (uuidTypeObject.TryGetValue(id, out before))
                    {
                        if (before == type) return;
                        //todo Master.SendNewEvent("uuid-change",""+id, before, type);
                        Debug("uuid change" + id + " " + before + " -> " + type);
                    }
                    uuidTypeObject[id] = type;
                }
            }
        }
        /*
		         
         On-Folder-Updated
        folderID: "29a6c2e7-cfd0-4c59-a629-b81262a0d9a2"
         */

        public override void Inventory_OnFolderUpdated(object sender, FolderUpdatedEventArgs e)
        {
            var folderID = e.FolderID;
            RegisterUUID(folderID, client.Inventory.Store[folderID]); //;;typeof(OpenMetaverse.InventoryFolder);
            //base.Inventory_OnFolderUpdated(folderID);
        }

        public override void Objects_OnObjectPropertiesFamily(object sender, ObjectPropertiesFamilyEventArgs e)
        {
            var simulator = e.Simulator;
            base.Objects_OnObjectPropertiesFamily(sender, e);
            Objects_OnObjectProperties(sender, new ObjectPropertiesEventArgs(e.Simulator,e.Properties));
            // Properties = new Primitive.ObjectProperties();
            //Properties.SetFamilyProperties(props);
            // GotPermissions = true;
            // GotPermissionsEvent.Set();        
            //  SendNewEvent("On-Object-PropertiesFamily", simulator, props, type);
        }

        public override void Objects_OnNewPrim(object sender, PrimEventArgs e)
        {                        
            var simulator = e.Simulator;
            var prim = e.Prim;
            var regionHandle = e.Simulator.Handle;
            Objects_OnNewPrimReal(simulator, prim, regionHandle);
        }

        void Objects_OnNewPrimReal(Simulator simulator, Primitive prim, ulong regionHandle)
        {
            CheckConnected(simulator);

            if (prim.ID != UUID.Zero)
            {
                if (IsMaster(simulator))
                {
                    prim.RegionHandle = regionHandle;
                    if (MaintainPropertiesFromQueue)
                    {
                        if (prim.ParentID == 0)
                            PropertyQueue.AddFirst(() => InternPrim(simulator, prim));
                        else PropertyQueue.Enqueue(() => InternPrim(simulator, prim));
                    }
                    else
                    {
                        InternPrim(simulator, prim);
                    }
                    // Make an initial "ObjectUpdate" for later diff-ing
                    EnsureSelected(prim.LocalID, simulator);
                    EnsureSelected(prim.ParentID, simulator);
                }
            }

            //CalcStats(prim);
            //UpdateTextureQueue(prim.Textures);
        }

        private void InternPrim(Simulator simulator, Primitive prim)
        {
            SimObject O = GetSimObject(prim, simulator);
            DeclareProperties(prim, prim.Properties, simulator);
            O.ResetPrim(prim, client, simulator);
            DeclareRequested(simulator, prim.LocalID);
            if (MaintainObjectUpdates)
                lock (LastObjectUpdate) LastObjectUpdate[O] = updatFromPrim0(prim);
        }


        //public override void Objects_OnNewAttachment(Simulator simulator, Primitive prim, ulong regionHandle,
        //                                             ushort timeDilation)
        //{
        //    if (!IsMaster(simulator)) return;
        //    if (ScriptHolder == null && prim.ParentID != 0 && prim.ParentID == client.Self.LocalID)
        //    {
        //        EnsureSelected(prim.LocalID, simulator);
        //    }
        //    if (!MaintainAttachments) return;
        //    Objects_OnNewPrim(simulator, prim, regionHandle, timeDilation);
        //    EventQueue.Enqueue(() => GetSimObject(prim, simulator).IsAttachment = true);
        //}

        public override void Avatars_OnAvatarAppearance(object sender, AvatarAppearanceEventArgs e)
        {
            client.Avatars.AvatarAppearance -= Avatars_OnAvatarAppearance;
            base.Avatars_OnAvatarAppearance(sender, e);
        }

        //object Objects_OnNewAvatarLock = new object();
        public override void Objects_OnNewAvatar(object sender, AvatarUpdateEventArgs e)
        {
            Avatar avatar = e.Avatar;
            var simulator = e.Simulator;
            var regionHandle = e.Simulator.Handle;
            if (regionHandle==0)
            {
                return;
            }
            SimObject AV = GetSimObject(avatar, simulator);
            if (avatar.ID == client.Self.AgentID)
            {
                if (AV is SimActor)
                {
                    m_TheSimAvatar = (SimActor)AV;
                    m_TheSimAvatar.SetClient(client);
                }
            }
            AV.IsKilled = false;
            if (IsMaster(simulator))
            //lock (Objects_OnNewAvatarLock)
            {
                AV.ResetPrim(avatar, client, simulator);
            }
            Objects_OnNewAvatar1(simulator, avatar, regionHandle);
        }


        public void Objects_OnNewAvatar1(Simulator simulator, Avatar avatar, ulong regionHandle)
        {
            try
            {
                Objects_OnNewPrimReal(simulator, avatar, regionHandle);
                if (avatar.LocalID == client.Self.LocalID)
                {
                    SimObject AV = (SimObject)GetSimObject(avatar, simulator);
                    if (AV is SimActor)
                    {
                        m_TheSimAvatar = (SimActor)AV;
                        m_TheSimAvatar.SetClient(client);
                    }
                }
            }
            catch (Exception e)
            {
                WriteLine(String.Format("err :{0}", e.StackTrace));
            }
        }


        public SimObject GetSimObject(uint sittingOn, Simulator simulator)
        {
            if (sittingOn == 0) return null;
            if (sittingOn == 13720000)
            {
            }
            EnsureSelected(sittingOn, simulator);
            Primitive p = GetPrimitive(sittingOn, simulator);
            int maxTries = 10;
            while (p == null && maxTries-- > 0)
            {
                Thread.Sleep(1000);
                p = GetPrimitive(sittingOn, simulator);
            }
            if (p == null)
            {
                client.Objects.RequestObject(simulator, sittingOn);
                //client.Objects.SelectObject(simulator, sittingOn);
                p = GetPrimitive(sittingOn, simulator);
                if (p != null) return GetSimObject(p, simulator);
                Debug("WARN: cant get prim " + sittingOn + " sim " + simulator);

                return null;
            }
            return GetSimObject(p, simulator);
        }

        public Avatar GetAvatar(UUID avatarID, Simulator simulator)
        {
            if (UUID.Zero == avatarID) throw new NullReferenceException("GetAvatar");
            Primitive prim = GetPrimitive(avatarID, simulator);
            if (prim is Avatar) return (Avatar)prim;
            // in case we request later
            if (!uuidTypeObject.ContainsKey(avatarID))
            {
                if (client.Network.Connected) RequestAvatarName(avatarID);
            }
            //   prim = GetPrimitive(avatarID, simulator);
            return null;
        }

        public Type GetType(UUID uuid)
        {
            Object found = GetObject(uuid);
            if (found == null) return null;
            //RegisterUUID(uuid] = found;
            if (found is Type) return (Type)found;
            return found.GetType();
        }


        static public object GetScriptableObject(object id)
        {
            if (id is NamedParam)
            {
                NamedParam kv = (NamedParam)id;
                id = new NamedParam(kv.Key, GetScriptableObject(kv.Value));
            }
            if (id is UUID)
            {
                UUID uid = (UUID)id;
                if (uid == UUID.Zero) return id;
                id = GridMaster.GetObject(uid);
            }
            if (id is Primitive) return GridMaster.GetSimObject((Primitive)id);
            // if (id is String) return Master.GetObject((string) id);
            return id;
        }

        public object GetObject(UUID id)
        {
            if (id == UUID.Zero) return null;

            object found;
            //lock (uuidTypeObject)
            if (uuidTypeObject.TryGetValue(id, out found))
            {
                //object found = uuidTypeObject[id];
                //if (found != null)
                return found;
            }
            //lock (uuidTypeObject)
            if (uuid2Group.TryGetValue(id, out found))
            {
                //object found = uuidTypeObject[id];
                //if (found != null)
                return found;
            }
            object ret = GetPrimitive(id, null);
            if (ret != null) return ret;
            //ret = GetAvatar(id);
            //if (ret != null) return ret;
            ret = GetAsset(id);
            if (ret != null) return ret;
            ret = GetAnimationName(id);
            if (ret != null) return ret;
            return id;
        }

        public string GetAnimationName(UUID id)
        {
            string name = SimAssetSystem.GetAssetName(id);
            if (name != null) return name;
            //lock (uuidTypeObject)
            {
                Object assetObject;
                if (uuidTypeObject.TryGetValue(id, out assetObject))
                    return "" + assetObject;
            }
            //            name = "unknown_anim " + id;
            //            RequestAsset(id, AssetType.Animation, true);
            //                RegionMasterTexturePipeline.OnAssetReceived+=
            //ImageDownload IMD = RegionMasterTexturePipeline.GetTextureToRender(id);
            //Debug(name);
            return "" + id;
        }


        public Primitive GetPrimitive(UUID id, Simulator simulator)
        {
            //lock (uuidTypeObject)                        
            Primitive found = null;
            {
                object simobject;
                if (uuidTypeObject.TryGetValue(id, out simobject))
                {
                    //object simobject = uuidTypeObject[id];
                    if (simobject != null && simobject is SimObject)
                        found = ((SimObject)simobject).Prim;
                    if (found != null) return found;
                }
            }
            if (simulator == null)
            {
                List<Simulator> sims = new List<Simulator>();
                {
                    lock (client.Network.Simulators)
                    {
                        sims.AddRange(client.Network.Simulators);
                    }

                    foreach (Simulator sim in sims)
                    {
                        Primitive p = GetPrimitive(id, sim);
                        if (p != null) return p;
                    }
                    return null;
                }
            }


            // lock (simulator.ObjectsPrimitives.Dictionary)
            {
                found = simulator.ObjectsPrimitives.Find(delegate(Primitive prim0)
                                                             {
                                                                 //EnsureSelected(prim0.LocalID, simulator);
                                                                 //EnsureSelected(prim0.ParentID, simulator);
                                                                 return (prim0.ID == id);
                                                             });
                if (found != null) return found;
            }
            ///lock (simulator.ObjectsAvatars.Dictionary)
            {
                found = simulator.ObjectsAvatars.Find(prim0 =>
                                                          {
                                                              if (prim0.ID == id)
                                                                  return true;
                                                              return false;
                                                          });
                if (found != null) return found;
            }

            //ulong handle = simulator.Handle;
            ////  lock (AllSimulators)
            //foreach (Simulator sim in AllSimulators)
            //{
            //    if (sim.Handle == handle && sim != simulator)
            //    {
            //        //  lock (sim.ObjectsPrimitives.Dictionary)
            //        {
            //            found = sim.ObjectsPrimitives.Find(delegate(Primitive prim0)
            //                                                   {
            //                                                       //EnsureSelected(prim0.LocalID, sim);
            //                                                       //EnsureSelected(prim0.ParentID, sim);
            //                                                       return (prim0.ID == id);
            //                                                   });
            //            if (found != null) return found;
            //        }
            //        //  lock (sim.ObjectsAvatars.Dictionary)
            //        {
            //            found = sim.ObjectsAvatars.Find(delegate(Avatar prim0)
            //                                                {
            //                                                    if (prim0.ID == id)
            //                                                    {
            //                                                        return true;
            //                                                    }
            //                                                    return false;
            //                                                });
            //            if (found != null) return found;
            //        }
            //    }
            //}
            //lock (uuidTypeObject)
            object simobjectf;
            if (uuidTypeObject.TryGetValue(id, out simobjectf))
            {
                //object simobject = uuidTypeObject[id];
                if (simobjectf != null && simobjectf is SimObject)
                    return ((SimObject)simobjectf).Prim;
            }
            return null;
        }

        public Primitive GetPrimitive(String str)
        {
            int argsUsed;
            List<SimObject> primitives = GetPrimitives(new[] { str }, out argsUsed);
            if (primitives.Count==0) return null;
            return primitives[0].Prim;
        }

        public Primitive GetPrimitive(uint id, Simulator simulator)
        {
            if (simulator == null)
            {
                List<Simulator> sims = new List<Simulator>();
                lock (client.Network.Simulators)
                {
                    sims.AddRange(client.Network.Simulators);
                }

                foreach (Simulator sim in sims)
                {
                    Primitive p = GetPrimitive(id, sim);
                    if (p != null) return p;
                }
                return null;
            }
            Primitive prim;
            lock (simulator.ObjectsPrimitives.Dictionary)
                if (simulator.ObjectsPrimitives.TryGetValue(id, out prim))
                {
                    return prim;
                }
            Avatar av;
            lock (simulator.ObjectsAvatars.Dictionary)
                if (simulator.ObjectsAvatars.TryGetValue(id, out av))
                {
                    return av;
                }
            RequestObject(simulator, id);
            //ulong handle = simulator.Handle;
            //EnsureSelected(id, simulator);
            //lock (AllSimulators)
            //foreach (Simulator sim in AllSimulators)
            //{
            //    if (sim.Handle == handle && sim != simulator)
            //    {
            //        if (sim.ObjectsPrimitives.TryGetValue(id, out prim))
            //        {
            //            return prim;
            //        }
            //        if (sim.ObjectsAvatars.TryGetValue(id, out av))
            //        {
            //            return av;
            //        }
            //    }
            //}
            return null;
        }

        public void SendNewRegionEvent(SimEventType type, string eventName, params object[] args)
        {
            client.SendPipelineEvent(new SimObjectEvent(type, SimEventClass.REGIONAL, eventName, args));
        }

        public void CalcStats(SimObject prim)
        {
            if (boringNamesHeuristic(prim) == 0)
                client.BoringNamesCount++;
            else
                client.GoodNamesCount++;
        }

        public string describePrim(Primitive target, bool detailed)
        {
            if (target == null) return "null";
            SimObject simObject = GetSimObject(target);
            string str = string.Empty;
            if (simObject != null)
            {
                if (detailed) str += simObject.DebugInfo();
                else str += simObject.ToString();
                str += String.Format("\n {0}", TheSimAvatar.DistanceVectorString(simObject));
                if (target is Avatar)
                {
                    str += String.Format(" {0}", target);
                }
                if (detailed)
                {
                    str += String.Format("\n GroupLeader: {0}", simObject.GetGroupLeader());
                }
            }
            else
            {
                str += target;
            }
            if (target.Properties != null && target.Properties.SalePrice != 0)
                str += " Sale: L" + target.Properties.SalePrice;
            if (!detailed) return str;
            //str += "\nPrimInfo: " + target.ToString());
            //str += "\n Type: " + GetPrimTypeName(target));
            str += "\n Light: " + target.Light;
            if (target.ParticleSys.CRC != 0)
                str += "\nParticles: " + target.ParticleSys;

            str += "\n TextureEntry:";
            if (target.Textures != null)
            {
                if (target.Textures.DefaultTexture != null)
                    str += "\n" + (String.Format("  Default texture: {0}",
                                                 target.Textures.DefaultTexture.TextureID.ToString()));

                for (int i = 0; i < target.Textures.FaceTextures.Length; i++)
                {
                    if (target.Textures.FaceTextures[i] != null)
                    {
                        str += "\n" + (String.Format("  Face {0}: {1}", i,
                                                     target.Textures.FaceTextures[i].TextureID.ToString()));
                    }
                }
            }
            return str; // WriteLine(str);
        }

        public void describePrimToAI(Primitive prim, Simulator simulator)
        {
            if (prim is Avatar)
            {
                Avatar avatar = (Avatar)prim;
                describeAvatarToAI(avatar);
                return;
            }
            //if (!primsKnown.Contains(prim))	return;
            if (true) return;
            //botenqueueLispTask("(on-prim-description '(" + prim.Properties.Name + ") '" + prim.Properties.Description + "' )");
            SimObject A = GetSimObject(prim);
            //WriteLine(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
            //WriteLine(avatar.Name + " is " + Vector3.Distance(GetSimPosition(), avatar.Position).ToString() + " distant.");
            client.SendPersonalEvent(SimEventType.MOVEMENT, "on-prim-dist", A, A.Distance(TheSimAvatar));
            SendNewRegionEvent(SimEventType.MOVEMENT, "on-prim-pos", A, A.GlobalPosition);
            BlockUntilProperties(prim, simulator);
            if (prim.Properties.Name != null)
            {
                SendNewRegionEvent(SimEventType.EFFECT, "on-prim-description", prim, "" + prim.Properties.Description);
                //WriteLine(prim.Properties.Name + ": " + prim.Properties.Description);
                //if (prim.Sound != UUID.Zero)
                //    WriteLine("This object makes sound.");
                //if (prim.Properties.SalePrice != 0)
                //    WriteLine("This object is for sale for L" + prim.Properties.SalePrice);
            }
        }

        public int comp(SimObject p1, SimObject p2)
        {
            return (int)(getFitness(p2) - getFitness(p1));
        }

        public List<Primitive> getPrimitives(int num)
        {
            List<SimObject> ret = new List<SimObject>();
            TheSimAvatar.ScanNewObjects(10, 100, false);
            var set = TheSimAvatar.GetKnownObjects();
            lock (set) set.ForEach(prim => ret.Add(prim));
            //foreach (Primitive prim in prims[simulator.Handle].ForEach.Values)
            //{
            //    ret.Add(prim);
            //}

            ret.Sort(new Comparison<SimObject>(comp));
            if (ret.Count > num)
                ret = ret.GetRange(0, num);

            ret.Sort(TheSimAvatar.CompareDistance);

            List<Primitive> ps = new List<Primitive>();
            foreach (SimObject os in ret)
            {
                var osp = os.Prim;
                ListAsSet<Primitive>.AddIfMissing(ps, osp);
            }
            return ps;
        }


        private float getFitness(SimObject prim)
        {
            if (true)
            {
                return /* ((float)prim.ToString().Length/5 -*/ (float)TheSimAvatar.Distance(prim);
            }
            float fitness = 1;
            foreach (SimObjectHeuristic heuristic in objectHeuristics)
            {
                fitness *= heuristic(prim);
            }
            return fitness;
        }


        private float boringNamesHeuristic(SimObject prim)
        {
            return prim.ToString().Length;
        }

        public string getObjectName(Primitive prim)
        {
            SimObject so = GetSimObject(prim);
            if (so != null) return "" + so;
            return "" + prim;
        }


        public string GetPrimTypeName(Primitive target)
        {
            if (target.PrimData.PCode == PCode.Prim)
                return target.Type.ToString();
            return target.PrimData.PCode.ToString();
        }


        public int numAvatars()
        {
            return SimAvatars.Count;
        }




        public void SetPrimFlags(Primitive UnPhantom, PrimFlags fs)
        {
            client.Objects.SetFlags(GetSimulator(UnPhantom),UnPhantom.LocalID, ((fs & PrimFlags.Physics) != 0), //
                                    ((fs & PrimFlags.Temporary) != 0),
                                    ((fs & PrimFlags.Phantom) != 0),
                                    ((fs & PrimFlags.CastShadows) != 0));
        }

        public IEnumerable<SimObject> GetAllSimObjects()
        {
            return SimObjects.CopyOf();
        }

        //public Primitive RequestMissingObject(uint localID, Simulator simulator)
        //{
        //    if (localID == 0) return null;
        //    EnsureSelected(localID, simulator);
        //    client.Objects.RequestObject(simulator, localID);
        //    Thread.Sleep(500);
        //    Primitive prim = GetPrimitive(localID, simulator);
        //    return prim;
        //}
        readonly static Dictionary<ulong, HashSet<uint>> RequestedObjects = new Dictionary<ulong, HashSet<uint>>();

        internal static void RequestObject(Simulator simulator, uint id)
        {
            if (id==0) return;
            //if (IsOpenSim) return;
            if (DeclareRequested(simulator, id))
                simulator.Client.Objects.RequestObject(simulator, id);
        }

        private static bool DeclareRequested(Simulator simulator, uint id)
        {
            if (id == 0 || simulator == null) return false;
            HashSet<uint> uints;
            lock (RequestedObjects)
            {
                if (!RequestedObjects.TryGetValue(simulator.Handle, out uints))
                {
                    RequestedObjects[simulator.Handle] = uints = new HashSet<uint>();
                }
            }
            lock (uints)
            {
                //if (true) return false;
                if (uints.Contains(id)) return false;
                uints.Add(id);
                return true;
            }
        }

        public static void EnsureSelected(uint LocalID, Simulator simulator)
        {
            if (LocalID == 0) return;
            if (NeverSelect(LocalID, simulator))
                ReallyEnsureSelected(simulator, LocalID);
        }

        public static bool NeverSelect(uint LocalID, Simulator simulator)
        {
            ulong Handle = simulator.Handle;
            if (LocalID != 0)
                lock (primsSelected)
                {
                    if (!primsSelected.ContainsKey(Handle))
                    {
                        primsSelected[Handle] = new HashSet<uint>();
                    }
                    lock (primsSelected[Handle])
                    {
                        if (!primsSelected[Handle].Contains(LocalID))
                        {
                            primsSelected[Handle].Add(LocalID);
                            return true;
                        }
                    }
                }
            return false;
        }

        private static void ReallyEnsureSelected(Simulator simulator, uint LocalID)
        {
            if (LocalID == 0) return;
            ulong Handle = simulator.Handle;
            if (Handle == 0)
            {
                return;
                throw new AbandonedMutexException();
            }
            lock (primsSelectedOutbox)
            {
                if (!primsSelectedOutbox.ContainsKey(Handle))
                {
                    primsSelectedOutbox[Handle] = new List<uint>();
                }
                lock (primsSelectedOutbox[Handle])
                    primsSelectedOutbox[Handle].Add(LocalID);
            }
        }

        private static void ReallyEnsureSelected_Thread(object sender)
        {
            lock (SelectObjectsTimerLock)
            {
                if (inTimer)
                {
                    Logger.DebugLog("ReallyEnsureSelected_Thread getting behind");
                    return;
                }
                inTimer = true;
            }
            lock (primsSelectedOutbox)
            {
                foreach (ulong simulator in new List<ulong>(primsSelectedOutbox.Keys))
                {
                    uint[] askFor;
                    Simulator S = SimRegion.GetRegion(simulator).TheSimulator;
                    if (S==null)
                    {
                        Debug("No sim yet for " + simulator);
                        continue;
                    }
                    lock (primsSelectedOutbox[simulator])
                    {
                        List<uint> uints = primsSelectedOutbox[simulator];
                        if (uints.Count > 200)
                        {
                            askFor = uints.GetRange(0, 200).ToArray();
                            uints.RemoveRange(0, 200);
                        }
                        else if (uints.Count > 0)
                        {
                            primsSelectedOutbox[simulator] = new List<uint>();
                            askFor = uints.ToArray();
                        }
                        else
                        {
                            continue;
                        }
                    }
                    S.Client.Objects.SelectObjects(S, askFor);
                }
            }
            lock (SelectObjectsTimerLock)
                inTimer = false;
        }


        public UUID GetAssetUUID(string a, AssetType type)
        {
            return SimAssetSystem.GetAssetUUID(a, type);
        }

        public SimObject GetSimObject(Primitive prim)
        {
            Simulator sim = GetSimulator(prim);
            if (sim==null)
            {
                
            }
            return GetSimObject(prim, sim);
        }

        private static void DoEvents()
        {
            //todo  throw new Exception("The method or operation is not implemented.");
        }


        internal static void ResetSelectedObjects()
        {
            lock (primsSelected)
                foreach (HashSet<uint> UInts in primsSelected.Values)
                {
                    lock (UInts) UInts.Clear();
                }
        }

        internal void ReSelectObject(Primitive P)
        {
            if (P == null)
            {
                DLRConsole.DebugWriteLine("NULL RESELECTOBJECT");
                return;
            }
            Simulator sim = GetSimulator(P);
            if (P.ParentID != 0)
            {
                client.Objects.SelectObjects(sim, new uint[] { P.LocalID, P.ParentID });
                return;
            }
            client.Objects.SelectObject(sim, P.LocalID);
        }

        internal SimAvatarImpl CreateSimAvatar(UUID uuid, WorldObjects objects, Simulator simulator)
        {
            if (uuid == UUID.Zero)
            {
                throw new NullReferenceException("UUID.Zero!");
            }
            // Request all of the packets that make up an avatar profile
            // lock (GetSimObjectLock)
            SimAvatarImpl obj0 = GetSimObjectFromUUID(uuid) as SimAvatarImpl;
            if (obj0 != null) return (SimAvatarImpl)obj0;
            lock (GetSimLock(simulator ?? client.Network.CurrentSim))
            {
                lock (uuidTypeObject)
                    //lock (SimObjects)
                    //  lock (SimAvatars)
                {
                    SimObject simObj = GetSimObjectFromUUID(uuid);
                    obj0 = simObj as SimAvatarImpl;
                    if (obj0 != null) return (SimAvatarImpl) obj0;
                    if (simObj != null)
                    {
                        Debug("SimObj->SimAvatar!?! " + simObj);
                    }
                    obj0 = new SimAvatarImpl(uuid, objects, simulator);
                    SimAvatars.Add((SimAvatar) obj0);
                    //client.Avatars.RequestAvatarPicks(uuid);
                    SimObjects.AddTo(obj0);
                    RegisterUUID(uuid, obj0);
                    RequestAvatarMetadata(uuid);
                    return (SimAvatarImpl) obj0;
                }
            }
        }

        internal SimObject CreateSimObject(UUID uuid, WorldObjects WO, Simulator simulator)
        {
            if (uuid == UUID.Zero)
            {
                throw new NullReferenceException("UUID.Zero!");
            }
            //  lock (GetSimObjectLock)
            SimObject obj0 = GetSimObjectFromUUID(uuid);
            if (obj0 != null) return obj0;
            simulator = simulator ?? client.Network.CurrentSim;
            lock (GetSimLock(simulator ?? client.Network.CurrentSim))
                lock (uuidTypeObject)
                   // lock (SimObjects)
                     //   lock (SimAvatars)
                        {
                            obj0 = GetSimObjectFromUUID(uuid);
                            if (obj0 != null) return obj0;
                            obj0 = new SimObjectImpl(uuid, WO, simulator);
                            SimObjects.AddTo(obj0);
                            RegisterUUID(uuid, obj0);
                            return obj0;
                        }
        }
    }
}