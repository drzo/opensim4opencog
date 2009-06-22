using System;
using System.Collections.Generic;
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
    public delegate float SimObjectHeuristic(SimObject prim);

    public partial class WorldObjects : AllEvents
    {

        public static bool CanPhantomize = false;
        public static bool CanUseSit = true;
        public static bool DoSimulatorsCatchUp = true;
        public static bool MaintainAnims = true;
        public static bool MaintainAnimsInFolders = false;
        public static bool MaintainAttachments = true;
        public static bool MaintainCollisions = true;
        public static bool MaintainEffects = true;
        public static bool MaintainPropertiesFromQueue = false;
        public static bool MaintainObjectUpdates = false;
        public static bool MaintainSounds = true;
        public static bool UseNewEventSystem = true;
        public static bool SimplifyBoxes = false; // true takes longer startup but speeds up runtime path finding
        public static bool SendAllEvents = MaintainObjectUpdates;


        private static readonly Dictionary<ulong, object> GetSimObjectLock = new Dictionary<ulong, object>();

        private static readonly Dictionary<Simulator, List<uint>> primsSelected = new Dictionary<Simulator, List<uint>>();
        private static readonly Dictionary<Simulator, List<uint>> primsSelectedOutbox = new Dictionary<Simulator, List<uint>>();

        private static readonly Queue<ThreadStart> PropertyQueue = new Queue<ThreadStart>();
        private static readonly object SelectObjectsTimerLock = new object();
        private static readonly List<ThreadStart> ShutdownHooks = new List<ThreadStart>();
        private static readonly Queue<ThreadStart> UpdateQueue = new Queue<ThreadStart>();
        private static readonly Dictionary<UUID, object> uuidTypeObject = new Dictionary<UUID, object>();
        private static readonly object WorldObjectsMasterLock = new object();

        private static int CountnumAvatars;

        public static readonly ListAsSet<SimAvatar> SimAvatars = new ListAsSet<SimAvatar>();
        public static readonly ListAsSet<SimObject> SimObjects = new ListAsSet<SimObject>();

        private static Thread TrackUpdateLagThread;
        private static Thread TrackUpdatesThread;

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

        public override string GetModuleName()
        {
            return "WorldSystem";
        }

        public override void StartupListener()
        {
            RegisterAll();
        }

        public override void ShutdownListener()
        {
            UnregisterAll();
        }


        public override bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {

            if (eventName.EndsWith("On-Image-Receive-Progress")) return true;
            if (eventName.EndsWith("Look-At")) return true;
            Console.WriteLine("\n" + eventName);
            for (int i = 0; i < paramNames.Length; i++)
            {
                Console.WriteLine(" " + paramNames[i] + ": " + client.argString(parameters[i]));
            }
            return true;
        }

        public WorldObjects(BotClient client)
            : base(client)
        {
            if (Utils.GetRunningRuntime() == Utils.Runtime.Mono)
            {
                client.Settings.USE_LLSD_LOGIN = true;
            } //else

            client.Settings.USE_LLSD_LOGIN = false;
            lock (WorldObjectsMasterLock)
            {
                if (Master == null) Master = this;

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
                client.Settings.SIMULATOR_TIMEOUT = 30 * 60000;
                client.Settings.SEND_AGENT_UPDATES = true;

                client.Settings.SEND_PINGS = false;

                client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                client.Self.Movement.AutoResetControls = false;
                client.Self.Movement.UpdateInterval = 0;

                client.Network.OnSimConnected += Network_OnSimConnectedHook;
                client.Inventory.OnScriptRunning += Inventory_OnScriptRunning;


                burstStartTime = DateTime.Now;
                burstInterval = new TimeSpan(0, 0, 0, 0, (int)(burstTime * 1000));
                searchStep = 1;

                numberedAvatars = new List<string>();


                if (Master == this)
                {
                    if (RegionMasterTexturePipeline == null)
                    {
                        RegionMasterTexturePipeline = client.Assets;
                        //RegionMasterTexturePipeline.OnDownloadFinished += new TexturePipeline.DownloadFinishedCallback(RegionMasterTexturePipeline_OnDownloadFinished);
                        client.Settings.USE_TEXTURE_CACHE = true;
                    }
                    else
                    {
                        //client.Settings.USE_TEXTURE_CACHE = false;
                    }

                    {
                        //BotWorld = this;
                        SimTypeSystem.LoadDefaultTypes();
                    }
                    if (TrackUpdatesThread == null)
                    {
                        TrackUpdatesThread = new Thread(TrackUpdates) { Name = "Track Updates/Properties" };
                        //TrackPathsThread.Priority = ThreadPriority.Lowest;
                        TrackUpdatesThread.Start();
                    }
                    if (TrackUpdateLagThread == null)
                    {
                        TrackUpdateLagThread = new Thread(Lagometer)
                                                   {
                                                       Name = "Lagometer thread",
                                                       Priority = ThreadPriority.Lowest
                                                   };
                        TrackUpdateLagThread.Start();
                    }
                    EnsureSelectedTimer = new Timer(ReallyEnsureSelected_Thread, null, 1000, 1000);
                    _SimPaths = new WorldPathSystem(this);
                    _simAssetSystem = new SimAssetStore(client);
                }
                //SetWorldMaster(false);
                //RegisterAll();
            }
        }

        public SimActor TheSimAvatar
        {
            get
            {
                if (m_TheSimAvatar == null)
                {
                    while (m_TheSimAvatar == null)
                    {
                        m_TheSimAvatar =
                            (SimActor)GetSimObject(GetAvatar(client.Self.AgentID, client.Network.CurrentSim));
                        if (m_TheSimAvatar == null)
                        {
                            Thread.Sleep(1000);
                            continue;
                        }

                        m_TheSimAvatar.SetClient(client);
                    }
                }
                return m_TheSimAvatar;
            }
        }


        public static implicit operator GridClient(WorldObjects m)
        {
            return m.client.gridClient;
        }

        internal static BotClient BotClientFor(GridClient client)
        {
            foreach (BotClient bc in TextForm.SingleInstance.Clients.Values)
            {
                if (bc.gridClient == client) return bc;
            }
            return null;
        }

        public override void Self_OnCameraConstraint(Vector4 collidePlane)
        {
            //base.Self_OnCameraConstraint(collidePlane);
        }


        private static void Lagometer()
        {
            while (true)
            {
                int tick = Environment.TickCount;
                lock (UpdateQueue)
                {
                    UpdateQueue.Enqueue(() => Console.WriteLine("\nUpdate lag {0}ms", (Environment.TickCount - tick)));
                }
                Thread.Sleep(30000);
            }
        }

        public void SetSimAvatar(SimActor simAvatar)
        {
            m_TheSimAvatar = simAvatar;
        }

        private static int DequeueProperties()
        {
            int didProps = 0;
            ThreadStart P;
            int Properties = 0;
            lock (PropertyQueue)
            {
                Properties = PropertyQueue.Count;
            }
            if (Properties > 0)
            {
                //todo   Debug("Start Processing Properties: " + Properties);

                while (Properties > 0)
                {
                    lock (PropertyQueue)
                    {
                        P = PropertyQueue.Dequeue();
                        Properties = PropertyQueue.Count;
                    }
                    P();
                    didProps++;
                }
                //todo Debug("Done processing Properties: " + didProps);
            }
            return didProps;
        }

        private static void Debug(string p)
        {
            if (Settings.LOG_LEVEL != Helpers.LogLevel.None)
                Console.WriteLine(p);
        }

        // these will be shared between Clients and regions

        //public static BotRegionModel BotWorld = null;
        //        TheBotsInspector inspector = new TheBotsInspector();

        ///  inspector.Show();
        public void CatchUp(Simulator simulator)
        {
            List<Primitive> primsCatchup;
            lock (simulator.ObjectsPrimitives.Dictionary)
                primsCatchup = new List<Primitive>(simulator.ObjectsPrimitives.Dictionary.Values);
            lock (simulator.ObjectsAvatars.Dictionary)
            {
                simulator.ObjectsAvatars.ForEach(a => primsCatchup.Add(a));
            }
            bool known = false;
            foreach (Primitive item in primsCatchup)
            {
                if (item.ID != UUID.Zero)
                {
                    //       lock (uuidTypeObject)
                    //         known = uuidTypeObject.ContainsKey(item.ID);
                    //   if (!known)
                    if (item.ParentID == 0 && SimRegion.OutOfRegion(item.Position)) continue;
                    GetSimObject(item, simulator);
                }
            }
        }


        public override string ToString()
        {
            return "(.WorldSystem " + client + ")";
        }

        public SimObject GetSimObject(Primitive prim, Simulator simulator)
        {
            if (prim == null) return null;
            //if (prim.ID == null) return null;
            SimObject obj0 = GetSimObjectFromPrimUUID(prim);
            if (obj0 != null)
            {
                if (simulator != null && prim.Properties == null)
                {
                    EnsureSelected(prim.LocalID, simulator);
                }
                return obj0;
            }

            if (simulator == null)
            {
                simulator = GetSimulator(prim);
            }
            lock (GetSimObjectLock)
            {
                if (!GetSimObjectLock.ContainsKey(simulator.Handle))
                    GetSimObjectLock[simulator.Handle] = new object();
            }

            lock (GetSimObjectLock[simulator.Handle])
            {
                obj0 = GetSimObjectFromPrimUUID(prim);
                if (obj0 != null) return obj0;
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
                    obj0 = new SimAvatarImpl((Avatar)prim, this, simulator);
                    // Request all of the packets that make up an avatar profile
                    lock (SimAvatars) SimAvatars.Add((SimAvatar)obj0);
                    client.Avatars.RequestAvatarProperties(prim.ID);
                }
                else
                {
                    obj0 = new SimObjectImpl(prim, this, simulator);
                }
                RegisterUUID(prim.ID, obj0);
                lock (SimObjects) SimObjects.AddTo((SimObject)obj0);
                SendOnAddSimObject(obj0);
            }
            return (SimObject)obj0;
        }

        private static SimObject GetSimObjectFromUUID(UUID id)
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
            WorldObjects WO = Master;

            Primitive p = WO.GetPrimitive(id, null);
            if (p == null) return null;
            return WO.GetSimObject(p);
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
            Debug(String.Format(p, args));
        }

        public void WriteLine(string p)
        {
            Debug(p);
            client.WriteLine(p);
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
            }
            return prim;
        }

        public static Primitive BlockUntilPrimValid(Primitive prim, Simulator simulator)
        {
            if (prim.Properties != null) return prim;
            EnsureSelected(prim.LocalID, simulator);
            while (prim.ID == UUID.Zero)
            {
                // TODO maybe add a timer
                Thread.Sleep(1000);
            }
            return prim;
        }

        
        public override void Objects_OnObjectKilled(Simulator simulator, uint objectID)
        {
            // had to move this out of the closure because the Primitive is gone later
            Primitive p = GetPrimitive(objectID, simulator);
            if (p == null)
            {
                //   base.Objects_OnObjectKilled(simulator, objectID);
                return;
            }
            SimObject O = GetSimObjectFromPrimUUID(p);
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() =>
                                        {
                                            if (O == null)
                                                O = GetSimObjectFromPrimUUID(p);
                                            // if (O == null)
                                            //     O = GetSimObject(p, simulator);
                                            if (O == null)
                                            {
                                                //  SendNewEvent("on-prim-killed", p);
                                                return;
                                            }
                                            if (Settings.LOG_LEVEL != Helpers.LogLevel.Info)
                                                Debug("Killing object: " + O);
                                            if (!(O is SimAvatar))
                                            {
                                                {
                                                    O.IsKilled = true;
                                                    lock (SimAvatars)
                                                        foreach (SimAvatar A in SimAvatars)
                                                        {
                                                            A.RemoveObject(O);
                                                        }
                                                    //lock (SimObjects) SimObjects.Remove(O);
                                                }
                                            }
                                            else
                                            {
                                                if (simulator == O.GetSimulator())
                                                {
                                                    Debug("Killing Avatar: " + O);
                                                    O.IsKilled = true;
                                                }
                                                else
                                                {
                                                    Debug("NOT Killing Avatar: " + O);
                                                }
                                            }
                                        });
        }

        public static void RegisterUUIDMaybe(UUID id, object type)
        {
            lock (uuidTypeObject)
            {
                object before;
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
                        if ("" + before == "" + type) return;
                        //todo Master.SendNewEvent("uuid-change",""+id, before, type);
                    }
                    uuidTypeObject[id] = type;
                }
            }
        }
        /*
		         
         On-Folder-Updated
        folderID: "29a6c2e7-cfd0-4c59-a629-b81262a0d9a2"
         */

        public override void Inventory_OnFolderUpdated(UUID folderID)
        {
            RegisterUUID(folderID, client.Inventory.Store[folderID]); //;;typeof(OpenMetaverse.InventoryFolder);
            //base.Inventory_OnFolderUpdated(folderID);
        }

        public override void Objects_OnObjectPropertiesFamily(Simulator simulator, Primitive.ObjectProperties props,
                                                              ReportType type)
        {
            base.Objects_OnObjectPropertiesFamily(simulator, props, type);
            Objects_OnObjectProperties(simulator, props);
            // Properties = new Primitive.ObjectProperties();
            //Properties.SetFamilyProperties(props);
            // GotPermissions = true;
            // GotPermissionsEvent.Set();        
            //  SendNewEvent("On-Object-PropertiesFamily", simulator, props, type);
        }

        public override void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle,
                                               ushort timeDilation)
        {
            CheckConnected(simulator);
            EnsureSimulator(simulator);

            if (prim.ID != UUID.Zero)
            {
                if (IsMaster(simulator))
                {
                    prim.RegionHandle = regionHandle;
                    SimObject O = GetSimObject(prim, simulator);
                    O.ResetPrim(prim, client, simulator);
                    // Make an initial "ObjectUpdate" for later diff-ing
                    if (MaintainObjectUpdates)
                        LastObjectUpdate[O] = updatFromPrim0(prim);
                }
            }
            EnsureSelected(prim.LocalID, simulator);
            EnsureSelected(prim.ParentID, simulator);

            //CalcStats(prim);
            //UpdateTextureQueue(prim.Textures);
        }



        public override void Objects_OnNewAttachment(Simulator simulator, Primitive prim, ulong regionHandle,
                                                     ushort timeDilation)
        {
            if (ScriptHolder == null && prim.ParentID != 0 && prim.ParentID == client.Self.LocalID)
            {
                EnsureSelected(prim.LocalID, simulator);
            }
            if (!MaintainAttachments) return;
            Objects_OnNewPrim(simulator, prim, regionHandle, timeDilation);
            lock (UpdateQueue) UpdateQueue.Enqueue(() => GetSimObject(prim, simulator).IsAttachment = true);
        }

        public override void Avatars_OnAvatarAppearance(UUID avatarID, bool isTrial,
                                                        Primitive.TextureEntryFace defaultTexture,
                                                        Primitive.TextureEntryFace[] faceTextures,
                                                        List<byte> visualParams)
        {
            client.Avatars.OnAvatarAppearance -= Avatars_OnAvatarAppearance;
            base.Avatars_OnAvatarAppearance(avatarID, isTrial, defaultTexture, faceTextures, visualParams);
        }

        //object Objects_OnNewAvatarLock = new object();
        public override void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle,
                                                 ushort timeDilation)
        {
            SimObject AV = GetSimObject(avatar, simulator);
            AV.IsKilled = false;
            if (IsMaster(simulator))
            //lock (Objects_OnNewAvatarLock)
            {
                AV.ResetPrim(avatar, client, simulator);
            }
            Objects_OnNewAvatar1(simulator, avatar, regionHandle, timeDilation);
        }


        public void Objects_OnNewAvatar1(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            try
            {
                Objects_OnNewPrim(simulator, avatar, regionHandle, timeDilation);
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
                client.Objects.SelectObject(simulator, sittingOn);
                p = GetPrimitive(sittingOn, simulator);
                if (p != null) return GetSimObject(p, simulator);
                Debug("WARN: cant get prim " + sittingOn + " sim " + simulator);

                return null;
            }
            return GetSimObject(p, simulator);
        }

        public Avatar GetAvatar(UUID avatarID, Simulator simulator)
        {
            Primitive prim = GetPrimitive(avatarID, simulator);
            if (prim is Avatar) return (Avatar)prim;
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
            if (id is UUID)
            {
                UUID uid = (UUID)id;
                if (uid == UUID.Zero) return id;
                return Master.GetObject(uid);
            }
            if (id is Primitive) return Master.GetSimObject((Primitive)id);
            // if (id is String) return Master.GetObject((string) id);
            return id;
        }

        public object GetObject(UUID id)
        {
            if (id == UUID.Zero) return null;

            lock (uuidTypeObject)
                if (uuidTypeObject.ContainsKey(id))
                {
                    object found = uuidTypeObject[id];
                    if (found != null)
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
            lock (uuidTypeObject)
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
            lock (uuidTypeObject)
            {
                if (uuidTypeObject.ContainsKey(id))
                {
                    object simobject = uuidTypeObject[id];
                    if (simobject != null && simobject is SimObject)
                        return ((SimObject)simobject).Prim;
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

            Primitive found = null;
            EnsureSimulator(simulator);

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

            ulong handle = simulator.Handle;
            //  lock (AllSimulators)
            foreach (Simulator sim in AllSimulators)
            {
                if (sim.Handle == handle && sim != simulator)
                {
                    //  lock (sim.ObjectsPrimitives.Dictionary)
                    {
                        found = sim.ObjectsPrimitives.Find(delegate(Primitive prim0)
                                                               {
                                                                   //EnsureSelected(prim0.LocalID, sim);
                                                                   //EnsureSelected(prim0.ParentID, sim);
                                                                   return (prim0.ID == id);
                                                               });
                        if (found != null) return found;
                    }
                    //  lock (sim.ObjectsAvatars.Dictionary)
                    {
                        found = sim.ObjectsAvatars.Find(delegate(Avatar prim0)
                                                            {
                                                                if (prim0.ID == id)
                                                                {
                                                                    return true;
                                                                }
                                                                return false;
                                                            });
                        if (found != null) return found;
                    }
                }
            }
            lock (uuidTypeObject)
                if (uuidTypeObject.ContainsKey(id))
                {
                    object simobject = uuidTypeObject[id];
                    if (simobject != null && simobject is SimObject)
                        return ((SimObject)simobject).Prim;
                }
            return null;
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
            EnsureSimulator(simulator);
            Primitive prim;
            if (simulator.ObjectsPrimitives.TryGetValue(id, out prim))
            {
                return prim;
            }
            Avatar av;
            if (simulator.ObjectsAvatars.TryGetValue(id, out av))
            {
                return av;
            }
            ulong handle = simulator.Handle;
            EnsureSelected(id, simulator);
            //lock (AllSimulators)
            foreach (Simulator sim in AllSimulators)
            {
                if (sim.Handle == handle && sim != simulator)
                {
                    if (sim.ObjectsPrimitives.TryGetValue(id, out prim))
                    {
                        return prim;
                    }
                    if (sim.ObjectsAvatars.TryGetValue(id, out av))
                    {
                        return av;
                    }
                }
            }
            return null;
        }

        public void SendNewEvent(string eventName, params object[] args)
        {
           // if (!IsRegionMaster) return;
            //if (UseNewEventSystem)
            {
                client.SendNewEvent(eventName, args);
                return;
            }
            for (int i = 0; i < args.Length; i++)
            {
                object arg = args[i];
                //     if (arg is UUID)
                {
                    object o = GetScriptableObject(arg);
                    if (o == null)
                    {
                        o = arg;
                    }
                    args[i] = o;
                }
            }
            client.SendNewEvent(eventName, args);
        }

        public void CalcStats(SimObject prim)
        {
            if (boringNamesHeuristic(prim) == 0)
                client.BoringNamesCount++;
            else
                client.GoodNamesCount++;
        }

        public BotMentalAspect GetObject(string name)
        {
            Primitive prim;
            if (tryGetPrim(name, out prim)) return GetSimObject(prim);
            return null;
        }
        public bool tryGetPrim(string name, out Primitive prim)
        {
            name = name.Trim().Replace("  ", " ");
            prim = null;
            uint pickNum = 0;
            UUID uuid;
            if (name.Contains("-") && UUID.TryParse(name, out uuid))
            {
                prim = GetPrimitive(uuid, null);
                if (prim != null) return true;
            }
            if (name.ToLower().StartsWith("primid"))
            {
                if (name.Length > 6)
                {
                    String s = name.Substring(6);
                    if (uint.TryParse(s, out pickNum))
                    {
                        prim = GetPrimitive(pickNum, null);
                        if (prim != null) return true;
                    }
                    pickNum = 0;
                }
            }

            if (name.Contains(" "))
            {
                string[] splitted = Parsing.ParseArguments(name);
                if (splitted.Length > 1)
                {
                    if (UInt32.TryParse(splitted[splitted.Length - 1], out pickNum))
                    {
                        name = String.Join("*", splitted, 0, splitted.Length - 1);
                    }
                }
            }
            List<SimObject> matches = new List<SimObject>();
            if (m_TheSimAvatar != null)
            {
                List<SimObject> set = TheSimAvatar.GetKnownObjects();
                lock (set)
                    if (set.Count == 0)
                    {
                        TheSimAvatar.ScanNewObjects(5, 100, false);
                        set = TheSimAvatar.GetKnownObjects();
                    }
                lock (set)
                    foreach (SimObject obj in set)
                    {
                        if (obj.Matches(name))
                        {
                            matches.Add(obj);
                        }
                    }
            }
            if (matches.Count == 0)
            {
                matches.AddRange(GetAllSimObjects(name));
            }
            if (matches.Count == 0) return false;
            if (matches.Count == 1)
            {
                prim = matches[0].Prim;
                return true;
            }
            bool retVal = false;

            if (m_TheSimAvatar != null) TheSimAvatar.SortByDistance(matches);
            if (pickNum != 0 && pickNum <= matches.Count)
            {
                prim = matches[(int)pickNum - 1].Prim;
                return true;
            }
            WriteLine("Found " + matches.Count + " matches: ");
            int num = 0;
            foreach (SimObject obj in matches)
            {
                num++;
                if (m_TheSimAvatar != null)
                    WriteLine(" " + num + ": " + obj + " " + TheSimAvatar.DistanceVectorString(obj));
                if (num == pickNum)
                {
                    prim = obj.Prim;
                    retVal = true;
                }
            }
            if (!retVal)
            {
                WriteLine("Use '" + name + " ###'");
            }
            return retVal;
        }

        public string describePrim(Primitive target, bool detailed)
        {
            if (target == null) return "null";
            SimObject simObject = GetSimObject(target);
            string str = string.Empty;
            if (simObject != null)
            {
                str += simObject.ToString();
                str += String.Format(" {0}", TheSimAvatar.DistanceVectorString(simObject));
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
            if (true) return;
            if (prim is Avatar)
            {
                Avatar avatar = (Avatar)prim;
                describeAvatarToAI(avatar);
                return;
            }
            //if (!primsKnown.Contains(prim))	return;
            BlockUntilProperties(prim, simulator);
            if (prim.Properties.Name != null)
            {
                //botenqueueLispTask("(on-prim-description '(" + prim.Properties.Name + ") '" + prim.Properties.Description + "' )");
                SendNewEvent("on-prim-dist", prim, Vector3.Distance(client.Self.SimPosition, prim.Position));
                SendNewEvent("on-prim-pos", prim, prim.Position);
                SendNewEvent("on-prim-description", prim, "" + prim.Properties.Description);
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
            List<SimObject> set = TheSimAvatar.GetKnownObjects();
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
                ps.Add(os.Prim);
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


        public void updateNumberedAvatars(List<Avatar> avatars)
        {
            numberedAvatars.Clear();
            for (int i = 0; i < avatars.Count; ++i)
                numberedAvatars.Add(avatars[i].Name);
        }

        public bool tryGetAvatarById(UUID id, out Avatar avatar)
        {
            avatar = GetAvatar(id, null);
            return (avatar is Avatar);
        }

        public bool tryGetAvatar(string name, out Avatar avatar)
        {
            avatar = null;
            Primitive prim;
            if (!tryGetPrim(name, out prim)) return false;
            if (prim is Avatar)
            {
                avatar = (Avatar)prim;
                return true;
            }
            return false;
        }

        public string getAvatarName(Avatar avatar)
        {
            string name = avatar.Name;
            for (int i = 0; i < numberedAvatars.Count; ++i)
                if (numberedAvatars[i] == name)
                    name = (i + 1) + ": " + name;
            return name;
        }


        public void SetPrimFlags(Primitive UnPhantom, PrimFlags fs)
        {
            client.Objects.SetFlags(UnPhantom.LocalID, ((fs & PrimFlags.Physics) != 0), //
                                    ((fs & PrimFlags.Temporary) != 0),
                                    ((fs & PrimFlags.Phantom) != 0),
                                    ((fs & PrimFlags.CastShadows) != 0));
        }

        public IEnumerable<SimObject> GetAllSimObjects()
        {
            return SimObjects.CopyOf();
        }

        public Primitive RequestMissingObject(uint localID, Simulator simulator)
        {
            if (localID == 0) return null;
            EnsureSelected(localID, simulator);
            client.Objects.RequestObject(simulator, localID);
            Thread.Sleep(1000);
            Primitive prim = GetPrimitive(localID, simulator);
            return prim;
        }

        public static void EnsureSelected(uint LocalID, Simulator simulator)
        {
            if (NeverSelect(LocalID, simulator))
                ReallyEnsureSelected(simulator, LocalID);
        }

        public static bool NeverSelect(uint LocalID, Simulator simulator)
        {
            if (LocalID != 0)
                lock (primsSelected)
                {
                    if (!primsSelected.ContainsKey(simulator))
                    {
                        primsSelected[simulator] = new List<uint>();
                    }
                    lock (primsSelected[simulator])
                    {
                        if (!primsSelected[simulator].Contains(LocalID))
                        {
                            primsSelected[simulator].Add(LocalID);
                            return true;
                        }
                    }
                }
            return false;
        }

        private static void ReallyEnsureSelected(Simulator simulator, uint LocalID)
        {
            lock (primsSelectedOutbox)
            {  
                if (!primsSelectedOutbox.ContainsKey(simulator))
                {
                    primsSelectedOutbox[simulator] = new List<uint>();
                }
                lock (primsSelectedOutbox[simulator])
                    primsSelectedOutbox[simulator].Add(LocalID);
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
                foreach (Simulator simulator in new List<Simulator>(primsSelectedOutbox.Keys))
                {
                    lock (primsSelectedOutbox[simulator])
                    {
                        List<uint> uints = primsSelectedOutbox[simulator];
                        if (uints.Count > 200)
                        {
                            simulator.Client.Objects.SelectObjects(simulator, uints.GetRange(0, 200).ToArray());
                            uints.RemoveRange(0, 200);
                        }
                        else if (uints.Count > 0)
                        {
                            primsSelectedOutbox[simulator] = new List<uint>();
                            simulator.Client.Objects.SelectObjects(simulator, uints.ToArray());
                        }
                    }
                }
            }
            lock (SelectObjectsTimerLock)
                inTimer = false;
        }


        public UUID GetAnimationUUID(string a)
        {
            return SimAssetSystem.GetAssetUUID(a);
        }

        public Primitive GetPrimitive(string[] args, out int argsUsed)
        {
            argsUsed = 0;
            if (args.Length == 0) return TheSimAvatar.theAvatar;
            int consume = args.Length;
            while (consume > 0)
            {
                string s = String.Join(" ", args, 0, consume);
                Primitive prim;


                if (tryGetPrim(s, out prim))
                {
                    SimObject simObject = GetSimObject(prim);
                    //         if (simObject.IsRegionAttached())
                    {
                        argsUsed = consume;
                        return simObject.Prim;
                    }
                }
                consume--;
            }
            return null;
        }

        public List<SimObject> GetAllSimObjects(string name)
        {
            List<SimObject> matches = new List<SimObject>();
            foreach (SimObject obj in GetAllSimObjects())
            {
                if (obj.Matches(name))
                {
                    matches.Add(obj);
                }
            }
            matches.Sort(TheSimAvatar.CompareDistance);
            return matches;
        }

        public SimObject GetSimObject(Primitive prim)
        {
            return GetSimObject(prim, null);
        }

        private static void DoEvents()
        {
            //todo  throw new Exception("The method or operation is not implemented.");
        }


        internal static void ResetSelectedObjects()
        {
            lock (primsSelected)
                foreach (List<uint> UInts in primsSelected.Values)
                {
                    lock (UInts) UInts.Clear();
                }
        }

        internal void ReSelectObject(Primitive P)
        {
            Simulator sim = GetSimulator(P);
            if (P == null)
            {
                Console.WriteLine("NULL RESELECTOBJECT");
                return;
            }
            client.Objects.SelectObject(sim, P.LocalID);
        }
      }
}