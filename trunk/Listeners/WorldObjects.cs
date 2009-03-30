using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using System.Reflection;
using OpenMetaverse.Packets;
using cogbot.TheOpenSims;
using System.Threading;
using cogbot.Actions;
using cogbot.TheOpenSims.Navigation;
using System.Windows.Forms; //using libsecondlife;

namespace cogbot.Listeners
{
    public delegate float ObjectHeuristic(SimObject prim);

    public class WorldObjects : DebugAllEvents
    {
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

        public SimGlobalRoutes GlobalRoutes = SimGlobalRoutes.Instance;
        public SimPathStore SimPaths
        {
            get { return TheSimAvatar.GetSimRegion().PathStore; }
        }
        public override void Parcels_OnSimParcelsDownloaded(Simulator simulator, InternalDictionary<int, Parcel> simParcels, int[,] parcelMap)
        {
            base.Parcels_OnSimParcelsDownloaded(simulator, simParcels, parcelMap);
        }

        public override void Network_OnConnected(object sender)
        {
            //Network_OnSimConnectedHook( (Simulator)sender);
            base.Network_OnConnected(sender);
            if (sender != client.gridClient)
            {
                throw new ArgumentException("wrong client " + sender);
            }
            RequestGridInfos();
        }

        static bool RequestedGridInfos = false;
        private void RequestGridInfos()
        {
            if (!RequestedGridInfos)
            {
                RequestedGridInfos = true;
                client.Grid.RequestMainlandSims(GridLayerType.Objects);
                //  client.Grid.RequestMainlandSims(GridLayerType.Terrain);
                //   client.Grid.RequestMapLayer(GridLayerType.Objects);
                // client.Grid.RequestMapLayer(GridLayerType.Terrain);
            }
        }

        public void Network_OnSimConnectedHook(Simulator simulator)
        {
            base.Network_OnSimConnected(simulator);
            lock (WorldObjectsMasterLock)
            {
                EnsureSimulator(simulator);
                IsConnected = true;
                if (SimRegion.IsMaster(simulator, client.gridClient))
                {
                    Debug("---SIMMASTER---------" + client + " region: " + simulator);
                    WorldMaster(true);
                    client.Grid.RequestMapRegion(simulator.Name, GridLayerType.Objects);
                    //client.Grid.RequestMapRegion(simulator.Name, GridLayerType.Terrain);
                    //client.Grid.RequestMapRegion(simulator.Name, GridLayerType.LandForSale);
                    //client.Grid.RequestMapItems(simulator.Handle,OpenMetaverse.GridItemType.Classified,GridLayerType.Terrain);

                    RegisterAll();
                    MasteringRegions.Add(simulator.Handle);
                    RequestGridInfos();
                }
                else
                {
                    Debug("-----NOT SIMMASTER-------" + client + " region: " + simulator);
                    MasteringRegions.Remove(simulator.Handle);
                    if (MasteringRegions.Count == 0)
                    {
                        WorldMaster(false);
                        Debug("------UNREGISTERING------" + client);
                        UnregisterAll();
                    }
                }
            }
        }

        public void CheckConnected(Simulator simulator)
        {
            if (!IsConnected)
            {
                Network_OnSimConnectedHook(simulator);
            }
        }

        public override void Network_OnEventQueueRunning(Simulator simulator)
        {
            base.Network_OnEventQueueRunning(simulator);
        }
        public override void Network_OnCurrentSimChanged(Simulator PreviousSimulator)
        {
            if (TheSimAvatar._CurrentRegion.TheSimulator == PreviousSimulator)
            {
                Debug("TheSimAvatar._CurrentRegion.TheSimulator == PreviousSimulator " + PreviousSimulator);
            }
            base.Network_OnCurrentSimChanged(PreviousSimulator);
        }
        public override void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason)
        {
            if (TheSimAvatar._CurrentRegion.TheSimulator == simulator)
            {
                Debug("TheSimAvatar._CurrentRegion.TheSimulator == simulator " + simulator);
            }
            base.Network_OnSimDisconnected(simulator, reason);
        }

        public override void Grid_OnGridRegion(GridRegion region)
        {
            SimRegion R = SimRegion.GetRegion(region.RegionHandle);
            R.GridInfo = region;
            base.Grid_OnGridRegion(region);
        }



        public override void Grid_OnRegionHandleReply(UUID regionID, ulong regionHandle)
        {
            RegisterUUID(regionID, SimRegion.GetRegion(regionHandle));
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
                }
            }
        }

        bool RegisterAllOnce = false;
        static public WorldObjects Master;
        List<ulong> MasteringRegions = new List<ulong>();
        public static Dictionary<ulong, WorldObjects> SimMaster = new Dictionary<ulong, WorldObjects>();
        //public volatile static WorldObjects Master;
        public void WorldMaster(bool isMaster)
        {
            lock (WorldObjectsMasterLock)
            {
                if (isMaster) Master = this;
                if (MasteringRegions.Count > 0 && !isMaster) throw new ArgumentException("Cant unmaster!");

                // client.Settings.OBJECT_TRACKING = isMaster;

                if (isMaster) RegisterAll();
            }
        }


        bool IsConnected = false;
        //readonly public OpenMetaverse.GUI.MiniMap miniMap;
        //protected Dictionary<string, Avatar> avatarCache = new Dictionary<string,Avatar>();
        public Vector3 compPos;
        public int searchStep;
        public List<string> numberedAvatars;
        public int burstSize = 100;
        public float burstTime = 1;
        public DateTime burstStartTime;
        public TimeSpan burstInterval;
        public float buildingSize = 5;
        public List<ObjectHeuristic> objectHeuristics;
        public Dictionary<UUID, List<Primitive>> primGroups;
        public Object newLock = new Object();
        public Dictionary<uint, OSDMap> lastOSD = new Dictionary<uint, OSDMap>();

        Dictionary<UUID, ObjectUpdate> lastObjectUpdate = new Dictionary<UUID, ObjectUpdate>();
        Dictionary<UUID, ObjectUpdate> lastObjectUpdateDiff = new Dictionary<UUID, ObjectUpdate>();
        Dictionary<Avatar, List<UUID>> avatarAminsSent = new Dictionary<Avatar, List<UUID>>();
        Dictionary<Avatar, UUID> avatarAminCurrent = new Dictionary<Avatar, UUID>();
        //Dictionary<UUID, UUID> texturesFinished = new Dictionary<UUID, UUID>();
        static Dictionary<Simulator, List<uint>> primsSelected = new Dictionary<Simulator, List<uint>>();
        static Dictionary<Simulator, List<uint>> primsSelectedOutbox = new Dictionary<Simulator, List<uint>>();

        static object WorldObjectsMasterLock = new object();
        public WorldObjects(BotClient client)
            : base(client)
        {
            lock (WorldObjectsMasterLock)
            {
                if (Master == null) Master = this;


                primGroups = new Dictionary<UUID, List<Primitive>>();

                objectHeuristics = new List<ObjectHeuristic>();
                objectHeuristics.Add(new ObjectHeuristic(distanceHeuristic));
                //objectHeuristics.Add(new ObjectHeuristic(nameLengthHeuristic));
                objectHeuristics.Add(new ObjectHeuristic(boringNamesHeuristic));

                //client.Settings.ENABLE_CAPS = false;
                client.Settings.ENABLE_SIMSTATS = true;
                client.Settings.AVATAR_TRACKING = true;
                client.Settings.THROTTLE_OUTGOING_PACKETS = false;
                client.Settings.MULTIPLE_SIMS = true;
                client.Settings.SEND_AGENT_UPDATES = true;
                client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                client.Self.Movement.AutoResetControls = false;
                client.Self.Movement.UpdateInterval = 0;

                client.Network.OnSimConnected += Network_OnSimConnectedHook;


                burstStartTime = DateTime.Now;
                burstInterval = new TimeSpan(0, 0, 0, 0, (int)(burstTime * 1000));
                searchStep = 1;

                numberedAvatars = new List<string>();


                if (RegionMasterTexturePipeline == null)
                {
                    RegionMasterTexturePipeline = new TexturePipeline(client, 100);
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
                if (TrackPathsThread == null)
                {
                    TrackPathsThread = new Thread(new ThreadStart(TrackPaths));
                    TrackPathsThread.Name = "TrackPathsThread";
                    //TrackPathsThread.Priority = ThreadPriority.Lowest;
                    TrackPathsThread.Start();
                }
                //WorldMaster(false);
                //RegisterAll();
            }
        }

        public SimAvatar TheSimAvatar
        {
            get
            {
                if (m_TheSimAvatar == null)
                {

                    m_TheSimAvatar = (SimAvatar)GetSimObject(GetAvatar(client.Self.AgentID, client.Network.CurrentSim));
                    m_TheSimAvatar.SetClient(client);
                }
                return m_TheSimAvatar;
            }
        }

        public SimAvatar m_TheSimAvatar;
        public void SetSimAvatar(SimAvatar simAvatar)
        {
            m_TheSimAvatar = simAvatar;
        }

        static public ListAsSet<SimAvatar> SimAvatars = new ListAsSet<SimAvatar>();
        //public SimPathStore SimPaths = SimPathStore.Instance;

        static Thread TrackPathsThread;

        static System.Threading.Timer InterpolationTimer; 
        static void TrackPaths()
        {
            InterpolationTimer = new System.Threading.Timer(new TimerCallback(ReallyEnsureSelected_Thread), null, 1000, 1000);
            Thread.Sleep(30000);
            int lastCount = 0;
            while (true)
            {
                Thread.Sleep(10000);
                ObjectUpdateItem U;
                int updates = 0;
                lock (updateQueue)
                {
                    updates = updateQueue.Count;
                }
                if (updates > 0)
                {
                    int did = 0;
                    Debug("Start Processing Updates: " + updates);
                    Console.WriteLine("Total Memory: {0}", GC.GetTotalMemory(false));

                    while (updates > 0)
                    {
                        lock (updateQueue)
                        {
                            U = updateQueue.Dequeue();
                            updates = updateQueue.Count;
                        }
                        U();
                        did++;
                    }
                    Debug("Done processing Updates: " + did);
                    Console.WriteLine("Total Memory: {0}", GC.GetTotalMemory(false));

                }
                int beforeCatchUp = SimObjects.Count;
                lock (AllSimulators)
                    foreach (Simulator S in AllSimulators)
                    {
                        Master.CatchUp(S);
                    }
                int thisCount = SimObjects.Count;
                if (beforeCatchUp != thisCount)
                {
                    Debug("Simulator catchup found: " + beforeCatchUp + " -> " + thisCount);
                    Console.WriteLine("Total Memory: {0}", GC.GetTotalMemory(false));
                }
                if (thisCount == lastCount)
                {
                    Thread.Sleep(20000);
                    Console.WriteLine("Total Memory: {0}", GC.GetTotalMemory(false));
                    continue;
                }

                Debug("TrackPaths Started: " + lastCount + "->" + thisCount);
                Console.WriteLine("Total Memory: {0}", GC.GetTotalMemory(false));
                lastCount = thisCount;
                int occUpdate = 0;
                foreach (SimObject O in SimObjects.CopyOf())
                {
                    //   DoEvents();
                    if (O.IsRegionAttached())
                    {
                        O.UpdateOccupied();
                        occUpdate++;
                    }
                    if (occUpdate % 100 == 0)
                    {
                        Console.Write(".");
                        Console.Out.Flush();
                    }
                    //if (occUpdate
                }
                Debug("TrackPaths Completed: " + thisCount);
                Console.WriteLine("Total Memory: {0}", GC.GetTotalMemory(false));
                SimRegion.BakeRegions();
                Console.WriteLine("Total Memory: {0}", GC.GetTotalMemory(false));
            }
        }

        private static void HeapShot()
        {
            System.GC.Collect();
            Console.WriteLine("kill -PROF ");
            //System.Runtime.InteropServices.SEHException
            /// Console.WriteLine(Console.ReadLine());
        }

        static void Debug(string p)
        {
            Console.WriteLine(p);
        }

        // these will be shared between Clients and regions
        static public ListAsSet<SimObject> SimObjects = new ListAsSet<SimObject>();

        //public static BotRegionModel BotWorld = null;
        //        TheBotsInspector inspector = new TheBotsInspector();

        ///  inspector.Show();

        public void CatchUp(Simulator simulator)
        {
            List<Primitive> primsCatchup;
            lock (simulator.ObjectsPrimitives.Dictionary)
                primsCatchup = new List<Primitive>(simulator.ObjectsPrimitives.Dictionary.Values);
            foreach (Primitive item in primsCatchup) GetSimObject(item, simulator);
        }


        static int CountnumAvatars;

        public SimObject GetSimObject(Primitive prim, Simulator simulator)
        {
            Object obj0;
            lock (uuidTypeObject)
            {
                if (prim.ID != UUID.Zero && uuidTypeObject.TryGetValue(prim.ID, out obj0))
                {
                    if (obj0 is SimObject)
                    {
                        SimObject O = (SimObject)obj0;
                        if (O.Prim.RegionHandle == prim.RegionHandle)
                        {
                            return O;
                        }
                        else
                        {
                            //if (prim is Avatar)
                            //{
                            //    Debug("Avatar moved regions? " + O);
                            //    O.ResetRegion(simulator.Handle);
                            //    O.ResetPrim(prim);
                            //    return O;
                            //}
                            if (prim.ParentID == 0)
                            {
                                if (!OutOfRegion(prim.Position))
                                {
                                    O.ResetPrim(prim);
                                }
                                Debug("Prim with differnt region handle " + prim);
                            }
                            else
                            {
                                Debug("Child with differnt region handle " + prim);
                            }
                            return O;
                        }
                    }
                }


                // not found
                if (prim is Avatar)
                {
                    CountnumAvatars++;
                    Debug("+++++++++++++++Making AVATAR" + prim);
                    if (prim.ID == UUID.Zero)
                    {
                        Debug("  - - -#$%#$%#$%% - ------- - Wierd Avatar " + prim);
                        BlockUntilPrimValid(prim, simulator);
                        Debug("  - - -#$%#$%#$%% - ------- - Unwird Avatar " + prim);
                    }
                    obj0 = new SimAvatar((Avatar)prim, this, simulator);
                    lock (SimAvatars) SimAvatars.Add((SimAvatar)obj0);
                }
                else
                {
                    obj0 = new SimObject(prim, this, simulator);
                }
                RegisterUUID(prim.ID, obj0);
                lock (SimObjects) SimObjects.AddTo((SimObject)obj0);

            }
            return (SimObject)obj0;

        }

        public override void Assets_OnXferReceived(XferDownload xfer)
        {
            RegisterUUID(xfer.ID, xfer);
        }

        public override void Assets_OnAssetReceived(AssetDownload transfer, Asset asset)
        {
            RegisterUUID(transfer.ID, transfer);
            RegisterUUID(asset.AssetID, asset);
        }
        /*
        On-Image-Received
             image: "{OpenMetaverse.ImageDownload,PacketCount=33,Codec=J2C,NotFound=False,Simulator=OpenSim Test (71.197.210.170:9000),PacketsSeen=System.Collections.Generic.SortedList`2[System.UInt16,System.UInt16],ImageType=Normal,DiscardLevel=-1,Priority=1013000,ID=728dd7fa-a688-432d-a4f7-4263b1f97395,Size=33345,AssetData=System.Byte[],Transferred=33345,Success=True,AssetType=Texture}"
             asset: "{OpenMetaverse.AssetTexture,Image=,LayerInfo=,Components=0,AssetData=System.Byte[],Temporary=False}"
            41031 [9] DEBUG - Worker 3 Downloaded texture 728dd7fa-a688-432d-a4f7-4263b1f97395
         */
        public override void Assets_OnImageReceived(ImageDownload image, AssetTexture asset)
        {
            RegisterUUID(image.ID, image);
            RegisterUUID(asset.AssetID, asset);
        }


        static TexturePipeline RegionMasterTexturePipeline;

        public void StartTextureDownload(UUID id)
        {
            RegionMasterTexturePipeline.RequestTexture(id, ImageType.Normal);
        }
        void RegionMasterTexturePipeline_OnDownloadFinished(UUID id, bool success)
        {
            if (success)
            {
                // Save this texture to the hard drive
                ImageDownload image = RegionMasterTexturePipeline.GetTextureToRender(id);
                try
                {
                    RegisterUUID(id, image);
                    //lock (uuidTextures) uuidTextures[id] = image;
                }
                catch (Exception)
                {
                }
            }
            else
            {
                //   WriteLine("Texture failed to download: " + id.ToString(), Helpers.LogLevel.Warning);
            }
        }

        public virtual void Debug(string p, params object[] args)
        {
            Debug(String.Format(p, args));
        }

        public void output(string p)
        {
            Debug(p);
            client.output(p);
        }

        public override void Self_OnMeanCollision(MeanCollisionType type, UUID perp, UUID victim, float magnitude, DateTime time)
        {
            Avatar perpAv, victimAv;
            if (tryGetAvatarById(perp, out perpAv) && tryGetAvatarById(victim, out victimAv))
            {
                if (victimAv.Name == client.Self.Name)
                    output(perpAv.Name + " bumped into $bot.");
                else if (perpAv.Name == client.Self.Name)
                    output("$bot bumped into " + victimAv.Name + ".");

                SendNewEvent("on-meanCollision", perpAv.Name, victimAv.Name, magnitude);

            }
        }

        public override void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, float gain, ulong regionHandle, Vector3 position)
        {
            //throw new NotImplementedException();
            SendNewEvent("On-Sound-Trigger", soundID, soundID, ownerID, objectID, parentID, gain, regionHandle, position);
        }


        public override void Sound_OnPreloadSound(UUID soundID, UUID ownerID, UUID objectID)
        {
            base.Sound_OnPreloadSound(soundID, ownerID, objectID);
            //output("preload sound " + soundID);
        }


        public Primitive BlockUntilProperties(Primitive prim, Simulator simulator)
        {
            if (prim.Properties != null) return prim;
            EnsureSelected(prim.LocalID, simulator);
            while (prim.Properties == null)
            { // TODO maybe add a timer
                Thread.Sleep(1000);
            }
            return prim;
        }

        public Primitive BlockUntilPrimValid(Primitive prim, Simulator simulator)
        {
            if (prim.Properties != null) return prim;
            EnsureSelected(prim.LocalID, simulator);
            while (prim.ID == UUID.Zero)
            { // TODO maybe add a timer
                Thread.Sleep(1000);
            }
            return prim;
        }

        public override void Avatars_OnAvatarAnimation(UUID avatarID, InternalDictionary<UUID, int> anims)
        {
            Avatar avatar = GetAvatar(avatarID, null);
            if (avatar == null) return;

            //List<UUID> currents = new List<UUID>();
            List<String> names = new List<String>();
            UUID mostCurrentAnim = UUID.Zero;
            int mostCurrentSequence = -1;


            anims.ForEach(delegate(UUID key)
                {
                    int animNumber;
                    anims.TryGetValue(key, out animNumber);
                    if (animNumber >= mostCurrentSequence)
                    {
                        mostCurrentSequence = animNumber;
                        mostCurrentAnim = key;
                    }
                    // currents.Add(key);
                    names.Add(GetAnimationName(key));
                });
            lock (avatarAminCurrent)
            {
                String newName = GetAnimationName(mostCurrentAnim);
                if (avatarAminCurrent.ContainsKey(avatar))
                {
                    UUID oldAnim = avatarAminCurrent[avatar];
                    if (oldAnim != mostCurrentAnim)
                    {
                        String oldName = GetAnimationName(oldAnim);
                        if (oldName.Length > 4 && newName.Length > 4 && oldName.Substring(0, 5) == newName.Substring(0, 5))
                        {
                        }
                        else
                            SendNewEvent("On-Object-Animation", avatar, newName);
                    }
                }
                else
                {
                    SendNewEvent("On-Object-Animation", avatar, newName);
                }
            }
            avatarAminCurrent[avatar] = mostCurrentAnim;


            if (avatarAminsSent.ContainsKey(avatar))
            {

            }
            //SendNewEvent("On-Avatar-Animation", avatar, names);
        }

        public override void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        {
            Avatars_OnAvatarAnimation(client.Self.AgentID, agentAnimations);
        }

        public override void  Grid_OnCoarseLocationUpdate(Simulator sim, List<UUID> newEntries, List<UUID> removedEntries)
        {

            //for (int i = 0; i < coarse.Location.Length; i++)
            //{
            //    if (i == coarse.Index.$bot)
            //    {
            //        simulator.positionIndexYou = i;
            //    }
            //    else if (i == coarse.Index.Prey)
            //    {
            //        simulator.positionIndexPrey = i;
            //    }
            //    simulator.avatarPositions.Add(new Vector3(coarse.Location[i].X, coarse.Location[i].Y,
            //        coarse.Location[i].Z * 4));
            //}


            //OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, simulator);
        }



        static readonly string[] paramNamesOnObjectKilled = new string[] { "simulator", "objectID" };
        static readonly Type[] paramTypesOnObjectKilled = new Type[] { typeof(Simulator), typeof(uint) };

        public override void Objects_OnObjectKilled(Simulator simulator, uint objectID)
        {
            base.Objects_OnObjectKilled(simulator, objectID);
            Primitive p = GetPrimitive(objectID, simulator);
            if (p != null)
            {
                SimObject O = GetSimObject(p, simulator);
                Debug("Killing object: " + O);
                if (!(O is SimAvatar))
                {
                    if (O != null)
                    {
                        O.IsKilled = true;
                        lock (SimAvatars)
                            foreach (SimAvatar A in SimAvatars)
                            {
                                A.RemoveObject(O);
                            }
                        lock (SimObjects) SimObjects.Remove(O);

                    }
                }
                // O.Prim = null;
                //lock (simulator.ObjectsPrimitives.Dictionary)
                //{
                //    if (simulator.ObjectsPrimitives.Dictionary.ContainsKey(objectID))
                //    {
                //        simulator.ObjectsPrimitives.Dictionary.Remove(objectID);
                //    }

                //}
                //lock (simulator.ObjectsAvatars.Dictionary)
                //{
                //    if (simulator.ObjectsAvatars.Dictionary.ContainsKey(objectID))
                //    {
                //        simulator.ObjectsAvatars.Dictionary.Remove(objectID);
                //    }

                //}
                SendNewEvent("on-prim-killed", p);
            }
        }
        /*
        On-Effect
        type: "Sphere"
        sourceID: "00000000-0000-0000-0000-000000000000"
        targetID: "00000000-0000-0000-0000-000000000000"
        targetPos: '(Vector3d 256126.750573638 256129.791631699 25.2663780227304)
        duration: 0.25
        id: "a31efee6-a426-65a6-5ef2-d1345be49233"
         */
        public override void Avatars_OnEffect(EffectType type, UUID sourceID, UUID targetID, Vector3d targetPos, float duration, UUID id)
        {
            //            SendNewEvent("on-effect",targetPos,id)
            RegisterUUID(id, type);
            base.Avatars_OnEffect(type, sourceID, targetID, targetPos, duration, id);
            SimRegion.TaintArea(targetPos);
        }

        public static void RegisterUUID(UUID id, object type)
        {
            if (type is Primitive)
            {
                Debug("cant register " + type);
            }
           // if (type is SimObject)
                lock (uuidTypeObject) uuidTypeObject[id] = type;
        }
        /*
         
         On-Folder-Updated
        folderID: "29a6c2e7-cfd0-4c59-a629-b81262a0d9a2"
         */

        static Dictionary<UUID, object> uuidTypeObject = new Dictionary<UUID, object>();
        public override void Inventory_OnFolderUpdated(UUID folderID)
        {
            RegisterUUID(folderID, client.Inventory.Store[folderID]); //;;typeof(OpenMetaverse.InventoryFolder);
            //base.Inventory_OnFolderUpdated(folderID);
        }
        static readonly string[] paramNamesOnObjectPropertiesFamily = new string[] { "simulator", "props", "type" };
        static readonly Type[] paramTypesOnObjectPropertiesFamily = new Type[] { typeof(Simulator), typeof(Primitive.ObjectProperties), typeof(ReportType) };

        public override void Objects_OnObjectPropertiesFamily(Simulator simulator, Primitive.ObjectProperties props, ReportType type)
        {
            // Properties = new Primitive.ObjectProperties();
            //Properties.SetFamilyProperties(props);
            // GotPermissions = true;
            // GotPermissionsEvent.Set();        
            //Objects_OnObjectProperties(simulator, props);
            SendNewEvent("On-Object-PropertiesFamily", simulator, props, type);
        }

        static readonly string[] paramNamesOnObjectUpdated = new string[] { "simulator", "update", "regionHandle", "timeDilation" };
        static readonly Type[] paramTypesOnObjectUpdated = new Type[] { typeof(Simulator), typeof(ObjectUpdate), typeof(ulong), typeof(ushort) };

        public override void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
        {
            CheckConnected(simulator);
            EnsureSimulator(simulator);

            if (prim.ID != UUID.Zero)
            {
                GetSimObject(prim, simulator).Prim = prim;
                lastObjectUpdate[prim.ID] = updatFromPrim(prim);
            }
            // Make an intial "ObjectUpdate" for later diffing

            EnsureSelected(prim.LocalID, simulator);
            EnsureSelected(prim.ParentID, simulator);
            //CalcStats(prim);
            //UpdateTextureQueue(prim.Textures);
        }

        /// <summary>
        /// This is all bot simulaor references: the Count would be Bots*Regions
        /// </summary>
        static List<Simulator> _AllSimulators = new List<Simulator>();

        static List<Simulator> AllSimulators
        {
            get
            {
                List<Simulator> sims = null;

                lock (_AllSimulators)
                    sims = new List<Simulator>(_AllSimulators);

                return sims;
            }
        }

        public void EnsureSimulator(Simulator simulator)
        {
            if (simulator == null) return;
            lock (_AllSimulators)
            {
                if (!_AllSimulators.Contains(simulator))
                    _AllSimulators.Add(simulator);
            }
        }

        public override void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        {
            SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), properties);
        }

        static Queue<ObjectUpdateItem> updateQueue = new Queue<ObjectUpdateItem>();

        delegate void ObjectUpdateItem();
        public override void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {

            CheckConnected(simulator);
            lock (updateQueue) updateQueue.Enqueue(delegate()
            {
                Objects_OnObjectProperties1(simulator, props);
            });
        }
        public void Objects_OnObjectProperties1(Simulator simulator, Primitive.ObjectProperties props)
        {
            Primitive prim = GetPrimitive(props.ObjectID, simulator);
            if (prim != null)
            {
                SimObject updateMe = GetSimObject(prim, simulator);
                updateMe.UpdateProperties(props);
            }
            describePrimToAI(prim, simulator);
        }

        public override void Avatars_OnAvatarAppearance(UUID avatarID, bool isTrial, Primitive.TextureEntryFace defaultTexture, Primitive.TextureEntryFace[] faceTextures, List<byte> visualParams)
        {
            base.Avatars_OnAvatarAppearance(avatarID, isTrial, defaultTexture, faceTextures, visualParams);
        }

        object Objects_OnNewAvatarLock = new object();
        public override void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            //lock (Objects_OnNewAvatarLock)
            {
                GetSimObject(avatar, simulator).Prim = avatar;
            }
            Objects_OnNewAvatar1(simulator, avatar, regionHandle, timeDilation);
        }
        public void Objects_OnNewAvatar1(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            try
            {
                Objects_OnNewPrim(simulator, avatar, regionHandle, timeDilation);
                lock (updateQueue) updateQueue.Enqueue(delegate()
                {
                    SimAvatar AV = (SimAvatar)GetSimObject(avatar, simulator);
                    if (avatar.LocalID == client.Self.LocalID)
                    {
                        if (AV != null)
                        {
                            m_TheSimAvatar = AV;
                            AV.SetClient(client);
                        }
                    }
                });

            }
            catch (Exception e)
            {
                output("err :" + e.StackTrace);
            }
        }

        public override void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            /// return;
            if (simulator.Handle != regionHandle)
            {
                Debug("Strange update" + simulator);
                base.Objects_OnObjectUpdated(simulator, update, regionHandle, timeDilation);
            }
            // return;
            CheckConnected(simulator);
            if (update.Avatar)
            {
                Primitive av = GetPrimitive(update.LocalID, simulator);
                if (av != null)
                {
                    SimAvatar AV = null;
                    lock (uuidTypeObject)
                    {
                        if (uuidTypeObject.ContainsKey(av.ID))
                        {

                            AV = (SimAvatar)uuidTypeObject[av.ID];
                        }
                    }
                    if (AV != null)
                    {
                        {
                            ulong rh = AV.theAvatar.RegionHandle;
                            if (!OutOfRegion(update.Position))
                            {
                                if (rh != regionHandle)
                                {
                                    AV.ResetRegion(regionHandle);
                                }
                                AV.Prim = (Avatar)av;
                            }
                        }
                        if (av.ParentID == 0 && !OutOfRegion(update.Position))
                        {
                            SimPathStore PathStore = SimRegion.GetRegion(simulator).PathStore;
                            PathStore.UpdateTraveled(av.ID, av.Position, av.Rotation);
                        }
                    }
                }
                return;
            }
            lock (updateQueue) updateQueue.Enqueue(delegate()
                {
                    Objects_OnObjectUpdated1(simulator, update, regionHandle, timeDilation);
                });

        }

        internal bool OutOfRegion(Vector3 v3)
        {
            if (v3.X < 0 || v3.X > 255.99f)
                return true;
            if (v3.Y < 0 || v3.Y > 255.99f)
                return true;
            return false;
        }

        public void Objects_OnObjectUpdated1(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            // base.Objects_OnObjectUpdated(simulator, update, regionHandle, timeDilation);
            Primitive objectUpdated = GetPrimitive(update.LocalID, simulator);
            //Debug("timeDilation " + timeDilation);

            if (objectUpdated != null)
            {
                //lock (objectUpdated)
                {
                    // other OMV code already updated the Primitive
                    // updateToPrim(prim, update);

                    if (objectUpdated.Properties != null)
                    {
                        //CalcStats(objectUpdated);
                        describePrimToAI(objectUpdated, simulator);
                    }
                    else
                    {
                        EnsureSelected(objectUpdated.LocalID, simulator);
                    }

                    // bool needsOsdDiff = false; //for debugging should be false otherwise

                    // Make a Last Object Update from the Primitive if we knew nothing about it
                    lock (lastObjectUpdate) if (!lastObjectUpdate.ContainsKey(objectUpdated.ID))
                        {
                            lastObjectUpdate[objectUpdated.ID] = updatFromPrim(objectUpdated);
                        }

                    // Make a "diff" from previous
                    {
                        Object diffO = notifyUpdate(objectUpdated, lastObjectUpdate[objectUpdated.ID], update, InformUpdate);
                        if (diffO != null)
                        {
                            ObjectUpdate diff = (ObjectUpdate)diffO;
                            //if (lastObjectUpdateDiff.ContainsKey(objectUpdated.ID))
                            //{
                            //    notifyUpdate(objectUpdated, lastObjectUpdateDiff[objectUpdated.ID], diff, InformUpdateDiff);
                            //}
                            lock (lastObjectUpdateDiff) lastObjectUpdateDiff[objectUpdated.ID] = diff;
                        }

                        else
                        {
                            // someThingElseNeedsUpdate(objectUpdated);
                            //  needsOsdDiff = true;
                        }
                    }

                    SimObject simObject = GetSimObject(objectUpdated, simulator);
                    if (simObject != null)
                    {
                        // AddTracking(simObject);
                        if (update.Avatar)
                        {
                            //   if (false)
                            //    if (objectUpdated.ID != client.Self.LocalID)
                            //  {  // Way point creation from other avatars moving
                            //   new Thread(new ThreadStart(delegate()
                            {
                                // if (m_TheSimAvatar!=null) {
                                // SimRegion.GetRegion(regionHandle).PathStore.
                                //     Update(objectUpdated.ID, simObject.GetSimPosition(), update.Rotation);
                                // }
                                //   })).Start();
                                // output("Updating state for Avatar " + prim.Name);
                            }
                        }
                        // Call SimObject Update the Previous object update will be saved in the "lastObjectUpdate[objectUpdated.ID]"
                        //Delegate d= new delegate()
                        {
                            ObjectUpdate TheDiff = default(ObjectUpdate);
                            lock (lastObjectUpdateDiff)
                            {
                                if (lastObjectUpdateDiff.ContainsKey(objectUpdated.ID))
                                {
                                    TheDiff = lastObjectUpdateDiff[objectUpdated.ID];
                                }
                                else
                                {
                                    lastObjectUpdateDiff[objectUpdated.ID] = TheDiff;
                                }
                            }
                            SendNewEvent("OnObjectUpdated1", simObject, update);
                            simObject.UpdateObject(update, TheDiff);
                        }
                    }


                    // BlockUntilProperties(objectUpdated);
                    //if (false && objectUpdated.Properties != null)
                    //{
                    //    OSDMap after = (OSDMap)objectUpdated.GetOSD();
                    //    //  Debug(OSDParser.SerializeLLSDXmlString(after));
                    //    if (lastOSD.ContainsKey(objectUpdated.ID))
                    //    {
                    //        if (needsOsdDiff)
                    //        {
                    //            //Primitive before = lastDeepCopy[objectUpdated.ID];
                    //            OSDMap before = lastOSD[objectUpdated.ID];
                    //            String osdDiff = OSDDiff(before, after);
                    //            if (osdDiff.Length > 0)
                    //            {
                    //                Debug(osdDiff);
                    //            }
                    //        }

                    //    }

                    //    lastOSD[objectUpdated.ID] = after;
                    //}

                    //Primitive deep = (Primitive)deepCopy(objectUpdated);
                    //lastDeepCopy[objectUpdated.ID] = deep;
                    lock (lastObjectUpdate) lastObjectUpdate[objectUpdated.ID] = update;
                }
            }
            else
            {
                output("missing Objects_OnObjectUpdated");
            }
        }

        delegate object DoWhat(Primitive objectUpdated, string p, Object vector3, Object vector3_4, Object diff);
        public object InformUpdate(Primitive objectUpdated, string p, Object before, Object after, Object diff)
        {
            //Debug("{0} {1} DIFF {2} BEFORE {3} AFTER {4}", p, objectUpdated, diff, before, after);
            if (diff is Vector3)
            {
                // if the change is too small skip the event
                if ((1 > ((Vector3)diff).Length()))
                {
                    //  return after;
                }
            }
            //  String lispName = "on-" + ((objectUpdated is Avatar) ? "avatar-" : "prim-") + p.ToLower() + "-updated";
            String lispName = "on-object-" + p.ToLower();
            SendNewEvent(lispName, objectUpdated, after);
            return after;
        }

        Object notifyUpdate(Primitive objectUpdated, ObjectUpdate before, ObjectUpdate after, DoWhat didUpdate)
        {
            ObjectUpdate diff = updateDiff(before, after);
            bool wasChanged = false;
            bool wasPositionUpdateSent = false;
            if (before.Acceleration != after.Acceleration)
            {
                after.Acceleration = (Vector3)didUpdate(objectUpdated, "Acceleration", before.Acceleration, after.Acceleration, diff.Acceleration);
                wasChanged = true;
            }
            if (before.AngularVelocity != after.AngularVelocity)
            {
                after.AngularVelocity = (Vector3)didUpdate(objectUpdated, "AngularVelocity", before.AngularVelocity, after.AngularVelocity, diff.AngularVelocity);
                wasChanged = true;
            }
            if (before.CollisionPlane != after.CollisionPlane)
            {
                after.CollisionPlane = (Vector4)didUpdate(objectUpdated, "CollisionPlane", before.CollisionPlane, after.CollisionPlane, diff.CollisionPlane);
                wasChanged = true;
            }
            if (before.Position != after.Position)
            {
                after.Position = (Vector3)didUpdate(objectUpdated, "Position", before.Position, after.Position, diff.Position);
                wasChanged = true;
                wasPositionUpdateSent = true;
            }
            if (ChangedBetween(before.Rotation, after.Rotation) > 0.1f)
            {
                after.Rotation = (Quaternion)didUpdate(objectUpdated, "Rotation", before.Rotation, after.Rotation, diff.Rotation);
                wasChanged = true;
            }
            if (before.State != after.State)
            {
                didUpdate(objectUpdated, "State", before.State, after.State, diff.State);
                wasChanged = true;
            }
            if (before.Textures != after.Textures)
            {
                didUpdate(objectUpdated, "Textures", before.Textures, after.Textures, diff.Textures);
                wasChanged = true;
            }
            if (before.Velocity != after.Velocity)
            {
                // didUpdate(objectUpdated, "Velocity", before.Velocity, after.Velocity, diff.Velocity);
                if (before.Velocity == Vector3.Zero)
                {
                    SendNewEvent("on-object-start-velosity", objectUpdated, after.Velocity);
                    if (!wasPositionUpdateSent) SendNewEvent("on-object-position", objectUpdated, after.Position);
                }
                else if (after.Velocity == Vector3.Zero)
                {
                    if (!wasPositionUpdateSent) SendNewEvent("on-object-position", objectUpdated, after.Position);
                    SendNewEvent("on-object-stop-velosity", objectUpdated, -before.Velocity);
                }
                else
                {
                    //SendNewEvent("on-object-change-velosity", objectUpdated, after.Velocity);
                    if (!wasPositionUpdateSent) SendNewEvent("on-object-position", objectUpdated, after.Position);
                }
                wasChanged = true;
            }
            if (!wasChanged) return null;
            return diff;
        }

        public float ChangedBetween(Quaternion quaternion, Quaternion quaternion_2)
        {
            Quaternion diff = quaternion - quaternion_2;
            return diff.Length();
        }

        //removes from taintable
        public String OSDDiff(OSDMap before, OSDMap after)
        {
            string xmlBefore = OpenMetaverse.StructuredData.OSDParser.SerializeLLSDXmlString(before);
            string xmlAfter = OpenMetaverse.StructuredData.OSDParser.SerializeLLSDXmlString(after);
            if (xmlBefore.Equals(xmlAfter)) return "";
            return "\n\n\n\nBEFORE\n" + before + "\n\nAFTER\n" + after;
        }
        /*
         * TODO
         On-Avatar-Sit-Changed
 simulator: "OpenSim Test"
 avatar: '(avatar "Nephrael Rae")
 sittingOn: 3135593693
 oldSeat: 0
         */
        class SomeChange
        {
            Object before;
            Object after;
            String named;
            SomeChange(String _named, Object _before, Object _after)
            {
                named = _named;
                before = _before;
                after = _after;
            }
            public override string ToString()
            {
                return "SomeChange " + named + " from " + before + " to " + after;
            }

        }
        public Vector3 QuatToRotation(Quaternion quat)
        {


            float X, Y, Z;
            //        quat.GetAxisAngle(CoordinateFrame.Z_AXIS, out Z);
            //          quat.GetAxisAngle(CoordinateFrame.X_AXIS, out X);
            //            quat.GetAxisAngle(CoordinateFrame.Y_AXIS, out Y);

            float r2d = 57.29577951F;
            quat.GetEulerAngles(out X, out Y, out Z);
            return new Vector3(X * r2d, Y * r2d, Z * r2d);
        }

        public Primitive deepCopy(Primitive objectUpdated)
        {
            OpenMetaverse.StructuredData.OSD osd = objectUpdated.GetOSD();
            if (objectUpdated is Avatar)
            {
                return Avatar.FromOSD(osd);
            }
            return Primitive.FromOSD(osd);
        }

        //public void someThingElseNeedsUpdate(Primitive objectUpdated)
        //{
        //    //throw new Exception("The method or operation is not implemented.");
        //}

        public ObjectUpdate updatFromPrim(Primitive fromPrim)
        {
            ObjectUpdate update = new ObjectUpdate();
            update.Acceleration = fromPrim.Acceleration;
            update.AngularVelocity = fromPrim.AngularVelocity;
            update.CollisionPlane = fromPrim.CollisionPlane;
            update.Position = fromPrim.Position;
            update.Rotation = fromPrim.Rotation;
            update.State = fromPrim.PrimData.State;
            update.Textures = fromPrim.Textures;
            update.Velocity = fromPrim.Velocity;
            update.LocalID = fromPrim.LocalID;
            update.Avatar = (fromPrim is Avatar);
            return update;
        }



        public ObjectUpdate updateDiff(ObjectUpdate fromPrim, ObjectUpdate diff)
        {
            ObjectUpdate update = new ObjectUpdate();
            update.LocalID = fromPrim.LocalID;
            update.Avatar = fromPrim.Avatar;
            update.Acceleration = fromPrim.Acceleration - diff.Acceleration;
            update.AngularVelocity = fromPrim.AngularVelocity - diff.AngularVelocity;
            update.CollisionPlane = fromPrim.CollisionPlane - diff.CollisionPlane;
            update.Position = fromPrim.Position - diff.Position;
            update.Rotation = fromPrim.Rotation - diff.Rotation;
            update.State = (byte)(fromPrim.State - diff.State);
            update.Textures = fromPrim.Textures != diff.Textures ? diff.Textures : null;
            update.Velocity = fromPrim.Velocity - diff.Velocity;
            return update;
        }


        public void updateToPrim(Primitive prim, ObjectUpdate update)
        {
            prim.Acceleration = update.Acceleration;
            prim.AngularVelocity = update.AngularVelocity;
            prim.CollisionPlane = update.CollisionPlane;
            prim.Position = update.Position;
            prim.Rotation = update.Rotation;
            prim.PrimData.State = update.State;
            prim.Textures = update.Textures;
            prim.Velocity = update.Velocity;
        }

        public override void Terrain_OnLandPatch(Simulator simulator, int x, int y, int width, float[] data)
        {
            Console.Write(",");
            //SimRegion R = SimRegion.GetRegion(simulator);
            //base.Terrain_OnLandPatch(simulator, x, y, width, null);

            //throw new NotImplementedException();
            //   SendNewEvent("On-Land-Patch", x, y, width, data);
            //            output("TextForm Terrain_OnLandPatch: "+simulator.ToString()+"/"+x.ToString()+"/"+y.ToString()+" w="+width.ToString());
        }


        public override void Avatars_OnPointAt(UUID sourceID, UUID targetID, Vector3d targetPos, PointAtType lookType, float duration, UUID id)
        {
            // output("TextForm Avatars_OnLookAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + lookType.ToString() + " duration " + duration.ToString());
            if (targetID == client.Self.AgentID)
            {
                output("  (TARGET IS SELF)");
                SendNewEvent("on-self-point-target", /*GetObject*/(sourceID), lookType);
            }
            SendNewEvent("on-avatar-point", /*GetObject*/(sourceID), /*GetObject*/(targetID), targetPos, lookType.ToString(), duration, /*GetObject*/(id));
        }

        public override void Avatars_OnLookAt(UUID sourceID, UUID targetID, Vector3d targetPos, LookAtType lookType, float duration, UUID id)
        {
            if (lookType == LookAtType.Idle) return;
            // output("TextForm Avatars_OnLookAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + lookType.ToString() + " duration " + duration.ToString());
            if (targetID == client.Self.AgentID)
            {
                output("  (TARGET IS SELF)");
                SendNewEvent("on-self-look-target", /*GetObject*/(sourceID), lookType);
            }
            RegisterUUID(id, lookType);
            SendNewEvent("on-avatar-look", /*GetObject*/(sourceID), /*GetObject*/(targetID), targetPos, lookType.ToString(), duration, /*GetObject*/(id));
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
            return cogbot.TheOpenSims.SimAnimation.GetAnimationName(id);
        }

        public Asset GetAsset(UUID id)
        {
            lock (uuidTypeObject)
            {
                Object assetObject;
                uuidTypeObject.TryGetValue(id, out assetObject);
                if (assetObject is Asset)
                {
                    return (Asset)assetObject;
                }
            }
            //IAssetProvider assetProvider = null;// TextForm.simulator.Assets;
            //if (assetProvider == null)
            //{
            //    Debug("Asset Provider still offline for " + id);
            //    return null;
            //}
            Asset asset;
            //assetProvider.TryGetAsset(id, out asset);
            //if (asset != null)
            //{
            //    RegisterUUID(id, asset);
            //}
            //return asset;
            return null;
        }

        public Primitive GetPrimitive(UUID id, Simulator simulator)
        {
            lock (uuidTypeObject)
                if (uuidTypeObject.ContainsKey(id))
                {
                    object simobject = uuidTypeObject[id];
                    if (simobject != null && simobject is SimObject)
                        return ((SimObject)simobject).Prim;
                }

            if (simulator == null)
            {
                List<Simulator> sims = null;
                lock (client.Network.Simulators) sims = new List<Simulator>(client.Network.Simulators);

                foreach (Simulator sim in sims)
                {
                    Primitive p = GetPrimitive(id, sim);
                    if (p != null) return p;
                }
                return null;
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
                found = simulator.ObjectsAvatars.Find(delegate(Avatar prim0)
                {
                    if (prim0.ID == id)
                    {
                        return true;
                    }
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
                List<Simulator> sims = null;
                lock (client.Network.Simulators) sims = new List<Simulator>(client.Network.Simulators);

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

        Dictionary<Primitive, Vector3> primVect = new Dictionary<Primitive, Vector3>();
        public void SendNewEvent(string eventName, params object[] args)
        {
            if (true) return;
            if (eventName.Contains("on-avatar-look")) return;
            //	Debug(eventName + " " + client.argsListString(args));
            String evtStr = eventName.ToString();
            if (evtStr == "on-object-position")
            {
                Primitive prim = (Primitive)args[0];
                Vector3 vect = (Vector3)args[1];

                if (!primVect.ContainsKey(prim))
                {
                    primVect[prim] = vect;
                    client.SendNewEvent(eventName, args);
                }
                else
                {
                    Vector3 v3 = primVect[prim] - vect;
                    if (v3.Length() > 0.5)
                    {
                        client.SendNewEvent(eventName, args);
                        primVect[prim] = vect;
                    }
                }
                return;

            }
            client.SendNewEvent(eventName, args);
        }


        void Avatars_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        {

            // this was ourself and is now handled in Avatar Animations Changed

        }

        public void CalcStats(SimObject prim)
        {
            if (boringNamesHeuristic(prim) == 0)
                client.BoringNamesCount++;
            else
                client.GoodNamesCount++;
        }

        public bool tryGetPrim(string name, out Primitive prim)
        {
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
            if (TheSimAvatar != null)
            {
                List<SimObject> set = TheSimAvatar.GetKnownObjects();
                if (set.Count == 0)
                {
                    TheSimAvatar.ScanNewObjects(5, 100);
                    set = TheSimAvatar.GetKnownObjects();
                }
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

            TheSimAvatar.SortByDistance(matches);
            if (pickNum != 0 && pickNum <= matches.Count)
            {
                prim = matches[(int)pickNum - 1].Prim;
                return true;
            }
            output("Found " + matches.Count + " matches: ");
            int num = 0;
            foreach (SimObject obj in matches)
            {
                num++;
                output(" " + num + ": " + obj + " " + TheSimAvatar.DistanceVectorString(obj));
                if (num == pickNum)
                {
                    prim = obj.Prim;
                    retVal = true;
                }
            }
            if (!retVal)
            {
                output("Use '" + name + " ###'");
            }
            return retVal;
        }

        public string describePrim(Primitive prim)
        {
            SimObject simObject = GetSimObject(prim);
            string str = simObject.ToString();
            str += " " + TheSimAvatar.DistanceVectorString(simObject);
            if (prim.Properties != null && prim.Properties.SalePrice != 0)
                str += " Sale: L" + prim.Properties.SalePrice;
            return str;// output(str);
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
                //output(prim.Properties.Name + ": " + prim.Properties.Description);
                //if (prim.Sound != UUID.Zero)
                //    output("This object makes sound.");
                //if (prim.Properties.SalePrice != 0)
                //    output("This object is for sale for L" + prim.Properties.SalePrice);
            }
        }

        public int comp(SimObject p1, SimObject p2)
        {
            return (int)(getFitness(p2) - getFitness(p1));
        }

        public List<Primitive> getPrimitives(int num)
        {
            List<SimObject> ret = new List<SimObject>();
            TheSimAvatar.ScanNewObjects(10, 100);
            TheSimAvatar.GetKnownObjects().ForEach(delegate(SimObject prim)
            {
                ret.Add(prim);
            });

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


        float getFitness(SimObject prim)
        {
            if (true)
            {
                return /* ((float)prim.ToString().Length/5 -*/ (float)TheSimAvatar.Distance(prim);
            }
            float fitness = 1;
            foreach (ObjectHeuristic heuristic in objectHeuristics)
            {
                fitness *= heuristic(prim);
            }
            return fitness;
        }

        float distanceHeuristic(SimObject prim)
        {
            if (prim != null)
                return (float)(1.0 / Math.Exp((double)TheSimAvatar.Distance(prim))
                    );
            else
                return (float)0.01;
        }


        float boringNamesHeuristic(SimObject prim)
        {
            return prim.ToString().Length;
        }

        bool tryGetBuildingPos(List<Primitive> group, out Vector3 centroid)
        {
            centroid = new Vector3();
            if (group.Count < 4)
                return false;
            else
            {
                bool first = true;
                Vector3 min = new Vector3(), max = new Vector3(), pos;
                foreach (Primitive prim in group)
                {
                    if (prim != null && prim.Position != null)
                    {
                        pos = prim.Position;

                        if (first)
                        {
                            min = pos;
                            max = pos;
                            first = false;
                        }
                        else
                        {
                            if (pos.X < min.X)
                                min.X = pos.X;
                            if (pos.Y < min.Y)
                                min.Y = pos.Y;
                            if (pos.Z < min.Z)
                                min.Z = pos.Z;

                            if (pos.X > max.X)
                                max.X = pos.X;
                            if (pos.Y > max.Y)
                                max.Y = pos.Y;
                            if (pos.Z > max.Z)
                                max.Z = pos.Z;
                        }
                    }
                }

                Vector3 size = max - min;
                if (size.X > buildingSize && size.Y > buildingSize && size.Z > buildingSize)
                {
                    centroid = min + (size * (float)0.5);
                    return true;
                }
                else
                    return false;
            }
        }

        public int posComp(Vector3 v1, Vector3 v2)
        {
            return (int)(Vector3.Mag(client.Self.RelativePosition - v1) -
                        Vector3.Mag(client.Self.RelativePosition - v2));
        }

        public List<Vector3> getBuildings(int num)
        {
            List<Vector3> ret = new List<Vector3>();
            foreach (List<Primitive> group in primGroups.Values)
            {
                Vector3 pos = new Vector3();
                if (tryGetBuildingPos(group, out pos))
                    ret.Add(pos);
            }

            if (ret.Count <= num)
                return ret;
            else
            {
                ret.Sort(new Comparison<Vector3>(posComp));
                return ret.GetRange(0, num);
            }
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

        public int comp(Avatar a1, Avatar a2)
        {
            return (int)(Vector3.Distance(a1.Position, compPos) - Vector3.Distance(a2.Position, compPos));
        }

        public List<Avatar> getAvatarsNear(Vector3 pos, int num)
        {
            compPos = pos;
            List<Avatar> avatarList = new List<Avatar>();
            foreach (SimAvatar simavatar in SimAvatars)
            {
                Avatar avatar = simavatar.theAvatar;
                if (avatar.Name != client.Self.Name)
                    avatarList.Add(avatar);
            }

            if (avatarList.Count > num)
            {
                avatarList.Sort(new Comparison<Avatar>(comp));

                for (; searchStep * num > avatarList.Count; --searchStep) ;

                List<Avatar> ret = new List<Avatar>();
                for (int i = 0; i < num && i < avatarList.Count; i += searchStep)
                    ret.Add(avatarList[i]);
                searchStep = (searchStep + 1) % 4 + 1;
                updateNumberedAvatars(ret);
                return ret;
            }
            else
            {
                updateNumberedAvatars(avatarList);
                return avatarList;
            }
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

        public void describeAvatar(Avatar avatar)
        {
            //	string verb;
            //if (avatar.SittingOn == 0)
            //    verb = "standing";
            //else
            //    verb = "sitting";
            //output(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
            output(avatar.Name + " is " + TheSimAvatar.DistanceVectorString(GetSimObject(avatar)) + " distant.");
            if (avatar.ProfileProperties.BornOn != null)
                output("Born on: " + avatar.ProfileProperties.BornOn);
            if (avatar.ProfileProperties.AboutText != null)
                output("About their second life: " + avatar.ProfileProperties.AboutText);
            if (avatar.ProfileProperties.FirstLifeText != null)
                output("About their first life: " + avatar.ProfileProperties.FirstLifeText);
            if (avatar.ProfileInterests.LanguagesText != null)
                output("Languages spoken: " + avatar.ProfileInterests.LanguagesText);
            if (avatar.ProfileInterests.SkillsText != null)
                output("Skills: " + avatar.ProfileInterests.SkillsText);
            if (avatar.ProfileInterests.WantToText != null)
                output("Wants to: " + avatar.ProfileInterests.WantToText);
        }

        public void describeAvatarToAI(Avatar avatar)
        {
            // string verb;
            // if (avatar.SittingOn == 0)
            //     verb = "standing";
            // else
            //     verb = "sitting";
            //output(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
            //output(avatar.Name + " is " + Vector3.Distance(Client.Self.SimPosition, avatar.Position).ToString() + " distant.");
            SendNewEvent("on-avatar-dist", avatar, Vector3.Distance(client.Self.SimPosition, avatar.Position));
            SendNewEvent("on-avatar-pos", avatar, avatar.Position);
            SendNewEvent("on-avatar-description", avatar, avatar.GroupName);
            //  botenqueueLispTask("(on-avatar-posture (@\"" + avatar.Name + "\") (@\"" + verb + "\") )");

            /*
            if (avatar.ProfileProperties.BornOn != null)
                output("Born on: " + avatar.ProfileProperties.BornOn);
            if (avatar.ProfileProperties.AboutText != null)
                output("About their second life: " + avatar.ProfileProperties.AboutText);
            if (avatar.ProfileProperties.FirstLifeText != null)
                output("About their first life: " + avatar.ProfileProperties.FirstLifeText);
            if (avatar.ProfileInterests.LanguagesText != null)
                output("Languages spoken: " + avatar.ProfileInterests.LanguagesText);
            if (avatar.ProfileInterests.SkillsText != null)
                output("Skills: " + avatar.ProfileInterests.SkillsText);
            if (avatar.ProfileInterests.WantToText != null)
                output("Wants to: " + avatar.ProfileInterests.WantToText);
            */

        }


        public void SetPrimFlags(Primitive UnPhantom, PrimFlags fs)
        {
            client.Objects.SetFlags(UnPhantom.LocalID, ((fs & PrimFlags.Physics) != 0),//
                ((fs & PrimFlags.Temporary) != 0),
                ((fs & PrimFlags.Phantom) != 0),
                ((fs & PrimFlags.CastShadows) != 0));
        }

        public IEnumerable<SimObject> GetAllSimObjects()
        {
            return SimObjects.CopyOf();
        }

        public void DeletePrim(Primitive thePrim)
        {
            if (thePrim is Avatar) return;
            SimObjects.Remove(GetSimObject(thePrim));
            uint objectLocalID = thePrim.LocalID;
            client.Inventory.RequestDeRezToInventory(objectLocalID, DeRezDestination.AgentInventoryTake,
                    client.Inventory.FindFolderForType(AssetType.TrashFolder), UUID.Random());
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

        public void EnsureSelected(uint LocalID, Simulator simulator)
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
                            lock (primsSelectedOutbox)
                                ReallyEnsureSelected(simulator, LocalID);
                        }
                    }
                }
        }

        private void ReallyEnsureSelected(Simulator simulator, uint LocalID)
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

        static bool inTimer = false;
        static object interpolationTimerLock = new object();

        static void ReallyEnsureSelected_Thread(object sender)
        {
            lock (interpolationTimerLock)
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
                foreach (Simulator simulator in new List<Simulator>( primsSelectedOutbox.Keys))
                {
                    lock (primsSelectedOutbox[simulator])
                    {
                        List<uint> uints = primsSelectedOutbox[simulator];
                        if (uints.Count > 200)
                        {
                            simulator.Client.Objects.SelectObjects(simulator, uints.GetRange(0, 200).ToArray());
                            uints.RemoveRange(0, 200);
                        } else
                        if (uints.Count > 0)
                        {
                            primsSelectedOutbox[simulator] = new List<uint>();
                            simulator.Client.Objects.SelectObjects(simulator, uints.ToArray());
                        }
                    }
                }
            }
            lock (interpolationTimerLock)
                inTimer = false;
        }

        public void RescanTypes()
        {
            int count = SimObjects.Count;
            output("Rescaning " + count + " simobjects");
            foreach (SimObject obj in SimObjects)
            {
                //obj._Parent = obj.Parent;
                obj.UpdateProperties(obj.Prim.Properties);
            }
            if (count != SimObjects.Count)
            {
                RescanTypes();
            }
        }

        public override void Groups_OnCurrentGroups(Dictionary<UUID, Group> groups)
        {
            base.Groups_OnCurrentGroups(groups);
            foreach (UUID key in groups.Keys)
            {
                Group g = groups[key];
                RegisterUUID(key, g);
            }
            //OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, groups);

        }

        public UUID GetAnimationUUID(string a)
        {
            return cogbot.TheOpenSims.SimAnimation.GetAnimationUUID(a);
        }

        public SimWaypoint GetWaypoint(Vector3d gloabl)
        {
            return SimRegion.GetWaypoint(gloabl);
        }

        public SimPosition GetVector(string[] args, out int argsUsed)
        {
            argsUsed = 0;
            if (args.Length == 0) return TheSimAvatar;
            if (args.Length >= 2)
            {
                Vector3 target;
                if (float.TryParse(args[0], out target.X) &&
                     float.TryParse(args[1], out target.Y))
                {
                    argsUsed = 2;
                    target.Z = client.Self.SimPosition.Z;
                    if (args.Length == 3)
                    {
                        Single.TryParse(args[2], out target.Z);
                        argsUsed = 3;
                    }
                    return SimWaypoint.CreateLocal(target, SimPaths);
                }
            }

            int consume = args.Length;
            Primitive prim = GetPrimitive(args, out argsUsed);
            if (prim != null) return GetSimObject(prim);
            return null;
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

        public List<SimObject> GetNearByObjects(Vector3d here, object except, float maxDistance, bool rootOnly)
        {
            if (here.X < 256f)
            {
                throw new ArgumentException("GetNearByObjects is not using GetWorldPostion?");
            }
            List<SimObject> nearby = new List<SimObject>();
            foreach (SimObject obj in GetAllSimObjects())
            {
                if (obj != except)
                    if (!(rootOnly && !obj.IsRoot() && !obj.IsTyped()))
                        if (obj.IsRegionAttached() && Vector3d.Distance(obj.GetWorldPosition(), here) <= maxDistance)
                            nearby.Add(obj);
            };
            return nearby;
        }

        public SimObject GetSimObject(Primitive prim)
        {
            return GetSimObject(prim, SimRegion.GetRegion(prim.RegionHandle).TheSimulator);
        }

        static List<UUID> TexturesSkipped = new List<UUID>();

        public byte[] TextureBytesToUUID(UUID uUID)
        {
            ImageDownload ID = null;
            lock (uuidTypeObject)
            {
                object iObject;
                if (uuidTypeObject.TryGetValue(uUID, out iObject))
                {
                    if (iObject is ImageDownload)
                    {
                        ID = (ImageDownload)iObject;
                    }
                }
            }
            if (ID == null)
            {
                ID = RegionMasterTexturePipeline.GetTextureToRender(uUID);
                if (ID == null)
                {
                    int tried = 20;
                    RegionMasterTexturePipeline.RequestTexture(uUID, ImageType.Normal);
                    int giveUpTick = Environment.TickCount + 1 * 60000;
                    while (ID == null)
                    {
                        ID = RegionMasterTexturePipeline.GetTextureToRender(uUID);
                        if (ID == null)
                        {
                            lock (TexturesSkipped) if (TexturesSkipped.Contains(uUID)) return null;
                            if (Environment.TickCount > giveUpTick)
                            {
                                lock (TexturesSkipped) TexturesSkipped.Add(uUID);
                                Debug("-- ---- ---GIVEUP SculptMesh " + uUID);
                                return null;
                            }
                            if (tried-- < 0)
                            {
                                //   Debug("-- ---- ---WAITING SculptMesh " + uUID);
                                tried = 20;
                            }
                            Thread.Sleep(5000);
                            DoEvents();
                        }
                    }
                }
                lock (uuidTypeObject)
                {
                    uuidTypeObject[uUID] = ID;
                }
            }
            Debug("-|-|- SUCCEED SculptMesh " + uUID);

            return ID.AssetData;
        }

        static void DoEvents()
        {
          //  throw new Exception("The method or operation is not implemented.");
        }

        internal void SetObjectPosition(Primitive Prim, Vector3 localPos)
        {
            Simulator sim = GetSimulator(Prim.RegionHandle);
            client.Objects.SetPosition(sim, Prim.LocalID, localPos);
        }

        internal void SetObjectRotation(Primitive Prim, Quaternion localPos)
        {
            Simulator sim = GetSimulator(Prim.RegionHandle);
            client.Objects.SetRotation(sim, Prim.LocalID, localPos);
        }

        internal Simulator GetSimulator(ulong handle)
        {
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
            return SimRegion.GetRegion(handle).TheSimulator;
        }

        internal static void ResetSelectedObjects()
        {
            lock (primsSelected) foreach (List<uint> UInts in primsSelected.Values)
                {
                    lock (UInts) UInts.Clear();
                }
        }

        internal void ReSelectObject(Primitive P)
        {
            Simulator sim = GetSimulator(P.RegionHandle);
            client.Objects.SelectObject(sim, P.LocalID);
        }

        internal Primitive AddTempPrim(SimRegion R, string name, PrimType primType, Vector3 scale, Vector3 loc)
        {
            OpenMetaverse.Primitive.ConstructionData CD = ObjectManager.BuildBasicShape(primType);
            CD.Material = Material.Light;
            CD.ProfileHole = HoleType.Triangle;

            bool success = false;

            Simulator simulator = R.TheSimulator;
            Primitive newPrim = null;
            // Register a handler for the creation event
            AutoResetEvent creationEvent = new AutoResetEvent(false);
            Quaternion rot = Quaternion.Identity;
            OpenMetaverse.ObjectManager.NewPrimCallback callback =
                delegate(Simulator simulator0, Primitive prim, ulong regionHandle, ushort timeDilation)
                {
                    if (regionHandle != R.RegionHandle) return;
                    if ((loc - prim.Position).Length() > 3)
                    {
                        Debug("Not the prim " + (loc - prim.Position).Length());
                        return;
                    }
                    if (prim.PrimData.ProfileHole != HoleType.Triangle)
                    {
                        Debug("Not the prim?  prim.PrimData.ProfileHole != HoleType.Triangle: {0}!={1}", prim.PrimData.ProfileHole, HoleType.Triangle);
                        // return;       //
                    }
                    if (Material.Light != prim.PrimData.Material)
                    {
                        Debug("Not the prim? Material.Light != prim.PrimData.Material: {0}!={1}", Material.Light, prim.PrimData.Material);
                        // return;
                    }
                    if ((prim.Flags & PrimFlags.CreateSelected) == 0)
                    {
                        Debug("Not the prim? (prim.Flags & PrimFlags.CreateSelected) == 0) was {0}", prim.Flags);
                        // return;
                    }
                    if (primType != prim.Type)
                    {
                        Debug("Not the prim? Material.Light != prim.PrimData.Material: {0}!={1}", Material.Light, prim.PrimData.Material);
                        // return;
                    }
                    //if (prim.Scale != scale) return;
                    //     if (prim.Rotation != rot) return;

                    //  if (Material.Light != prim.PrimData.Material) return;
                    //if (CD != prim.PrimData) return;
                    newPrim = prim;
                    creationEvent.Set();
                };

            client.Objects.OnNewPrim += callback;

            // Start the creation setting process (with baking enabled or disabled)
            client.Objects.AddPrim(simulator, CD, UUID.Zero, loc, scale, rot, PrimFlags.CreateSelected | PrimFlags.Phantom | PrimFlags.Temporary);

            // Wait for the process to complete or time out
            if (creationEvent.WaitOne(1000 * 120, false))
                success = true;

            // Unregister the handler
            client.Objects.OnNewPrim -= callback;

            // Return success or failure message
            if (!success)
            {
                Debug("Timeout on new prim " + name);
                return null;
            }
            uint LocalID = newPrim.LocalID;
            client.Objects.SetName(simulator, LocalID, name);
            client.Objects.SetPosition(simulator, LocalID, loc);
            client.Objects.SetScale(simulator, LocalID, scale, true, true);
            client.Objects.SetRotation(simulator, LocalID, rot);
            client.Objects.SetFlags(LocalID, false, true, true, false);
            return newPrim;
        }
    }
}
