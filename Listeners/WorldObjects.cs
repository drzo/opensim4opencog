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
        public SimGlobalRoutes GlobalRoutes = SimGlobalRoutes.Instance;
        public SimPathStore SimPaths
        {
            get { return TheSimAvatar.GetSimRegion().PathStore; }
        }
        public override void Parcels_OnSimParcelsDownloaded(Simulator simulator, InternalDictionary<int, Parcel> simParcels, int[,] parcelMap)
        {
            base.Parcels_OnSimParcelsDownloaded(simulator, simParcels, parcelMap);
        }



        //public bool Network_OnSimConnectingHook(Simulator simulator)
        //{
        //    //if (simulator.Handle != 0)
        //    //{
        //    //    SimRegion.GetRegion(simulator).TheSimulator = simulator;
        //    //}
        //    /// EnsureSimulator(simulator);
        //    ///
        //   // return base.Network_OnSimConnecting(simulator);
        //}

        public void Network_OnConnectedHook(object sender)
        {
            //Network_OnSimConnectedHook( (Simulator)sender);
            if (sender != client)
            {
                throw new ArgumentException("wrong client " + sender);
            }
            //  base.Network_OnConnected(sender);
        }

        public void Network_OnSimConnectedHook(Simulator simulator)
        {
            base.Network_OnSimConnected(simulator);
            lock (WorldObjectsMasterLock)
            {
                EnsureSimulator(simulator);
                IsConnected = true;
                if (SimRegion.IsMaster(simulator, client))
                {
                    Debug("---SIMMASTER---------" + client + " region: " + simulator);
                    WorldMaster(true);
                    RegisterAll();
                    MasteringRegions.Add(simulator.Handle);
                    client.Grid.RequestMainlandSims(GridLayerType.Objects);
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

        internal void CheckConnected(Simulator simulator)
        {
            if (!IsConnected) Network_OnSimConnectedHook(simulator);
        }

        public override void Network_OnEventQueueRunning(Simulator simulator)
        {
            base.Network_OnEventQueueRunning(simulator);
        }
        public override void Network_OnCurrentSimChanged(Simulator PreviousSimulator)
        {
            base.Network_OnCurrentSimChanged(PreviousSimulator);
        }
        public override void Network_OnSimDisconnected(Simulator simulator, NetworkManager.DisconnectType reason)
        {
            EnsureSimulator(simulator);
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

        //public SimAvatar GetSimAvatar(Avatar prim)
        //{
        //    lock (SimAvatars) foreach (SimAvatar obj in SimAvatars)
        //        {
        //            if (obj.theAvatar.Name == prim.Name)
        //                return obj;
        //        }
        //    SimAvatar obj0 = new SimAvatar(prim, this);
        //    obj0.SetClient(client);
        //    uuidTypeObject[prim.LocalID] = obj0;
        //    lock (SimAvatars) SimAvatars.AddTo(obj0);
        //    lock (SimObjects) SimObjects.AddTo(obj0);
        //    return obj0;
        //}

        //  public volatile static Dictionary<Simulator, GridClient> Master = new Dictionary<Simulator, GridClient>();

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
        public void WorldMaster(bool simulator)
        {
            lock (WorldObjectsMasterLock)
            {
                if (simulator) Master = this;
                if (MasteringRegions.Count > 0 && !simulator) throw new ArgumentException("Cant unmaster!");

                client.Settings.OBJECT_TRACKING = simulator;
                //client.Settings.PARCEL_TRACKING = simulator;
                //client.Settings.FETCH_MISSING_INVENTORY = simulator;
                //client.Settings.ALWAYS_DECODE_OBJECTS = true;
                //client.Settings.ALWAYS_REQUEST_OBJECTS = simulator;
                //client.Settings.ALWAYS_REQUEST_PARCEL_DWELL = simulator;
                //client.Settings.ALWAYS_REQUEST_PARCEL_ACL = simulator;
                //client.Settings.ENABLE_CAPS = true;
                // client.Settings.ENABLE_SIMSTATS = simulator;
                //client.Settings.STORE_LAND_PATCHES = simulator;
                //client.Settings.USE_TEXTURE_CACHE = false;
                client.Settings.SEND_PINGS = true;

                if (simulator) RegisterAll();
                //if (simulator)
                //    lock (SimMaster)
                //        if (!SimMaster.ContainsKey(simulator))
                //        {
                //            SimMaster[simulator] = this;
                //            RegisterAll();
                //            {
                //                client.Network.OnLogin += delegate(LoginStatus login, string message)
                //                        {
                //                            if (login == LoginStatus.Success)
                //                                CatchUp(simulator);
                //                        };

                //            }
                //            Debug("-------------------SimMaster {0} {1}", simulator, client);
                //        }
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
        //public int ExpectedObjects = 1000;

        static Dictionary<ulong, Dictionary<uint, Primitive>> prims = new Dictionary<ulong, Dictionary<uint, Primitive>>();
        //DoubleDictionary<uint, UUID, Avatar> prims[simulator.Handle] = new DoubleDictionary<uint, UUID, Avatar>();
        Dictionary<UUID, ObjectUpdate> lastObjectUpdate = new Dictionary<UUID, ObjectUpdate>();
        Dictionary<UUID, ObjectUpdate> lastObjectUpdateDiff = new Dictionary<UUID, ObjectUpdate>();
        Dictionary<Avatar, List<UUID>> avatarAminsSent = new Dictionary<Avatar, List<UUID>>();
        Dictionary<Avatar, UUID> avatarAminCurrent = new Dictionary<Avatar, UUID>();
        //Dictionary<UUID, UUID> texturesFinished = new Dictionary<UUID, UUID>();
        static Dictionary<Simulator, List<uint>> primsSelected = new Dictionary<Simulator, List<uint>>();

        static object WorldObjectsMasterLock = new object();
        public WorldObjects(BotClient client)
            : base(client)
        {
            lock (WorldObjectsMasterLock)
            {
                if (Master == null) Master = this;

                //  miniMap = new OpenMetaverse.GUI.MiniMap(client);
                //	client.Self.OnMeanCollision += new AgentManager.MeanCollisionCallback(Self_OnMeanCollision);

                // primsSelected[simulator] = new List<Primitive>();
                // prims[simulator.Handle] = new Dictionary<string, Primitive>();
                //  primsByLocalID = new Dictionary<uint, Primitive>();
                // pendingPrims = new Dictionary<UUID, Primitive>();

                primGroups = new Dictionary<UUID, List<Primitive>>();

                objectHeuristics = new List<ObjectHeuristic>();
                objectHeuristics.Add(new ObjectHeuristic(distanceHeuristic));
                //objectHeuristics.Add(new ObjectHeuristic(nameLengthHeuristic));
                objectHeuristics.Add(new ObjectHeuristic(boringNamesHeuristic));

                //client.Settings.ENABLE_CAPS = false;
                client.Settings.ENABLE_SIMSTATS = true;
                client.Settings.AVATAR_TRACKING = true;
                // client.Settings.THROTTLE_OUTGOING_PACKETS = false;
                client.Settings.MULTIPLE_SIMS = true;
                client.Settings.SEND_AGENT_UPDATES = true;
                client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                client.Self.Movement.AutoResetControls = true;
                client.Self.Movement.UpdateInterval = 50;
                //client.Settings.SEND_AGENT_THROTTLE = false;

                //client.Network.OnSimConnecting += Network_OnSimConnectingHook;
                client.Network.OnSimConnected += Network_OnSimConnectedHook;
                //client.Network.OnConnected += Network_OnConnectedHook;

                /*
                            client.Objects.OnNewPrim += new ObjectManager.NewPrimCallback(Objects_OnNewPrim);
                            client.Objects.OnObjectProperties += new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);
                            client.Objects.OnNewAvatar += new ObjectManager.NewAvatarCallback(Objects_OnNewAvatar);
                            client.Avatars.OnAvatarProperties += new AvatarManager.AvatarPropertiesCallback(Avatars_OnAvatarProperties);
                            // botclient.Objects.OnObjectKilled += new ObjectManager.KillObjectCallback(Objects_OnObjectKilled);
                            client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
                            client.Objects.OnObjectPropertiesFamily += new ObjectManager.ObjectPropertiesFamilyCallback(Objects_OnObjectPropertiesFamily);
                            client.Avatars.OnAvatarAnimation += AvatarAnimationCallback;// new AgentManager.AnimationsChangedCallback();
                            //replaced bu the above line Self.OnAnimationsChanged += new AgentManager.AnimationsChangedCallback(Self_OnAnimationsChanged);
                            client.Sound.OnPreloadSound += new SoundManager.PreloadSoundCallback(Sound_OnPreloadSound);
                            client.Sound.OnSoundTrigger += new SoundManager.SoundTriggerCallback(Sound_OnSoundTrigger);
                            client.Avatars.OnLookAt += new AvatarManager.LookAtCallback(Avatars_OnLookAt);
                            client.Avatars.OnPointAt += new AvatarManager.PointAtCallback(Avatars_OnPointAt);
                           // client.Objects.OnNewFoliage += new ObjectManager.NewFoliageCallback(Objects_OnNewFoliage);
                            client.Terrain.OnLandPatch += new TerrainManager.LandPatchCallback(Terrain_OnLandPatch);
                  */
                // Animation callback
                //            client.Network.RegisterCallback(PacketType.AvatarAnimation, new NetworkManager.PacketCallback(AvatarAnimationHandler));

                burstStartTime = DateTime.Now;
                burstInterval = new TimeSpan(0, 0, 0, 0, (int)(burstTime * 1000));
                searchStep = 1;
                //avatarCache = new Dictionary<string, Avatar>();
                numberedAvatars = new List<string>();

                //parent.Objects.OnNewAvatar += new ObjectManager.NewAvatarCallback(Objects_OnNewAvatar);
                //parent.Objects.OnAvatarSitChanged += new ObjectManager.AvatarSitChanged(Objects_OnAvatarSitChanged);
                // parent.Avatars.OnAvatarAppearance += new AvatarManager.AvatarAppearanceCallback(Avatars_OnAvatarAppearance);
                //AgentManager.AvatarSitChanged(Objects_OnAvatarSitChanged);
                // new DebugAllEvents(client);


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

                    m_TheSimAvatar = (SimAvatar)GetSimObject(GetAvatar(client.Self.AgentID, null));
                    m_TheSimAvatar.SetClient(client);
                }
                return m_TheSimAvatar;
            }
        }

        public SimAvatar m_TheSimAvatar;
        internal void SetSimAvatar(SimAvatar simAvatar)
        {
            m_TheSimAvatar = simAvatar;
        }

        static public ListAsSet<SimAvatar> SimAvatars = new ListAsSet<SimAvatar>();
        //public SimPathStore SimPaths = SimPathStore.Instance;

        static Thread TrackPathsThread;

        static void TrackPaths()
        {
            for (int i = 0; i < 30; i++)
            {
                Thread.Sleep(1000);
                Application.DoEvents();
            }
            int lastCount = 0;
            while (true)
            {
                ObjectUpdateItem U;
                int updates = 0;
                lock (updateQueue)
                {
                    updates = updateQueue.Count;
                }
                if (updates > 0)
                {
                    Debug("Processing Updates: " + updates);
                    while (updates > 0)
                    {
                        lock (updateQueue)
                        {
                            U = updateQueue.Dequeue();
                            updates = updateQueue.Count;
                        }
                        U();
                    }
                }

                Thread.Sleep(30000);
                lock (Master.client.Network.Simulators)
                    foreach (Simulator S in Master.client.Network.Simulators)
                    {
                        Master.CatchUp(S);
                    }
                int thisCount = SimObjects.Count;
                if (thisCount == lastCount) continue;
                Debug("TrackPaths Started: " + lastCount + "->" + thisCount);
                lastCount = thisCount;
                foreach (SimObject O in SimObjects)
                {
                    //Application.DoEvents();
                    if (O.IsRegionAttached())
                        O.UpdateOccupied();
                }
                Debug("TrackPaths Completed: " + thisCount);
                SimRegion.BakeRegions();
            }
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

        internal void CatchUp(Simulator simulator)
        {
            //lock (simulator.ObjectsAvatars.Dictionary)
            //simulator.ObjectsAvatars.ForEach(delegate(Avatar item)
            //{
            //    GetSimObject(item, simulator);
            //});
            lock (simulator.ObjectsPrimitives.Dictionary)
                simulator.ObjectsPrimitives.ForEach(delegate(Primitive item)
                {
                    GetSimObject(item, simulator);
                });
            //miniMap.UpdateMiniMap(simulator);
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
                        if (O.thePrim.RegionHandle == prim.RegionHandle)
                        {
                            return O;
                        }
                        else
                        {
                            if (prim is Avatar)
                            {
                                Debug("Avatar moved regions? " + O);
                                O.thePrim = prim;
                                O._CurrentRegion = null;
                                return O;
                            }
                            Debug("Prim with differnt region handle " + prim);
                            O.ResetRegion(simulator.Handle);
                            O.thePrim = prim;
                            O._CurrentRegion = null;
                            return O;
                        }
                    }
                }

                // not found
                if (prim is Avatar)
                {
                    CountnumAvatars++;
                    if (CountnumAvatars > 6)
                    {
                        Debug("++++++++" + CountnumAvatars + "+++++++Making AVATAR" + prim);
                    }
                    Debug("+++++++++++++++Making AVATAR" + prim);
                    if (prim.ID == UUID.Zero)
                    {
                        Debug("  - - -#$%#$%#$%% - ------- - Wierd Avatar " + prim);
                        BlockUntilPrimValid(prim, simulator);
                        Debug("  - - -#$%#$%#$%% - ------- - Unwird Avatar " + prim);
                    }
                    obj0 = new SimAvatar((Avatar)prim, this, SimRegion.GetRegion(simulator));
                    lock (SimAvatars) SimAvatars.Add((SimAvatar)obj0);
                }
                else
                {
                    obj0 = new SimObject(prim, this, SimRegion.GetRegion(prim.RegionHandle));
                }
                RegisterUUID(prim.ID, obj0);
                lock (SimObjects) SimObjects.AddTo((SimObject)obj0);

                return (SimObject)obj0;
            }
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
            //lock (uuidTextures) uuidTextures[image.ID] = image;
            //base.Assets_OnImageReceived(image, asset);
        }       // Dictionary<UUID, Parcel> uuidParcels = new Dictionary<UUID, Parcel>();

        //Dictionary<UUID, ImageDownload> uuidTextures = new Dictionary<UUID, ImageDownload>();
        static TexturePipeline RegionMasterTexturePipeline;

        internal void StartTextureDownload(UUID id)
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
        //public override void Parcels_OnSimParcelsDownloaded(Simulator simulator, InternalDictionary<int, Parcel> simParcels, int[,] parcelMap)
        //{
        //    lock (totalPrimsLock)
        //    {
        //        totalPrims = 0;
        //        simParcels.ForEach(
        //            delegate(Parcel parcel)
        //            { 
        //                uuidParcels[parcel.LocalID]
        //              //  totalPrims += parcel.TotalPrims;
        //            });
        //    }
        //    base.Parcels_OnSimParcelsDownloaded(simulator, simParcels, parcelMap);
        //}


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
                System.Windows.Forms.Application.DoEvents();
            }
            return prim;
        }

        public Primitive BlockUntilPrimValid(Primitive prim, Simulator simulator)
        {
            if (prim.Properties != null) return prim;
            EnsureSelected(prim.LocalID, simulator);
            while (prim.ID == UUID.Zero)
            { // TODO maybe add a timer
                System.Windows.Forms.Application.DoEvents();
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
        public override void Grid_OnCoarseLocationUpdate(Simulator simulator)
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

            Primitive p;
            if (prims[simulator.Handle].TryGetValue(objectID, out p))
            {
                SimObject O = GetSimObject(p, simulator);
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
                SendNewEvent("on-prim-killed", p);
            }
            else
            {
                base.Objects_OnObjectKilled(simulator, objectID);
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
        }

        public static void RegisterUUID(UUID id, object type)
        {
            if (type is Primitive)
            {
                Debug("cant register " + type);
            }
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
                lock (prims[simulator.Handle])
                    prims[simulator.Handle][prim.LocalID] = prim;
                lastObjectUpdate[prim.ID] = updatFromPrim(prim);
            }
            // Make an intial "ObjectUpdate" for later diffing

            EnsureSelected(prim.LocalID, simulator);
            //CalcStats(prim);
            //UpdateTextureQueue(prim.Textures);
        }

        /// <summary>
        /// This is all bot simulaor references: the Count would be Bots*Regions
        /// </summary>
        static List<Simulator> AllSimulators = new List<Simulator>();
        internal void EnsureSimulator(Simulator simulator)
        {
            lock (AllSimulators)
            {
                if (AllSimulators.Contains(simulator))
                    AllSimulators.Add(simulator);
            }

            lock (prims)
                if (!prims.ContainsKey(simulator.Handle))
                {
                    prims[simulator.Handle] = new Dictionary<uint, Primitive>();
                }
        }

        public override void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        {
            SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), properties);
        }

        static Queue<ObjectUpdateItem> updateQueue = new Queue<ObjectUpdateItem>();

        delegate void ObjectUpdateItem();
        static object Objects_OnObjectPropertiesLock = new object();
        public override void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {

            CheckConnected(simulator);
            // lock (Objects_OnObjectPropertiesLock)
            //if (prim != null)
                lock (Objects_OnObjectPropertiesLock) updateQueue.Enqueue(delegate()
            {
                Objects_OnObjectProperties1(simulator, props);
            });
        }
        internal void Objects_OnObjectProperties1(Simulator simulator, Primitive.ObjectProperties props)
        {
            Primitive prim = GetPrimitive(props.ObjectID, simulator);
            if (prim != null)
            {
                SimObject updateMe = GetSimObject(prim, simulator);
                updateMe.UpdateProperties(props);
            }
            //if (Program.Verbosity > 2)
            ///output("Received properties for " + props.ObjectID.ToString());               
            //lock (prim)  prim.Properties = props;
            //lock (primsKnown)
            //    if (!primsKnown.Contains(prim))
            //    {
            //        primsKnown.Add(prim);
            //        SendNewEvent("on-new-prim", props.Name, props.ObjectID.ToString(), props.Description, prim.Position);
            //        CalcStats(prim);
            //    }
            describePrimToAI(prim, simulator);
            ///EnsureSimulator(simulator);
            //if (prims[simulator.Handle].Count > simulator.Stats.Objects - 11)
            //{
            //    OnPrimsLoaded();
            //}
        }

        public override void Avatars_OnAvatarAppearance(UUID avatarID, bool isTrial, Primitive.TextureEntryFace defaultTexture, Primitive.TextureEntryFace[] faceTextures, List<byte> visualParams)
        {
            base.Avatars_OnAvatarAppearance(avatarID, isTrial, defaultTexture, faceTextures, visualParams);
        }

        object Objects_OnNewAvatarLock = new object();
        public override void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            //lock (Objects_OnNewAvatarLock)
            Objects_OnNewAvatar1(simulator, avatar, regionHandle, timeDilation);
        }
        internal void Objects_OnNewAvatar1(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            Objects_OnNewPrim(simulator, avatar, regionHandle, timeDilation);
            lock (Objects_OnObjectPropertiesLock) updateQueue.Enqueue(delegate()
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

            //lock (prims[simulator.Handle])
            //GetSimAvatar(avatar);
            // prims[simulator.Handle].Add(avatar.LocalID, avatar.ID, avatar);
            //lock (prims[simulator.Handle])  prims[simulator.Handle].Add(avatar.LocalID, avatar.ID, avatar);
            try
            {
                //lock (avatarCache)
                //{
                //    if (avatar != null)
                //    {
                //        if (!avatarCache.ContainsKey(avatar.Name))
                //        {
                //            avatarCache[avatar.Name] = avatar;
                //        }
                //    }
                //}
                //  describeAvatarToAI(avatar);
            }
            catch (Exception e)
            {
                output("err :" + e.StackTrace);
            }
        }
        //void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        //{
        //    simulator = simulator;
        //    lock (pendingPrims)
        //    {
        //        if (pendingPrims.ContainsKey(properties.ObjectID))
        //        {
        //            lock (prims[simulator.Handle])
        //            {
        //                Primitive p = pendingPrims[properties.ObjectID];
        //                prims[simulator.Handle][properties.Name] = p;
        //                primsByLocalID[p.LocalID] = p;
        //                p.Properties = properties;
        //                describePrimToAI(p);
        //                pendingPrims.Remove(properties.ObjectID);

        //                UUID groupId = properties.GroupID;
        //                if (groupId != UUID.Zero)
        //                {
        //                    lock (primGroups)
        //                    {
        //                        if (!primGroups.ContainsKey(groupId))
        //                            primGroups[groupId] = new List<Primitive>();
        //                        primGroups[groupId].Add(prims[simulator.Handle][properties.Name]);
        //                        //output("group count " + groupId + " " + primGroups[groupId].Count);
        //                    }
        //                }
        //            }
        //            if (maxNameLength == -1 || properties.Name.Length > maxNameLength)
        //                maxNameLength = properties.Name.Length;
        //        }
        //    }
        //}

        //void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
        //{
        //    simulator = simulator;
        //    try
        //    {
        //        lock (newLock)
        //        {
        //            primsSelected[simulator].Add(prim);
        //            CalcStats(prim);

        //            TimeSpan dtime = DateTime.Now - burstStartTime;
        //            if (primsSelected[simulator].Count >= burstSize && dtime > burstInterval)
        //            {
        //                burstStartTime = DateTime.Now;

        //                uint[] ids = new uint[burstSize];
        //                for (int i = 0; i < burstSize; ++i)
        //                {
        //                    ids[i] = primsSelected[simulator][i].LocalID;
        //                    pendingPrims[primsSelected[simulator][i].ID] = primsSelected[simulator][i];
        //                }

        //                botclient.Objects.SelectObjects(simulator, ids);
        //                primsSelected[simulator].RemoveRange(0, burstSize);
        //                //primsSelected[simulator].Clear();
        //            }
        //        }
        //CalcStats(prim);
        //        describePrimToAI(prim);
        //    }
        //    catch (Exception e)
        //    {
        //        output("ERR:" + e.StackTrace);
        //    }
        //}

        public override void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (simulator.Handle != regionHandle)
            {
                Debug("Strange update" + simulator);
                base.Objects_OnObjectUpdated(simulator, update, regionHandle, timeDilation);
            }
            CheckConnected(simulator);
            if (update.Avatar)
            {
                Primitive av = GetPrimitive(update.LocalID, simulator);
                if (av != null)
                {
                    lock (uuidTypeObject)
                    {
                        if (uuidTypeObject.ContainsKey(av.ID))
                        {
                            SimAvatar AV = (SimAvatar)uuidTypeObject[av.ID];
                            ulong rh  =AV.theAvatar.RegionHandle;
                            if (rh != regionHandle)
                            {
                                AV.ResetRegion(regionHandle); 
                            }

                        }
                    }
                }
                return;
            }
            return;
            lock (Objects_OnObjectPropertiesLock) updateQueue.Enqueue(delegate()
          {
              Objects_OnObjectUpdated1(simulator, update, regionHandle, timeDilation);
          });
        }
        internal void Objects_OnObjectUpdated1(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
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
                }
            }
            else
            {
                output("missing Objects_OnObjectUpdated");
            }
            lock (lastObjectUpdate) lastObjectUpdate[objectUpdated.ID] = update;
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

        internal float ChangedBetween(Quaternion quaternion, Quaternion quaternion_2)
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
            //throw new NotImplementedException();
            //   SendNewEvent("On-Land-Patch", x, y, width, data);
            //            output("TextForm Terrain_OnLandPatch: "+simulator.ToString()+"/"+x.ToString()+"/"+y.ToString()+" w="+width.ToString());
        }

        //void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        //{
        //    //throw new NotImplementedException();
        //    SendNewEvent("On-Animations-Changed", agentAnimations);
        //    //           output("TextForm Self_OnAnimationsChanged: ");
        //}

        //void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        //{
        //    SendNewEvent("On-Object-Properties", simulator, properties);
        //    // Handled by Object Listener
        //    //throw new NotImplementedException();
        //    //           output("TextForm Objects_OnObjectProperties: ");
        //}

        //void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        //{
        //    SendNewEvent("On-Object-Updated", simulator, update, regionHandle, timeDilation);
        //    //throw new NotImplementedException();
        //    //            output("TextForm Objects_OnObjectUpdated: ");
        //}

        //void Objects_OnObjectKilled(Simulator simulator, uint objectID)
        //{
        //    //throw new NotImplementedException();
        //    SendNewEvent("On-Object-Killed", simulator, objectID);
        //}

        //void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
        //{
        //    // Handled by Object Listener
        //    //throw new NotImplementedException();
        //    //            output("TextForm Objects_OnNewPrim: "+simulator.ToString()+" "+prim.ToString());
        //    //Listeners.Objects objects = (Listeners.Objects)ObjectSystem;
        //    //objects.SetCurrentSimulator(simulator);
        //    //objects.BlockUntilProperties(prim);
        //    //if (prim.Properties.Name != null)
        //    //    SendNewEvent("on-new-prim",prim.Properties.Name,prim.Properties.ObjectID,prim.Properties.Description);
        //    //else SendNewEvent("On-New-Prim", simulator, prim, regionHandle, timeDilation);

        //}

        //void Objects_OnNewFoliage(Simulator simulator, Primitive foliage, ulong regionHandle, ushort timeDilation)
        //{
        //    //throw new NotImplementedException();
        //    //            output("TextForm Objects_OnNewFoliage: ");

        //    Listeners.Objects objects = (Listeners.Objects)ObjectSystem;
        //    objects.SetCurrentSimulator(simulator);
        //    objects.BlockUntilProperties(foliage);

        //    if (foliage.Properties.Name != null)
        //        SendNewEvent("on-new-foliage",foliage.Properties.Name,foliage.Properties.ObjectID,foliage.Properties.Description);
        //    else SendNewEvent("On-New-Foliage", simulator, foliage, regionHandle, timeDilation);
        //}

        //void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        //{
        //    //throw new NotImplementedException();
        //    SendNewEvent("On-New-Avatar-Hook", simulator, avatar, regionHandle, timeDilation);
        //    SendNewEvent("on-new-avatar",avatar.Name,avatar.ID);
        //}

        //void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        //{
        //    //throw new NotImplementedException();
        //    output("TextForm Avatars_OnAvatarProperties: ");
        //}

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

        internal Asset GetAsset(UUID id)
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
            if (simulator == null)
            {
                lock (client.Network.Simulators)
                    foreach (Simulator S in client.Network.Simulators)
                    {
                        EnsureSimulator(S);
                        Primitive p = GetPrimitive(id, S);
                        if (p != null) return p;
                    }
                return null;
            }

            lock (uuidTypeObject)
                if (uuidTypeObject.ContainsKey(id))
                {
                    object found = uuidTypeObject[id];
                    if (found != null && found is SimObject)
                        return ((SimObject)found).thePrim;
                }
            // lock (GetPrimitiveLock)
            {
                //Primitive prim;
                //Object obj;
                //if (uuidTypeObject.TryGetValue(id, out obj))
                //{
                //    RegisterUUID(id, prim);
                //    return prim;
                //}
                //lock (simulator.ObjectsAvatars)
                {

                    Primitive found = null;

                    lock (simulator.ObjectsPrimitives.Dictionary)
                    {
                        found = simulator.ObjectsPrimitives.Find(delegate(Primitive prim0)
                        {
                            EnsureSelected(prim0.LocalID, simulator);
                            EnsureSelected(prim0.ParentID, simulator);
                            return (prim0.ID == id);
                        });
                    }
                    if (found == null) found = simulator.ObjectsAvatars.Find(delegate(Avatar prim0)
                    {
                        if (prim0.ID == id)
                        {
                            return true;
                        }
                        return false;
                    });
                    if (found != null)
                    {
                        ///EnsureSimulator(simulator);
                        lock (prims[simulator.Handle]) prims[simulator.Handle][found.LocalID] = found;
                    }
                    return found;
                }
            }
        }

        public Primitive GetPrimitive(uint id, Simulator simulator)
        {
            if (simulator == null)
            {
                foreach (Simulator sim in client.Network.Simulators)
                {
                    Primitive p = GetPrimitive(id, sim);
                    if (p != null) return p;
                }
                return null;
            }
            // lock (GetPrimitiveLock)

            EnsureSimulator(simulator);
            {
                Primitive prim;
                if (prims[simulator.Handle].TryGetValue(id, out prim))
                {
                }
                else
                    if (simulator.ObjectsPrimitives.TryGetValue(id, out prim))
                    {
                    }
                    else
                    {
                        Avatar avatar;
                        if (simulator.ObjectsAvatars.TryGetValue(id, out avatar))
                        {
                            prim = avatar;
                        }
                        else
                        {
                            EnsureSelected(id, simulator);
                            return null;
                        }
                    }
                EnsureSelected(id, simulator);
                if (prim.ID == UUID.Zero)
                {
                    Debug("  - - -#$%#$%#$%% - ------- - Wierd prim " + prim);
                    while (prim.ID == UUID.Zero)
                    {
                        lock (prims[simulator.Handle])
                        {
                            if (prims[simulator.Handle].ContainsKey(prim.LocalID))
                            {
                                prim = prims[simulator.Handle][prim.LocalID];
                            }
                        }
                        // TODO maybe add a timer
                        Thread.Sleep(100);
                        System.Windows.Forms.Application.DoEvents();

                    }
                    Debug("  - - -#$%#$%#$%% - ------- - Unwird prim " + prim);
                }
                lock (prims[simulator.Handle]) prims[simulator.Handle][prim.LocalID] = prim;
                lock (simulator.ObjectsPrimitives.Dictionary) simulator.ObjectsPrimitives[prim.LocalID] = prim;
                return prim;
            }

        }

        //internal void AvatarCacheAdd(string p, Avatar prim0)
        //{
        //    lock (avatarCache)
        //    {
        //        if (!avatarCache.ContainsKey(p))
        //        avatarCache[p] = prim0;
        //    }
        //    RegisterUUID(prim0.ID, prim0);
        //}

        object GetPrimitiveLock = new Object();




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
                prim = matches[0].thePrim;
                return true;
            }
            bool retVal = false;

            TheSimAvatar.SortByDistance(matches);
            if (pickNum != 0 && pickNum <= matches.Count)
            {
                prim = matches[(int)pickNum - 1].thePrim;
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
                    prim = obj.thePrim;
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
            SimObject simObject = GetSimObject(prim, null);
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
                ps.Add(os.thePrim);
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

        //float nameLengthHeuristic(Primitive prim)
        //{
        //    if ((prim != null) && (prim.Properties!=null) && (prim.Properties.Name != null))
        //    {
        //        return(float)prim.Properties.Name.Length / (float)maxNameLength;
        //    } else
        //        return(float)0.1;
        //}

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
            SimObject so = GetSimObject(prim, null);
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
            output(avatar.Name + " is " + TheSimAvatar.DistanceVectorString( GetSimObject(avatar)) + " distant.");
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


        internal void SetPrimFlags(Primitive UnPhantom, PrimFlags fs)
        {
            client.Objects.SetFlags(UnPhantom.LocalID, ((fs & PrimFlags.Physics) != 0),//
                ((fs & PrimFlags.Temporary) != 0),
                ((fs & PrimFlags.Phantom) != 0),
                ((fs & PrimFlags.CastShadows) != 0));
        }

        internal IEnumerable<SimObject> GetAllSimObjects()
        {
            return SimObjects.CopyOf();
        }

        internal void DeletePrim(Primitive thePrim)
        {
            if (thePrim is Avatar) return;
            SimObjects.Remove(GetSimObject(thePrim, null));
            // client.Inventory.RequestDeRezToInventory(thePrim.LocalID);
        }

        internal Primitive RequestMissingObject(uint localID, Simulator simulator)
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
                            client.Objects.SelectObject(simulator, LocalID);
                        }
                    }
                }
        }


        internal void RescanTypes()
        {
            int count = SimObjects.Count;
            output("Rescaning " + count + " simobjects");
            foreach (SimObject obj in SimObjects)
            {
                //obj._Parent = obj.Parent;
                obj.UpdateProperties(obj.thePrim.Properties);
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

        //Dictionary<int, Parcel> parcelLocalIds = new Dictionary<int, Parcel>();
        //public override void Parcels_OnParcelProperties(Simulator simulator, Parcel parcel, ParcelResult result, int primsSelected, int sequenceID, bool snapSelection)
        //{
        //    base.Parcels_OnParcelProperties(simulator, parcel, result, primsSelected, sequenceID, snapSelection);
        //    parcelLocalIds[parcel.LocalID] = parcel;
        //    if (simulator.Stats.Objects != parcel.SimWideTotalPrims)
        //    {
        //    }
        //}
        //public override void Parcels_OnParcelDwell(UUID parcelID, int localID, float dwell)
        //{
        //    base.Parcels_OnParcelDwell(parcelID, localID, dwell);
        //    Parcel p = parcelLocalIds[localID];
        //    p.SnapshotID = parcelID;
        //    RegisterUUID(parcelID, p);
        //}

        internal UUID GetAnimationUUID(string a)
        {
            return cogbot.TheOpenSims.SimAnimation.GetAnimationUUID(a);
        }


        internal SimPosition GetVector(string[] args, out int argsUsed)
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
            if (prim != null) return GetSimObject(prim, null);
            return null;
        }


        internal Primitive GetPrimitive(string[] args, out int argsUsed)
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
                    SimObject simObject = GetSimObject(prim, null);
                    //         if (simObject.IsRegionAttached())
                    {
                        argsUsed = consume;
                        return simObject.thePrim;
                    }
                }
                consume--;
            }
            return null;
        }

        internal List<SimObject> GetAllSimObjects(string name)
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

        internal SimObject GetSimObject(Primitive prim)
        {
            return GetSimObject(prim, null);
        }

        internal byte[] TextureBytesToUUID(UUID uUID)
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
                    while (ID == null)
                    {
                        ID = RegionMasterTexturePipeline.GetTextureToRender(uUID);
                        if (ID == null)
                        {
                            if (tried-- < 0)
                            {
                                Debug("---------WAITING SculptMesh " + uUID);
                                tried = 20;
                            }
                            Thread.Sleep(1000);
                            Application.DoEvents();
                        }
                    }
                }
                lock (uuidTypeObject)
                {
                    uuidTypeObject[uUID] = ID;
                }
            }
            Debug("---------SUCCEED SculptMesh " + uUID);

            return ID.AssetData;
        }
    }
}
