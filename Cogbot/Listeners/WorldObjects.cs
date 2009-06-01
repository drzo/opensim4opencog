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
    public delegate float ObjectHeuristic(SimObject prim);

    public partial class WorldObjects : DebugAllEvents
    {
        public static bool CanPhantomize = false;
        public static bool CanUseSit = true;
        public static bool DoCatchUp = true;
        public static bool MaintainAnims = false;
        public static bool MaintainAnimsInFolders = false;
        public static bool MaintainAttachments = false;
        public static bool MaintainCollisions = true;
        public static bool MaintainEffects = false;
        public static bool MaintainObjectUpdates = false;
        public static bool MaintainSounds = false;
        public static bool SimplifyBoxes = false; // true takes longer startup but speeds up runtime path finding

        /// <summary>
        /// Assets that WorldObjects requested
        /// </summary>
        private static readonly Dictionary<UUID, UUID> AssetRequests = new Dictionary<UUID, UUID>();

        /// <summary>
        /// Locked on AssetRequests
        /// </summary>
        private static readonly Dictionary<UUID, AssetType> AssetRequestType = new Dictionary<UUID, AssetType>();

        private static readonly Dictionary<ulong, object> GetSimObjectLock = new Dictionary<ulong, object>();
        private static readonly Dictionary<UUID, ObjectUpdate> LastObjectUpdate = new Dictionary<UUID, ObjectUpdate>();

        private static readonly Dictionary<UUID, ObjectUpdate> LastObjectUpdateDiff =
            new Dictionary<UUID, ObjectUpdate>();

        private static readonly Dictionary<Simulator, List<uint>> primsSelected =
            new Dictionary<Simulator, List<uint>>();

        private static readonly Dictionary<Simulator, List<uint>> primsSelectedOutbox =
            new Dictionary<Simulator, List<uint>>();

        private static readonly Dictionary<Primitive, Vector3> primVect = new Dictionary<Primitive, Vector3>();
        private static readonly Queue<ObjectUpdateItem> PropertyQueue = new Queue<ObjectUpdateItem>();
        private static readonly object SelectObjectsTimerLock = new object();
        private static readonly List<ThreadStart> ShutdownHooks = new List<ThreadStart>();
        private static readonly List<UUID> TexturesSkipped = new List<UUID>();
        private static readonly Queue<ObjectUpdateItem> UpdateQueue = new Queue<ObjectUpdateItem>();
        private static readonly Dictionary<UUID, object> uuidTypeObject = new Dictionary<UUID, object>();
        private static readonly object WorldObjectsMasterLock = new object();
        private static SimAnimationStore _SimAnimationSystem;
        private static WorldPathSystem _SimPaths;


        private static int CountnumAvatars;
        private static List<UUID> EffectsSent = new List<UUID>();
        private static Timer InterpolationTimer;
        private static bool inTimer = false;
        private static AssetManager RegionMasterTexturePipeline;

        public static readonly ListAsSet<SimAvatar> SimAvatars = new ListAsSet<SimAvatar>();
        public static readonly ListAsSet<SimObject> SimObjects = new ListAsSet<SimObject>();
        public static readonly List<String> SkippedEffects = new List<string>();


        private static Thread TrackUpdateLagThread;
        private static Thread TrackUpdatesThread;


        public static readonly Dictionary<uint, OSDMap> lastOSD = new Dictionary<uint, OSDMap>();

        public static float buildingSize = 5;
        public static TimeSpan burstInterval;
        public static int burstSize = 100;
        public DateTime burstStartTime;
        public static float burstTime = 1;
        public Vector3 compPos;
        public SimActor m_TheSimAvatar;
        public List<string> numberedAvatars;
        public List<ObjectHeuristic> objectHeuristics;
        public Dictionary<UUID, List<Primitive>> primGroups;
        public int searchStep;

        static WorldObjects()
        {
            SkippedEffects.Add("LookAtType.Idle");
            SkippedEffects.Add("LookAtType.FreeLook");
            SkippedEffects.Add("PointAtType.None");
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

                objectHeuristics = new List<ObjectHeuristic>();
                objectHeuristics.Add(new ObjectHeuristic(distanceHeuristic));
                //objectHeuristics.Add(new ObjectHeuristic(nameLengthHeuristic));
                objectHeuristics.Add(new ObjectHeuristic(boringNamesHeuristic));

                client.Settings.ENABLE_CAPS = true;
                client.Settings.ENABLE_SIMSTATS = true;
                client.Settings.AVATAR_TRACKING = true;
                client.Settings.THROTTLE_OUTGOING_PACKETS = false;
                client.Settings.MULTIPLE_SIMS = true;
                client.Settings.SIMULATOR_TIMEOUT = 30*60000;
                client.Settings.SEND_AGENT_UPDATES = true;

                client.Settings.SEND_PINGS = false;

                client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                client.Self.Movement.AutoResetControls = false;
                client.Self.Movement.UpdateInterval = 0;

                client.Network.OnSimConnected += Network_OnSimConnectedHook;
                client.Inventory.OnScriptRunning += Inventory_OnScriptRunning;


                burstStartTime = DateTime.Now;
                burstInterval = new TimeSpan(0, 0, 0, 0, (int) (burstTime*1000));
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
                        TrackUpdatesThread = new Thread(TrackUpdates);
                        TrackUpdatesThread.Name = "Track Updates/Properties";
                        //TrackPathsThread.Priority = ThreadPriority.Lowest;
                        TrackUpdatesThread.Start();
                    }
                    if (TrackUpdateLagThread == null)
                    {
                        TrackUpdateLagThread = new Thread(Lagometer);
                        TrackUpdateLagThread.Name = "Lagometer thread";
                        TrackUpdateLagThread.Priority = ThreadPriority.Lowest;
                        TrackUpdateLagThread.Start();
                    }
                    InterpolationTimer = new Timer(ReallyEnsureSelected_Thread, null, 1000, 1000);
                    _SimPaths = new WorldPathSystem(this);
                    _SimAnimationSystem = new SimAnimationStore(client);
                }
                //SetWorldMaster(false);
                //RegisterAll();
                InitConsoleBot();
            }
        }

        public SimAnimationStore SimAnimationSystem
        {
            get { return _SimAnimationSystem; }
        }

        public WorldPathSystem SimPaths
        {
            get { return _SimPaths; }
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
                            (SimActor) GetSimObject(GetAvatar(client.Self.AgentID, client.Network.CurrentSim));
                        if (m_TheSimAvatar == null)
                        {
                            Thread.Sleep(100);
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

        private static void TrackUpdates()
        {
            Thread.Sleep(30000);
            int lastUpdateCount = 0;

            while (true)
            {
                Thread.Sleep(10);
                {
                    int found = DequeueProperties();
                    ObjectUpdateItem U;
                    int queuedUpdatesCount = 0;
                    int didUpdate = 0;
                    lock (UpdateQueue)
                    {
                        queuedUpdatesCount = UpdateQueue.Count;
                    }
                    if (queuedUpdatesCount > 0)
                    {
                        //todo          Debug("Start Processing Updates: " + queuedUpdatesCount);

                        while (queuedUpdatesCount > 0)
                        {
                            DequeueProperties();
                            lock (UpdateQueue)
                            {
                                U = UpdateQueue.Dequeue();
                                queuedUpdatesCount = UpdateQueue.Count;
                            }
                            U();
                            didUpdate++;
                        }
                        //todo      Debug("Done processing Updates: " + didUpdate);
                    }

                    if (DoCatchUp)
                    {
                        int beforeCatchUp = SimObjects.Count;
                        // lock (AllSimulators)
                        foreach (Simulator S in AllSimulators)
                        {
                            Master.CatchUp(S);
                        }
                        int thisCount = SimObjects.Count;
                        if (beforeCatchUp != thisCount)
                        {
                            Debug("Simulator catchup found: " + beforeCatchUp + " -> " + thisCount);
                        }
                    }
                    if (didUpdate + found == 0)
                    {
                        Thread.Sleep(100);
                        continue;
                    }
                }
            }
        }

        private static int DequeueProperties()
        {
            int didProps = 0;
            ObjectUpdateItem P;
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
                    if (item.ParentID == 0 && !SimRegion.OutOfRegion(item.Position))
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
            if (obj0 != null) return obj0;

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
                    Debug("+++++++++++++++Making {0} {1}", prim, ((Avatar) prim).Name);
                    if (prim.ID == UUID.Zero)
                    {
                        Debug("  - - -#$%#$%#$%% - ------- - Weird Avatar " + prim);
                        BlockUntilPrimValid(prim, simulator);
                        Debug("  - - -#$%#$%#$%% - ------- - Unweird Avatar " + prim);
                    }
                    obj0 = new SimAvatarImpl((Avatar) prim, this, simulator);
                    lock (SimAvatars) SimAvatars.Add((SimAvatar) obj0);
                }
                else
                {
                    obj0 = new SimObjectImpl(prim, this, simulator);
                }
                RegisterUUID(prim.ID, obj0);
                lock (SimObjects) SimObjects.AddTo((SimObject) obj0);
            }
            return (SimObject) obj0;
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
                    return (SimObject) obj0;
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

        private static SimObject GetSimObjectFromPrimUUID(Primitive prim)
        {
            if (prim == null || prim.ID == UUID.Zero) return null;
            Object obj0;
            //lock (uuidTypeObject)
            if (uuidTypeObject.TryGetValue(prim.ID, out obj0))
            {
                if (obj0 is SimObject)
                {
                    return (SimObject) obj0;
                }
            }
            return null;
        }

        public override void Assets_OnXferReceived(XferDownload xfer)
        {
            //AssetRequests.
            //RegisterUUIDMaybe(xfer.ID, xfer);
        }

        public override void Assets_OnAssetReceived(AssetDownload transfer, Asset asset)
        {
            if (asset == null)
            {
                lock (AssetRequests)
                    if (AssetRequests.ContainsValue(transfer.ID))
                    {
                        Debug("Failed transfer for " + AssetRequestType[transfer.ID] +
                              " " + transfer.ID + " ");
                    }
                    else
                    {
                        Debug("Unknown transfer Failed for " + transfer.AssetType + " " + transfer.ID + " ");
                    }
                return;
            }
            lock (AssetRequests)
                if (AssetRequests.ContainsValue(transfer.ID))
                {
                    AssetType assetRequestType = AssetRequestType[transfer.ID];
                    if (assetRequestType == asset.AssetType)
                        Debug("Transfer succeeded for " + assetRequestType +
                              " " + transfer.ID + " ");
                    else
                        Debug("Transfer succeeded weirdly as " + asset.AssetType + " for " + assetRequestType +
                              " " + transfer.ID + " ");
                }
                else
                {
                    Debug("Unknown transfer succeeded for " + asset.AssetType + " " + transfer.ID + " ");
                }
            RegisterAsset(asset.AssetID, asset);
        }

        private void RegisterAsset(UUID uUID, Asset asset)
        {
            switch (asset.AssetType)
            {
                    /// <summary>Unknown asset type</summary>
                case AssetType.Unknown:
                    {
                        break;
                    } //-1,
                    /// <summary>Texture asset, stores in JPEG2000 J2C stream format</summary>
                case AssetType.Texture:
                    {
                        break;
                    } //0,
                    /// <summary>Sound asset</summary>
                case AssetType.Sound:
                    {
                        break;
                    } //1,
                    /// <summary>Calling card for another avatar</summary>
                case AssetType.CallingCard:
                    {
                        break;
                    } //2,
                    /// <summary>Link to a location in world</summary>
                case AssetType.Landmark:
                    {
                        break;
                    } //3,
                    // <summary>Legacy script asset, you should never see one of these</summary>
                    //[Obsolete]
                    //Script: {break;}//4,
                    /// <summary>Collection of textures and parameters that can be 
                    /// worn by an avatar</summary>
                case AssetType.Clothing:
                    {
                        break;
                    } //5,
                    /// <summary>Primitive that can contain textures, sounds, 
                    /// scripts and more</summary>
                case AssetType.Object:
                    {
                        break;
                    } //6,
                    /// <summary>Notecard asset</summary>
                case AssetType.Notecard:
                    {
                        break;
                    } //7,
                    /// <summary>Holds a collection of inventory items</summary>
                case AssetType.Folder:
                    {
                        break;
                    } //8,
                    /// <summary>Root inventory folder</summary>
                case AssetType.RootFolder:
                    {
                        break;
                    } //9,
                    /// <summary>Linden scripting language script</summary>
                case AssetType.LSLText:
                    {
                        break;
                    } //10,
                    /// <summary>LSO bytecode for a script</summary>
                case AssetType.LSLBytecode:
                    {
                        break;
                    } //11,
                    /// <summary>Uncompressed TGA texture</summary>
                case AssetType.TextureTGA:
                    {
                        break;
                    } //12,
                    /// <summary>Collection of textures and shape parameters that can
                    /// be worn</summary>
                case AssetType.Bodypart:
                    {
                        break;
                    } //13,
                    /// <summary>Trash folder</summary>
                case AssetType.TrashFolder:
                    {
                        break;
                    } //14,
                    /// <summary>Snapshot folder</summary>
                case AssetType.SnapshotFolder:
                    {
                        break;
                    } //15,
                    /// <summary>Lost and found folder</summary>
                case AssetType.LostAndFoundFolder:
                    {
                        break;
                    } //16,
                    /// <summary>Uncompressed sound</summary>
                case AssetType.SoundWAV:
                    {
                        break;
                    } //17,
                    /// <summary>Uncompressed TGA non-square image, not to be used as a
                    /// texture</summary>
                case AssetType.ImageTGA:
                    {
                        break;
                    } //18,
                    /// <summary>Compressed JPEG non-square image, not to be used as a
                    /// texture</summary>
                case AssetType.ImageJPEG:
                    {
                        break;
                    } //19,
                    /// <summary>Animation</summary>
                case AssetType.Animation:
                    SimAnimationSystem.OnAnimDownloaded(uUID, (AssetAnimation) asset);
                    //20,
                    break;
                    /// <summary>Sequence of animations, sounds, chat, and pauses</summary>
                case AssetType.Gesture:
                    {
                        break;
                    } //21,
                    /// <summary>Simstate file</summary>
                case AssetType.Simstate:
                    {
                        break;
                    } //22,
                default:
                    {
                        break;
                    }
            }
            RegisterUUIDMaybe(uUID, asset);
        }

        /*
        On-Image-Received
             image: "{OpenMetaverse.ImageDownload,PacketCount=33,Codec=J2C,NotFound=False,Simulator=OpenSim Test (71.197.210.170:9000),PacketsSeen=System.Collections.Generic.SortedList`2[System.UInt16,System.UInt16],ImageType=Normal,DiscardLevel=-1,Priority=1013000,ID=728dd7fa-a688-432d-a4f7-4263b1f97395,Size=33345,AssetData=System.Byte[],Transferred=33345,Success=True,AssetType=Texture}"
             asset: "{OpenMetaverse.AssetTexture,Image=,LayerInfo=,Components=0,AssetData=System.Byte[],Temporary=False}"
         */

        public override void Assets_OnImageReceived(ImageDownload image, AssetTexture asset)
        {
            RegisterUUIDMaybe(image.ID, image);
            RegisterAsset(asset.AssetID, asset);
        }


        public ImageDownload StartTextureDownload(UUID id)
        {
            if (RegionMasterTexturePipeline.Cache.HasImage(id))
            {
                return RegionMasterTexturePipeline.Cache.GetCachedImage(id);
            }
            lock (TexturesSkipped) if (TexturesSkipped.Contains(id)) return null;
            RegionMasterTexturePipeline.RequestImage(id, ImageType.Normal,
                                                     RegionMasterTexturePipeline_OnDownloadFinished);
            return null;
        }

        private void RegionMasterTexturePipeline_OnDownloadFinished(TextureRequestState state, AssetTexture asset)
        {
            if (state == TextureRequestState.Finished)
            {
                UUID id = asset.AssetID;
                ImageDownload image = RegionMasterTexturePipeline.Cache.GetCachedImage(id);
                if (image == null)
                {
                    Console.WriteLine("AssetTexture is null?! " + id);
                }
                else
                {
                    RegisterUUIDMaybe(id, image);
                    //lock (uuidTextures) uuidTextures[id] = image;
                }
            }
            else if (state == TextureRequestState.NotFound || state == TextureRequestState.Timeout)
            {
                Debug("Texture failed to download: " + asset, Helpers.LogLevel.Warning);
                lock (TexturesSkipped) TexturesSkipped.Add(asset.AssetID);
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

        public override void Self_OnMeanCollision(MeanCollisionType type, UUID perp, UUID victim, float magnitude,
                                                  DateTime time)
        {
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() =>
                                        {
                                            SimObject perpAv, victimAv;
                                            if (TryGetSimObject(perp, out perpAv) &&
                                                TryGetSimObject(victim, out victimAv))
                                            {
                                                // if (victimAv.Name == client.Self.Name)
                                                //   output(perpAv.Name + " bumped into $bot like " + type);
                                                // else if (perpAv.Name == client.Self.Name)
                                                //   output("$bot bumped into " + victimAv.Name + " like " + type);   
                                                perpAv.LogEvent("" + type, victimAv, magnitude);
                                                SendNewEvent("on-meanCollision", type, perpAv, victimAv,
                                                             magnitude);
                                            }
                                        });
        }

        public bool TryGetSimObject(UUID victim, out SimObject victimAv)
        {
            victimAv = GetSimObjectFromUUID(victim);
            return victimAv != null;
        }

        public void OnObjectSound(UUID objectID, UUID soundID, float gain)
        {
            if (!MaintainSounds) return;
            RequestAsset(soundID, AssetType.Sound, true);
            SimObject o = GetSimObjectFromUUID(objectID);
            if (o == null) return;
            o.OnSound(soundID, gain);
            // RegionMasterTexturePipeline.RequestAsset(soundID, AssetType.SoundWAV, true);
        }

        public override void Sound_OnSoundTrigger(UUID soundID, UUID ownerID, UUID objectID, UUID parentID, float gain,
                                                  ulong regionHandle, Vector3 position)
        {
            if (!MaintainSounds) return;

            RequestAsset(soundID, AssetType.Sound, true);
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() =>
                                        {
                                            if (objectID != UUID.Zero) OnObjectSound(objectID, soundID, gain);
                                            else
                                            {
                                                SendNewEvent("On-Sound-Position-Trigger", soundID,
                                                             ownerID, parentID, gain, regionHandle, position);
                                            }
                                        });
        }


        public override void Sound_OnPreloadSound(UUID soundID, UUID ownerID, UUID objectID)
        {
            if (!MaintainSounds) return;
            RequestAsset(soundID, AssetType.Sound, true);
            //base.Sound_OnPreloadSound(soundID, ownerID, objectID);
            //output("preload sound " + soundID);
        }


        public override void Sound_OnAttachSound(UUID soundID, UUID ownerID, UUID objectID, float gain, SoundFlags flags)
        {
            if (!MaintainSounds) return;
            RequestAsset(soundID, AssetType.Sound, true);
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() => OnObjectSound(objectID, soundID, gain));
            //SendNewEvent("On-Attach-Sound", soundID, ownerID, objectID, gain, flags);
            //base.Sound_OnAttachSound(soundID, ownerID, objectID, gain, flags);
        }

        public override void Sound_OnAttachSoundGainChange(UUID objectID, float gain)
        {
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() =>
                                        {
                                            OnObjectSound(objectID, UUID.Zero, gain);
                                            SendNewEvent("On-Attach-Sound-Gain-Change", objectID, gain);
                                        });
            //base.Sound_OnAttachSoundGainChange(objectID, gain);
        }


        public Primitive BlockUntilProperties(Primitive prim, Simulator simulator)
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

        public Primitive BlockUntilPrimValid(Primitive prim, Simulator simulator)
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

        public override void Avatars_OnAvatarAnimation(UUID avatarID, InternalDictionary<UUID, int> anims)
        {
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() =>
                                        {
                                            SimAvatar avatar = (SimAvatar) GetSimObjectFromUUID(avatarID);
                                            if (avatar == null) return;
                                            if (UseEventSource(avatar))
                                            {
                                                avatar.OnAvatarAnimations(anims);
                                            }
                                        });
        }

        public override void Self_OnAnimationsChanged(InternalDictionary<UUID, int> agentAnimations)
        {
            Avatars_OnAvatarAnimation(client.Self.AgentID, agentAnimations);
        }

        public override void Grid_OnCoarseLocationUpdate(Simulator sim, List<UUID> newEntries, List<UUID> removedEntries)
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

        /*
            On-Effect
            type: "Sphere"
            sourceID: "00000000-0000-0000-0000-000000000000"
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 256126.750573638 256129.791631699 25.2663780227304)
            duration: 0.25
            id: "a31efee6-a426-65a6-5ef2-d1345be49233"

            On-Effect
            type: "Beam"
            sourceID: '(avatar "Kuffher Hauptmann")
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234680.541992188 277992.395370483 350.615783691406)
            duration: 0.7
            id: "6f777a51-56a2-4c2e-2266-3ea6b9bc4945"

            On-Effect
            type: "Beam"
            sourceID: "ec8777dc-62cd-46df-8d36-98373459c23a"
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234556.786750793 277914.178848267 1049.44689941406)
            duration: 0.7
            id: "0c743618-6c68-171e-d515-dbda81f58258"

            On-Effect
            type: "Beam"
            sourceID: '(avatar "Kuffher Hauptmann")
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234680.526870728 277992.386642456 351.192932128906)
            duration: 0.7
            id: "eb064996-7879-fbab-972c-acb272ea3a91"

            On-Effect
            type: "Beam"
            sourceID: "109bacd0-650d-46f4-84cf-0e8c154123c4"
            targetID: "00000000-0000-0000-0000-000000000000"
            targetPos: '(Vector3d 234559.817420959 277769.408673286 1002.50750732422)
            duration: 0.7
            id: "bd25925f-7688-5d81-8834-e7631942c5c5"

		         
         */

        public override void Avatars_OnEffect(EffectType type, UUID sourceID, UUID targetID, Vector3d targetPos,
                                              float duration, UUID id)
        {
            if (!MaintainEffects) return;
            SendEffect(sourceID, targetID, targetPos, "EffectType." + type.ToString(), duration, id);
            //SimRegion.TaintArea(targetPos);
        }


        /// <summary>
        /// Process an incoming effect
        /// </summary>
        private void ViewerEffectHandler(Packet packet, Simulator simulator)
        {
            if (!MaintainEffects) return;
            ViewerEffectPacket effect = (ViewerEffectPacket) packet;
            GridClient Client = client;

            foreach (ViewerEffectPacket.EffectBlock block in effect.Effect)
            {
                EffectType type = (EffectType) block.Type;

                // most effect types have at least these properties
                UUID sourceAvatar = new UUID(block.TypeData, 0);
                UUID targetObject = new UUID(block.TypeData, 16);
                Vector3d targetPos = new Vector3d(block.TypeData, 32);
                //Color4 color;
                //if (block.Color.Length == 4)
                //{
                //    color = new Color4(block.Color, 0);
                //}
                //else
                //{
                //    Client.Log("Received a ViewerEffect.EffectBlock.Color array with " + block.Color.Length + 
                //        " bytes", Helpers.LogLevel.Warning);
                //    color = Color4.Black;
                //}

                // Each ViewerEffect type uses it's own custom binary format for additional BVHData. Fun eh?
                switch (type)
                {
                    case EffectType.Beam:
                    case EffectType.Point:
                    case EffectType.Trail:
                    case EffectType.Sphere:
                    case EffectType.Spiral:
                    case EffectType.Edit:
                        // if (Avatars_OnEffect != null)
                        {
                            if (block.TypeData.Length == 56)
                            {
                                try
                                {
                                    Avatars_OnEffect(type, sourceAvatar, targetObject, targetPos, block.Duration,
                                                     block.ID);
                                }
                                catch (Exception e)
                                {
                                    Debug(e.Message, Helpers.LogLevel.Error, Client, e);
                                }
                            }
                            else
                            {
                                Debug("Received a " + type.ToString() +
                                      " ViewerEffect with an incorrect TypeData size of " +
                                      block.TypeData.Length + " bytes", Helpers.LogLevel.Warning, Client);
                            }
                            return;
                        }
                        break;
                    case EffectType.LookAt:
                        //if (OnLookAt != null)
                        {
                            if (block.TypeData.Length == 57)
                            {
                                LookAtType lookAt = (LookAtType) block.TypeData[56];

                                try
                                {
                                    Avatars_OnLookAt(sourceAvatar, targetObject, targetPos, lookAt, block.Duration,
                                                     block.ID);
                                }
                                catch (Exception e)
                                {
                                    Debug(e.Message, Helpers.LogLevel.Error, Client, e);
                                }
                                return;
                            }
                            else
                            {
                                Debug("Received a LookAt ViewerEffect with an incorrect TypeData size of " +
                                      block.TypeData.Length + " bytes", Helpers.LogLevel.Warning, Client);
                            }
                        }
                        break;
                    case EffectType.PointAt:
                        // if (Avatars_OnPointAt != null)
                        {
                            if (block.TypeData.Length == 57)
                            {
                                PointAtType pointAt = (PointAtType) block.TypeData[56];

                                try
                                {
                                    Avatars_OnPointAt(sourceAvatar, targetObject, targetPos, pointAt, block.Duration,
                                                      block.ID);
                                }
                                catch (Exception e)
                                {
                                    Debug(e.Message, Helpers.LogLevel.Error, Client, e);
                                }
                                return;
                            }
                            else
                            {
                                Debug("Received a PointAt ViewerEffect with an incorrect TypeData size of " +
                                      block.TypeData.Length + " bytes", Helpers.LogLevel.Warning, Client);
                            }
                        }
                        break;
                    case EffectType.Text:
                    case EffectType.Icon:
                    case EffectType.Connector:
                    case EffectType.FlexibleObject:
                    case EffectType.AnimalControls:
                    case EffectType.AnimationObject:
                    case EffectType.Cloth:
                    case EffectType.Glow:
                    default:
                        SendNewEvent("OnViewerEffect", type, sourceAvatar, targetObject, targetPos);
                        break;
                }
                SendEffect(sourceAvatar, targetObject, targetPos, "EffectType." + type, 0.1f, block.ID);
            }
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
                    SimObject O = GetSimObject(prim, simulator);
                    O.ResetPrim(prim, client, simulator);
                    // Make an initial "ObjectUpdate" for later diff-ing
                    if (MaintainObjectUpdates)
                        LastObjectUpdate[prim.ID] = updatFromPrim(prim);
                    EnsureSelected(prim.LocalID, simulator);
                    EnsureSelected(prim.ParentID, simulator);
                }
            }

            //CalcStats(prim);
            //UpdateTextureQueue(prim.Textures);
        }


        public override void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        {
            SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID, null), properties);
        }

        public void Objects_OnPrimitiveProperties(Simulator simulator, Primitive prim, Primitive.ObjectProperties props)
        {
            if (ScriptHolder == null && prim.ParentID != 0 && prim.ParentID == client.Self.LocalID)
            {
                if ("ScriptHolder" == props.Name)
                {
                    Debug("!!!!!XOXOX!!! Found our ScriptHolder " + prim);
                    ScriptHolder = prim;
                    ScriptHolderAttached = true;
                    ScriptHolderAttachWaiting.Set();
                }
            }
            CheckConnected(simulator);
            NeverSelect(prim.LocalID, simulator);

            lock (PropertyQueue)
                PropertyQueue.Enqueue(delegate() { Objects_OnObjectProperties1(simulator, prim, props); });
        }

        public override void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {
            CheckConnected(simulator);
            //NeverSelect(props.LocalID, simulator);
            lock (PropertyQueue)
                PropertyQueue.Enqueue(delegate() { Objects_OnObjectProperties1(simulator, null, props); });
        }

        public void Objects_OnObjectProperties1(Simulator simulator, Primitive prim, Primitive.ObjectProperties props)
        {
            //Primitive prim = GetPrimitive(props.ObjectID, simulator);
            if (prim != null)
            {
                SimObject updateMe = GetSimObject(prim, simulator);
                updateMe.UpdateProperties(props);
                //Debug("UpdateProperties: {0}", updateMe.DebugInfo());
            }
            describePrimToAI(prim, simulator);
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
                output(String.Format("err :{0}", e.StackTrace));
            }
        }

        private void Objects_OnPrimitiveUpdate(Simulator simulator, Primitive av, ObjectUpdate update,
                                               ulong regionHandle, ushort timeDilation)
        {
            if (av == null || av.ID == UUID.Zero) return; // too early
            SimObject AV = null;
            Object Obj;
            lock (uuidTypeObject)
                if (uuidTypeObject.TryGetValue(av.ID, out Obj))
                {
                    AV = (SimObject) Obj;
                }
                else
                {
                    //AV = GetSimObject(av, simulator);
                }
            if (AV != null)
            {
                if (!SimRegion.OutOfRegion(update.Position))
                {
                    AV.ResetPrim(av, client, simulator);
                }

                if (av.ParentID == 0 && !SimRegion.OutOfRegion(update.Position))
                {
                    if (update.Avatar)
                    {
                        SimRegion.GetRegion(simulator).UpdateTraveled(av.ID, av.Position, av.Rotation);
                        return;
                    }
                }
            }
            if (!MaintainObjectUpdates) return;
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() => Objects_OnObjectUpdated1(simulator, update, regionHandle, timeDilation));
        }

        public override void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle,
                                                     ushort timeDilation)
        {
            /// return;
            if (simulator.Handle != regionHandle)
            {
                Debug("Strange update" + simulator);
                base.Objects_OnObjectUpdated(simulator, update, regionHandle, timeDilation);
            }
            // return;
            CheckConnected(simulator);
            //all things if (update.Avatar)

            Primitive av = GetPrimitive(update.LocalID, simulator);
            Objects_OnPrimitiveUpdate(simulator, av, update, regionHandle, timeDilation);
        }

        public void Objects_OnObjectUpdated1(Simulator simulator, ObjectUpdate update, ulong regionHandle,
                                             ushort timeDilation)
        {
            if (!MaintainObjectUpdates) return;
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
                    if (MaintainObjectUpdates)
                    {
                        lock (LastObjectUpdate)
                            if (!LastObjectUpdate.ContainsKey(objectUpdated.ID))
                            {
                                LastObjectUpdate[objectUpdated.ID] = updatFromPrim(objectUpdated);
                            }

                        // Make a "diff" from previous
                        {
                            Object diffO = notifyUpdate(objectUpdated, LastObjectUpdate[objectUpdated.ID], update,
                                                        InformUpdate);
                            if (diffO != null)
                            {
                                ObjectUpdate diff = (ObjectUpdate) diffO;
                                //if (lastObjectUpdateDiff.ContainsKey(objectUpdated.ID))
                                //{
                                //    notifyUpdate(objectUpdated, lastObjectUpdateDiff[objectUpdated.ID], diff, InformUpdateDiff);
                                //}
                                lock (LastObjectUpdateDiff) LastObjectUpdateDiff[objectUpdated.ID] = diff;
                            }

                            else
                            {
                                // someThingElseNeedsUpdate(objectUpdated);
                                //  needsOsdDiff = true;
                            }
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
                        // Call ISimObject Update the Previous object update will be saved in the "lastObjectUpdate[objectUpdated.ID]"
                        //Delegate d= new delegate()
                        if (MaintainObjectUpdates)
                        {
                            ObjectUpdate TheDiff = default(ObjectUpdate);
                            lock (LastObjectUpdateDiff)
                            {
                                if (LastObjectUpdateDiff.ContainsKey(objectUpdated.ID))
                                {
                                    TheDiff = LastObjectUpdateDiff[objectUpdated.ID];
                                }
                                else
                                {
                                    LastObjectUpdateDiff[objectUpdated.ID] = TheDiff;
                                }
                            }
                            simObject.UpdateObject(update, TheDiff);
                        }
                        //SendNewEvent("OnObjectUpdated", simObject, update);
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
                        lock (LastObjectUpdate) LastObjectUpdate[objectUpdated.ID] = update;
                    }
                }
            }
            else
            {
                //output("missing Objects_OnObjectUpdated");
            }
        }

        public object InformUpdate(Primitive objectUpdated, string p, Object before, Object after, Object diff)
        {
            //Debug("{0} {1} DIFF {2} BEFORE {3} AFTER {4}", p, objectUpdated, diff, before, after);
            if (diff is Vector3)
            {
                // if the change is too small skip the event
                if ((1 > ((Vector3) diff).Length()))
                {
                    //  return after;
                }
            }
            //  String lispName = "on-" + ((objectUpdated is Avatar) ? "avatar-" : "prim-") + p.ToLower() + "-updated";
            String lispName = "on-object-" + p.ToLower();
            SendNewEvent(lispName, objectUpdated, after);
            return after;
        }

        private Object notifyUpdate(Primitive objectUpdated, ObjectUpdate before, ObjectUpdate after, DoWhat didUpdate)
        {
            ObjectUpdate diff = updateDiff(before, after);
            bool wasChanged = false;
            bool wasPositionUpdateSent = false;
            if (before.Acceleration != after.Acceleration)
            {
                after.Acceleration =
                    (Vector3)
                    didUpdate(objectUpdated, "Acceleration", before.Acceleration, after.Acceleration, diff.Acceleration);
                wasChanged = true;
            }
            if (before.AngularVelocity != after.AngularVelocity)
            {
                after.AngularVelocity =
                    (Vector3)
                    didUpdate(objectUpdated, "AngularVelocity", before.AngularVelocity, after.AngularVelocity,
                              diff.AngularVelocity);
                wasChanged = true;
            }
            if (before.CollisionPlane != after.CollisionPlane)
            {
                after.CollisionPlane =
                    (Vector4)
                    didUpdate(objectUpdated, "CollisionPlane", before.CollisionPlane, after.CollisionPlane,
                              diff.CollisionPlane);
                wasChanged = true;
            }
            if (before.Position != after.Position)
            {
                after.Position =
                    (Vector3) didUpdate(objectUpdated, "Position", before.Position, after.Position, diff.Position);
                wasChanged = true;
                wasPositionUpdateSent = true;
            }
            if (ChangedBetween(before.Rotation, after.Rotation) > 0.1f)
            {
                after.Rotation =
                    (Quaternion) didUpdate(objectUpdated, "Rotation", before.Rotation, after.Rotation, diff.Rotation);
                wasChanged = true;
            }
            if (before.State != after.State)
            {
                didUpdate(objectUpdated, "State", before.State, after.State, diff.State);
                wasChanged = true;
            }
            if (before.Textures != after.Textures)
            {
                //didUpdate(objectUpdated, "Textures", before.Textures, after.Textures, diff.Textures);
                //wasChanged = true;
            }
            if (before.Velocity != after.Velocity)
            {
                // didUpdate(objectUpdated, "Velocity", before.Velocity, after.Velocity, diff.Velocity);
                if (before.Velocity == Vector3.Zero)
                {
                    SendNewEvent("on-object-start-velocity", objectUpdated, after.Velocity);
                    if (!wasPositionUpdateSent) SendNewEvent("on-object-position", objectUpdated, after.Position);
                }
                else if (after.Velocity == Vector3.Zero)
                {
                    if (!wasPositionUpdateSent) SendNewEvent("on-object-position", objectUpdated, after.Position);
                    SendNewEvent("on-object-stop-velocity", objectUpdated, -before.Velocity);
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
            string xmlBefore = OSDParser.SerializeLLSDXmlString(before);
            string xmlAfter = OSDParser.SerializeLLSDXmlString(after);
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

        public override void Objects_OnAvatarSitChanged(Simulator simulator, Avatar avatar, uint sittingOn, uint oldSeat)
        {
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() =>
                                        {
                                            SimObject user = GetSimObject(avatar, simulator);
                                            SimObject newSit = GetSimObject(sittingOn, simulator);
                                            SimObject oldSit = GetSimObject(oldSeat, simulator);
                                            object[] eventArgs = new object[] {user, newSit, oldSit};
                                            string sitName = null;
                                            if (newSit != null)
                                            {
                                                sitName = newSit.SitName;
                                            }
                                            else
                                            {
                                                if (oldSit != null) sitName = oldSit.SitName;
                                            }

                                            SendNewEvent("On-Avatar-Sit-Changed", user, newSit, oldSit);
                                            if (user != null)
                                            {
                                                if (sitName != null) user.LogEvent(sitName, newSit, oldSit);
                                                else
                                                {
                                                    if (sittingOn + oldSeat > 0)
                                                    {
                                                        user.LogEvent("SitOnGround");
                                                        Thread.Sleep(1000);
                                                        newSit = GetSimObject(sittingOn, simulator);
                                                        oldSit = GetSimObject(oldSeat, simulator);
                                                        if (newSit != null || oldSit != null)
                                                        {
                                                            Objects_OnAvatarSitChanged(simulator, avatar, sittingOn,
                                                                                       oldSeat);
                                                        }
                                                    }
                                                    else
                                                        user.LogEvent("SitOnGround");
                                                }
                                            }
                                            else
                                            {
                                                if (newSit != null)
                                                    newSit.AddCanBeTargetOf(newSit.SitName, 1, eventArgs);
                                                else if (oldSit != null)
                                                    oldSit.AddCanBeTargetOf(oldSit.SitName, 2, eventArgs);
                                            }
                                        });
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
                p = GetPrimitive(sittingOn, simulator);
                Debug("WARN: cant get prim " + sittingOn + " sim " + simulator);
                return null;
            }
            return GetSimObject(p, simulator);
        }

        public Primitive deepCopy(Primitive objectUpdated)
        {
            OSD osd = objectUpdated.GetOSD();
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
            update.State = (byte) (fromPrim.State - diff.State);
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

        public override void Avatars_OnPointAt(UUID sourceID, UUID targetID,
                                               Vector3d targetPos, PointAtType lookType, float duration, UUID id)
        {
            SendEffect(sourceID, targetID, targetPos, "PointAtType." + lookType.ToString(), duration, id);
        }

        public override void Avatars_OnLookAt(UUID sourceID, UUID targetID,
                                              Vector3d targetPos, LookAtType lookType, float duration, UUID id)
        {
            SendEffect(sourceID, targetID, targetPos, "LookAtType." + lookType.ToString(), duration, id);
        }

        public bool UseEventSource(Object so)
        {
            if (so is SimAvatar)
            {
                if (so.ToString().Contains("rael")) return true;
            }
            return false;
        }

        public void SendEffect(UUID sourceID, UUID targetID, Vector3d targetPos, string effectType, float duration,
                               UUID id)
        {
            if (id != UUID.Zero)
            {
                // if (EffectsSent.Contains(id)) return;
                // EffectsSent.Add(id);
            }
            object s = sourceID;
            object t = targetID;
            object p = targetPos;

            if (SkippedEffects.Contains(effectType)) return;
            SimObject source = GetSimObjectFromUUID(sourceID);
            if (source == null)
            {
                RequestAsset(sourceID, AssetType.Object, true);
            }
            else
            {
                s = source;
            }
            SimObject target = GetSimObjectFromUUID(targetID);
            if (target == null)
            {
                RequestAsset(targetID, AssetType.Object, true);
            }
            else
            {
                t = target;
            }
            if (targetPos.X < 256)
            {
                p = new Vector3((float) targetPos.X, (float) targetPos.Y, (float) targetPos.Z);
            }
            else
            {
                double dist;
                SimObject posTarget = GetSimObjectFromVector(targetPos, out dist);
                if (dist < 0.5)
                {
                    p = posTarget;
                }
            }

            lock (UpdateQueue)
                UpdateQueue.Enqueue(() =>
                                        {
                                            //if (source != null) source;
                                            // output("TextForm Avatars_OnLookAt: " + sourceID.ToString() + " to " + targetID.ToString() + " at " + targetID.ToString() + " with type " + lookType.ToString() + " duration " + duration.ToString());
                                            if (targetID == client.Self.AgentID)
                                            {
                                                // if (lookType == LookAtType.Idle) return;
                                                //output("  (TARGET IS SELF)");
                                                SendNewEvent("on-effect-targeted-self", s, p, effectType);
                                                // ()/*GetObject*/(sourceID), effectType);
                                            }
                                            if (source != null)
                                            {
                                                source.OnEffect(effectType, t, p, duration, id);
                                            }
                                            else
                                            {
                                                if (target != null)
                                                {
                                                    target.AddCanBeTargetOf(effectType, 1,
                                                                            new object[] {s, t, p, duration, id});
                                                }
                                                RegisterUUID(id, effectType);
                                                //TODO 
                                                if (UseEventSource(s))
                                                    SendNewEvent("on-effect", effectType, s, t, p, duration, id);
                                            }
                                        });
        }

        internal SimObject GetSimObjectFromVector(Vector3d here, out double dist)
        {
            SimObject retObj = null;
            dist = double.MaxValue;
            if (here == Vector3d.Zero) return retObj;
            foreach (SimObject obj in GetAllSimObjects())
            {
                if (obj.IsRegionAttached())
                {
                    Vector3d at = obj.GetWorldPosition();
                    if (at == here)
                    {
                        dist = 0;
                        return obj;
                    }
                    double ld = Vector3d.Distance(at, here);
                    if (ld < dist)
                    {
                        retObj = obj;
                        dist = ld;
                    }
                }
            }
            return retObj;
        }


        public Avatar GetAvatar(UUID avatarID, Simulator simulator)
        {
            Primitive prim = GetPrimitive(avatarID, simulator);
            if (prim is Avatar) return (Avatar) prim;
            //   prim = GetPrimitive(avatarID, simulator);
            return null;
        }

        public Type GetType(UUID uuid)
        {
            Object found = GetObject(uuid);
            if (found == null) return null;
            //RegisterUUID(uuid] = found;
            if (found is Type) return (Type) found;
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
            string name = SimAnimationSystem.GetAnimationName(id);
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

        public Asset GetAsset(UUID id)
        {
            lock (uuidTypeObject)
            {
                Object assetObject;
                uuidTypeObject.TryGetValue(id, out assetObject);
                if (assetObject is Asset)
                {
                    return (Asset) assetObject;
                }
            }
            //RequestAsset(id, AssetType.Object, true);
            //IAssetProvider assetProvider = null;// TextForm.simulator.Assets;
            //if (assetProvider == null)
            //{
            //    Debug("Asset Provider still off line for " + id);
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

        public static void RequestAsset(UUID id, AssetType assetType, bool p)
        {
            if (id == UUID.Zero) return;
            if (assetType == AssetType.Animation)
            {
                if (Master.SimAnimationSystem.GetAnimationName(id) != null) return;
            }
            lock (AssetRequests)
            {
                if (AssetRequests.ContainsKey(id)) return;
                UUID req = RegionMasterTexturePipeline.RequestAsset(id, assetType, p);
                AssetRequests[id] = req;
                AssetRequestType[req] = assetType;
            }
        }

        public Primitive GetPrimitive(UUID id, Simulator simulator)
        {
            lock (uuidTypeObject)
            {
                if (uuidTypeObject.ContainsKey(id))
                {
                    object simobject = uuidTypeObject[id];
                    if (simobject != null && simobject is SimObject)
                        return ((SimObject) simobject).Prim;
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
                        return ((SimObject) simobject).Prim;
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
            if (!IsRegionMaster) return;
            //	Debug(eventName + " " + client.argsListString(args));
            String evtStr = eventName.ToString();
            if (evtStr == "on-object-position")
            {
                Primitive prim = (Primitive) args[0];
                Vector3 vect = (Vector3) args[1];

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
            for (int i = 0; i < args.Length; i++)
            {
                object arg = args[i];
                if (arg is UUID)
                {
                    object o = GetObject((UUID) arg);
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

            TheSimAvatar.SortByDistance(matches);
            if (pickNum != 0 && pickNum <= matches.Count)
            {
                prim = matches[(int) pickNum - 1].Prim;
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

        public string describePrim(Primitive target)
        {
            if (target == null) return "null";
            SimObject simObject = GetSimObject(target);
            string str = string.Empty;
            if (simObject != null)
            {
                str += simObject.ToString();
                str += String.Format(" {0}", TheSimAvatar.DistanceVectorString(simObject));
                str += String.Format("\n GroupLeader: {0}", simObject.GetGroupLeader());
            }
            else
            {
                str += target;
            }
            if (target.Properties != null && target.Properties.SalePrice != 0)
                str += " Sale: L" + target.Properties.SalePrice;
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
            return str; // output(str);
        }

        public void describePrimToAI(Primitive prim, Simulator simulator)
        {
            if (true) return;
            if (prim is Avatar)
            {
                Avatar avatar = (Avatar) prim;
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
            return (int) (getFitness(p2) - getFitness(p1));
        }

        public List<Primitive> getPrimitives(int num)
        {
            List<SimObject> ret = new List<SimObject>();
            TheSimAvatar.ScanNewObjects(10, 100);
            TheSimAvatar.GetKnownObjects().ForEach(delegate(SimObject prim) { ret.Add(prim); });

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
                return /* ((float)prim.ToString().Length/5 -*/ (float) TheSimAvatar.Distance(prim);
            }
            float fitness = 1;
            foreach (ObjectHeuristic heuristic in objectHeuristics)
            {
                fitness *= heuristic(prim);
            }
            return fitness;
        }

        private float distanceHeuristic(SimObject prim)
        {
            if (prim != null)
                return (float) (1.0/Math.Exp((double) TheSimAvatar.Distance(prim))
                               );
            else
                return (float) 0.01;
        }


        private float boringNamesHeuristic(SimObject prim)
        {
            return prim.ToString().Length;
        }

        private bool tryGetBuildingPos(List<Primitive> group, out Vector3 centroid)
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
                    centroid = min + (size*(float) 0.5);
                    return true;
                }
                else
                    return false;
            }
        }

        public int posComp(Vector3 v1, Vector3 v2)
        {
            return (int) (Vector3.Mag(client.Self.RelativePosition - v1) -
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
            return (int) (Vector3.Distance(a1.Position, compPos) - Vector3.Distance(a2.Position, compPos));
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

                for (; searchStep*num > avatarList.Count; --searchStep) ;

                List<Avatar> ret = new List<Avatar>();
                for (int i = 0; i < num && i < avatarList.Count; i += searchStep)
                    ret.Add(avatarList[i]);
                searchStep = (searchStep + 1)%4 + 1;
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
                avatar = (Avatar) prim;
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
            client.Objects.SetFlags(UnPhantom.LocalID, ((fs & PrimFlags.Physics) != 0), //
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
                                                     client.Inventory.FindFolderForType(AssetType.TrashFolder),
                                                     UUID.Random());
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
            if (NeverSelect(LocalID, simulator))
                ReallyEnsureSelected(simulator, LocalID);
        }

        public bool NeverSelect(uint LocalID, Simulator simulator)
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
            return SimAnimationSystem.GetAnimationUUID(a);
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
                    return SimWaypointImpl.CreateLocal(target, TheSimAvatar.GetPathStore());
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
                {
                    if (rootOnly && !obj.IsRoot) continue;
                    if (obj.IsRegionAttached() && Vector3d.Distance(obj.GetWorldPosition(), here) <= maxDistance)
                        nearby.Add(obj);
                }
            }
            ;
            return nearby;
        }

        public SimObject GetSimObject(Primitive prim)
        {
            return GetSimObject(prim, null);
        }

        public byte[] TextureBytesFormUUID(UUID uUID)
        {
            ImageDownload ID = null;
            lock (uuidTypeObject)
            {
                object iObject;
                if (uuidTypeObject.TryGetValue(uUID, out iObject))
                {
                    if (iObject is ImageDownload)
                    {
                        ID = (ImageDownload) iObject;
                    }
                }
            }
            if (ID == null)
            {
                ID = StartTextureDownload(uUID);
                if (ID == null)
                {
                    int tried = 20;
                    int giveUpTick = Environment.TickCount + 1*60000;
                    while (ID == null)
                    {
                        ID = StartTextureDownload(uUID);
                        if (ID == null)
                        {
                            lock (TexturesSkipped) if (TexturesSkipped.Contains(uUID)) return null;
                            if (Environment.TickCount > giveUpTick)
                            {
                                lock (TexturesSkipped) TexturesSkipped.Add(uUID);
                                if (Settings.LOG_LEVEL == Helpers.LogLevel.Warning)
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
            if (Settings.LOG_LEVEL == Helpers.LogLevel.Debug) Debug("-|-|- SUCCEED SculptMesh " + uUID);

            return ID.AssetData;
        }

        private static void DoEvents()
        {
            //todo  throw new Exception("The method or operation is not implemented.");
        }

        internal void SetObjectPosition(Primitive Prim, Vector3 localPos)
        {
            Simulator sim = GetSimulator(Prim);
            client.Objects.SetPosition(sim, Prim.LocalID, localPos);
        }

        private Simulator GetSimulator(Primitive Prim)
        {
            ulong handle = Prim.RegionHandle;
            if (handle != 0) return GetSimulator(handle);
            Debug("GetSimulator returning current sim for " + Prim);
            return client.Network.CurrentSim;
        }

        internal void SetObjectRotation(Primitive Prim, Quaternion localPos)
        {
            Simulator sim = GetSimulator(Prim);
            client.Objects.SetRotation(sim, Prim.LocalID, localPos);
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
            return SimRegion.GetRegion(handle).TheSimulator;
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
            client.Objects.SelectObject(sim, P.LocalID);
        }

        internal Primitive AddTempPrim(SimRegion R, string name, PrimType primType, Vector3 scale, Vector3 loc)
        {
            Primitive.ConstructionData CD = ObjectManager.BuildBasicShape(primType);
            CD.Material = Material.Light;
            CD.ProfileHole = HoleType.Triangle;

            bool success = false;

            Simulator simulator = R.TheSimulator;
            Primitive newPrim = null;
            // Register a handler for the creation event
            AutoResetEvent creationEvent = new AutoResetEvent(false);
            Quaternion rot = Quaternion.Identity;
            ObjectManager.NewPrimCallback callback =
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
                            Debug("Not the prim?  prim.PrimData.ProfileHole != HoleType.Triangle: {0}!={1}",
                                  prim.PrimData.ProfileHole, HoleType.Triangle);
                            // return;       //
                        }
                        if (Material.Light != prim.PrimData.Material)
                        {
                            Debug("Not the prim? Material.Light != prim.PrimData.Material: {0}!={1}", Material.Light,
                                  prim.PrimData.Material);
                            // return;
                        }
                        if ((prim.Flags & PrimFlags.CreateSelected) == 0)
                        {
                            Debug("Not the prim? (prim.Flags & PrimFlags.CreateSelected) == 0) was {0}", prim.Flags);
                            // return;
                        }
                        if (primType != prim.Type)
                        {
                            Debug("Not the prim? Material.Light != prim.PrimData.Material: {0}!={1}", Material.Light,
                                  prim.PrimData.Material);
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
            client.Objects.AddPrim(simulator, CD, UUID.Zero, loc, scale, rot,
                                   PrimFlags.CreateSelected | PrimFlags.Phantom | PrimFlags.Temporary);

            // Wait for the process to complete or time out
            if (creationEvent.WaitOne(1000*120, false))
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

        public override void Self_OnChat(string message, ChatAudibleLevel audible, ChatType type,
                                         ChatSourceType sourceType, string fromName, UUID id, UUID ownerid,
                                         Vector3 position)
        {
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() => SendNewEvent("on-chat", message, audible, type, sourceType, fromName, id,
                                                       ownerid, position));
        }

        public override void Self_OnInstantMessage(InstantMessage im, Simulator simulator)
        {
            lock (UpdateQueue)
                UpdateQueue.Enqueue(() => SendNewEvent("on-instantmessage", im.FromAgentName, im.Message, im.ToAgentID,
                                                       im.Offline, im.IMSessionID, im.GroupIM, im.Position, im.Dialog,
                                                       im.ParentEstateID));
        }

        public override void Self_OnAlertMessage(string msg)
        {
            lock (UpdateQueue) UpdateQueue.Enqueue(() => SendNewEvent("On-Alert-Message", client.gridClient, msg));
        }

        #region Nested type: DoWhat

        private delegate object DoWhat(Primitive objectUpdated, string p, Object vector3, Object vector3_4, Object diff);

        #endregion

        #region Nested type: ObjectUpdateItem

        private delegate void ObjectUpdateItem();

        #endregion
    }
}