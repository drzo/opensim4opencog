using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using System.Reflection;
using OpenMetaverse.Packets;
using cogbot.TheOpenSims;
using Simian;
using System.Threading;
using cogbot.Actions;
using cogbot.TheOpenSims.Navigation; //using libsecondlife;

namespace cogbot.Listeners
{
    public delegate float ObjectHeuristic(SimObject prim);

    public class WorldObjects : DebugAllEvents
    {

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
        public int ExpectedObjects = 1000;

        DoubleDictionary<uint, UUID, Primitive> prims = new DoubleDictionary<uint, UUID, Primitive>();
        //DoubleDictionary<uint, UUID, Avatar> prims = new DoubleDictionary<uint, UUID, Avatar>();
        Dictionary<uint, ObjectUpdate> lastObjectUpdate = new Dictionary<uint, ObjectUpdate>();
        Dictionary<uint, ObjectUpdate> lastObjectUpdateDiff = new Dictionary<uint, ObjectUpdate>();
        Dictionary<Avatar, List<UUID>> avatarAminsSent = new Dictionary<Avatar, List<UUID>>();
        Dictionary<Avatar, UUID> avatarAminCurrent = new Dictionary<Avatar, UUID>();
        //Dictionary<uint, uint> selectedPrims = new Dictionary<uint, uint>();
        //Dictionary<UUID, UUID> texturesFinished = new Dictionary<UUID, UUID>();
        static List<uint> primsAwaitingSelect = new List<uint>();
        static List<uint> primsSelectedOnce = new List<uint>();

        public SimAvatar TheSimAvatar
        {
            get
            {
                if (m_TheSimAvatar == null)
                {
                    m_TheSimAvatar = (SimAvatar)GetSimObject(GetAvatar(client.Self.AgentID));
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
        public SimPathStore SimPaths = SimPathStore.Instance;

        static Thread TrackPathsThread;
        static public Queue<SimObject> UpdateMeQueue = new Queue<SimObject>();
        internal void AddTracking(SimObject simObject)
        {
            if (true) return;
            lock (UpdateMeQueue)
                if (TrackPathsThread == null)
                {
                    TrackPathsThread = new Thread(new ThreadStart(TrackPaths));
                    TrackPathsThread.Name = "TrackPathsThread";
                    TrackPathsThread.Priority = ThreadPriority.Lowest;
                    TrackPathsThread.Start();
                }
            //lock (UpdateMeQueue)
            //{
            //    UpdateMeQueue.Enqueue(simObject);
            //}
            //    if (!SimObjectsPathUpdates.Contains*() SimObjectsPathUpdates.Add(simObject);

            //  Client.Objects.OnObjectUpdated += Objects_OnObjectUpdated;
            //throw new Exception("The method or operation is not implemented.");
        }
        static void TrackPaths()
        {
            int lastCount = 0;
            while (true)
            {
                Thread.Sleep(10000);

                int thisCount = SimObjects.Count;
                if (thisCount == lastCount) continue;
                lastCount = thisCount;
                //List<SimObject> DoNow = new List<SimObject>();

                //lock (UpdateMeQueue)
                //{
                //    if (UpdateMeQueue.Count == 0) continue;
                //    while (UpdateMeQueue.Count > 0)
                //    {
                //        DoNow.Add(UpdateMeQueue.Dequeue());
                //    }
                //}

                foreach (SimObject O in SimObjects)
                {
                    O.UpdatePaths(SimPathStore.Instance);
                }
            }
        }

        // these will be shared between Clients
        static ListAsSet<SimObject> SimObjects = new ListAsSet<SimObject>();

        //public static BotRegionModel BotWorld = null;
        //        TheBotsInspector inspector = new TheBotsInspector();

        ///  inspector.Show();

        private void CatchUp(Simulator simulator)
        {
            simulator.ObjectsAvatars.ForEach(delegate(Avatar item)
            {
                GetSimObject(item);
            });
            simulator.ObjectsPrimitives.ForEach(delegate(Primitive item)
            {
                GetSimObject(item);
            });
            //miniMap.UpdateMiniMap(simulator);
        }



        static Dictionary<uint, SimObject> LocalIdToSimObject = new Dictionary<uint, SimObject>();

        public SimObject GetSimObject(Primitive prim)
        {
            SimObject obj0;
            lock (LocalIdToSimObject)
            {
                if (LocalIdToSimObject.TryGetValue(prim.LocalID, out obj0))
                {
                    return obj0;
                }

                // not found
                if (prim is Avatar)
                {
                    obj0 = new SimAvatar((Avatar)prim, this);
                }
                else
                {
                    obj0 = new SimObject(prim, this);
                }
                LocalIdToSimObject[prim.LocalID] = obj0;
                lock (SimObjects) SimObjects.AddTo(obj0);

                return obj0;
            }
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
        //    LocalIdToSimObject[prim.LocalID] = obj0;
        //    lock (SimAvatars) SimAvatars.AddTo(obj0);
        //    lock (SimObjects) SimObjects.AddTo(obj0);
        //    return obj0;
        //}

        static WorldObjects Master;
        public bool IsWorldMaster() {
            return Master == this;
        }

        public WorldObjects(BotClient client)
            : base(client)
        {
            if (Master == null) Master = this;
          //  miniMap = new OpenMetaverse.GUI.MiniMap(client);
            //	client.Self.OnMeanCollision += new AgentManager.MeanCollisionCallback(Self_OnMeanCollision);

            // primsAwaitingSelect = new List<Primitive>();
            // prims = new Dictionary<string, Primitive>();
            //  primsByLocalID = new Dictionary<uint, Primitive>();
            // pendingPrims = new Dictionary<UUID, Primitive>();

            primGroups = new Dictionary<UUID, List<Primitive>>();

            objectHeuristics = new List<ObjectHeuristic>();
            objectHeuristics.Add(new ObjectHeuristic(distanceHeuristic));
            //objectHeuristics.Add(new ObjectHeuristic(nameLengthHeuristic));
            objectHeuristics.Add(new ObjectHeuristic(boringNamesHeuristic));

            client.Settings.SIMULATOR_TIMEOUT = 3 * 60000;
            client.Settings.CAPS_TIMEOUT = 3 * 60000;
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

            /*
                texturePipeline = new TexturePipeline(client, 10);
                texturePipeline.OnDownloadFinished += new TexturePipeline.DownloadFinishedCallback(texturePipeline_OnDownloadFinished);
             */
            {
                //BotWorld = this;
                SimTypeSystem.LoadDefaultTypes();

                if (IsWorldMaster())
                {
                    if (client.Network.CurrentSim != null)
                    {
                        CatchUp(client.Network.CurrentSim);
                    }
                    else
                    {
                        client.Network.OnLogin += delegate(LoginStatus login, string message)
                        {
                            if (login == LoginStatus.Success)
                                CatchUp(GetSimulator());
                        };
                    }
                }
            }
            if (IsWorldMaster()) RegisterAll();
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
        TexturePipeline texturePipeline;

        void texturePipeline_OnDownloadFinished(UUID id, bool success)
        {
            if (success)
            {
                // Save this texture to the hard drive
                ImageDownload image = texturePipeline.GetTextureToRender(id);
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


        public void output(string p)
        {
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
            //botoutput("preload sound " + soundID);
        }


        public Primitive BlockUntilProperties(Primitive prim)
        {
            if (prim.Properties != null) return prim;
            EnsureSelected(prim.LocalID);
            while (prim.Properties == null)
            { // TODO maybe add a timer
                System.Windows.Forms.Application.DoEvents();
            }
            return prim;
        }

        public override void Avatars_OnAvatarAnimation(UUID avatarID, InternalDictionary<UUID, int> anims)
        {
            Avatar avatar = GetAvatar(avatarID);
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


            //OnEvent("On-Coarse-Location-Update", paramNamesOnCoarseLocationUpdate, paramTypesOnCoarseLocationUpdate, sim);
        }



        static readonly string[] paramNamesOnObjectKilled = new string[] { "simulator", "objectID" };
        static readonly Type[] paramTypesOnObjectKilled = new Type[] { typeof(Simulator), typeof(uint) };

        public override void Objects_OnObjectKilled(Simulator simulator, uint objectID)
        {

            Primitive p;
            if (prims.TryGetValue(objectID, out p))
            {
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
            if (!IsWorldMaster()) return;
            lock (primsAwaitingSelect)
            {
                //lock (prim)                   prims.Add(prim.LocalID, prim.ID, prim);
                EnsureSelected(prim.LocalID);
            }
            RegisterUUID(prim.ID, prim);

            // Make an intial "ObjectUpdate" for later diffing
            lastObjectUpdate[prim.LocalID] = updatFromPrim(prim);
            //CalcStats(prim);
            //UpdateTextureQueue(prim.Textures);
        }

        public override void Avatars_OnAvatarProperties(UUID avatarID, Avatar.AvatarProperties properties)
        {
            SendNewEvent("On-Avatar-Properties", GetAvatar(avatarID), properties);
        }

        object Objects_OnObjectPropertiesLock = new object();
        public override void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {
            // lock (Objects_OnObjectPropertiesLock)
            Objects_OnObjectProperties1(simulator, props);
        }
        private void Objects_OnObjectProperties1(Simulator simulator, Primitive.ObjectProperties props)
        {
            if (!IsWorldMaster()) return;
            Primitive prim = GetPrimitive(props.ObjectID);
            if (prim != null)
            {
                if (primsAwaitingSelect.Contains(prim.LocalID))
                {
                    primsAwaitingSelect.Remove(prim.LocalID);
                }
                SimObject updateMe = GetSimObject(prim);
                updateMe.UpdateProperties(props);
            }
            //if (Program.Verbosity > 2)
            ///botoutput("Received properties for " + props.ObjectID.ToString());               
            //lock (prim)  prim.Properties = props;
            //lock (primsKnown)
            //    if (!primsKnown.Contains(prim))
            //    {
            //        primsKnown.Add(prim);
            //        SendNewEvent("on-new-prim", props.Name, props.ObjectID.ToString(), props.Description, prim.Position);
            //        CalcStats(prim);
            //    }
            describePrimToAI(prim);
            if (prims.Count>ExpectedObjects - 11)
            {
                OnPrimsLoaded();
            }
        }

        private void OnPrimsLoaded()
        {
            output("OnPrimsLoaded");
            ExpectedObjects += 2;
        }



        object Objects_OnNewAvatarLock = new object();
        public override void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            lock (Objects_OnNewAvatarLock)
                Objects_OnNewAvatar1(simulator, avatar, regionHandle, timeDilation);
        }
        private void Objects_OnNewAvatar1(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            if (!IsWorldMaster()) return;
            //lock (prims)
            //GetSimAvatar(avatar);
            // prims.Add(avatar.LocalID, avatar.ID, avatar);
            //lock (prims)  prims.Add(avatar.LocalID, avatar.ID, avatar);
            Objects_OnNewPrim(simulator, avatar, regionHandle, timeDilation);
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
                describeAvatarToAI(avatar);
            }
            catch (Exception e)
            {
                output("err :" + e.StackTrace);
            }
        }
        //void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        //{
        //    sim = simulator;
        //    lock (pendingPrims)
        //    {
        //        if (pendingPrims.ContainsKey(properties.ObjectID))
        //        {
        //            lock (prims)
        //            {
        //                Primitive p = pendingPrims[properties.ObjectID];
        //                prims[properties.Name] = p;
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
        //                        primGroups[groupId].Add(prims[properties.Name]);
        //                        //botoutput("group count " + groupId + " " + primGroups[groupId].Count);
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
        //    sim = simulator;
        //    try
        //    {
        //        lock (newLock)
        //        {
        //            primsAwaitingSelect.Add(prim);
        //            CalcStats(prim);

        //            TimeSpan dtime = DateTime.Now - burstStartTime;
        //            if (primsAwaitingSelect.Count >= burstSize && dtime > burstInterval)
        //            {
        //                burstStartTime = DateTime.Now;

        //                uint[] ids = new uint[burstSize];
        //                for (int i = 0; i < burstSize; ++i)
        //                {
        //                    ids[i] = primsAwaitingSelect[i].LocalID;
        //                    pendingPrims[primsAwaitingSelect[i].ID] = primsAwaitingSelect[i];
        //                }

        //                botclient.Objects.SelectObjects(simulator, ids);
        //                primsAwaitingSelect.RemoveRange(0, burstSize);
        //                //primsAwaitingSelect.Clear();
        //            }
        //        }
        //CalcStats(prim);
        //        describePrimToAI(prim);
        //    }
        //    catch (Exception e)
        //    {
        //        botoutput("ERR:" + e.StackTrace);
        //    }
        //}

        object Objects_OnObjectUpdated1Sync = new Object();
        public override void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
         /// lock (Objects_OnObjectUpdated1Sync)
                Objects_OnObjectUpdated1(simulator, update, regionHandle, timeDilation);
        }


        private void Objects_OnObjectUpdated1(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (!IsWorldMaster()) return;
            Primitive objectUpdated = GetPrimitive(update.LocalID);
            //Console.WriteLine("timeDilation " + timeDilation);

            if (objectUpdated != null)
            {
                lock (objectUpdated)
                {
                    SimObject simObject = GetSimObject(objectUpdated);
                    AddTracking(simObject);
                    if (update.Avatar)
                    {
                        //   if (false)
                    //    if (update.LocalID != client.Self.LocalID)
                        //  {  // Way point creation from other avatars moving
                        //   new Thread(new ThreadStart(delegate()
                        {
                            SimPaths.Update(update.LocalID, simObject.GetSimPosition(), update.Rotation);
                            //   })).Start();
                            // output("Updating state for Avatar " + prim.Name);
                        }
                    }

                    // other OMV code already updated the Primitive
                    // updateToPrim(prim, update);

                    if (objectUpdated.Properties != null)
                    {
                        //CalcStats(objectUpdated);
                        describePrimToAI(objectUpdated);
                    }
                    else
                    {
                        EnsureSelected(objectUpdated.LocalID);
                    }

                    // bool needsOsdDiff = false; //for debugging should be false otherwise

                    // Make a Last Object Update from the Primitive if we knew nothing about it
                    if (!lastObjectUpdate.ContainsKey(update.LocalID))
                    {
                        lastObjectUpdate[update.LocalID] = updatFromPrim(objectUpdated);
                    }

                    // Make a "diff" from previous
                    Object diffO = notifyUpdate(objectUpdated, lastObjectUpdate[update.LocalID], update, InformUpdate);
                    if (diffO != null)
                    {
                        ObjectUpdate diff = (ObjectUpdate)diffO;
                        //if (lastObjectUpdateDiff.ContainsKey(update.LocalID))
                        //{
                        //    notifyUpdate(objectUpdated, lastObjectUpdateDiff[update.LocalID], diff, InformUpdateDiff);
                        //}
                        lastObjectUpdateDiff[update.LocalID] = diff;
                    }
                    else
                    {
                        // someThingElseNeedsUpdate(objectUpdated);
                        //  needsOsdDiff = true;
                    }

                    if (simObject != null)
                    {
                        // Call SimObject Update the Previous object update will be saved in the "lastObjectUpdate[update.LocalID]"
                       //Delegate d= new delegate()
                        {
                            simObject.UpdateObject(update, lastObjectUpdateDiff[update.LocalID]);
                        }
                    }
                    // BlockUntilProperties(objectUpdated);
                    //if (false && objectUpdated.Properties != null)
                    //{
                    //    OSDMap after = (OSDMap)objectUpdated.GetOSD();
                    //    //  Console.WriteLine(OSDParser.SerializeLLSDXmlString(after));
                    //    if (lastOSD.ContainsKey(update.LocalID))
                    //    {
                    //        if (needsOsdDiff)
                    //        {
                    //            //Primitive before = lastDeepCopy[update.LocalID];
                    //            OSDMap before = lastOSD[update.LocalID];
                    //            String osdDiff = OSDDiff(before, after);
                    //            if (osdDiff.Length > 0)
                    //            {
                    //                Console.WriteLine(osdDiff);
                    //            }
                    //        }

                    //    }

                    //    lastOSD[update.LocalID] = after;
                    //}

                    //Primitive deep = (Primitive)deepCopy(objectUpdated);
                    //lastDeepCopy[update.LocalID] = deep;
                }
            }
            else
            {
                output("missing Objects_OnObjectUpdated");
            }
            lastObjectUpdate[update.LocalID] = update;
        }

        delegate object DoWhat(Primitive objectUpdated, string p, Object vector3, Object vector3_4, Object diff);
        public object InformUpdate(Primitive objectUpdated, string p, Object before, Object after, Object diff)
        {
            //Console.WriteLine("{0} {1} DIFF {2} BEFORE {3} AFTER {4}", p, objectUpdated, diff, before, after);
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

        private float ChangedBetween(Quaternion quaternion, Quaternion quaternion_2)
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

        public Avatar GetAvatar(UUID avatarID)
        {
            Primitive prim = GetPrimitive(avatarID);
            if (prim is Avatar) return (Avatar)prim;
            prim = GetPrimitive(avatarID);
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

            object ret = GetPrimitive(id);
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

        private Asset GetAsset(UUID id)
        {
            Asset asset;
            IAssetProvider assetProvider = null;// TextForm.simulator.Assets;
            if (assetProvider == null)
            {
                Console.WriteLine("Asset Provider still offline for " + id);
                return null;
            }
            assetProvider.TryGetAsset(id, out asset);
            if (asset != null)
            {
                RegisterUUID(id, asset);
            }
            return asset;
        }



        public Primitive GetPrimitive(UUID id)
        {
            lock (uuidTypeObject)
                if (uuidTypeObject.ContainsKey(id))
                {
                    object found = uuidTypeObject[id];
                    if (found != null && found is Primitive)
                        return (Primitive)found;
                }
            // lock (GetPrimitiveLock)
            {
                Primitive prim;
                if (prims.TryGetValue(id, out prim))
                {
                    RegisterUUID(id, prim);
                    return prim;
                }
                //lock (GetSimulator().ObjectsAvatars)
                {
                    Primitive found = GetSimulator().ObjectsPrimitives.Find(delegate(Primitive prim0)
                    {
                        EnsureSelected(prim0.LocalID);
                        EnsureSelected(prim0.ParentID);
                        return (prim0.ID == id);
                    });
                    if (found == null) found = GetSimulator().ObjectsAvatars.Find(delegate(Avatar prim0)
                    {
                        RegisterUUID(prim0.ID, prim0);
                        if (prim0.ID == id)
                        {

                            //  AvatarCacheAdd(prim0.Name, prim0);
                            return true;
                        }
                        return false;
                    });
                    if (found != null)
                    {
                        lock (prims) prims.Add(found.LocalID, found.ID, found);
                        RegisterUUID(found.ID, found);
                    }
                    return found;
                }
            }
        }

        public Primitive GetPrimitive(uint id)
        {
            // lock (GetPrimitiveLock)
            {
                Primitive prim;
                if (prims.TryGetValue(id, out prim))
                {
                    return prim;
                }
                if (GetSimulator().ObjectsPrimitives.TryGetValue(id, out prim))
                {
                    RegisterUUID(prim.ID, prim);
                    EnsureSelected(prim.LocalID);
                    EnsureSelected(prim.ParentID);
                    lock (prims) prims.Add(prim.LocalID, prim.ID, prim);
                    return prim;
                };
                Avatar avatar;
                if (GetSimulator().ObjectsAvatars.TryGetValue(id, out avatar))
                {
                    RegisterUUID(avatar.ID, avatar);
                    lock (prims) prims.Add(avatar.LocalID, avatar.ID, avatar);
                    return avatar;
                };
                EnsureSelected(id);
                return null;
            }
        }

        //private void AvatarCacheAdd(string p, Avatar prim0)
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
            //	Console.WriteLine(eventName + " " + client.argsListString(args));
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
                prim = GetPrimitive(uuid);
                if (prim != null) return true;
            }
            if (name.ToLower().StartsWith("primid"))
            {
                if (name.Length > 6)
                {
                    String s = name.Substring(6);
                    if (uint.TryParse(s, out pickNum))
                    {
                        prim = GetPrimitive(pickNum);
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
            SimObject simObject = GetSimObject(prim);
            string str = simObject.ToString();
            str += " " + TheSimAvatar.DistanceVectorString(simObject);
            if (prim.Properties != null && prim.Properties.SalePrice != 0)
                str += " Sale: L" + prim.Properties.SalePrice;
            return str;// output(str);
        }

        public void describePrimToAI(Primitive prim)
        {
            if (true) return;
            if (prim is Avatar)
            {
                Avatar avatar = (Avatar)prim;
                describeAvatarToAI(avatar);
                return;
            }     
            //if (!primsKnown.Contains(prim))	return;
            BlockUntilProperties(prim);
            if (prim.Properties.Name != null)
            {
                //botenqueueLispTask("(on-prim-description '(" + prim.Properties.Name + ") '" + prim.Properties.Description + "' )");
                SendNewEvent("on-prim-dist", prim, Vector3.Distance(client.Self.SimPosition, prim.Position));
                SendNewEvent("on-prim-pos", prim, prim.Position);
                SendNewEvent("on-prim-description", prim, "" + prim.Properties.Description);
                //botoutput(prim.Properties.Name + ": " + prim.Properties.Description);
                //if (prim.Sound != UUID.Zero)
                //    botoutput("This object makes sound.");
                //if (prim.Properties.SalePrice != 0)
                //    botoutput("This object is for sale for L" + prim.Properties.SalePrice);
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

            //foreach (Primitive prim in prims.ForEach.Values)
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
            SimObject so = GetSimObject(prim);
            if (so != null) return "" + so;
            return "" + prim;
        }


        public string GetPrimTypeName(Primitive target)
        {
            if (target.PrimData.PCode == PCode.Prim)
                return target.PrimData.Type.ToString();
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
            avatar = GetAvatar(id);
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
            //botoutput(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
            output(avatar.Name + " is " + Vector3.Distance(client.Self.SimPosition, avatar.Position).ToString() + " distant.");
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
            //botoutput(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
            //botoutput(avatar.Name + " is " + Vector3.Distance(Client.Self.SimPosition, avatar.Position).ToString() + " distant.");
            SendNewEvent("on-avatar-dist", avatar, Vector3.Distance(client.Self.SimPosition, avatar.Position));
            SendNewEvent("on-avatar-pos", avatar, avatar.Position);
            SendNewEvent("on-avatar-description", avatar, avatar.GroupName);
            //  botenqueueLispTask("(on-avatar-posture (@\"" + avatar.Name + "\") (@\"" + verb + "\") )");

            /*
            if (avatar.ProfileProperties.BornOn != null)
                botoutput("Born on: " + avatar.ProfileProperties.BornOn);
            if (avatar.ProfileProperties.AboutText != null)
                botoutput("About their second life: " + avatar.ProfileProperties.AboutText);
            if (avatar.ProfileProperties.FirstLifeText != null)
                botoutput("About their first life: " + avatar.ProfileProperties.FirstLifeText);
            if (avatar.ProfileInterests.LanguagesText != null)
                botoutput("Languages spoken: " + avatar.ProfileInterests.LanguagesText);
            if (avatar.ProfileInterests.SkillsText != null)
                botoutput("Skills: " + avatar.ProfileInterests.SkillsText);
            if (avatar.ProfileInterests.WantToText != null)
                botoutput("Wants to: " + avatar.ProfileInterests.WantToText);
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
            SimObjects.Remove(GetSimObject(thePrim));
           // client.Inventory.RequestDeRezToInventory(thePrim.LocalID);
        }

        internal Primitive RequestMissingObject(uint localID)
        {
            if (localID == 0) return null;
            EnsureSelected(localID);
            client.Objects.RequestObject(GetSimulator(), localID);
            Thread.Sleep(1000);
            Primitive prim = GetPrimitive(localID);
            return prim;
        }
        public Simulator GetSimulator()
        {
            if (client.Network.CurrentSim != null) return client.Network.CurrentSim;
            if (client.Network.Simulators.Count == 0) return null;
            return client.Network.Simulators[0];
        }

        public void EnsureSelected(uint LocalID)
        {
            if (LocalID != 0)
                lock (primsAwaitingSelect)
                {
                    if (!primsSelectedOnce.Contains(LocalID))
                    {
                        primsSelectedOnce.Add(LocalID);
                        if (!primsAwaitingSelect.Contains(LocalID))
                        {
                            primsAwaitingSelect.Add(LocalID);
                            client.Objects.SelectObject(GetSimulator(), LocalID);
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
            foreach (UUID key in groups.Keys)
            {
                Group g = groups[key];
                RegisterUUID(key, g);
            }
            base.Groups_OnCurrentGroups(groups);
            //OnEvent("On-Current-Groups", paramNamesOnCurrentGroups, paramTypesOnCurrentGroups, groups);

        }

        Dictionary<int, Parcel> parcelLocalIds = new Dictionary<int, Parcel>();
        public override void Parcels_OnParcelProperties(Simulator simulator, Parcel parcel, ParcelResult result, int selectedPrims, int sequenceID, bool snapSelection)
        {
            parcelLocalIds[parcel.LocalID] = parcel;
            ExpectedObjects = parcel.SimWideTotalPrims;
            base.Parcels_OnParcelProperties(simulator, parcel, result, selectedPrims, sequenceID, snapSelection);
        }
        public override void Parcels_OnParcelDwell(UUID parcelID, int localID, float dwell)
        {
            Parcel p = parcelLocalIds[localID];
            p.SnapshotID = parcelID;
            RegisterUUID(parcelID, p);
            base.Parcels_OnParcelDwell(parcelID, localID, dwell);
        }

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
                    return SimWaypoint.Create(target,SimPaths);
                }
            }
            int consume = args.Length;
            while (consume > 0)
            {
                string s = String.Join(" ", args, 0, consume);
                Primitive prim;


                if (tryGetPrim(s, out prim))
                {
                    SimObject simObject = GetSimObject(prim);
                    if (simObject.IsRegionAttached())
                    {
                        argsUsed = consume;
                        return simObject;
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
    }
}
