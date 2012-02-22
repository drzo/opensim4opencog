using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using OpenMetaverse.Assets;

using MushDLR223.ScriptEngines;
using PathSystem3D.Mesher;

namespace cogbot.Actions.SimExport
{
    public class IHO
    {
        public InventoryItem I;
        public AssetManager.AssetReceivedCallback H;
        public SimObject O;
        public override string ToString()
        {
            return ExportCommand.ItemDesc(I,O);
        }
    }
    public class TIOBJ
    {
        public InventoryObject Task;
        public InventoryObject Inv;
        public SimObject WasInside;
        private SimObject _live;
        public SimObject LiveVersion
        {
            get
            {
                if (_live == null)
                {
                    DateTime timeOut = DateTime.Now + TimeSpan.FromSeconds(1);
                    while (DateTime.Now < timeOut)
                    {
                        _live = WorldObjects.GetSimObjectFromUUID(ObjectID);
                        if (_live != null) break;
                        Thread.Sleep(100);
                    }
                }
                return _live;
            }
            set
            {
                _live = value;
            }
        }
        public uint LocalID;
        public UUID ObjectID;
        public override string ToString()
        {
            return Task.Name + "(" + Task.AssetType + " " + Task.AssetUUID + " "
                   + ExportCommand.named(LiveVersion) + ")@" + ExportCommand.named(WasInside);
        }
    }
    public class SO
    {
        public string S;
        public string F;
        public SimObject O;
        public override string ToString()
        {
            return ExportCommand.named(O) + ":" + S;
        }
    }
    public class ExportCommand : Command, RegionMasterCommand
    {
        private bool DownloadingTerrain = false;
        private readonly TaskQueueHandler slowlyExport = new TaskQueueHandler("slowlyExport", TimeSpan.FromMilliseconds(100),
                                                                            true);
        /// <summary>
        /// Create a Synchronization event object
        /// </summary>
        private readonly AutoResetEvent terrainXferTimeout = new AutoResetEvent(false);

        public readonly HashSet<UUID> ToDownloadAssets = new HashSet<UUID>();
        public readonly Dictionary<UUID, AssetType> AllRelatedAssets = new Dictionary<UUID, AssetType>();
        public readonly HashSet<UUID> PrimDepsAssets = new HashSet<UUID>();
        public readonly Dictionary<UUID, SO> PrimWaitingLinkset = new Dictionary<UUID, SO>();
        public readonly Dictionary<InventoryItem, IHO> TaskAssetWaiting = new Dictionary<InventoryItem, IHO>();
        public readonly Dictionary<InventoryItem, TIOBJ> TasksRezed = new Dictionary<InventoryItem, TIOBJ>();
        public readonly HashSet<InventoryBase> CompletedTaskItem = new HashSet<InventoryBase>();
        public readonly Dictionary<UUID, InventoryItem> UUID2ITEM = new Dictionary<UUID, InventoryItem>();
        static public string dumpDir = "cog_export/objects/";
        static public string assetDumpDir = "cog_export/assets/";
        static public string terrainDir = "cog_export/terrain/";
        static public string siminfoDir = "cog_export/siminfo/";
        static public string terrainFileName = terrainDir + "terrain.r32";        

        public bool IsExporting = false;
        private readonly HashSet<SimObject> exportedPrims = new HashSet<SimObject>();
        static readonly Dictionary<string, UUID> inventoryHolder = new Dictionary<string, UUID>();
        public bool Incremental = true;
        static private readonly Dictionary<string, InventoryItem> lslScripts = new Dictionary<string, InventoryItem>();
        private int LocalFailures;
        static readonly object fileWriterLock = new object();
        public bool showsStatus;
        public bool showPermsOnly;
        public bool skipPerms;
        public bool quietly = false;
        private bool showsMissingOnly;
        private bool verbosely;
        private bool taskobj;
        private bool forced;
        private int needFiles;
        //private int TaskInvFailures = 0;
        private InventoryObject WaitingFolderObjects;
        private bool WaitingFolderObjectBool;
        private SimObject WaitingFolderSimObject;
        private HashSet<string> arglist;
        static public bool UseBinarySerialization = false;
        private Thread moverThread;
        private bool shouldBeMoving = false;
        private Random rnd = new Random(DateTime.Now.Millisecond);
        private int moveSleep = 60;
        Box3Fill haveBeenTo = new Box3Fill(true);
        Box3Fill seenObjectsAt = new Box3Fill(true);
        private float maxHeigth = 4000f;

        public UUID inventoryHolderUUID
        {
            get { return FolderCalled("TaskInvHolder"); }
        }
        public UUID FolderCalled(string name)
        {
            UUID uuid;
            if (inventoryHolder.TryGetValue(name, out uuid)) return uuid;
            var rid = Client.Inventory.Store.RootFolder.UUID;
            List<InventoryBase> cnt = null;
            while (cnt == null)
            {
                cnt = Client.Inventory.FolderContents(rid, Client.Self.AgentID, true, false, InventorySortOrder.ByDate,
                                                      10000);
            }

            foreach (var c in cnt)
            {
                if (c.Name == name)
                {
                    return inventoryHolder[name] = c.UUID;

                }
            }
            return inventoryHolder[name] = Client.Inventory.CreateFolder(rid, name);
        }

        public ExportCommand(BotClient testClient)
        {
            // testClient.Objects.ObjectPropertiesFamily += new EventHandler<ObjectPropertiesFamilyEventArgs>(Objects_OnObjectPropertiesFamily);

            //testClient.Objects.ObjectProperties += new EventHandler<ObjectPropertiesEventArgs>(Objects_OnObjectProperties);
            //testClient.Avatars.ViewerEffectPointAt += new EventHandler<ViewerEffectPointAtEventArgs>(Avatars_ViewerEffectPointAt);
            //SClient = SClient ?? testClient;
            testClient.Self.ChatFromSimulator += listen_forLinkset;
            testClient.Assets.XferReceived += Asset_Xfer;
            Name = "simexport";
            Description = "Exports an object to an xml file. Usage: simexport exportPrim-spec directory";
            Category = CommandCategory.Objects;
            if (!Incremental) lock (fileWriterLock) PurgeExport();
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
			WorldObjects.MaintainSimObjectInfoMap = false;
			SimObjectImpl.AffordinancesGuessSimObjectTypes = false;
			WorldObjects.IgnoreKillObjects = true;
            inventoryHolder.Clear();
            lslScripts.Clear();
            exportedPrims.Clear();
            const string hlp = @"
            
            Toplevel Directives

            // todo  = shows what must be done for export to be complete (suggest adding verbose)
            // perms  = shows what perms are going to be a problem (suggest adding verbose)
            // clear - clear the export dir
            // reset - reset the exporter state
            // cache - blow away asset cache
            // move - begin moving arround
            // nomove - stop moving arround

            // spec [spec] - do only prims meeting spec (default is 'spec $region') 
            // incr - do only do what is 'todo'
            // nonincr - do things 'todo' but also 'redo' things already done

            // noperms = dont skip things when perms might be a problem
            // quietly = terser output
            // verbose = more verbose
            // request = will rerequest missing things like textures
            // force = will allow unequal LLSD files - this should only be used as last resort

            // users - users files 
            // groups - groups files 
            // terrain - terrain files 
            // parcels - parcels files 
            // estate - estate files (TODO)
            // siminfo - estate files (TODO)
            // llsd - llsd files 
            // links - operate on linset
            // deps - operate on dependant assets
            // dl - operate on dependant downloads
            // tasks - save task files
            // taskobj - task objects
            // all = llsd tasks deps links (dl and taskobj not included)

           
            ";
            if (args == null || args.Length == 0) return Failure(hlp);
            string[] nargs = { "$region" };
            arglist = new HashSet<string>();
            foreach (string s in args)
            {
                arglist.Add(s.TrimEnd(new[] { 's' }).ToLower().TrimStart(new[] { '-' }));
            }
            if (arglist.Contains("help")) return Success(hlp);
            if (args.Length > 1)
            {
                if (args[0] == "spec")
                {
                    nargs = Parser.SplitOff(args, 1);
                }
                int fnd = Array.IndexOf(args, "move");
                if (fnd > -1 && (fnd + 1 < args.Length))
                {
                    int mv;
                    if (int.TryParse(args[fnd + 1], out mv))
                    {
                        moveSleep = mv;
                        if ((fnd + 2 < args.Length) && int.TryParse(args[fnd + 2], out mv))
                        {
                            maxHeigth = mv;
                        }
                    }
                }

                if (args[0] == "spec")
                {
                    nargs = Parser.SplitOff(args, 1);
                }
            }
            if (arglist.Contains("move"))
            {
                BeginMoving();
                GiveStatus();
                return Success("Began moving");
            }
            if (arglist.Contains("nomove"))
            {
                StopMoving();
                GiveStatus();
                return Success("Began moving");
            }
            quietly = arglist.Contains("quietly");
            if (arglist.Contains("prim"))
            {
                arglist.Add("llsd");
                arglist.Add("dep");
                arglist.Add("link");
            }
            if (arglist.Contains("all"))
            {
                arglist.Add("llsd");
                arglist.Add("task");
                arglist.Add("dep");
                arglist.Add("link");
            }

            needFiles = 0;
            taskobj = arglist.Contains("taskobj");
            forced = arglist.Contains("force");
            if (arglist.Contains("nonincr")) Incremental = false;
            if (arglist.Contains("incr")) Incremental = true;
            bool fileOnly = false;
            lock (fileWriterLock)
            {
                if (arglist.Contains("clear"))
                {
                    KillAllUnpacked(WriteLine);
                    PurgeExport();
                    arglist.Add("reset");
                }

                if (!Directory.Exists(dumpDir)) Directory.CreateDirectory(dumpDir);
                if (!Directory.Exists(assetDumpDir)) Directory.CreateDirectory(assetDumpDir);
                if (!Directory.Exists(terrainDir)) Directory.CreateDirectory(terrainDir);
                if (!Directory.Exists(siminfoDir)) Directory.CreateDirectory(siminfoDir);

                if (arglist.Contains("cache"))
                {
                    fileOnly = true;
                    PurgeCache();
                }
            }
            if (arglist.Contains("reset"))
            {
                slowlyExport.Clear();
                lock (ToDownloadAssets) ToDownloadAssets.Clear();
                lock (PrimWaitingLinkset) PrimWaitingLinkset.Clear();
                lock (AllRelatedAssets) AllRelatedAssets.Clear();
                lock (PrimDepsAssets) PrimDepsAssets.Clear();
                lock (TaskAssetWaiting) TaskAssetWaiting.Clear();
                lock (CompletedTaskItem) CompletedTaskItem.Clear();
                lock (TasksRezed) TasksRezed.Clear();
                GiveStatus();
                return Success("Reset SimExport State");
            }

            if (fileOnly)
            {
                GiveStatus();
                return Success("Manipulated filesystem");
            }

            if (arglist.Contains("cleanup"))
            {
                return CleanupAfterExport(fromAgentID, WriteLine);
            }
            IsExporting = true;
            FolderCalled("TaskInvHolder");
            //string file = args[args.Length - 1];
            int used;
            List<SimObject> PS = WorldSystem.GetPrimitives(nargs, out used);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            showsStatus = arglist.Contains("statu");
            showPermsOnly = arglist.Contains("perm");
            skipPerms = !arglist.Contains("obeyperm");
            showsMissingOnly = arglist.Contains("todo");
            if (showsMissingOnly) quietly = true;
            verbosely = arglist.Contains("verbose");
            if (verbosely && !arglist.Contains("quietly")) quietly = false;
            int missing = 0;
            var canExport =  new List<SimObject>();
            int objects = 0;
            if (arglist.Contains("terrain"))
            {
                SaveTerrainHeight();
                // Create a delegate which will be fired when the simulator receives our download request
                // Starts the actual transfer request
                var dt = new Thread(DownloadTerrain);
                dt.Name = "SimExport DownloadTerrain";
                dt.Start();
            }
            if (arglist.Contains("parcel"))
            {
                var dt = new Thread(SaveParcelInfo);
                dt.Name = "SimExport SaveParcelInfo";
                dt.Start();
            }
            bool primsAtAll = arglist.Contains("link") || arglist.Contains("task") || arglist.Contains("llsd") ||
                              arglist.Contains("taskobj") || arglist.Contains("all");

            foreach (var P in PS)
            {
                if (!primsAtAll) break;
                // skip attachments and avatars
                if (IsSkipped(P)) continue;
                if (!P.HasPrim)
                {
                    if (!quietly) Failure("Missing Prim: " + named(P));
                    continue;
                }
                if (P.RegionHandle != Client.Network.CurrentSim.Handle) continue;
                objects++;
                string issues = P.MissingData;
                if (!string.IsNullOrEmpty(issues))
                {
                    missing++;
                    if (!quietly) Failure("Issues " + issues + " " + named(P));
                    continue;
                }
                bool exportPossible =
                    checkPerms(Client, P, showPermsOnly ? (OutputDelegate)LocalFailure : SilientFailure, false) || skipPerms;
                if (exportPossible)
                {
                    SnagUsers(P);
                    canExport.Add(P);
                }
            }

            Success("Can export " + canExport.Count + " of " + objects);
            if (showPermsOnly) return Success("Shown perms");

            foreach (var P in canExport)
            {
                if (!primsAtAll) break;
                if (P is SimAvatar) continue;
                // skip attachments
                if (P.Parent is SimAvatar) continue;
                string issues = P.MissingData;
                if (!string.IsNullOrEmpty(issues))
                {
                    continue;
                }
                //if (exportedPrims.Contains(P)) continue;
                LocalFailures = 0;
                PrimDepsAssets.Clear();
                ExportPrim(Client, P, LocalFailure, arglist);
                if (P.IsRoot)
                {
                    float pSimPositionZ = P.SimPosition.Z;
                    if (pSimPositionZ > maxHeigth)
                    {
                        maxHeigth = pSimPositionZ + 10;
                    }
                    seenObjectsAt.AddPoint(P.SimPosition);
                }
                if (LocalFailures == 0)
                {
                    if (!exportedPrims.Contains(P)) exportedPrims.Add(P);
                }
            }
            if (showsStatus)
            {
                arglist.Add("link");
                arglist.Add("task");
                arglist.Add("llsd");
            }

            if (arglist.Contains("link"))
            {
                // lock (PrimWaitingLinkset)
                {
                    InventoryItem found = GetInvItem(Client, "LinksetSpeaker");
                    foreach (var pa in LockInfo.CopyOf(PrimWaitingLinkset))
                    {
                        var exportPrim = pa.Value.O;
                        if (verbosely) Failure("Awaiting Linkset " + named(exportPrim));
                        if (arglist.Contains("request"))
                        {
                            if (found == null)
                            {
                                ScanForLinksets(exportPrim);
                            }
                            else
                            {
                                PutItemToTaskInv(Client, exportPrim, "LinksetSpeaker");
                            }
                        }
                    }
                }
            }

            List<UUID> xferStarted = new List<UUID>();
            if (arglist.Contains("task"))
            {
                // lock (TaskAssetWaiting)
                {
                    foreach (var pa in LockInfo.CopyOf(TaskAssetWaiting))
                    {
                        InventoryItem item = pa.Key;
                        UUID assetID = item.AssetUUID;
                        AssetType assetType = item.AssetType;
                        if (verbosely) Failure("Awaiting TaskAsset " + pa.Value);
                        if (arglist.Contains("request"))
                        {
                            StartAssetDownload(xferStarted, assetID, assetType);
                        }
                    }
                }
            }

            var res = ExportRelatedAssets();
            foreach (var assetID in LockInfo.CopyOf(ToDownloadAssets))
            {
                AssetType assetType = assetTypeOf(assetID);
                byte[] b = Client.Assets.Cache.GetCachedAssetBytes(assetID, assetType);
                if (b != null)
                {
                    string file = Path.GetFileName(Client.Assets.Cache.FileName(assetID, assetType));
                    lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + file, b);
                    lock (ToDownloadAssets) ToDownloadAssets.Remove(assetID);
                }
            }
            if (arglist.Contains("dl"))
            {
                foreach (var assetID in LockInfo.CopyOf(ToDownloadAssets))
                {
                    AssetType assetType = assetTypeOf(assetID);
                    if (verbosely) Failure("Awaiting DL " + assetID + " " + assetType);
                    byte[] b = Client.Assets.Cache.GetCachedAssetBytes(assetID, assetType);
                    if (b != null)
                    {
                        string file = Path.GetFileName(Client.Assets.Cache.FileName(assetID, assetType));
                        lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + file, b);
                        lock (ToDownloadAssets) ToDownloadAssets.Remove(assetID);
                    }
                    else
                    {
                        if (arglist.Contains("request"))
                        {
                            StartAssetDownload(xferStarted, assetID, assetType);
                        }
                    }
                }
            }
            Success("Missing PrimData: " + missing);
            Success("Started XFERS " + xferStarted.Count + " assets");
            GiveStatus();
            return res;
        }

        private void GiveStatus()
        {
            Success("Awaiting Linkset of " + PrimWaitingLinkset.Count + " objects");
            Success("Awaiting TaskAsset of " + TaskAssetWaiting.Count + " assets");
            Success("CompletedTaskAsset: " + CompletedTaskItem.Count + " assets");
            Success("Awaiting DL of " + ToDownloadAssets.Count + " assets");
            Success("Needed FILES " + needFiles + "");
            Success("seenObjectsAt = " + seenObjectsAt);
            Success("haveBeenTo = " + haveBeenTo);
            Success("maxHeigth = " + maxHeigth);
            Success("moveSleep = " + (!shouldBeMoving ? 0 : moveSleep));
        }

        void ScanForLinksets(SimObject O)
        {
            if (O.Children == null) return;
            if (O.Children.Count == 0)
            {
                lock (fileWriterLock) File.WriteAllText(dumpDir + O.ID + ".link", "");
            }

            var lst = new SortedList<uint, UUID>();
            foreach (var o in LockInfo.CopyOf(O.Children))
            {
                lst.Add(o.LocalID, o.ID);
            }
            string contents = "" + O.ID;
            foreach (KeyValuePair<uint, UUID> uuid in lst)
            {
                contents += "," + uuid.Value;
            }
            lock (fileWriterLock) File.WriteAllText(dumpDir + O.ID + ".link", contents);
        }

        private void StopMoving()
        {
            shouldBeMoving = false;
        }
        void MoveOnce()
        {
            int MaxGotos = 0;
            Client.Self.Movement.Camera.Far = 256;
            Client.Self.Movement.Fly = true;

            Vector3[] moveToPoints = new Vector3[6400];
            int lowZ = (int)Client.Self.SimPosition.Z;
            for (int z = lowZ; z < lowZ + 1000; z += 256)
            {
                for (int x = 16; x < 256; x += 32)
                {
                    for (int y = 16; y < 256; y += 32)
                    {
                        moveToPoints[MaxGotos++] = new Vector3(x, y, z);
                    }
                }
            }
            for (int z = lowZ + 1400; z < (lowZ + 4000); z += 300)
            {
                for (int x = 16; x < 256; x += 64)
                {
                    for (int y = 16; y < 256; y += 64)
                    {
                        moveToPoints[MaxGotos++] = new Vector3(x, y, z);
                    }
                }
            }
            ulong handle = Client.Network.CurrentSim.Handle;
            while (true)
            {
                int currentLoc = 0;
                if (!shouldBeMoving)
                {
                    Thread.Sleep(1000);
                    continue;
                }
                currentLoc = rnd.Next(0, MaxGotos - 1);
                var moveto = moveToPoints[currentLoc];
                if (moveto == Vector3.Zero || Math.Abs(moveto.Z - Client.Self.SimPosition.Z) > 512 || moveto.Z > maxHeigth)
                {
                    Thread.Sleep(100);
                    continue;
                }
                var v3d = Client.Self.GlobalPosition;
                uint globalX, globalY;
                Utils.LongToUInts(handle, out globalX, out globalY);
                Vector3 pos = moveToPoints[currentLoc];

                ExpectedGotoV3D =
                    new Vector3d(
                        (double) globalX + (double) pos.X,
                        (double) globalY + (double) pos.Y,
                        (double) pos.Z);
                //Client.Self
                Success("Trying to get to " + pos);
                Client.Self.Movement.Camera.Position = pos;
                Client.Self.Movement.SendUpdate(true);
                Client.Self.Teleport(handle, pos, pos);
                Thread.Sleep(4000);
                Client.Self.AutoPilot(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z);
                Success("AutoPilot to get to " + pos);
                Thread.Sleep(moveSleep * 1000);
                haveBeenTo.AddPoint(Client.Self.SimPosition);
                var at3d = Client.Self.GlobalPosition;
                if (Vector3d.Distance(new Vector3d(ExpectedGotoV3D.X, ExpectedGotoV3D.Y, ExpectedGotoV3D.Z), new Vector3d(at3d.X, at3d.Y, at3d.Z)) < 30)
                {
                    moveToPoints[currentLoc] = Vector3.Zero;
                }
            }
        }
        private void BeginMoving()
        {
            if (moverThread == null || !moverThread.IsAlive)
            {
                moverThread = new Thread(MoveOnce);
                moverThread.Name = "SimExport Mover";
                moverThread.Start();
            }
            shouldBeMoving = true;
        }

        private void SnagUsers(SimObject o)
        {
            var prim = o.Prim;
            AddExportUser(prim.OwnerID);
            AddExportGroup(prim.GroupID);
            Primitive.ObjectProperties primProperties = prim.Properties ?? o.Properties;
            if (primProperties != null)
            {
                AddExportUser(primProperties.CreatorID);
                AddExportGroup(primProperties.GroupID);
            }
        }

        private void AddExportGroup(UUID groupID)
        {
            if (CogbotHelpers.IsNullOrZero(groupID)) return;
            ExportGroup.Add(groupID);
        }

        private void AddExportUser(UUID userID)
        {
            if (CogbotHelpers.IsNullOrZero(userID)) return;
            ExportUsers.Add(userID);
        }

        private readonly AutoResetEvent ParcelsDownloaded = new AutoResetEvent(false);
        private readonly HashSet<UUID> ExportUsers = new HashSet<UUID>();
        private readonly HashSet<UUID> ExportGroup = new HashSet<UUID>();
        private readonly HashSet<uint> RequiredForExportLocalIDs = new HashSet<uint>();
        private Vector3d ExpectedGotoV3D;

        private void SaveParcelInfo()
        {
            Simulator CurSim = Client.Network.CurrentSim;
            Client.Parcels.RequestAllSimParcels(CurSim);

            if (CurSim.IsParcelMapFull())
                ParcelsDownloaded.Set();

            if (ParcelsDownloaded.WaitOne(30000, false) && Client.Network.Connected)
            {
                Success(string.Format("Downloaded {0} Parcels in {1} " + Environment.NewLine, CurSim.Parcels.Count, CurSim.Name));
                IDictionary<int, Parcel> curSimParcels = null;
                lock (CurSim.Parcels.Dictionary)
                {
                    curSimParcels = LockInfo.CopyOf(CurSim.Parcels.Dictionary);
                }

                List<UUID> ParcelPrimOwners = new List<UUID>();
                List<uint> ParcelObjectIDS = new List<uint>();
                foreach (KeyValuePair<int, Parcel> simParcel in curSimParcels)
                {
                    var parcel = simParcel.Value;
                    int parcelID = parcel.LocalID;

                    Success(string.Format(
                                "Parcel[{0}]: Name: \"{1}\", Description: \"{2}\" ACLBlacklist Count: {3}, ACLWhiteList Count: {5} Traffic: {4}" +
                                Environment.NewLine,
                                parcel.LocalID, parcel.Name, parcel.Desc,
                                parcel.AccessBlackList.Count, parcel.Dwell,
                                parcel.AccessWhiteList.Count));
                    AddExportUser(parcel.OwnerID);
                    AddExportGroup(parcel.GroupID);

                    Success(Helpers.StructToString(parcel));
                    foreach (
                        ParcelManager.ParcelAccessEntry white in parcel.AccessWhiteList)
                    {
                        if (white.AgentID != UUID.Zero)
                            Success(string.Format(
                                        "\tAllowed Avatar {0}" + Environment.NewLine,
                                        white.AgentID));
                    }
                    foreach (
                        ParcelManager.ParcelAccessEntry black in parcel.AccessBlackList)
                    {
                        //    if(black.AgentID != UUID.Zero)
                        Success(string.Format("\t Banned Avatar {0}" + Environment.NewLine,
                                              black.AgentID));
                    }
                    {
                        AutoResetEvent parcelOwnerWait = new AutoResetEvent(false);

                        EventHandler<ParcelObjectOwnersReplyEventArgs> callback =
                            delegate(object sender, ParcelObjectOwnersReplyEventArgs e)
                                {
                                    for (int i = 0; i < e.PrimOwners.Count; i++)
                                    {
                                        ParcelManager.ParcelPrimOwners ownerse = e.PrimOwners[i];
                                        if(ownerse.IsGroupOwned)
                                        {
                                            AddExportGroup(ownerse.OwnerID);
                                        } else
                                        {
                                            AddExportUser(ownerse.OwnerID);
                                        }
                                        Success(string.Format("Owner: {0} Count: {1}" + Environment.NewLine,
                                                              ownerse.OwnerID, ownerse.Count));
                                        if (!CogbotHelpers.IsNullOrZero(ownerse.OwnerID))
                                        {
                                            ParcelPrimOwners.Add(ownerse.OwnerID);
                                        }
                                    }
                                    parcelOwnerWait.Set();
                                };

                        Client.Parcels.ParcelObjectOwnersReply += callback;
                        try
                        {
                            Client.Parcels.RequestObjectOwners(CurSim, parcelID);
                            if (!parcelOwnerWait.WaitOne(10000, false))
                            {
                                Failure("Timed out waiting for packet.");
                            }

                        }
                        finally
                        {
                            Client.Parcels.ParcelObjectOwnersReply -= callback;
                        }
                    }
                    foreach (UUID ownerUUID in ParcelPrimOwners)
                    {
                        AutoResetEvent wait = new AutoResetEvent(false);
                        EventHandler<ForceSelectObjectsReplyEventArgs> callback = delegate(object sender, ForceSelectObjectsReplyEventArgs e)
                        {
                            ParcelObjectIDS.AddRange(e.ObjectIDs);

                            for (int i = 0; i < e.ObjectIDs.Count; i++)
                            {
                                // Success(string.Format(e.ObjectIDs[i].ToString() + " "));
                                // counter++;
                            }

                            if (e.ObjectIDs.Count < 251)
                                wait.Set();
                        };


                        Client.Parcels.ForceSelectObjectsReply += callback;
                        Client.Parcels.RequestSelectObjects(parcelID, (ObjectReturnType)16, ownerUUID);
                        wait.WaitOne(10000);
                    }
                }
                foreach (uint u in ParcelObjectIDS)
                {
                    RequiredForExportLocalIDs.Add(u);
                }
                Success("Parcel LocalIDs=" + ParcelObjectIDS.Count + " ParcelOwners=" + ParcelPrimOwners.Count);
            }
            else
                Failure("Failed to retrieve information on all the simulator parcels");
        }

        public void SaveTerrainHeight()
        {
            var CurSim = Client.Network.CurrentSim;
            OSDMap simInfoMap = new OSDMap();
            // leave these out of serialization
            simInfoMap["f_ObjectsPrimitives"] = true;
            simInfoMap["f_ObjectsAvatars"] = true;
            simInfoMap["f_Client"] = true;
            simInfoMap["f_SharedData"] = true;
            simInfoMap["f_Caps"] = true;
            var exceptFor = new HashSet<object>() {typeof (IList), typeof (IDictionary), typeof (object)};
            OSD.AddObjectOSD0(CurSim.Stats, simInfoMap, typeof(Simulator.SimStats), exceptFor, true);
            OSD.AddObjectOSD0(CurSim.SharedData, simInfoMap, typeof(Simulator.SimPooledData), exceptFor, true);
            OSD.AddObjectOSD0(CurSim, simInfoMap, typeof(Simulator), exceptFor, true);
            string output = OSDParser.SerializeLLSDXmlString(simInfoMap);
            {
                lock (fileWriterLock) File.WriteAllText(terrainDir + "simInfoMap.llsd", output);
            }
            AddRelated(CurSim.TerrainBase0, AssetType.Texture);
            AddRelated(CurSim.TerrainBase1, AssetType.Texture);
            AddRelated(CurSim.TerrainBase2, AssetType.Texture);
            AddRelated(CurSim.TerrainBase3, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail0, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail1, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail2, AssetType.Texture);
            AddRelated(CurSim.TerrainDetail3, AssetType.Texture);
            AddExportUser(CurSim.SimOwner);
            var Terrain = CurSim.Terrain;
            if (Terrain == null)
            {
                Failure("Terrain missing completely");
                return;
            }
            Client.Grid.RequestMapItems(CurSim.Handle, GridItemType.LandForSale, GridLayerType.Terrain);
            var number = CurSim.Terrain.Length;
            var missing = 0;
            foreach (TerrainPatch patch in Terrain)
            {
                if (patch != null && patch.Data != null && patch.Data.Length > 0)
                {
                    continue;
                }
                missing++;
            }
            var terrainReady = (missing == 0);
            if (!terrainReady)
            {
                Failure("Terrain missing chunks/total=" + missing + "/" + number);
            }
            if (terrainReady || forced)
            {
                SaveToDisk(terrainDir + "terrain.patches", Terrain);
                if (!terrainReady)
                {
                    Failure("SaveTerrainHeight Saved but not ready");
                } else
                {
                    Success("SaveTerrainHeight Success");
                }
                return;
            }
            Failure("Unable to SaveTerrainHeight (use --force)");
        }
        private void DownloadTerrain()
        {
            if (DownloadingTerrain) return;
            DownloadingTerrain = true;
            EventHandler<InitiateDownloadEventArgs> initiateDownloadDelegate =
                delegate(object sender, InitiateDownloadEventArgs e)
                    {
                        Client.Assets.RequestAssetXfer(e.SimFileName, false, false, UUID.Zero, AssetType.Unknown, false);
                    };

            var timeout = TimeSpan.FromMinutes(2);

            // Subscribe to the event that will tell us the status of the download
            Client.Assets.XferReceived += Terrain_XferReceived;
            // subscribe to the event which tells us when the simulator has received our request
            Client.Assets.InitiateDownload += initiateDownloadDelegate;

            // configure request to tell the simulator to send us the file
            List<string> parameters = new List<string>();
            parameters.Add("download filename");
            parameters.Add(terrainFileName);
            // send the request
            Client.Estate.EstateOwnerMessage("terrain", parameters);

            // wait for (timeout) seconds for the request to complete (defaults 2 minutes)
            if (!terrainXferTimeout.WaitOne(timeout, false))
            {
                Failure("Timeout while waiting for terrain data");
            }

            // unsubscribe from events
            Client.Assets.InitiateDownload -= initiateDownloadDelegate;
            Client.Assets.XferReceived -= Terrain_XferReceived;
        }

        private void Terrain_XferReceived(object sender, XferReceivedEventArgs e)
        {
            if (e.Xfer.Filename != terrainFileName) return;
            if (e.Xfer.Success)
            {
                // set the result message
                Success(string.Format("Terrain file {0} ({1} bytes) downloaded successfully, written to {2}",
                                      e.Xfer.Filename, e.Xfer.Size, terrainFileName));

                // write the file to disk
                FileStream stream = new FileStream(terrainFileName, FileMode.Create);
                BinaryWriter w = new BinaryWriter(stream);
                w.Write(e.Xfer.AssetData);
                w.Close();

                // tell the application we've gotten the file
                terrainXferTimeout.Set();
            }
        }

        private CmdResult CleanupAfterExport(UUID agent, OutputDelegate outputDelegate)
        {
            KillAllUnpacked(outputDelegate);
            return Execute(new[] {"reset"}, agent, outputDelegate);
        }

        private bool IsSkipped(SimObject P)
        {
            if (P is SimAvatar) return true;
            if (P == null) return true;
            if (P.IsKilled) return true;
            SimObject parent = P.Parent;
            if (parent is SimAvatar) return true;
            // yes SL really does have links two deep! (called attachment linksets)
            if (parent != null && parent.Parent is SimAvatar)
            {
                return true;
            }
            return false;
        }

        public void PurgeCache()
        {
            string sfile = Path.GetDirectoryName(SimAsset.CFileName(UUID.Zero, AssetType.Texture));
            if (Directory.Exists(sfile))
            {
                Directory.Delete(sfile, true);
                Directory.CreateDirectory(sfile);
            }
        }

        public void PurgeExport()
        {
            if (Directory.Exists(dumpDir)) Directory.Delete(dumpDir, true);
            if (Directory.Exists(assetDumpDir)) Directory.Delete(assetDumpDir, true);
            if (Directory.Exists(terrainDir)) Directory.Delete(terrainDir, true);
            if (Directory.Exists(siminfoDir)) Directory.Delete(siminfoDir, true);
        }

        private void StartAssetDownload(List<UUID> xferStarted, UUID assetID, AssetType assetType)
        {
            if (xferStarted.Contains(assetID)) return;
            xferStarted.Add(assetID);
            // string filename = assetID + ".asset";
            // ulong xferID = Client.Assets.RequestAssetXfer(filename, false, true, assetID, assetType, false);
            Client.Assets.RequestAsset(assetID, assetType, true, Assets_OnReceived);
            if (assetType == AssetType.Texture)
            {
               // Client.Assets.RequestImage(assetID, ImageType.Normal, Assets_OnImageReceived);
                WorldSystem.TextureBytesForUUID(assetID);
            }
        }

        private void SilientFailure(string s, object[] args)
        {
            LocalFailures++;
            //Failure(DLRConsole.SafeFormat(s, args));
        }

        private void LocalFailure(string s, object[] args)
        {
            LocalFailures++;
            if (!quietly || verbosely) Failure(DLRConsole.SafeFormat(s, args));
        }

        public void ExportPrim(BotClient Client, SimObject exportPrim, OutputDelegate Failure, HashSet<string> arglist)
        {
            Simulator CurSim = exportPrim.GetSimulator();
            WorldObjects.EnsureSelected(exportPrim.LocalID, CurSim);
            string pathStem = Path.Combine(dumpDir, exportPrim.ID.ToString());

            string issues = exportPrim.MissingData;
            if (!string.IsNullOrEmpty(issues))
            {
                Failure("Missing " + issues + " " + named(exportPrim));
                return;
            }
            if (arglist.Contains("llsd")) SaveLLSD(Client, pathStem, exportPrim, Failure);
            if (exportPrim.IsRoot && (true || exportPrim.Children.Count > 0))
            {
                if (arglist.Contains("link")) SaveLinksetInfo(Client, pathStem, exportPrim, Failure);
                string exportFile = pathStem + ".link";
                //lock (fileWriterLock) if (File.Exists(exportFile))
                {
                    foreach (var c in exportPrim.Children)
                    {
                        ExportPrim(Client, c, Failure, arglist);
                    }
                }
            }
            if (arglist.Contains("task")) SaveTaskInv(Client, pathStem, exportPrim, Failure);
            if (!arglist.Contains("dep")) return;                
            AddRelatedTextures(exportPrim);
            SaveRelatedAssets(pathStem, exportPrim, Failure);
        }

        private void SaveRelatedAssets(string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".deps";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED DEPS for " + named(exportPrim));
                return;
            }
            if (PrimDepsAssets.Count == 0)
            {
                lock (fileWriterLock) File.WriteAllText(exportFile, "");
                return;
            }
            string content = "";
            foreach (UUID assetID in PrimDepsAssets)
            {
                content += assetTypeOf(assetID) + "," + assetID + "\n";
            }
            lock (fileWriterLock) File.WriteAllText(exportFile, content);
        }

        private AssetType assetTypeOf(UUID uuid)
        {
            AssetType assetType;
            AllRelatedAssets.TryGetValue(uuid, out assetType);
            return assetType;
        }

        void SaveLinksetInfo(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".link";
            if (Incremental || true) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            if (false && exportPrim.Children.Count == 0)
            {
                // so we dont do it again
                if (Incremental) lock (fileWriterLock) File.WriteAllText(exportFile, "");
                return;
            }
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED LINK for " + named(exportPrim));
                return;
            }
            bool canScript = checkPerms(Client, exportPrim, SilientFailure, true);
            if (!canScript)
            {
                ScanForLinksets(exportPrim);
                return;
            }
            lock (PrimWaitingLinkset)
            {
                if (PrimWaitingLinkset.ContainsKey(exportPrim.ID)) return;
                PrimWaitingLinkset.Add(exportPrim.ID, new SO { S = "", O = exportPrim, F = pathStem });
            }
            PutItemToTaskInv(Client, exportPrim, "LinksetSpeaker");
        }

        private void listen_forLinkset(object sender, ChatEventArgs e)
        {
            if (e.Type != ChatType.OwnerSay) return;
            UUID sourceId = e.SourceID;
            string fromWho = e.FromName;
            string eMessage = e.Message;
            if (fromWho == "RegionSay4200")
            {
                int findC = eMessage.IndexOf(":");
                string fu = eMessage.Substring(0, findC);
                UUID.TryParse(fu, out sourceId);
                eMessage = eMessage.Substring(findC + 1).TrimStart();
            }
            if (eMessage.StartsWith("INV-"))
            {
                return;
            }
            if (eMessage.StartsWith("MYKEY"))
            {
                return;
            }
            if (eMessage.StartsWith("RTI:"))
            {
                int popTo = eMessage.IndexOf("RTI:");
                eMessage = eMessage.Substring(4 + popTo).Trim();
                string[] lr = eMessage.Split(new[] {','});
                var objid = UUIDFactory.GetUUID(lr[0]);
                var assetID = UUIDFactory.GetUUID(lr[1]);
                var exportPrimID = UUIDFactory.GetUUID(lr[2]);
                string exportFile = assetDumpDir + assetID + ".object";
                lock (fileWriterLock) File.WriteAllText(exportFile, eMessage);

                var taskInv = WaitingFolderObjects;
                if (taskInv == null)
                {
                    //Error("cant find taskinv item");
                    return;
                }
                var itemID = taskInv.UUID;
                eMessage += "," + itemID;

                string exportFile2 = dumpDir + itemID + ".repack";
                lock (fileWriterLock) File.WriteAllText(exportFile2, eMessage);
                lock (TaskAssetWaiting)
                {
                    IHO ho;
                    TaskAssetWaiting.TryGetValue(taskInv, out ho);
                    TaskAssetWaiting.Remove(taskInv);
                    lock (CompletedTaskItem) CompletedTaskItem.Add(taskInv);
                }
                WaitingFolderSimObject = GetSimObjectFromUUID(objid);
                WaitingFolderObjectBool = false;
                return;
            }
            lock (PrimWaitingLinkset)
            {
                SO so;
                if (!PrimWaitingLinkset.TryGetValue(sourceId, out so))
                {
                    return;
                }
                if (so.S == "")
                {
                    if (!eMessage.StartsWith("Y,") || eMessage.StartsWith("\u2127"))
                    {
                        lock (PrimWaitingLinkset)
                        {
                            PrimWaitingLinkset.Remove(sourceId);
                        }
                        return;
                        throw new InvalidOperationException("wrong message came first " + so + " was " + eMessage);
                    }
                }
                else
                {
                    if (eMessage.StartsWith("Y,"))
                    {
                        lock (PrimWaitingLinkset)
                        {
                            PrimWaitingLinkset.Remove(sourceId);
                        }
                        return;
                        throw new InvalidOperationException("new message came to " + so + " was " + eMessage);
                    }
                }
                string soS = so.S;
                if (string.IsNullOrEmpty(soS))
                {
                    so.S = eMessage;
                }else
                {
                    so.S = soS + "," + eMessage;
                }
                if (eMessage.EndsWith(",Z"))
                {

                    var mustHave = so.S.Substring(2);
                    if (mustHave.StartsWith("1,"))
                    {
                        lock (fileWriterLock) File.WriteAllText(so.F + ".link", "");
                        return;
                    }
                    // get past count
                    int fc = mustHave.IndexOf(',');
                    mustHave = mustHave.Substring(fc+1);
                    // remove off ,Z
                    mustHave = mustHave.Substring(0, mustHave.Length - 2);
                    var childs = GetUUIDs(mustHave);
                    foreach (UUID list in childs)
                    {
                        if (GetSimObjectFromUUID(list) == null)
                        {
                            throw new InvalidOperationException("new message came to " + so + " was " + eMessage);
                        }
                    }
                    lock (fileWriterLock) File.WriteAllText(so.F + ".link", mustHave);
                    lock (PrimWaitingLinkset)
                    {
                        PrimWaitingLinkset.Remove(sourceId);
                    }
                }
            }
        }

        static public UUID[] GetUUIDs(string mustHave)
        {
            mustHave = mustHave.TrimEnd();
            if (string.IsNullOrEmpty(mustHave)) return new UUID[0];
            string[] mh = mustHave.Split(new[]{','},StringSplitOptions.RemoveEmptyEntries);
            UUID[] childs = new UUID[mh.Length];
            for (int i = 0; i < mh.Length; i++)
            {
                string mh1 = mh[i];
                try
                {
                    childs[i] = UUIDFactory.GetUUID(mh1);
                }
                catch (Exception)
                {
                    
                    throw;
                }
            }
            return childs;
        }

        public void Error(string s)
        {
            throw new NotImplementedException(s);
        }

        static internal SimObject GetSimObjectFromUUID(UUID objid)
        {
            DateTime timeOut = DateTime.Now + TimeSpan.FromSeconds(5);
            while (DateTime.Now < timeOut)
            {
                var O = WorldObjects.GetSimObjectFromUUID(objid);
                if (O != null) return O;
                Thread.Sleep(500);
            }
            return null;
        }

        private InventoryItem itemFor(UUID itemid)
        {
            InventoryItem item;
            if (UUID2ITEM.TryGetValue(itemid, out item)) return item;
            return null;
        }

        private void PutItemToTaskInv(BotClient Client, SimObject exportPrim, string name)
        {
            InventoryItem found = GetInvItem(Client, name);
            if (found.InventoryType == InventoryType.LSL)
            {
                Client.Inventory.CopyScriptToTask(exportPrim.LocalID, (InventoryItem) found, true);
                Client.Inventory.RequestSetScriptRunning(exportPrim.ID, found.AssetUUID, true);
            }
        }

        public InventoryItem GetInvItem(GridClient Client, string name)
        {
            if (lslScripts.ContainsKey(name)) return lslScripts[name];
            Client.Inventory.FolderContents(Client.Inventory.FindFolderForType(AssetType.LSLText), Client.Self.AgentID,
                                            false, true, InventorySortOrder.ByName, 10000);
            foreach (var item in Client.Inventory.Store.GetContents(Client.Inventory.FindFolderForType(AssetType.LSLText)))
            {
                if (!(item is InventoryItem)) continue;
                if (item.Name == name)
                {
                    return lslScripts[name] = (InventoryItem)item;
                }
            }
            return null;
        }
        static public InventoryItem GetInvItem(GridClient Client, string name, AssetType type)
        {
            if (lslScripts.ContainsKey(name)) return lslScripts[name];
            Client.Inventory.FolderContents(Client.Inventory.FindFolderForType(type), Client.Self.AgentID,
                                            false, true, InventorySortOrder.ByName, 10000);
            foreach (var item in Client.Inventory.Store.GetContents(Client.Inventory.FindFolderForType(type)))
            {
                if (item.Name == name)
                {
                    lslScripts[name] = item as InventoryItem;
                    break;
                }
            }
            return lslScripts[name];
        }

        private bool checkTaskPerm(SimObject exportPrim, InventoryItem item, BotClient Client, OutputDelegate Failure)
        {
            SimAvatarClient theAvatar = Client.TheSimAvatar;
            PermissionWho pw = theAvatar.EffectivePermissionWho(item.OwnerID, item.GroupID, item.GroupOwned);
            PermissionMask pm = CogbotHelpers.PermMaskForWho(pw, item.Permissions);
            bool cmt = Permissions.HasPermissions(pm, PermissionMask.Copy) ||
                       Permissions.HasPermissions(pm, PermissionMask.Modify) ||
                       Permissions.HasPermissions(pm, PermissionMask.Transfer);

            if (!cmt)
            {
                Failure("ItemPerms " + pm + " for " + pw + " on " + ItemDesc(item, exportPrim));
                return false;
            }
            return true;
        }

        public static bool checkPerms(BotClient Client, SimObject exportPrim, OutputDelegate Failure, bool mustModify)
        {
            if (exportPrim != null)
            {

                var Properties = exportPrim.Properties;
                if (Properties == null)
                {
                    Client.Objects.RequestObjectPropertiesFamily(exportPrim.GetSimulator(), exportPrim.ID, true);
                    Failure("No props yet for " + named(exportPrim));
                    return false;
                }
                // Check for export permission first
                //GotPermissions = false;
                //
                //if (!GotPermissions)
                // {
                //   Properties = exportPrim.Properties ?? new Primitive.ObjectProperties();
                //}
                //   GotPermissionsEvent.WaitOne(1000 * 10, false);
                if (Properties.OwnerID != Client.Self.AgentID &&
                    Properties.OwnerID != Client.MasterKey &&
                    Properties.GroupID != Client.Self.ActiveGroup)
                {
                    Failure("That object is owned by " + Properties.OwnerID + ", we don't have permission " +
                            "to export " + named(exportPrim));
                }

                SimAvatarClient theAvatar = Client.TheSimAvatar;
                PermissionWho pw = theAvatar.EffectivePermissionWho(exportPrim);
                PermissionMask pm = theAvatar.EffectivePermissionsMask(exportPrim);

                bool modify = Permissions.HasPermissions(pm, PermissionMask.Modify);

                bool cmt = Permissions.HasPermissions(pm, PermissionMask.Copy) ||
                           Permissions.HasPermissions(pm, PermissionMask.Modify) ||
                           Permissions.HasPermissions(pm, PermissionMask.Transfer);

                if (mustModify)
                {
                    if (!modify)
                    {
                        Failure("ObjPerms NOMODIFY " + pm + " for " + pw + " on " + named(exportPrim));
                        return false;                        
                    }
                }
                if (!cmt)
                {
                    Failure("ObjPerms " + pm + " for " + pw + " on " + named(exportPrim));
                    return false;
                }

                //List<SimObject> family = new List<SimObject>();
                //family.Add(exportPrim);
                //family.AddRange(exportPrim.Children);

                /*bool complete = RequestObjectProperties(family, 250, exportPrim.GetSimulator());
                exportedPrims.AddRange(family);

                if (!complete)
                {
                    Logger.Log("Warning: Unable to retrieve full properties for:", Helpers.LogLevel.Warning, Client);
                    foreach (UUID uuid in PrimsWaiting.Keys)
                        Logger.Log(uuid.ToString(), Helpers.LogLevel.Warning, Client);
                }
                 * return true;*/
            }
            return true;
        }

        public void SaveLLSD(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            if (exportPrim != null)
            {
                string exportFile = pathStem + ".llsd";
                if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
                needFiles++;
                if (showsMissingOnly)
                {
                    Failure("NEED LLSD for " + named(exportPrim));
                    return;
                }

                try
                {
                    List<string> skipTag = new List<string>() { "Tag" };
                    Primitive prim = exportPrim.Prim;
                    //prim = prim.Clone(); 
                    ToFile(prim, exportFile);
                    if (forced && !verbosely) return;
                    Primitive prim2 = FromFile(exportFile) as Primitive;
                    string memberwiseCompare = MemberwiseCompare(prim, prim2, skipTag);
                    if (!string.IsNullOrEmpty(memberwiseCompare))
                    {
                        string failre = "Error in LLSD: " + memberwiseCompare;
                        Failure(failre);
                        if (!forced)
                        {
                            File.Delete(exportFile);
                            return;
                            Error(failre);
                        }
                    }
                }
                catch (Exception e)
                {
                    File.Delete(exportFile);
                    Failure("Writing file " + exportFile + " caused " + e);
                }
            }
        }

        public static string named(SimObject prim)
        {
            string s = ("" + prim);
            int start = s.IndexOf("localID");
            int fp = s.IndexOf(")", start + 1);
            //if (fp < 64) fp = 0;
            if (fp > 0) return s.Substring(0, fp + 1);
            if (s.Length < 100) return s;
            return s.Substring(0, 100);

        }

        void SaveTaskInv(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".task";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED TASK for " + named(exportPrim));
                return;
            }
			bool canScript = checkPerms(Client, exportPrim, SilientFailure, false);
			if (!canScript)
			{
				bool mightHaveTaskInv = false;
				const PrimFlags maybeScriptsInside = PrimFlags.AllowInventoryDrop | PrimFlags.Scripted | PrimFlags.Touch;
				if ((maybeScriptsInside & exportPrim.Prim.Flags) != 0) {
					mightHaveTaskInv = true;
				} else {
					mightHaveTaskInv = (exportPrim.Prim.ClickAction == ClickAction.Sit);
				}
                if (!mightHaveTaskInv) return;
			}

			var ib = exportPrim.TaskInventory;
            if (ib == null)
            {
                Failure("NULL TaskInv for " + named(exportPrim));
				if (!canScript)
				{
					return;
				}
                return;
            }
            if (ib.Count == 0)
            {
                if (!exportPrim.InventoryEmpty)
                {
                    if (verbosely) Failure("ZEROITEM TaskInv for " + named(exportPrim));
                    //return;
                }
                lock (fileWriterLock) File.WriteAllText(exportFile, "");
                return;
            }
            if (ib.Count == 1)
            {
                if (ib[0].Name == "Contents" && ib[0] is InventoryFolder)
                {
                    lock (fileWriterLock) File.WriteAllText(exportFile, "");
                    return;
                }
            }
            string contents = "";
            List<SimObject> foundObject = new List<SimObject>();
            List<InventoryObject> folderObject = new List<InventoryObject>();

            string TaskInvFailures = "";
            foreach (InventoryBase b in ib)
            {
                bool missing;
                string was = SaveTaskItems(Client, exportPrim, b, Failure, folderObject, out missing);
                contents += was;
                if (missing)
                {
                    if (forced)
                    {
                        Failure("Missing but forced: " + was);
                    }
                    else
                    {
                        TaskInvFailures += was;
                    }
                }
            }
            if (folderObject.Count > 0 && !taskobj)
            {
                // dont save it since we are skipping task objects
                string ObjectFailures = "Run with 'taskobj' for:\n" + contents + " for " + named(exportPrim) + "\n";
                TaskInvFailures += ObjectFailures;
                Failure(ObjectFailures);
                return;
            }
            if (taskobj && folderObject.Count > 0)
            {
                UUID into = FolderCalled("TaskInvKilled") ?? Client.Inventory.FindFolderForType(AssetType.TrashFolder);
                bool placed = false;
                foreach (var oi in folderObject)
                {
                    WaitingFolderObjectBool = true;
                    WaitingFolderObjects = oi;
                    if (!placed)
                    {
                        placed = true;
                        PutItemToTaskInv(Client, exportPrim, "ObjectUnpacker");
                    }
                    else
                    {
                        Client.Self.Chat("" + exportPrim.ID.ToString().ToLower() + " RezNext ", 4201, ChatType.Normal);
                    }
                    WaitingFolderSimObject = null;
                    SimObject folderSimObject = null;
                    try
                    {
                        DateTime until = DateTime.Now + TimeSpan.FromSeconds(6);
                        while (DateTime.Now < until && WaitingFolderObjectBool)
                        {
                            Thread.Sleep(500);
                        }
                        if (WaitingFolderObjectBool || WaitingFolderSimObject == null)
                        {
                            string was = "!WaitingFolderObjectBool\n";
                            if (forced)
                            {
                                Failure("Missing but forced: " + was);
                            }
                            else
                            {
                                TaskInvFailures += was;
                            }
                            break;
                        }
                        folderSimObject = WaitingFolderSimObject;
                        Simulator CurSim = folderSimObject.GetSimulator();
                        PutItemToTaskInv(Client, folderSimObject, "LinksetSpeaker");
                        uint localID = exportPrim.LocalID;
                        var posChilds = new List<uint>();
                        for (int i = 1; i < 64; i++)
                        {
                            posChilds.Add((uint)(localID + i));
                        }
                        Client.Objects.RequestObjects(CurSim, posChilds);
                        Thread.Sleep(1000);
                        Client.WorldSystem.CatchUp(CurSim);
                        ExportPrim(Client, folderSimObject, Failure, arglist);
                    }
                    finally
                    {
                        if (folderSimObject != null)
                        {
                            Client.Inventory.RequestDeRezToInventory(folderSimObject.LocalID,
                                                                     DeRezDestination.TrashFolder, into,
                                                                     UUID.Random());
                        }
                        string exportFile2 = dumpDir + oi.UUID + ".repack";
                        File.Delete(exportFile2);                        
                    }
                }
                Client.Self.Chat("" + exportPrim.ID.ToString().ToLower() + " KillScript ", 4201, ChatType.Normal);
            }
            if (string.IsNullOrEmpty(TaskInvFailures))
            {
                lock (fileWriterLock) File.WriteAllText(exportFile, contents);
            }
            else
            {
                Failure("Skipping writting contents unil Items/Objects can be resolved:\n" + TaskInvFailures + " for " +
                        named(exportPrim));
            }
        }

        string SaveTaskItems(BotClient Client, SimObject exportPrim, InventoryBase b, OutputDelegate Failure, List<InventoryObject> folderObject, out bool missing)
        {
            string primName = " from " + named(exportPrim);
            //primName = "";
            InventoryFolder fldr = b as InventoryFolder;
            if (fldr != null)
            {
                if (fldr.Name == "Contents")
                {
                    missing = false;
                    return "";
                }
                //  Success("Folder " + fldr.Name + primName);

                //                List<InventoryBase> currentContents = Client.Inventory.GetContents(fldr);
                //              fldr
                missing = false;
                return b.UUID + ",Folder," + UUID.Zero + "," + fldr.Name + "\n";
            }
            InventoryItem item = b as InventoryItem;

            if (item == null)
            {
                string errorMsg = "" + b.UUID + ",ERROR," + b.UUID + "," + b.Name;                
                Failure("No an Item");
                missing = true;
                return errorMsg;
            }
            string itemEntry = b.UUID + "," + item.AssetType + "," + item.AssetUUID + "," + item.Name + "\n";
            UUID2ITEM[b.UUID] = item;
            UUID2ITEM[item.AssetUUID] = item;
            bool exportable = checkTaskPerm(exportPrim, item, Client, Failure);
            lock (TaskAssetWaiting)
                lock (CompletedTaskItem)
                {
                    if (CompletedTaskItem.Contains(item))
                    {
                        missing = false;
                        return itemEntry;
                    }
                }
            if (item.InventoryType == InventoryType.Object)
            {
                UnpackTaskObject(exportPrim, item as InventoryObject, folderObject, Client, Failure, primName);
                missing = false;
                return itemEntry;
            }
            return UnpackTaskItem(Client, exportPrim, (InventoryItem) b, Failure, itemEntry, out missing);
        }
        string UnpackTaskItem(BotClient Client, SimObject exportPrim, InventoryItem item, OutputDelegate Failure, string itemEntry, out bool missing)
        {
            if (CompletedTaskItem.Contains(item))
            {
                missing = false;
                return itemEntry;
            }            
            if (TaskAssetWaiting.ContainsKey(item))
            {
                missing = true;
                return itemEntry;
            }
            AddRelated(item.AssetUUID, item.AssetType);

            AssetManager.AssetReceivedCallback rec = (trans, asset) =>
                                                         {
                                                             if (trans.AssetID != item.AssetUUID) return;
                                                             if (!trans.Success)
                                                             {
                                                                 lock (TaskAssetWaiting)
                                                                 {
                                                                     IHO iho;
                                                                     if (TaskAssetWaiting.TryGetValue(item, out iho))
                                                                     {
                                                                         SlowlyDo(() =>
                                                                                  Client.Assets.RequestInventoryAsset(
                                                                                      item.AssetUUID, item.UUID,
                                                                                      exportPrim.ID, item.OwnerID,
                                                                                      item.AssetType, true, iho.H));
                                                                         return;
                                                                     }
                                                                 }
                                                             }
                                                             Assets_OnReceived(trans, asset);
                                                             //AddRelated(item.AssetUUID, item.AssetType);
                                                             lock (TaskAssetWaiting) TaskAssetWaiting.Remove(item);
                                                             lock (CompletedTaskItem) CompletedTaskItem.Add(item);
                                                         };
            IHO ho = new IHO { I = item, H = rec, O = exportPrim };

            lock (TaskAssetWaiting)
                TaskAssetWaiting.Add(item, ho);

            SlowlyDo(() =>
                     Client.Assets.RequestInventoryAsset(item.AssetUUID, item.UUID, exportPrim.ID, item.OwnerID,
                                                         item.AssetType, true, rec));
            FindOrCreateAsset(item.AssetUUID, item.AssetType);
            missing = true;
            return itemEntry;
        }

        private void UnpackTaskObject(SimObject exportPrim, InventoryObject taskInv, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName)
        {
            folderObject.Add(taskInv);
            if (!taskobj) return;
            string exportFile = assetDumpDir + taskInv.AssetUUID + ".object";
            string exportFile2 = dumpDir + taskInv.UUID + ".repack";
            //if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile) || File.Exists(exportFile2)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED OBJ for " + named(exportPrim));
                return;
            }
            IHO ho = new IHO { I = taskInv, H = null, O = exportPrim };

            lock (TaskAssetWaiting)
                TaskAssetWaiting.Add(taskInv, ho);
        }

        private void UnpackTaskObjectP2(SimObject exportPrim, InventoryObject taskInv, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName, List<SimObject> foundObject)
        {
            throw new NotImplementedException("UnpackTaskObjectP2");

            string exportFile = assetDumpDir + taskInv.AssetUUID + ".object";
            AutoResetEvent are0 = new AutoResetEvent(false);
            AutoResetEvent are2 = new AutoResetEvent(false);
            uint localID = 0;
            UUID objectID = UUID.Zero;
            UUID newItemID = UUID.Zero;
            AttachmentPoint origAttach = taskInv.AttachPoint;

            EventHandler<TaskItemReceivedEventArgs> created0 = (o, e) =>
                                                                   {
                                                                       if (e.AssetID != taskInv.AssetUUID) return;
                                                                       // if (inventoryHolderUUID != e.FolderID) return;
                                                                       newItemID = e.ItemID;
                                                                       are0.Set();
                                                                   };

            EventHandler<ObjectPropertiesEventArgs> created2 = (o, e) =>
                                                                   {
                                                                       if (e.Properties.ItemID != newItemID) return;
                                                                       objectID = e.Properties.ObjectID;
                                                                       are2.Set();
                                                                   };
            Client.Inventory.TaskItemReceived += created0;
            Client.Inventory.MoveTaskInventory(exportPrim.LocalID, taskInv.UUID, inventoryHolderUUID,
                                               exportPrim.GetSimulator());
            bool success = are0.WaitOne(5000);
            Client.Inventory.TaskItemReceived -= created0;
            if (!success)
            {
                Failure("Cant MOVE taskinv object " + taskInv.Name + primName);
                return;
            }
            var newItem = Client.Inventory.Store[newItemID] as InventoryObject;

            Client.Objects.ObjectProperties += created2;
            Client.Appearance.Attach(newItem, origAttach, true);

            success = are2.WaitOne(5000);
            Client.Objects.ObjectProperties -= created2;
            if (!success)
            {
                Failure("CANT ATTACH taskinv object " + taskInv.Name + primName);
                return;
            }

            SimObject O = GetSimObjectFromUUID(objectID);
            if (O == null)
            {
                Failure("Cant FIND taskinv object " + taskInv.Name + primName);
                return;
            }
            foundObject.Add(O);
            Primitive prim = O.Prim;
            localID = localID > 0 ? localID : O.LocalID;
            Simulator simulator = O.GetSimulator();
            Client.Objects.DropObject(simulator, localID);
            lock (fileWriterLock) File.WriteAllText(exportFile, "" + localID + "," + simulator.Handle + "," + objectID);
            var tiobj = new TIOBJ()
            {
                Task = taskInv,
                Inv = newItem,
                WasInside = exportPrim,
                ObjectID = objectID,
                LocalID = localID,
                LiveVersion = O,
            };
            TasksRezed[taskInv] = tiobj;
        }

        public void KillAllUnpacked(OutputDelegate Failures)
        {

            UUID into = FolderCalled("TaskInvKilled") ?? Client.Inventory.FindFolderForType(AssetType.TrashFolder);
            lock (fileWriterLock)
            {
                foreach (var file in Directory.GetFiles(dumpDir, "*.repack"))
                {
                    string[] csv = File.ReadAllText(file).Split(new[] {','});
                    var O = GetSimObjectFromUUID(UUIDFactory.GetUUID(csv[0]));
                    if (O == null)
                    {
                        Failure("Cant find object for file: " + file + " " + string.Join(",", csv));
                    }
                    else
                    {
                        Client.Inventory.RequestDeRezToInventory(O.LocalID, DeRezDestination.TrashFolder, into,
                                                                 UUID.Random());
                    }
                    File.Delete(file);

                }
            }
        }
        private void UnpackTaskObject2(SimObject exportPrim, InventoryItem item, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName, List<SimObject> foundObject)
        {
            throw new NotImplementedException("UnpackTaskObjectP2");
            InventoryObject io = item as InventoryObject;
            folderObject.Add(io);
            Vector3 pos = new Vector3(66, 66, 66);
            Quaternion quat = Quaternion.Identity;
            UUID queryID = UUID.Random();
            InventoryItemFlags f = io.ItemFlags;
            AutoResetEvent are0 = new AutoResetEvent(false);
            AutoResetEvent are1 = new AutoResetEvent(false);
            AutoResetEvent are2 = new AutoResetEvent(false);
            UUID found = UUID.Zero;
            UUID newfound = UUID.Zero;

            EventHandler<TaskItemReceivedEventArgs> created0 = (o, e) =>
            {
                if (e.AssetID != item.AssetUUID) return;
                if (inventoryHolderUUID != e.FolderID) return;
                newfound = e.ItemID;
                are0.Set();
            };
            EventHandler<ObjectDataBlockUpdateEventArgs> created1 = (o, e) =>
            {
                Primitive prim = e.Prim;
                if (false && prim.OwnerID != io.OwnerID)
                {
                    return;
                }
                float dist = Vector3.Distance(prim.Position, pos);
                if (dist > 1) return;
                //e.Properties.Description == io.Description;
                found = prim.ID;
                are1.Set();
            };

            EventHandler<ObjectPropertiesEventArgs> created2 = (o, e) =>
            {
                if (e.Properties.ObjectID != found) return;
                are2.Set();
            };
            Client.Objects.ObjectProperties += created2;
            Client.Objects.ObjectDataBlockUpdate += created1;
            Client.Inventory.TaskItemReceived += created0;
            Client.Inventory.MoveTaskInventory(exportPrim.LocalID, item.UUID, inventoryHolderUUID, exportPrim.GetSimulator());
            if (!are0.WaitOne(5000))
            {
                Failure("Cant MOVE taskinv object " + item.Name + primName);
            }
            var newItem = Client.Inventory.Store[newfound] as InventoryObject;

            Client.Appearance.Attach(newItem, AttachmentPoint.Mouth, true);
            //Client.Inventory.RequestRezFromInventory(exportPrim.GetSimulator(), exportPrim.ID, quat, pos, io, io.GroupID, queryID, true);
            if (!are1.WaitOne(5000))
            {
                Failure("Cant get taskinv object " + item.Name + primName);
            }
            Client.Objects.ObjectDataBlockUpdate -= created1;
            if (!are2.WaitOne(5000))
            {
                Failure("Cant get taskinv object " + item.Name + primName);
            }
            Client.Objects.ObjectProperties -= created2;
            var O = WorldObjects.GetSimObjectFromUUID(found);
            foundObject.Add(O);
        }


        private void SlowlyDo(ThreadStart action)
        {
            slowlyExport.Enqueue(action);
        }


        /// <summary>
        /// Loads in inventory cache file into the inventory structure. Note only valid to call after login has been successful.
        /// </summary>
        /// <param name="filename">Name of the cache file to load</param>
        /// <returns>The number of inventory items sucessfully reconstructed into the inventory node tree</returns>
        public object RestoreFromDisk(string filename)
        {

            lock (fileWriterLock)
            {
                try
                {
                    if (!File.Exists(filename))
                        return null;

                    using (Stream stream = File.Open(filename, FileMode.Open))
                    {
                        BinaryFormatter bformatter = new BinaryFormatter();
                        return bformatter.Deserialize(stream);
                    }
                }
                catch (Exception e)
                {
                    Logger.Log("Error accessing inventory cache file :" + e.Message, Helpers.LogLevel.Error);
                    return null;
                }
            }
        }


        /// <summary>
        /// Saves the current inventory structure to a cache file
        /// </summary>
        /// <param name="filename">Name of the cache file to save to</param>
        public void SaveToDisk(string filename, Object item)
        {
            lock (fileWriterLock)
            {
                try
                {
                    using (Stream stream = File.Open(filename, FileMode.Create))
                    {
                        BinaryFormatter bformatter = new BinaryFormatter();
                        lock (item)
                        {
                            bformatter.Serialize(stream, item);
                        }
                    }
                }
                catch (Exception e)
                {
                    Logger.Log("Error saving inventory cache to disk :" + e.Message, Helpers.LogLevel.Error);
                }
            }
        }

        public CmdResult ExportRelatedAssets()
        {
            // Create a list of all of the textures to download
            DownloadAssets(ToDownloadAssets);

            return Success("XML exported, downloading " + ToDownloadAssets.Count + " assets for " + exportedPrims.Count);
        }

        void DownloadAssets(IEnumerable<UUID> downloadList)
        {
            List<ImageRequest> textureRequests = new List<ImageRequest>();
            Dictionary<UUID, AssetType> otherRequests = new Dictionary<UUID, AssetType>();


            // Create a request list from all of the images
            lock (downloadList)
            {
                foreach (var asset in downloadList)
                {
                    if (assetTypeOf(asset) == AssetType.Texture)
                    {
                        textureRequests.Add(new ImageRequest(asset, ImageType.Normal, 1013000.0f, 0));
                    }
                    else
                    {
                        otherRequests.Add(asset, assetTypeOf(asset));
                    }
                }
            }

            // Download all of the textures in the export list
            foreach (ImageRequest request in textureRequests)
            {
                SlowlyDo(() => Client.Assets.RequestImage(request.ImageID, request.Type, Assets_OnImageReceived));
                //SlowlyDo(() => Client.Assets.RequestAsset(request.ImageID, AssetType.Texture, true, Assets_OnReceived));
            }

            foreach (KeyValuePair<UUID, AssetType> asset in otherRequests)
            {
                if (asset.Value != AssetType.Texture)
                {
                    SlowlyDo(() => Client.Assets.RequestAsset(asset.Key, asset.Value, true, Assets_OnReceived));
                }
            }
        }

        void AddRelatedTextures(SimObject simObject)
        {
            var exportPrim = simObject.Prim;
            lock (ToDownloadAssets)
            {
                if (exportPrim.Textures!=null)
                {
                    Primitive.TextureEntryFace deftexture = exportPrim.Textures.DefaultTexture;
                    if (deftexture!=null)
                    {
                        AddRelated(deftexture.TextureID, AssetType.Texture);
                    }
                    for (int j = 0; j < exportPrim.Textures.FaceTextures.Length; j++)
                    {
                        if (exportPrim.Textures.FaceTextures[j] != null)
                        {
                            AddRelated(exportPrim.Textures.FaceTextures[j].TextureID, AssetType.Texture);
                        }
                    }
                }

                if (exportPrim.Sculpt != null)
                {
                    AddRelated(exportPrim.Sculpt.SculptTexture, AssetType.Texture);
                }
                AddRelatedTexturesFromProps(simObject);
            }
        }

        void AddRelatedTexturesFromProps(SimObject simObject)
        {
            UUID[] textureIDs = simObject.Properties.TextureIDs;
            if (textureIDs == null || textureIDs.Length == 0) return;
            foreach (var c in textureIDs)
            {
                AddRelated(c, AssetType.Texture);
            }
        }

        public void AddRelated(UUID assetID, AssetType assetType)
        {
            if (assetID == null || assetID == UUID.Zero || assetID == Primitive.TextureEntry.WHITE_TEXTURE) return;
            FindOrCreateAsset(assetID, assetType);
            //WorldObjects.GridMaster.EnqueueRequestAsset(assetID, assetType, true);
            lock (PrimDepsAssets)
                if (!PrimDepsAssets.Contains(assetID))
                {
                    PrimDepsAssets.Add(assetID);
                }
            lock (AllRelatedAssets)
                if (!AllRelatedAssets.ContainsKey(assetID))
                {
                    AllRelatedAssets.Add(assetID, assetType);
                }
            lock (ToDownloadAssets)
                if (!ToDownloadAssets.Contains(assetID))
                {
                    ToDownloadAssets.Add(assetID);
                }
        }

        private void FindOrCreateAsset(UUID uuid, AssetType type)
        {
            if (type != AssetType.Object) SimAssetStore.FindOrCreateAsset(uuid, type);
        }

        /*

        private bool RequestObjectProperties(IList<SimObject> objects, int msPerRequest, Simulator sim)
        {
            // Create an array of the local IDs of all the prims we are requesting properties for
            uint[] localids = new uint[objects.Count];

            lock (PrimsWaiting)
            {
                PrimsWaiting.Clear();

                for (int i = 0; i < objects.Count; ++i)
                {
                    localids[i] = objects[i].LocalID;
                    PrimsWaiting.Add(objects[i].ID, objects[i].Prim);
                }
            }

            Client.Objects.SelectObjects(sim, localids);

            return AllPropertiesReceived.WaitOne(2000 + msPerRequest * objects.Count, false);
        }
        */
        private void Assets_OnImageReceived(TextureRequestState state, AssetTexture asset)
        {

            lock (ToDownloadAssets) if (state == TextureRequestState.Finished && ToDownloadAssets.Contains(asset.AssetID))
                {
                    lock (ToDownloadAssets)
                        ToDownloadAssets.Remove(asset.AssetID);



                    if (state == TextureRequestState.Finished)
                    {
                        string sfile = Path.GetFileName(SimAsset.CFileName(asset.AssetID, asset.AssetType));
                        try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + sfile, asset.AssetData); }
                        catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                        return;

                        if (asset.Decode())
                        {
                            try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + asset.AssetID + ".tga", asset.Image.ExportTGA()); }
                            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                        }
                        else
                        {
                            try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + sfile, asset.AssetData); }
                            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                            return;
                        }

                        Logger.Log("Finished downloading image " + asset.AssetID, Helpers.LogLevel.Info, Client);
                    }
                    else
                    {
                        Logger.Log("Failed to download image " + asset.AssetID + ":" + state, Helpers.LogLevel.Warning, Client);
                    }
                }
        }

        private void Asset_Xfer(object sender, XferReceivedEventArgs e)
        {
            var assetID = e.Xfer.AssetID;
            var assetType = e.Xfer.AssetType;
            lock (ToDownloadAssets) if (e.Xfer.Success && ToDownloadAssets.Contains(assetID))
                {
                    lock (ToDownloadAssets)
                        ToDownloadAssets.Remove(assetID);
                    string sfile = SimAsset.CFileName(assetID, assetType);
                    try
                    {
                        lock (fileWriterLock)
                            File.WriteAllBytes(assetDumpDir + Path.GetFileName(sfile), e.Xfer.AssetData);
                    }
                    catch (Exception ex)
                    {
                        Logger.Log(ex.Message, Helpers.LogLevel.Error);
                    }
                }
        }

        void Assets_OnReceived(AssetDownload transfer, Asset asset)
        {
            lock (ToDownloadAssets) if (transfer.Success && ToDownloadAssets.Contains(asset.AssetID))
                {
                    lock (ToDownloadAssets)
                        ToDownloadAssets.Remove(asset.AssetID);
                    string sfile = SimAsset.CFileName(asset.AssetID, asset.AssetType);
                    try
                    {
                        var data = asset.AssetData;
                        if (data != null && data.Length > 1)
                        {
                            lock (fileWriterLock)
                                File.WriteAllBytes(assetDumpDir + Path.GetFileName(sfile), asset.AssetData);
                            return;
                        }
                    }
                    catch (Exception ex)
                    {
                        Logger.Log(ex.Message, Helpers.LogLevel.Error);
                    }
                }
        }

        /*
        void Objects_OnObjectPropertiesFamily(object sender, ObjectPropertiesFamilyEventArgs e)
        {            
            Properties.SetFamilyProperties(e.Properties);
            GotPermissions = true;
            GotPermissionsEvent.Set();
        }

        void Objects_OnObjectProperties(object sender, ObjectPropertiesEventArgs e)
        {
            lock (PrimsWaiting)
            {
                PrimsWaiting.Remove(e.Properties.ObjectID);

                if (PrimsWaiting.Count == 0)
                    AllPropertiesReceived.Set();
            }
        }*/

        static public string ItemDesc(InventoryItem I, SimObject O)
        {
            return I.Name + "(" + I.AssetType + " " + I.AssetUUID + ")@" + named(O);
        }

        static internal object FromFile(string filename)
        {
            if (ExportCommand.UseBinarySerialization)
            {
                lock (fileWriterLock)
                {
                    try
                    {
                        if (!File.Exists(filename))
                            return null;

                        using (Stream stream = File.Open(filename, FileMode.Open))
                        {
                            BinaryFormatter bformatter = new BinaryFormatter();

                            while (stream.Position < stream.Length)
                            {
                                return bformatter.Deserialize(stream);
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        Logger.Log("Error accessing object cache file :" + e.Message, Helpers.LogLevel.Error);
                        return null;
                    }
                }
            }
            return Primitive.FromTotalOSD(OSDParser.DeserializeLLSDXml(File.ReadAllText(filename)));
        }

        public void ToFile(Primitive prim, string exportFile)
        {

            if (UseBinarySerialization)
            {
                SaveToDisk(exportFile, prim);
                return;
            }
            OSD primOSD = prim.GetTotalOSD();
            string output = OSDParser.SerializeLLSDXmlString(primOSD);
            {
                lock (fileWriterLock) File.WriteAllText(exportFile, output);
            }
        }


        static public string MemberwiseCompare(object left, object right, ICollection<string> skipped)
        {
            if (Object.ReferenceEquals(left, right))
                return "";

            if (right == null) return "Right is Null";
            // Should it be ok for left to be null right now?
            if (left == null)
            {
                if (!skipped.Contains("NULL")) return "Left is null";
                return "";
            }

            Type ltype = left.GetType();
            Type rtype = right.GetType();
            if (ltype != rtype)
                return "Different Types (" + ltype + "!=" + rtype + ")";

            if (left as ValueType != null)
            {
                // do a field comparison, or use the override if Equals is implemented:
                return left.Equals(right) ? "" : "VTNotEqual: (" + left + "!=" + right + ")";
            }

            // check for override:
            if (false && ltype != typeof(object)
                && ltype == ltype.GetMethod("Equals").DeclaringType)
            {
                // the Equals method is overridden, use it:
                return left.Equals(right) ? "" : "NTNotEqual: (" + left + "!=" + right + ")";
            }

            // all Arrays, Lists, IEnumerable<> etc implement IEnumerable
            if (left as IEnumerable != null)
            {
                IEnumerable renum = right as IEnumerable;
                if (renum == null)
                {
                    return "Right is not enumerable";
                }
                IEnumerator rightEnumerator = renum.GetEnumerator();
                rightEnumerator.Reset();
                foreach (object leftItem in left as IEnumerable)
                {
                    // unequal amount of items
                    if (!rightEnumerator.MoveNext())
                        return "differnt size enumerations";
                    else
                    {
                        string memberwiseCompare = MemberwiseCompare(leftItem, rightEnumerator.Current, skipped);
                        if (!string.IsNullOrEmpty(memberwiseCompare))
                            return "enumers=" + memberwiseCompare;
                    }
                }
            }
            else
            {
                var memberwiseCompare12 = "";
                // compare each property
                foreach (PropertyInfo info in ltype.GetProperties(
                    BindingFlags.Public |
                    BindingFlags.NonPublic |
                    BindingFlags.Instance |
                    BindingFlags.GetProperty))
                {
                    if (skipped.Contains(info.Name)) continue;
                    if (info.IsDefined(typeof(NonSerializedAttribute), true)) continue;
                   
                    // TODO: need to special-case indexable properties
                    string memberwiseCompare1 = MemberwiseCompare(info.GetValue(left, null), info.GetValue(right, null),
                                                                  skipped);
                    if (!string.IsNullOrEmpty(memberwiseCompare1))
                        memberwiseCompare12 += info.DeclaringType + "." + info.Name + "=" + memberwiseCompare1 + "\n";
                }

                // compare each field
                foreach (FieldInfo info in ltype.GetFields(
                    BindingFlags.GetField |
                    BindingFlags.NonPublic |
                    BindingFlags.Public |
                    BindingFlags.Instance))
                {
                    if (skipped.Contains(info.Name)) continue;
                    if (info.IsDefined(typeof(NonSerializedAttribute), true)) continue;
                    string memberwiseCompare2 = MemberwiseCompare(info.GetValue(left), info.GetValue(right), skipped);
                    if (!string.IsNullOrEmpty(memberwiseCompare2))
                        memberwiseCompare12 += info.DeclaringType + "." + info.Name + "=" + memberwiseCompare2 + "\n";
                }
                return memberwiseCompare12;
            }
            return "";
        }

    }
}
