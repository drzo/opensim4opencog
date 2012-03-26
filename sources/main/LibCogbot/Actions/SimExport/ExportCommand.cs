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
using ExportCommand = cogbot.Actions.SimExport.ImportCommand;

namespace cogbot.Actions.SimExport
{
    public partial class ExportCommand : Command, RegionMasterCommand
    {
        private readonly TaskQueueHandler slowlyExport = new TaskQueueHandler("slowlyExport", TimeSpan.FromMilliseconds(100),
                                                                    true);
        public readonly HashSet<SimObject> SIPrims = new HashSet<SimObject>();
        public static Dictionary<UUID, ErrorInfo> Errors = new Dictionary<UUID, ErrorInfo>();
        static public string dumpDir = "cog_export/workflow/";
        static public string assetDumpDir = "cog_export/assets/";
        static public string terrainDir = "cog_export/terrains/";
        static public string siminfoDir = "cog_export/siminfo/";
        static public bool IsExporting = false;
        static readonly Dictionary<string, UUID> inventoryHolder = new Dictionary<string, UUID>();
        public bool Incremental = true;
        static private readonly Dictionary<string, InventoryItem> lslScripts = new Dictionary<string, InventoryItem>();
        public int LocalFailures;
        public static readonly object fileWriterLock = new object();
        public bool showsStatus;
        public bool showPermsOnly;
        public bool skipPerms;
        public bool quietly = false;
        public bool showsMissingOnly;
        private bool verbosely;
        private bool taskobj;
        private bool forced;
        public int needFiles;
        private readonly HashSet<SimObject> successfullyExportedPrims = new HashSet<SimObject>();

        /// <summary>
        /// (string eMessage, UUID sourceId)
        /// </summary>
        public event Action<String,UUID> ListenForRelay;

        //private HashSet<string> arglist;
        static public bool UseBinarySerialization = false;

        Box3Fill seenObjectsAt = new Box3Fill(true);
        Box3Fill onlyObjectAt = new Box3Fill(true);

        public UUID inventoryHolderUUID
        {
            get { return FolderCalled("TaskInvHolder"); }
        }
        public UUID FolderCalled(string name)
        {
            UUID uuid;
            if (inventoryHolder.TryGetValue(name, out uuid)) return uuid;
            var rid = Client.Inventory.Store.RootFolder.UUID;
            List<InventoryBase> cnt = Client.Inventory.FolderContents(rid, Client.Self.AgentID, true, false, InventorySortOrder.ByDate,
                                                      5000);
            if (cnt == null)
            {
                cnt = Client.Inventory.FolderContents(rid, Client.Self.AgentID, true, false, InventorySortOrder.ByDate,
                                                      5000);
            }
            if (cnt == null) return UUID.Zero;

            foreach (var c in cnt)
            {
                if (c.Name == name)
                {
                    Client.Inventory.RequestFolderContents(c.UUID, Client.Self.AgentID, true, false, InventorySortOrder.ByDate);
                    return inventoryHolder[name] = c.UUID;

                }
            }
            return inventoryHolder[name] = Client.Inventory.CreateFolder(rid, name);
        }
        public UUID FolderCalled(string name, UUID parent)
        {
            UUID uuid;
            if (inventoryHolder.TryGetValue(name, out uuid)) return uuid;
            var rid = parent;
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
                    Client.Inventory.RequestFolderContents(c.UUID, Client.Self.AgentID, true, false, InventorySortOrder.ByDate);
                    return inventoryHolder[name] = c.UUID;

                }
            }
            return inventoryHolder[name] = Client.Inventory.CreateFolder(rid, name);
        }

        public ExportCommand(BotClient testClient)
        {
            Exporting = this;
            // testClient.Objects.ObjectPropertiesFamily += new EventHandler<ObjectPropertiesFamilyEventArgs>(Objects_OnObjectPropertiesFamily);

            //testClient.Objects.ObjectProperties += new EventHandler<ObjectPropertiesEventArgs>(Objects_OnObjectProperties);
            //testClient.Avatars.ViewerEffectPointAt += new EventHandler<ViewerEffectPointAtEventArgs>(Avatars_ViewerEffectPointAt);
            //SClient = SClient ?? testClient;
            if (!Directory.Exists(dumpDir)) Directory.CreateDirectory(dumpDir);
            if (!Directory.Exists(assetDumpDir)) Directory.CreateDirectory(assetDumpDir);
            if (!Directory.Exists(terrainDir)) Directory.CreateDirectory(terrainDir);
            if (!Directory.Exists(siminfoDir)) Directory.CreateDirectory(siminfoDir);

            testClient.Self.ChatFromSimulator += listen_for_relay;
            ListenForRelay += listen_forLinkset;
            ListenForRelay += listen_TaskInv;
            testClient.Assets.XferReceived += Asset_Xfer;
            testClient.Groups.GroupNamesReply += GroupNames;
            testClient.Avatars.UUIDNameReply += UserNames;
            Name = "simexport";
            Description = "Exports an object to an xml file. Usage: simexport exportPrim-spec directory";
            Category = CommandCategory.Objects;
            if (!Incremental) lock (fileWriterLock) PurgeExport();
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            Client.Self.Movement.Camera.Far = 1023;
            Client.Self.Movement.SendUpdate(true);
            Exporting = this;
            IsExporting = true;
            var CurSim = Client.Network.CurrentSim;
            RegionHandle = CurSim.Handle;
            onlyObjectAt.AddPoint(new Vector3(-256, -256, -256));
            onlyObjectAt.AddPoint(new Vector3(512, 512, 9024));
            haveBeenTo.AddPoint(TheSimAvatar.SimPosition);
            AttemptSitMover();
            WorldObjects.MaintainSimObjectInfoMap = false;
            SimObjectImpl.AffordinancesGuessSimObjectTypes = false;
            WorldObjects.IgnoreKillObjects = true;
            inventoryHolder.Clear();
            lslScripts.Clear();
            successfullyExportedPrims.Clear();
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
            ImportSettings arglist = new ImportSettings
                                         {
                                             CurSim = Client.Network.CurrentSim,
                                             GroupID = Client.Self.ActiveGroup
                                         };
            if (args[0]=="hhp")
            {
                args = new string[] { "taskobjs", "nobuf", "all", "spec", "f0a89a9f-3f33-b2aa-2829-eaeec3d08b82" };
            }
            foreach (string s in args)
            {
                arglist.Add(s);
            }
            if (arglist.Contains("error")) return WriteErrors(args);
            if (arglist.Contains("help")) return Success(hlp);
            if (args.Length > 1)
            {
                int specIndex = Array.IndexOf(args, "spec");
                if (specIndex > 0)
                {
                    nargs = Parser.SplitOff(args, specIndex + 1);
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
            }
            if (arglist.Contains("move"))
            {
                if (arglist.Contains("wps"))
                {
                    AddRegionWaypoints();
                }

                BeginMoving();
                GiveStatus();
                return Success("Began moving");
            }
            if (arglist.Contains("nomove"))
            {
                StopMoving();
                GiveStatus();
                return Success("Stopped moving");
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
            forced = arglist.Contains("force") || arglist.Contains("forced");
            if (arglist.Contains("nonincr")) Incremental = false;
            if (arglist.Contains("incr")) Incremental = true;
            bool fileOnly = false;
            lock (fileWriterLock)
            {
                if (arglist.Contains("clear"))
                {
                    KillAllUnpacked(WriteLine, true);
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
                lock (CompletedAssets) CompletedAssets.Clear();
                lock (PrimWaitingLinkset) PrimWaitingLinkset.Clear();
                lock (AllRelatedAssets) AllRelatedAssets.Clear();
                lock (PrimDepsAssets) PrimDepsAssets.Clear();
                lock (TaskAssetWaiting) TaskAssetWaiting.Clear();
                lock (CompletedTaskItem) CompletedTaskItem.Clear();
#if OBJECTUNPACKER
                lock (TasksRezed) TasksRezed.Clear();
#endif
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
            if (arglist.Contains("killtasks"))
            {
                KillAllUnpacked(WriteLine, true);
                return SuccessOrFailure();
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
            var canExport = new List<SimObject>();
            int objects = 0;
            if (arglist.Contains("terrain"))
            {
                SaveTerrainHeight(arglist);
                StartTerrainDownload(arglist);
            }
            if (arglist.Contains("parcel"))
            {
                SaveParcelInfoCommand(arglist);
            }
            bool primsAtAll = arglist.Contains("link") || arglist.Contains("task") || arglist.Contains("llsd") ||
                              arglist.Contains("taskobj") || arglist.Contains("all");
            bool wasShouldBeMoving = shouldBeMoving;
            if (primsAtAll)
            {
                shouldBeMoving = false;
            }
            if (arglist.Contains("nobuf"))
            {
                PSBuf.Clear();
            }
            PSBuf.AddRange(PS);
            lock (ImportCommand.Running.MustExport)
            {
                foreach (UUID id in LockInfo.CopyOf(ImportCommand.Running.MustExport))
                {
                    var o = WorldObjects.GetSimObjectFromUUID(id);
                    if (o != null) PSBuf.Add(o);
                }
            }

            foreach (var P in PSBuf)
            {
                if (!primsAtAll) break;
                if (IsComplete(P.ID, false, arglist)) continue;
                // skip attachments and avatars
                if (IsSkipped(P, arglist)) continue;
                if (!P.HasPrim)
                {
                    if (!quietly) Failure("Missing Prim: " + named(P));
                    continue;
                }
                if (P.RegionHandle != RegionHandle) continue;

                Vector3 sp;
                if (!P.TryGetSimPosition(out sp)) continue;
                if (!onlyObjectAt.IsInside(sp.X, sp.Y, sp.Z)) continue;

                objects++;
                string issues = P.MissingData;
                if (!string.IsNullOrEmpty(issues) && !arglist.Allows(issues, P))
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
                if (!string.IsNullOrEmpty(issues) && !arglist.Allows(issues, P))
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
                    if (!successfullyExportedPrims.Contains(P)) successfullyExportedPrims.Add(P);
                }
            }
            ExportRelatedAssets();
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
                    foreach (var pa in LockInfo.CopyOf(PrimWaitingLinkset))
                    {
                        var exportPrim = pa.Value.O;
                        if (verbosely) Failure("Awaiting Linkset " + named(exportPrim));
                        if (arglist.Contains("request"))
                        {
                            RequestLinksetInfo(Client, Path.Combine(dumpDir, exportPrim.ID.ToString()), exportPrim,
                                               WriteLine, arglist);
                        }
                    }
                }
            }

            if (arglist.Contains("task"))
            {
                lock (TaskAssetWaiting)
                {
                    foreach (var pa in LockInfo.CopyOf(TaskAssetWaiting))
                    {
                        UUID assetID = pa.Value.SourceItem.AssetUUID;
                        if (!CogbotHelpers.IsNullOrZero(assetID))
                        {
                            if (CompletedAssets.Contains(assetID))
                            {
                                int count = TaskAssetWaiting.Count;
                                TaskAssetWaiting.Remove(pa.Key);
                                if (TaskAssetWaiting.Count != count - 1)
                                {
                                    Failure("VERY BAD!");
                                }
                            }
                        }
                        if (verbosely) Failure("Awaiting TaskAsset " + pa.Value);
                        if (arglist.Contains("request"))
                        {
                            pa.Value.Request();
                        }
                    }
                }
            }
            ExportRelatedAssets();
            foreach (var assetID in LockInfo.CopyOf(ToDownloadAssets))
            {
                PingAssetCache(assetID);
            }

            if (arglist.Contains("dl"))
            {
                foreach (var assetID in LockInfo.CopyOf(ToDownloadAssets))
                {
                    AssetType assetType = assetTypeOf(assetID);
                    if (verbosely) Failure("Awaiting DL " + assetID + " " + assetType);
                    if (arglist.Contains("request"))
                    {
                        StartAssetDownload(null, assetID, assetType);
                    }
                }
            }
            if (arglist.Contains("user"))
            {
                RequestUsersAndGroups();
            }
            if (arglist.Contains("siprim"))
            {
                foreach (SimObject o in LockInfo.CopyOf(SIPrims))
                {
                    if (o.Prim.ParentID == 0)
                    {
                        Client.Inventory.RequestDeRezToInventory(o.LocalID, DeRezDestination.AgentInventoryCopy,
                        FolderCalled("UseSIForCompleteness"), UUID.Random());
                    }
                    else
                    {
                        Failure("Child SIPrim " + o);
                    }
                }
            }
            Success("Missing PrimData: " + missing);
            Success("Started XFERS " + ToDownloadCalledAssets.Count + " assets");
            shouldBeMoving = wasShouldBeMoving;
            GiveStatus();
            if (primsAtAll)
            {
                shouldBeMoving = wasShouldBeMoving;
            }
            return Success("Done");
        }


        private void GiveStatus()
        {
            Success("Awaiting Linkset of " + PrimWaitingLinkset.Count + " objects");
            Success("Awaiting TaskAsset of " + TaskAssetWaiting.Count + " assets");
            Success("CompletedTaskAsset: " + CompletedTaskItem.Count + " assets");
            Success("Awaiting DL of " + ToDownloadAssets.Count + " assets");
            Success("CompletedDL of " + CompletedAssets.Count + " assets");
            Success("Needed FILES " + needFiles + "");
            Success("seenObjectsAt = " + seenObjectsAt);
            Success("haveBeenTo = " + haveBeenTo);
            Success("maxHeigth = " + maxHeigth);
            Success("moveSleep = " + moveSleep);
            Success("shouldBeMoving = " + shouldBeMoving);
            Success("moveToPoints = " + moveToPoints.Count);
            Success("llsd = " + FileCount("*.llsd"));
            Success("task = " + FileCount("*.task"));
            Success("link = " + FileCount("*.link"));
            Success("repack = " + FileCount("*.repack"));
            Success("taskobj = " + FileCount("*.taskobj"));
            Success("PSBuf = " + PSBuf.Count);
        }

        static int FileCount(string mask)
        {
            return Directory.GetFiles(dumpDir, mask).Length;
        }


        private readonly HashSet<uint> RequiredForExportLocalIDs = new HashSet<uint>();
        public static ExportCommand Exporting;
        private ulong RegionHandle;
        private ListAsSet<SimObject> PSBuf = new ListAsSet<SimObject>();
        public ImportSettings settings;
        //private Simulator CurSim;


        private CmdResult CleanupAfterExport(UUID agent, OutputDelegate outputDelegate)
        {
            KillAllUnpacked(outputDelegate, false);
            return Execute(new[] { "reset" }, agent, outputDelegate);
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

        private void SilientFailure(string s, object[] args)
        {
            LocalFailures++;
            //Failure(DLRConsole.SafeFormat(s, args));
        }

        public void LocalFailure(string s, object[] args)
        {
            LocalFailures++;
            if (!quietly || verbosely) Failure(DLRConsole.SafeFormat(s, args));
        }

        private void listen_for_relay(object sender, ChatEventArgs e)
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
            ListenForRelay(eMessage, sourceId);
        }
        static public UUID[] GetUUIDs(string mustHave)
        {
            mustHave = mustHave.TrimEnd();
            if (string.IsNullOrEmpty(mustHave)) return new UUID[0];
            string[] mh = mustHave.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
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

        public bool PutItemToTaskInv(BotClient Client, SimObject exportPrim, string name)
        {
            InventoryItem found = GetInvItem(Client, name);
            if (found == null)
            {
                Failure("Cant find InvItem " + name);
                return false;
            }
            if (found.InventoryType == InventoryType.LSL)
            {
                Client.Inventory.CopyScriptToTask(exportPrim.LocalID, (InventoryItem)found, true);
                Client.Inventory.RequestSetScriptRunning(exportPrim.ID, found.AssetUUID, true);
            } else
            {
                Client.Inventory.UpdateTaskInventory(exportPrim.LocalID, (InventoryItem)found);
            }
            return true;
        }

        public InventoryItem GetInvItem(GridClient Client, string name)
        {
            if (lslScripts.ContainsKey(name)) return lslScripts[name];
            Client.Inventory.FolderContents(Client.Inventory.FindFolderForType(AssetType.LSLText), Client.Self.AgentID,
                                            false, true, InventorySortOrder.ByDate, 10000);
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
        public InventoryItem GetInvItem(GridClient Client, string name, UUID folderID)
        {
            if (lslScripts.ContainsKey(name)) return lslScripts[name];
            if (CogbotHelpers.IsNullOrZero(folderID))
            {
                return null;
            }
            foreach (var item in Client.Inventory.Store.GetContents(folderID))
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
                                            false, true, InventorySortOrder.ByDate, 10000);
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


        public void SlowlyDo(ThreadStart action)
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


        static internal object FromFile(string filename, bool binary)
        {
            if (binary)
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
            OSDMap primOSD = prim.GetTotalOSD();
            AddExportUser(primOSD["CreatorID"]);
            AddExportGroup(primOSD["GroupID"]);
            AddExportUser(primOSD["OwnerID"]);
            AddExportUser(primOSD["LastOwnerID"]);
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

        public static void LogError(SimObject o, string error)
        {
            lock(Errors)
            {
                var id = o.ID;
                ErrorInfo err;
                if (!Errors.TryGetValue(o.ID, out err))
                {
                    err = Errors[id] = new ErrorInfo(id) {Obj = o};
                }
                err.Add(error);
                Exporting.Failure(error);
            }
        }
        public static void LogError(UUID id, string error)
        {
            lock (Errors)
            {
                //var id = o.ID;
                ErrorInfo err;
                if (!Errors.TryGetValue(id, out err))
                {
                    err = Errors[id] = new ErrorInfo(id) {};
                }
                err.Add(error);
                Exporting.Failure(error);
            }
        }
        public CmdResult WriteErrors(string[] strings)
        {
            lock (Errors)
            {
                File.WriteAllText("ErrorLog.txt", "");
                var append = new StreamWriter(File.OpenWrite("ErrorLog.txt"));
                foreach (KeyValuePair<UUID, ErrorInfo> info in Errors)
                {
                    info.Value.AppendFile(append);
                }                
                append.Close();
                Errors.Clear();
            }
            return SuccessOrFailure();
        }
    }

    public class ErrorInfo
    {
        public UUID ObjectID;
        SimObject o;
        public ErrorInfo(UUID id)
        {
            ObjectID = id;
        }

        public SimObject Obj
        {
            get
            {
                if (o == null)
                {
                    o = WorldObjects.GetSimObjectFromUUID(ObjectID);
                }
                return o;
            }
            set
            {
                o = value;
            }
        }
       public List<string> errors = new List<string>();

       internal void Add(string error)
       {
           if (!errors.Contains(error)) errors.Add(error);
       }

       internal void AppendFile(string fileName)
       {
           if (errors.Count == 0) return;
           lock (ExportCommand.fileWriterLock)
           {
               var append = new StreamWriter(File.OpenWrite(fileName));
               AppendFile(append);
               append.Close();
           }
       }
       internal void AppendFile(TextWriter append)
       {
           if (errors.Count == 0) return;
           lock (ExportCommand.fileWriterLock)
           {
               if (!ImportCommand.MissingLLSD(ObjectID))
               {
                   var ptc = ImportCommand.Running.GetOldPrim(ObjectID);
                   append.WriteLine("OBJ: " + ptc + " " + Obj);
                   append.WriteLine("LOC: " + ptc.SimPosition);
               }
               else
               {
                   var o = Obj;
                   if (o != null)
                   {
                       append.WriteLine("OBJ: " + o);
                       append.WriteLine("LOC: " + o.SimPosition);
                   }
               }
               foreach (string error in errors)
               {
                   append.WriteLine("  " + error);
               }
           }
       }
    }
}
