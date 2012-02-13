using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using OpenMetaverse.Assets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public class IHO
    {
        public InventoryItem I;
        public AssetManager.AssetReceivedCallback H;
        public SimObject O;
        public override string ToString()
        {
            return I.Name + "(" + I.AssetType + " " + I.AssetUUID + ")@" + ExportCommand.named(O);
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
        private static TaskQueueHandler slowlyExport = new TaskQueueHandler("slowlyExport", TimeSpan.FromMilliseconds(100),
                                                                            true);

        public static readonly Dictionary<uint, ulong> OnExitKill = new Dictionary<uint, ulong>();
        public static readonly List<UUID> ToDownloadAssets = new List<UUID>();
        public static readonly Dictionary<UUID, AssetType> AllRelatedAssets = new Dictionary<UUID, AssetType>();
        public static readonly List<UUID> PrimDepsAssets = new List<UUID>();
        public static readonly Dictionary<UUID, SO> PrimWaitingLinkset = new Dictionary<UUID, SO>();
        public static readonly Dictionary<InventoryItem, IHO> TaskAssetWaiting = new Dictionary<InventoryItem, IHO>();
        public static readonly Dictionary<InventoryItem, TIOBJ> TasksRezed = new Dictionary<InventoryItem, TIOBJ>();
        public static readonly List<InventoryBase> CompletedTaskItem = new List<InventoryBase>();
        public static string dumpDir = "cog_export/objects/";
        public static string assetDumpDir = "cog_export/assets/";
        public static bool IsExporting = false;
        static private List<SimObject> exportedPrims = new List<SimObject>();
        static Dictionary<string, UUID> inventoryHolder = new Dictionary<string, UUID>();
        public static bool Incremental = true;
        static InventoryItem linkSpeaker = null;
        private int LocalFailures;
        static readonly object fileWriterLock = new object();
        private static BotClient SClient;
        public static bool showsStatus;
        public static bool showPermsOnly;
        public static bool skipPerms;
        public static bool quietly = false;
        private static bool showsMissingOnly;
        static private bool verbosely;
        private static bool taskobj;
        private static int needFiles;

        static public UUID inventoryHolderUUID
        {
            get { return FolderCalled("TaskInvHolder"); }
        }
        static public UUID FolderCalled(string name)
        {
            UUID uuid;
            if (inventoryHolder.TryGetValue(name, out uuid)) return uuid;
            var rid = SClient.Inventory.Store.RootFolder.UUID;
            List<InventoryBase> cnt = null;
            while (cnt == null)
            {
                cnt = SClient.Inventory.FolderContents(rid, SClient.Self.AgentID, true, false, InventorySortOrder.ByDate,
                                                      10000);
            }

            foreach (var c in cnt)
            {
                if (c.Name == name)
                {
                    return inventoryHolder[name] = c.UUID;

                }
            }
            return inventoryHolder[name] = SClient.Inventory.CreateFolder(rid, name);
        }

        public ExportCommand(BotClient testClient)
        {
            // testClient.Objects.ObjectPropertiesFamily += new EventHandler<ObjectPropertiesFamilyEventArgs>(Objects_OnObjectPropertiesFamily);

            //testClient.Objects.ObjectProperties += new EventHandler<ObjectPropertiesEventArgs>(Objects_OnObjectProperties);
            //testClient.Avatars.ViewerEffectPointAt += new EventHandler<ViewerEffectPointAtEventArgs>(Avatars_ViewerEffectPointAt);
            SClient = SClient ?? testClient;
            testClient.Self.ChatFromSimulator += listen_forLinkset;
            testClient.Assets.XferReceived += Asset_Xfer;
            Name = "simexport";
            Description = "Exports an object to an xml file. Usage: simexport exportPrim-spec directory";
            Category = CommandCategory.Objects;
            if (!Incremental) lock (fileWriterLock) PurgeExport();
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            const string hlp = @"
            
            Toplevel Directives

            // todo  = shows what must be done for export to be complete (suggest adding verbose)
            // perms  = shows what perms are going to be a problem (suggest adding verbose)
            // clear - clear the export dir
            // reset - reset the exporter state
            // cache - blow away asset cache

            // prims [spec] - do only prims meeting spec (default is prims $region) 
            // incr - do only do what is 'todo'
            // nonincr - do things 'todo' but also 'redo' things already done

            // noperms = dont skip things when perms might be a problem
            // quietly = terser output
            // verbose = more verbose

            // llsd - save llsd files
            // tasks - save task files
            // deps - operate on dependant assets
            // links - operate on linset
            // dl - operate on dependant downloads
            // taskobj - task objects
            // all = llsd tasks deps links (dl and taskobj not included)

           
            ";
            if (args == null || args.Length == 0) return Failure(hlp);
            string[] nargs = { "$region" };
            List<string> arglist = new List<string>(args);
            if (arglist.Contains("help")) return Success(hlp);
            if (args.Length > 1)
            {
                if (args[0] == "prims")
                {
                    nargs = Parser.SplitOff(args, 1);
                }
            }

            quietly = arglist.Contains("quietly");
            if (arglist.Contains("all"))
            {
                arglist.Add("llsd");
                arglist.Add("tasks");
                arglist.Add("deps");
                arglist.Add("links");
            }

            needFiles = 0;
            taskobj = arglist.Contains("taskobj");
            if (arglist.Contains("nonincr")) Incremental = false;
            if (arglist.Contains("incr")) Incremental = true;
            bool fileOnly = false;
            lock (fileWriterLock)
            {
                if (arglist.Contains("clear"))
                {
                    PurgeExport();
                    arglist.Add("reset");
                }

                if (!Directory.Exists(dumpDir)) Directory.CreateDirectory(dumpDir);
                if (!Directory.Exists(assetDumpDir)) Directory.CreateDirectory(assetDumpDir);

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
                return Success("Reset SimExport State");
            }

            if (fileOnly) return Success("Manipulated filesystem");

            IsExporting = true;
            FolderCalled("TaskInvHolder");
            //string file = args[args.Length - 1];
            int used;
            List<SimObject> PS = WorldSystem.GetPrimitives(nargs, out used);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            showsStatus = arglist.Contains("status");
            showPermsOnly = arglist.Contains("perms");
            skipPerms = !arglist.Contains("noperms");
            showsMissingOnly = arglist.Contains("todo");
            if (showsMissingOnly) quietly = true;
            verbosely = arglist.Contains("verbose");
            if (verbosely) quietly = false;
            int missing = 0;
            var canExport =  new List<SimObject>();
            int objects = 0;
            foreach (var P in PS)
            {
                if (P is SimAvatar) continue;
                // skip attachments
                if (P.Parent is SimAvatar) continue;
                if (!P.HasPrim)
                {
                    Failure("Missing Prim: " + named(P));
                    continue;
                }
                objects++;
                string issues = P.Missing;
                if (!string.IsNullOrEmpty(issues))
                {
                    missing++;
                    if (!quietly) Failure("Issues " + issues + " " + named(P));
                    continue;
                }
                bool exportPossible =
                    checkPerms(Client, P, showPermsOnly ? (OutputDelegate) LocalFailure : SilientFailure) || skipPerms;
                if (exportPossible)
                {
                    canExport.Add(P);
                }
            }

            Success("Can export " + canExport.Count + " of " + objects);
            if (showPermsOnly) return Success("Shown perms");

            foreach (var P in canExport)
            {
                if (P is SimAvatar) continue;
                // skip attachments
                if (P.Parent is SimAvatar) continue;
                string issues = P.Missing;
                if (!string.IsNullOrEmpty(issues))
                {
                    continue;
                }
                //if (exportedPrims.Contains(P)) continue;
                LocalFailures = 0;
                PrimDepsAssets.Clear();
                ExportPrim(Client, P, LocalFailure, arglist);
                if (LocalFailures == 0)
                {
                    exportedPrims.Add(P);
                }
            }
            if (showsStatus)
            {
                arglist.Add("links");
                arglist.Add("tasks");
                arglist.Add("llsd");
            }

            if (arglist.Contains("links"))
            {
                // lock (PrimWaitingLinkset)
                {
                    InventoryItem found = GetLinkSpeaker(Client);
                    foreach (var pa in LockInfo.CopyOf(PrimWaitingLinkset))
                    {
                        var exportPrim = pa.Value.O;
                        if (verbosely) Failure("Awaiting Linkset " + named(exportPrim));
                        if (arglist.Contains("request"))
                        {
                            PutLinkSpeaker(Client, exportPrim);
                        }
                    }
                }
            }

            List<UUID> xferStarted = new List<UUID>();
            if (arglist.Contains("tasks"))
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
            Success("Awaiting Linkset of " + PrimWaitingLinkset.Count + " objects");
            Success("Awaiting TaskAsset of " + TaskAssetWaiting.Count + " assets");
            Success("CompletedTaskAsset: " + CompletedTaskItem.Count + " assets");
            Success("Awaiting DL of " + ToDownloadAssets.Count + " assets");
            Success("Started XFERS " + xferStarted.Count + " assets");
            Success("Needed FILES " + needFiles + "");
            Success("Missing PrimData: " + missing);
            return res;
        }

        public static void PurgeCache()
        {
            string sfile = Path.GetDirectoryName(SimAsset.CFileName(UUID.Zero, AssetType.Texture));
            if (Directory.Exists(sfile))
            {
                Directory.Delete(sfile, true);
                Directory.CreateDirectory(sfile);
            }
        }

        private void PurgeExport()
        {
            if (Directory.Exists(dumpDir)) Directory.Delete(dumpDir, true);
            if (Directory.Exists(assetDumpDir)) Directory.Delete(assetDumpDir, true);
        }

        private void StartAssetDownload(List<UUID> xferStarted, UUID assetID, AssetType assetType)
        {
            if (xferStarted.Contains(assetID)) return;
            xferStarted.Add(assetID);
            // string filename = assetID + ".asset";
            // ulong xferID = Client.Assets.RequestAssetXfer(filename, false, true, assetID, assetType, false);
            Client.Assets.RequestAsset(assetID, assetType, true, null);
        }

        private void SilientFailure(string s, object[] args)
        {
            LocalFailures++;
            //Failure(DLRConsole.SafeFormat(s, args));
        }

        private void LocalFailure(string s, object[] args)
        {
            LocalFailures++;
            if (!quietly) Failure(DLRConsole.SafeFormat(s, args));
        }

        public static void ExportPrim(BotClient Client, SimObject exportPrim, OutputDelegate Failure, List<string> arglist)
        {
            WorldObjects.EnsureSelected(exportPrim.LocalID, exportPrim.GetSimulator());
            string pathStem = Path.Combine(dumpDir, exportPrim.ID.ToString());

            string issues = exportPrim.Missing;
            if (!string.IsNullOrEmpty(issues))
            {
                Failure("Missing " + issues + " " + named(exportPrim));
                return;
            }
            if (arglist.Contains("llsd")) SaveLLSD(Client, pathStem, exportPrim, Failure);
            if (exportPrim.IsRoot && exportPrim.Children.Count > 1)
            {
                if (arglist.Contains("links")) SaveLinksetInfo(Client, pathStem, exportPrim, Failure);
            }
            if (arglist.Contains("tasks")) SaveTaskInv(Client, pathStem, exportPrim, Failure);
            if (!arglist.Contains("deps")) return;                
            AddRelatedTextures(exportPrim);
            SaveRelatedAssets(pathStem, exportPrim, Failure);
        }

        private static void SaveRelatedAssets(string pathStem, SimObject exportPrim, OutputDelegate Failure)
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

        private static AssetType assetTypeOf(UUID uuid)
        {
            AssetType assetType;
            AllRelatedAssets.TryGetValue(uuid, out assetType);
            return assetType;
        }

        static void SaveLinksetInfo(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".link";
            if (Incremental || true) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            if (exportPrim.Children.Count < 2)
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
            lock (PrimWaitingLinkset)
            {
                if (PrimWaitingLinkset.ContainsKey(exportPrim.ID)) return;
                PrimWaitingLinkset.Add(exportPrim.ID, new SO { S = "", O = exportPrim, F = pathStem });
            }
            PutLinkSpeaker(Client, exportPrim);
        }

        private static void listen_forLinkset(object sender, ChatEventArgs e)
        {
            if (e.Type != ChatType.OwnerSay) return;
            UUID sourceId = e.SourceID;
            string eMessage = e.Message;
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
                so.S = so.S + eMessage;
                if (eMessage.EndsWith(",Z"))
                {
                    lock (PrimWaitingLinkset)
                    {
                        PrimWaitingLinkset.Remove(sourceId);
                    }
                    lock (fileWriterLock) File.WriteAllText(so.F + ".link", so.S);
                }
            }
        }

        private static void PutLinkSpeaker(BotClient Client, SimObject exportPrim)
        {
            InventoryItem found = GetLinkSpeaker(Client);
            Client.Inventory.CopyScriptToTask(exportPrim.LocalID, (InventoryItem)found, true);
            Client.Inventory.RequestSetScriptRunning(exportPrim.ID, found.AssetUUID, true);
        }

        public static InventoryItem GetLinkSpeaker(GridClient Client)
        {
            if (linkSpeaker != null) return linkSpeaker;
            Client.Inventory.FolderContents(Client.Inventory.FindFolderForType(AssetType.LSLText), Client.Self.AgentID,
                                            false, true, InventorySortOrder.ByName, 10000);
            foreach (var item in Client.Inventory.Store.GetContents(Client.Inventory.FindFolderForType(AssetType.LSLText)))
            {
                if (item.Name == "LinksetSpeaker")
                {
                    linkSpeaker = item as InventoryItem;
                    break;
                }
            }
            return linkSpeaker;
        }


        static private bool checkPerms(BotClient Client, SimObject exportPrim, OutputDelegate Failure)
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
                bool cmt = Permissions.HasPermissions(pm, PermissionMask.Copy) ||
                           Permissions.HasPermissions(pm, PermissionMask.Modify) ||
                           Permissions.HasPermissions(pm, PermissionMask.Transfer);

                if (!cmt)
                {
                    Failure("ObjPerms " + pm + " for " + pw + " on " + named(exportPrim));
                    return false;
                }

                List<SimObject> family = new List<SimObject>();
                family.Add(exportPrim);
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

        public static void SaveLLSD(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
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

                OSD primOSD = exportPrim.Prim.GetOSD();
                string output = OSDParser.SerializeLLSDXmlString(primOSD);
                try
                {
                    lock (fileWriterLock) File.WriteAllText(exportFile, output);
                }
                catch (Exception e)
                {
                    Failure("Writing file " + exportFile);
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

        static void SaveTaskInv(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".task";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED TASK for " + named(exportPrim));
                return;
            }
            var ib = exportPrim.TaskInventory;
            if (ib == null)
            {
                Failure("NULL TaskInv for " + named(exportPrim));
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
            foreach (InventoryBase b in ib)
            {
                string was = SaveTaskItems(Client, exportPrim, b, Failure, foundObject, folderObject);
                contents += was;
            }
            if (folderObject.Count == 0)
            {
                lock (fileWriterLock) File.WriteAllText(exportFile, contents);
            }
            else
            {
                Failure("Skipping writting contents unil Objects can be resolved:\n" + contents + " for " +
                        named(exportPrim));
            }
        }

        static string SaveTaskItems(BotClient Client, SimObject exportPrim, InventoryBase b, OutputDelegate Failure, List<SimObject> foundObject, List<InventoryObject> folderObject)
        {
            string primName = " from " + named(exportPrim);
            //primName = "";
            InventoryFolder fldr = b as InventoryFolder;
            if (fldr != null)
            {
                if (fldr.Name == "Contents")
                {
                    return "";
                }
                //  Success("Folder " + fldr.Name + primName);

                //                List<InventoryBase> currentContents = Client.Inventory.GetContents(fldr);
                //              fldr
                return "Folder," + UUID.Zero + "," + fldr.Name + "\n";
            }
            InventoryItem item = b as InventoryItem;
            if (item.InventoryType == InventoryType.Object)
            {
                UnpackTaskObject(exportPrim, item as InventoryObject, folderObject, Client, Failure, primName, foundObject);

                //foundObject = true;
                return item.AssetType + "," + item.AssetUUID + "," + item.Name + "\n";
                //todo
                //
            }
            lock (TaskAssetWaiting)
            {
                if (TaskAssetWaiting.ContainsKey(item))
                {
                    return item.AssetType + "," + item.AssetUUID + "," + item.Name + "\n";
                }
            }
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
                                                             lock (TaskAssetWaiting)
                                                                 TaskAssetWaiting.Remove(item);
                                                             lock (CompletedTaskItem) CompletedTaskItem.Add(b);
                                                         };
            IHO ho = new IHO { I = item, H = rec, O = exportPrim };
            lock (TaskAssetWaiting)
                TaskAssetWaiting.Add(item, ho);


            SimAvatarClient theAvatar = Client.TheSimAvatar;
            PermissionWho pw = theAvatar.EffectivePermissionWho(item.OwnerID, item.GroupID, item.GroupOwned);
            PermissionMask pm = CogbotHelpers.PermMaskForWho(pw, item.Permissions);
            bool cmt = Permissions.HasPermissions(pm, PermissionMask.Copy) ||
                       Permissions.HasPermissions(pm, PermissionMask.Modify) ||
                       Permissions.HasPermissions(pm, PermissionMask.Transfer);

            if (!cmt)
            {
                Failure("ItemPerms " + pm + " for " + pw + " on " + ho);
            }


            SlowlyDo(() =>
                     Client.Assets.RequestInventoryAsset(item.AssetUUID, item.UUID, exportPrim.ID, item.OwnerID,
                                                         item.AssetType, true, rec));
            FindOrCreateAsset(item.AssetUUID, item.AssetType);
            return item.AssetType + "," + item.AssetUUID + "," + item.Name + "\n";
            /*if (!are.WaitOne(10000))
            {
                Failure("Cant get taskinv " + item.InventoryType + " " + item.Name + primName);
            }  else
            {
                // Success("Received taskinv " + item.InventoryType + " " + item.Name + primName);
            }*/
        }

        private static void UnpackTaskObject(SimObject exportPrim, InventoryObject taskInv, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName, List<SimObject> foundObject)
        {
            folderObject.Add(taskInv);
            if (!taskobj) return;
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

            DateTime timeOut = DateTime.Now + TimeSpan.FromSeconds(5);
            SimObject O = null;
            while (DateTime.Now < timeOut)
            {
                O = WorldObjects.GetSimObjectFromUUID(objectID);
                if (O == null) continue;
                foundObject.Add(O);
            }
            if (O == null)
            {
                Failure("Cant FIND taskinv object " + taskInv.Name + primName);
                return;
            }
            Primitive prim = O.Prim;
            localID = localID > 0 ? localID : O.LocalID;
            lock (OnExitKill) OnExitKill.Add(localID, exportPrim.GetSimulator().Handle);
            Client.Objects.DropObject(O.GetSimulator(), localID);
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

        public void KillAllUnpacked()
        {
            UUID into = FolderCalled("TaskInvKilled");
            foreach (var exitKill in OnExitKill)
            {
                Client.Inventory.RequestDeRezToInventory(exitKill.Key, DeRezDestination.AgentInventoryTake,
                                                         into, UUID.Random());
            }
        }
        private static void UnpackTaskObject2(SimObject exportPrim, InventoryItem item, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName, List<SimObject> foundObject)
        {
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


        private static void SlowlyDo(ThreadStart action)
        {
            slowlyExport.Enqueue(action);
        }


        /// <summary>
        /// Loads in inventory cache file into the inventory structure. Note only valid to call after login has been successful.
        /// </summary>
        /// <param name="filename">Name of the cache file to load</param>
        /// <returns>The number of inventory items sucessfully reconstructed into the inventory node tree</returns>
        public List<InventoryNode> RestoreFromDisk(string filename)
        {
            List<InventoryNode> nodes = new List<InventoryNode>();
            int item_count = 0;

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
                            OpenMetaverse.InventoryNode node = (InventoryNode)bformatter.Deserialize(stream);
                            nodes.Add(node);
                            item_count++;
                        }
                    }
                }
                catch (Exception e)
                {
                    Logger.Log("Error accessing inventory cache file :" + e.Message, Helpers.LogLevel.Error);
                    return null;
                }
            }
            return nodes;
        }


        /// <summary>
        /// Saves the current inventory structure to a cache file
        /// </summary>
        /// <param name="filename">Name of the cache file to save to</param>
        static public void SaveToDisk(string filename, Object item)
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
            List<ImageRequest> textureRequests = new List<ImageRequest>();
            Dictionary<UUID, AssetType> otherRequests = new Dictionary<UUID, AssetType>();


            // Create a request list from all of the images
            lock (ToDownloadAssets)
            {
                foreach (var asset in ToDownloadAssets)
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

            return Success("XML exported, downloading " + ToDownloadAssets.Count + " assets for " + exportedPrims.Count);

        }

        static void AddRelatedTextures(SimObject simObject)
        {
            var exportPrim = simObject.Prim;
            lock (ToDownloadAssets)
            {
                AddRelated(exportPrim.Textures.DefaultTexture.TextureID, AssetType.Texture);

                for (int j = 0; j < exportPrim.Textures.FaceTextures.Length; j++)
                {
                    if (exportPrim.Textures.FaceTextures[j] != null)
                    {
                        AddRelated(exportPrim.Textures.FaceTextures[j].TextureID, AssetType.Texture);
                    }
                }

                if (exportPrim.Sculpt != null)
                {
                    AddRelated(exportPrim.Sculpt.SculptTexture, AssetType.Texture);
                }
                AddRelatedTexturesFromProps(simObject);
            }
        }

        static void AddRelatedTexturesFromProps(SimObject simObject)
        {
            UUID[] textureIDs = simObject.Properties.TextureIDs;
            if (textureIDs == null || textureIDs.Length == 0) return;
            foreach (var c in textureIDs)
            {
                AddRelated(c, AssetType.Texture);
            }
        }

        public static void AddRelated(UUID assetID, AssetType assetType)
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
                    lock (ToDownloadAssets)
                        if (!ToDownloadAssets.Contains(assetID))
                        {
                            ToDownloadAssets.Add(assetID);
                        }
                }
        }

        private static void FindOrCreateAsset(UUID uuid, AssetType type)
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

        private static void Asset_Xfer(object sender, XferReceivedEventArgs e)
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

        static void Assets_OnReceived(AssetDownload transfer, Asset asset)
        {
            lock (ToDownloadAssets) if (transfer.Success && ToDownloadAssets.Contains(asset.AssetID))
                {
                    lock (ToDownloadAssets)
                        ToDownloadAssets.Remove(asset.AssetID);
                    string sfile = SimAsset.CFileName(asset.AssetID, asset.AssetType);
                    try
                    {
                        lock (fileWriterLock)
                            File.WriteAllBytes(assetDumpDir + Path.GetFileName(sfile), asset.AssetData);
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
    }
}
