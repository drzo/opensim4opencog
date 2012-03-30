#define OBJECTUNPACKER
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

    public partial class ExportTaskAsset
    {
        public InventoryItem SourceItem;
        public int NumRequests = 0;
        public String Error = "";
        public SimObject O;
        public AutoResetEvent waiting;
        public override string ToString()
        {
            return ExportCommand.ItemDesc(SourceItem, O);
        }

        public void Asset_Received(AssetDownload trans, Asset asset)
        {
            var Running = ExportCommand.Exporting;
            var item = SourceItem;
            UUID itemID = item.UUID;
            //if (trans.AssetID != item.AssetUUID) return;
            if (!trans.Success)
            {
                Error = "" + trans.Status;
                if (waiting != null) waiting.Set();
                lock (Running.TaskAssetWaiting)
                {
                    ExportTaskAsset exportTaskAsset;
                    if (!Running.CompletedTaskItem.Contains(itemID))
                    {
                        Request();
                        return;
                    }
                }
            }
            Running.Assets_OnReceived(trans, asset);
            //AddRelated(item.AssetUUID, item.AssetType);
            Running.TaskItemComplete(O.ID, itemID, asset.AssetID, asset.AssetType);
            if (waiting != null) waiting.Set();
        }

        public void Request()
        {
            if (!string.IsNullOrEmpty(Error))
            {
                return;
            }
            var Running = ExportCommand.Exporting;
            InventoryItem item = SourceItem;
            UUID itemID = item.UUID;
            if (item.AssetType == AssetType.LSLText || item.AssetType == AssetType.Notecard)
            {
                //Perhaps copy to AgentInventory first
            }
            Running.Client.Assets.RequestInventoryAsset(item.AssetUUID, itemID, O.ID, item.OwnerID,
                                                        item.AssetType, true, Asset_Received);
            NumRequests++;
        }
    }

    public partial class ExportCommand : Command, RegionMasterCommand
    {
        public readonly Dictionary<UUID, ExportTaskAsset> TaskAssetWaiting = new Dictionary<UUID, ExportTaskAsset>();
        public readonly Dictionary<UUID, ImportCommand.TaskItemToCreate> TasksRezed = new Dictionary<UUID, ImportCommand.TaskItemToCreate>();
        public readonly HashSet<UUID> CompletedTaskItem = new HashSet<UUID>();
        //public readonly Dictionary<UUID, InventoryItem> UUID2ITEM = new Dictionary<UUID, InventoryItem>();
        //private int TaskInvFailures = 0;

        private void listen_TaskInv(string eMessage, UUID arg2)
        {
            if (eMessage.StartsWith("RTI:"))
            {
                eMessage = eMessage.Substring(4).Trim();
                string[] lr = eMessage.Split(new[] { ',' });
                var objid = UUIDFactory.GetUUID(lr[0]);
                Importing.MustExport.Add(objid);
                lock (fileWriterLock) File.WriteAllText(dumpDir + objid + ".objectAsset", eMessage);
                var exportPrimID = UUIDFactory.GetUUID(lr[1]);
                var objectNumber = int.Parse(lr[2]);
                string exportFile = dumpDir + exportPrimID + "." + objectNumber + ".rti";
                lock (fileWriterLock) File.WriteAllText(exportFile, eMessage);
                return;
            }
        }

        public void TaskItemComplete(UUID primID, UUID itemID, UUID assetID, AssetType assetType)
        {
            lock (TaskAssetWaiting) TaskAssetWaiting.Remove(itemID);
            lock (CompletedTaskItem) CompletedTaskItem.Add(itemID);
            var O = GetSimObjectFromUUID(primID);
            AddRelated(assetID, assetType);
            UpdateTaskInvAssetID(O, itemID, assetID);
        }

        private void UpdateTaskInvAssetID(SimObject o, UUID uuid, UUID id)
        {
            if (o == null)
            {
                return;
            }
            var ti = o.TaskInventory;
            if (ti == null)
            {
                return;
            }
            bool fixedIt = false;
            bool somethingBroke = false;
            foreach (InventoryBase b in ti)
            {
                InventoryItem it = b as InventoryItem;
                if (it == null) continue;
                if (b.UUID == uuid)
                {
                    if (CogbotHelpers.IsNullOrZero(it.AssetUUID))
                    {
                        it.AssetUUID = id;
                        fixedIt = true;
                    }
                    if (it.AssetType == AssetType.Object)
                    {
                        if (CogbotHelpers.IsNullOrZero(it.RezzID))
                        {
                            it.RezzID = id;
                            fixedIt = true;
                        }
                    }
                }
                if (CogbotHelpers.IsNullOrZero(it.AssetUUID) && CogbotHelpers.IsNullOrZero(it.RezzID))
                {
                    somethingBroke = true;
                }
            }
            if (fixedIt)
            {
                if (somethingBroke)
                {
                  //  return;
                }
                SaveTaskOSD(o.ID, ti);
            }
        }

        public void SaveTaskOSD(UUID uuid, IEnumerable<InventoryBase> bases)
        {
            string exportFile = dumpDir + "" + uuid + ".task";
            OSDArray all = new OSDArray();
            foreach (InventoryBase b in bases)
            {
                all.Add(OSDSerializeMembers(b));
            }
            lock (fileWriterLock) File.WriteAllText(exportFile, OSDParser.SerializeLLSDXmlString(all));
        }

        static public bool PerfectTaskOSD(UUID uuid, ImportSettings sets)
        {
            string exportFile = dumpDir + "" + uuid + ".task";
            if (!File.Exists(exportFile)) return false;
            string osdText = File.ReadAllText(exportFile);
            if (osdText.Length < 20) return true;
            OSDArray osd = OSDParser.DeserializeLLSDXml(osdText) as OSDArray;
            foreach (OSDMap array in osd)
            {
                var type = array["AssetType"].AsInteger();
                if (type == (int)AssetType.Object)
                {
                    var r = array["RezzID"];
                    if (r.Type == OSDType.Unknown)
                    {
                        File.Delete(exportFile);
                        return false;
                    }
                    var ri = r.AsUUID();
                    if (CogbotHelpers.IsNullOrZero(ri))
                    {
                        return false;
                    }
                    if (sets.Contains("killrezids"))
                    {
                        File.Delete(exportFile);
                        return false;
                    }
                }
                var o = array["AssetUUID"].AsUUID();
                if (CogbotHelpers.IsNullOrZero(o)) return false;
            }
            return true;
        }

        private bool checkTaskPerm(SimObject exportPrim, InventoryItem item, BotClient Client, OutputDelegate Failure, bool mustModify)
        {
            SimAvatarClient theAvatar = Client.TheSimAvatar;
            PermissionWho pw = theAvatar.EffectivePermissionWho(item.OwnerID, item.GroupID, item.GroupOwned);
            PermissionMask pm = CogbotHelpers.PermMaskForWho(pw, item.Permissions);

            bool modify = Permissions.HasPermissions(pm, PermissionMask.Modify);

            bool cmt = Permissions.HasPermissions(pm, PermissionMask.Copy) ||
                       Permissions.HasPermissions(pm, PermissionMask.Modify) ||
                       Permissions.HasPermissions(pm, PermissionMask.Transfer);

            if (mustModify)
            {
                if (!modify)
                {
                    Failure("ItemPerms NOMODIFY " + pm + " for " + pw + " on " + ItemDesc(item, exportPrim));
                    return false;
                }
            }

            if (!cmt)
            {
                Failure("ItemPerms " + pm + " for " + pw + " on " + ItemDesc(item, exportPrim));
                var p = exportPrim.Parent;
                if (p != null && p != exportPrim)
                {
                    exportPrim = p;
                }
                lock (SIPrims) SIPrims.Add(exportPrim);
                return false;
            }
            return true;
        }

        internal bool SaveTaskInv(ImportSettings arglist, BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".task";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return true;
            needFiles++;
            exportPrim.StartGetTaskInventory();
            bool mightHaveTaskInv = exportPrim.TaskInventoryLikely;

            var ib = exportPrim.TaskInventory;
            if (ib == null || ib == SimObjectImpl.ERROR_TASK_INV)
            {
                bool canSee = checkPerms(Client, exportPrim, SilientFailure, false);
                bool canScript = checkPerms(Client, exportPrim, SilientFailure, true);

                if (!canSee)
                {
                    if (!mightHaveTaskInv)
                    {
                        LogError(exportPrim, "!canSee TaskInv");
                        lock (fileWriterLock) File.WriteAllText(exportFile, "!canSee");
                        return true;
                    }
                    Failure("Cant get/check TaskInv of " + named(exportPrim));
                }
                Failure("NULL TaskInv for " + named(exportPrim));
                if (!canScript)
                {
                    LogError(exportPrim, "!canScript to get TaskInv");
                    lock (fileWriterLock) File.WriteAllText(exportFile, "!canScript");
                    return true;
                }
                if (ib == SimObjectImpl.ERROR_TASK_INV)
                {
                    LogError(exportPrim, "TaskInv Null once");
                    lock (fileWriterLock) File.WriteAllText(exportFile, "!error");
                }               
                return true;
            }
            if (ib.Count == 0)
            {
                if (!exportPrim.InventoryEmpty)
                {
                    if (verbosely) Failure("ZEROITEM TaskInv for " + named(exportPrim));
                    //return;
                }
                lock (fileWriterLock) File.WriteAllText(exportFile, "");
                return true;
            }
            if (ib.Count == 1)
            {
                if (ib[0].Name == "Contents" && ib[0] is InventoryFolder)
                {
                    lock (fileWriterLock) File.WriteAllText(exportFile, "");
                    return true;
                }
            }
            SaveTaskOSD(exportPrim.ID, ib);
            return true;
            OSDArray contents = new OSDArray();

            string TaskInvFailures = "";
            bool hasObjects = false;
            foreach (InventoryBase b in ib)
            {
                if (b is InventoryObject)
                {
                    hasObjects = true;
                    break;                    
                }
            }
            bool wasShouldBeMoving = shouldBeMoving;
            shouldBeMoving = false;
            if (hasObjects && taskobj) MoveCloseTo(exportPrim);
            
            foreach (InventoryBase b in ib)
            {
                bool missing;
                OSDMap was = SaveEachTaskItem(arglist, Client, exportPrim, b, Failure, out missing);
                if (was != null) contents.Add(was);
                if (missing)
                {
                    if (forced && false)
                    {
                        Failure("Missing but forced: " + was);
                    }
                    else
                    {
                        TaskInvFailures += was;
                    }
                }
            }
            shouldBeMoving = wasShouldBeMoving;
            // TaskInvFailures = GetTaskInvFailures(Failure, exportPrim, Client, folderObject, contents, TaskInvFailures);
            if (showsMissingOnly)
            {
                Failure("NEED TASK for " + named(exportPrim));
                return false;
            }
            if (string.IsNullOrEmpty(TaskInvFailures))
            {
                lock (fileWriterLock) File.WriteAllText(exportFile, OSDParser.SerializeLLSDXmlString(contents));
            }
            else
            {
                Failure(string.Format("Skipping writting contents unil Items/Objects can be resolved: for {0}\n{1}",
                                      named(exportPrim), TaskInvFailures));
            }
            var ptc = Importing.APrimToCreate(exportPrim.ID);
            return ptc.EnsureTaskInv(false);
        }

        void IBToTaskFile(UUID filename, List<InventoryBase> ib)
        {
            
        }

        OSDMap SaveEachTaskItem(ImportSettings arglist, BotClient Client, SimObject exportPrim, InventoryBase b, OutputDelegate Failure, out bool missing)
        {
            InventoryFolder fldr = b as InventoryFolder;
            if (fldr != null)
            {
                missing = false;
                return OSDSerializeMembers(b);
            }
            InventoryItem item = b as InventoryItem;

            if (item == null)
            {
                string errorMsg = "" + b.UUID + ",ERROR," + b.UUID + "," + b.Name;
                Failure("No an Item");
                missing = true;
                return OSDSerializeMembers(b);
            }

            bool exportable = checkTaskPerm(exportPrim, item, Client, Failure, false);
            lock (TaskAssetWaiting)
            {
                lock (CompletedTaskItem)
                {
                    UUID itemID = item.UUID;
                    if (CompletedTaskItem.Contains(itemID))
                    {
                        missing = false;
                        return OSDSerializeMembers(item);
                    }
                }
            }
            if (item.InventoryType == InventoryType.Object)
            {
                //UUID newObjID;
                //return UnpackTaskObject(arglist, exportPrim, item as InventoryObject, Client, Failure, out missing, taskobj, out newObjID);
            }
            missing = CogbotHelpers.IsNullOrZero(item.AssetUUID);
            return OSDSerializeMembers(item); //UnpackTaskItem(Client, exportPrim, item, Failure, out missing);
        }

        public OSDMap UnpackTaskItem(BotClient Client, SimObject exportPrim, InventoryItem item, OutputDelegate Failure, out bool missing)
        {
            UUID itemID = item.UUID;
            lock (CompletedTaskItem) if (CompletedTaskItem.Contains(itemID))
            {
                missing = false;
                return OSDSerializeMembers(item);
            }
            lock (CompletedTaskItem) if (!CogbotHelpers.IsNullOrZero(item.AssetUUID) && CompletedAssets.Contains(item.AssetUUID))
            {
                TaskItemComplete(exportPrim.ID, itemID, item.AssetUUID, item.AssetType);
                missing = false;
                return OSDSerializeMembers(item);
            }
            if (showsMissingOnly)
            {
                Failure("NEED TASKITEM: " + ItemDesc(item, exportPrim));
                missing = true;
                return OSDSerializeMembers(item);
            }
            ExportTaskAsset ho;
            AutoResetEvent waitUntilDL = new AutoResetEvent(false);
            lock (TaskAssetWaiting) if (!TaskAssetWaiting.TryGetValue(itemID, out ho))
            {
                TaskAssetWaiting[itemID] = ho = new ExportTaskAsset { SourceItem = item, O = exportPrim };
                ho.waiting = waitUntilDL;

            }
            waitUntilDL = ho.waiting;
            lock (CompletedTaskItem) if (!CogbotHelpers.IsNullOrZero(item.AssetUUID) && CompletedAssets.Contains(item.AssetUUID))
            {
                TaskItemComplete(exportPrim.ID, itemID, item.AssetUUID, item.AssetType);
                missing = false;
                return OSDSerializeMembers(item);
            }
            SlowlyDo(ho.Request);
            missing = waitUntilDL == null || !waitUntilDL.WaitOne(4000);
            if (CogbotHelpers.IsNullOrZero(item.AssetUUID))
            {
                Failure("ASSET ZERO " + ItemDesc(item, exportPrim));
                missing = true;
            }
            AddRelated(item.AssetUUID, item.AssetType);
            FindOrCreateAsset(item.AssetUUID, item.AssetType);
            return OSDSerializeMembers(item);
        }

        public void KillAllUnpacked(OutputDelegate Failures, bool artifacts)
        {

            UUID into = null;
            if (!settings.Contains("info"))
            {
                into = FolderCalled("TaskInvKilled") ?? Client.Inventory.FindFolderForType(AssetType.TrashFolder);
            }
            int count = 0;
            lock (fileWriterLock)
            {
                foreach (var file in Directory.GetFiles(dumpDir, "*.repack"))
                {
                    count++;
                    if (count > 1) if (settings.Contains("once")) return;
                    string filetext = File.ReadAllText(file);
                    string[] csv = filetext.Split(new[] { ',' });
                    UUID assetID = UUIDFactory.GetUUID(csv[2]);
                    uint localID = uint.Parse(csv[0]);
                    var O1 = WorldObjects.GetSimObjectFromUUID(assetID);
                    if (O1 == null)
                    {
                        Client.Objects.RequestObject(Client.Network.CurrentSim, localID);
                    }
                    var O = GetSimObjectFromUUID(assetID);
                    if (O == null)
                    {
                        Failure("Cant find object for file: " + file + " " + filetext);
                        if (settings.Contains("info"))
                        {
                            Success("INFO: " + localID + " " + assetID);
                            continue;
                        }
                        Client.Inventory.RequestDeRezToInventory(localID, DeRezDestination.TrashFolder, into,
                                         UUID.Random());
                        File.Delete(file);
                        if (artifacts)
                        {
                            Importing.KillID(assetID);
                        }
                        if (settings.Contains("once")) return;
                    }
                    else
                    {
                        if (!settings.Contains("info"))
                        {
                            Success("INFO: " + localID + " " + assetID + " text=" + filetext);
                            continue;
                        }
                        Client.Inventory.RequestDeRezToInventory(O.LocalID, DeRezDestination.TrashFolder, into,
                                                                 UUID.Random());
                        File.Delete(file);
                        if (artifacts)
                        {
                            Importing.KillID(assetID);
                        }
                        if (settings.Contains("once")) return;
                    }
                }
                if (artifacts)
                {
                    foreach (var file in Directory.GetFiles(dumpDir, "*.taskobj"))
                    {
                        string filetext = File.ReadAllText(file);
                        string[] csv = filetext.Split(new[] { ',' });
                        UUID assetID = UUIDFactory.GetUUID(csv[2]);
                        Importing.KillID(assetID);
                        UUID holderID = UUIDFactory.GetUUID(csv[3]);
                        string exportFile = dumpDir + "" + holderID + ".task";
                        if (File.Exists(exportFile))
                        {
                            File.Delete(exportFile);
                        }
                        File.Delete(file);
                    }
                }
            }
        }
        static public string ItemDesc(InventoryItem I, SimObject O)
        {
            return I.Name + "(" + I.AssetType + " " + I.AssetUUID + ")@" + named(O);
        }

    }
}
