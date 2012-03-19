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
#if OBJECTUNPACKER
    public class ExportTaskObject
    {
        public InventoryObject Task;
        public InventoryObject Inv;
        public SimObject WasInside;
        public bool NoCopyItem;
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
#endif
    public partial class ExportCommand : Command, RegionMasterCommand
    {
        public readonly Dictionary<UUID, ExportTaskAsset> TaskAssetWaiting = new Dictionary<UUID, ExportTaskAsset>();
#if OBJECTUNPACKER 
        public readonly Dictionary<UUID, ExportTaskObject> TasksRezed = new Dictionary<UUID, ExportTaskObject>();
#endif
        public readonly HashSet<UUID> CompletedTaskItem = new HashSet<UUID>();
        //public readonly Dictionary<UUID, InventoryItem> UUID2ITEM = new Dictionary<UUID, InventoryItem>();
        //private int TaskInvFailures = 0;
        private InventoryObject WaitingFolderObjects;
        private bool WaitingFolderObjectBool;
        private SimObject WaitingFolderSimObject;

        private void listen_TaskInv(string eMessage, UUID arg2)
        {
            if (eMessage.StartsWith("RTI:"))
            {
                int popTo = eMessage.IndexOf("RTI:");
                eMessage = eMessage.Substring(4 + popTo).Trim();
                string[] lr = eMessage.Split(new[] { ',' });
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
                TaskItemComplete(objid, taskInv.UUID, assetID, taskInv.AssetType);                
                WaitingFolderSimObject = GetSimObjectFromUUID(objid);
                WaitingFolderObjectBool = false;
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
                }
                if (CogbotHelpers.IsNullOrZero(it.AssetUUID))
                {
                    somethingBroke = true;
                }
            }
            if (fixedIt)
            {
                if (somethingBroke)
                {
                    return;
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

        void SaveTaskInv(ImportSettings arglist, BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".task";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
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
                        return;
                    }
                    Failure("Cant get/check TaskInv of " + named(exportPrim));
                }
                Failure("NULL TaskInv for " + named(exportPrim));
                if (!canScript)
                {
                    LogError(exportPrim, "!canScript to get TaskInv");
                    lock (fileWriterLock) File.WriteAllText(exportFile, "!canScript");
                    return;
                }
                if (ib == SimObjectImpl.ERROR_TASK_INV)
                {
                    LogError(exportPrim, "TaskInv Null once");
                    lock (fileWriterLock) File.WriteAllText(exportFile, "!error");
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
                return;
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
                UUID newObjID;
                return UnpackTaskObject(arglist, exportPrim, item as InventoryObject, Client, Failure, out missing, taskobj, out newObjID);
            }
            return UnpackTaskItem(Client, exportPrim, item, Failure, out missing);
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

        internal OSDMap UnpackTaskObject(ImportSettings arglist, SimObject exportPrim, InventoryObject taskInv, BotClient Client,
            OutputDelegate Failure, out bool missing, bool dotaskobj, out UUID newAssetID)
        {
            newAssetID = UUID.Zero;
            missing = true;
            var itemID = taskInv.UUID; 
            ExportTaskAsset ho;
            UUID taskInvAssetUUID = taskInv.AssetUUID;
            if (!CogbotHelpers.IsNullOrZero(taskInvAssetUUID))
            {
                string exportFile = assetDumpDir + taskInvAssetUUID + ".object";
                if (Incremental || showsMissingOnly)
                {
                    if (File.Exists(exportFile))
                    {
                        missing = false;
                        return OSDSerializeMembers(taskInv); 
                    }
                }
            }
            string taskObjFile = dumpDir + itemID + ".taskobj";
            if (Incremental || showsMissingOnly)
            {
                if (File.Exists(taskObjFile))
                {
                    missing = false;
                    return OSDSerializeMembers(taskInv); 
                }
            }

            string repackFile = dumpDir + itemID + ".repack";
            if (showsMissingOnly || !dotaskobj)
            {
                needFiles++;
                Failure("NEED TASKOBJ: " + ItemDesc(taskInv, exportPrim));
                missing = true;
                return OSDSerializeMembers(taskInv);
            }
            UUID objectID = UUID.Zero;
            UUID newItemID = UUID.Zero;
            AttachmentPoint origAttach = taskInv.AttachPoint;
            UUID objectFolder = FolderCalled(exportPrim.ID.ToString(), inventoryHolderUUID);

            if (File.Exists(repackFile))
            {
                string[] conts = File.ReadAllText(repackFile).Split(',');
                //objectID = UUID.Parse(conts[2]);
            }
            SimAvatarClient theAvatar = Client.TheSimAvatar;
            PermissionWho pw = theAvatar.EffectivePermissionWho(taskInv.OwnerID, taskInv.GroupID, taskInv.GroupOwned);
            PermissionMask pm = CogbotHelpers.PermMaskForWho(pw, taskInv.Permissions);
            bool canModify = Permissions.HasPermissions(pm, PermissionMask.Modify);
            bool canCopy = Permissions.HasPermissions(pm, PermissionMask.Copy);
            bool noCopyItem = !canCopy;
            if (noCopyItem)
            {
                PermissionWho pwo = theAvatar.EffectivePermissionWho(exportPrim);
                PermissionMask pmo = CogbotHelpers.PermMaskForWho(pwo, exportPrim.Properties.Permissions);
                bool canModifyObject = Permissions.HasPermissions(pmo, PermissionMask.Modify);
                if (!canModifyObject)
                {
                    missing = true;
                    Failure("Cant modify object to borrow out the nocopy object " + ItemDesc(taskInv, exportPrim));
                    return OSDSerializeMembers(taskInv);
                }
            }

            if (CogbotHelpers.IsNullOrZero(objectID))
            {
                var conts = Client.Inventory.FolderContents(objectFolder, TheSimAvatar.ID, false, true,
                                                            InventorySortOrder.ByDate, 10000);
                InventoryObject newItem = null;
                if (conts != null)
                {
                    foreach (InventoryBase cont in conts)
                    {
                        InventoryItem it = cont as InventoryItem;
                        if (it == null) continue;
                        if (cont.Name == taskInv.Name && it.AssetType == AssetType.Object)
                        {
                            newItem = it as InventoryObject;
                            break;
                        }
                    }
                }
                if (newItem == null)
                {
                    var takeCopyEvent = new ManualResetEvent(false);
                    EventHandler<TaskItemReceivedEventArgs> copiedToInventory = (o, e) =>
                    {
                        if (e.FolderID != objectFolder)
                        {
                            return;
                        }
                        if (e.AssetID != taskInv.AssetUUID && !CogbotHelpers.IsNullOrZero(e.AssetID) && !CogbotHelpers.IsNullOrZero(taskInv.AssetUUID))
                        {
                            return;
                        }
                        var rItem = Client.Inventory.Store[e.ItemID] as InventoryObject;
                        if (e.Type != taskInv.InventoryType || rItem.Name != taskInv.Name)
                        {
                            return;
                        }                // if (inventoryHolderUUID != e.FolderID) return;
                        newItemID = e.ItemID;
                        takeCopyEvent.Set();
                    };

                    Client.Inventory.TaskItemReceived += copiedToInventory;

                    Client.Inventory.MoveTaskInventory(exportPrim.LocalID, taskInv.UUID, objectFolder,
                                                       exportPrim.GetSimulator());
                    bool successTC = takeCopyEvent.WaitOne(TimeSpan.FromSeconds(10));
                    Client.Inventory.TaskItemReceived -= copiedToInventory;
                    if (!successTC)
                    {
                        missing = true;
                        Failure("Cant MOVE taskinv object " + ItemDesc(taskInv, exportPrim));
                        if (noCopyItem)
                        {
                            //Move back from Personal Inventory to TaskInv (In case)
                            var invItem = GetInvItem(Client, taskInv.Name, objectFolder);
                            if (invItem != null)
                            {
                                Client.Inventory.UpdateTaskInventory(exportPrim.LocalID, invItem);
                                return OSDSerializeMembers(invItem);
                            }
                            else
                            {
                                Failure("Couldnt find it " + ItemDesc(taskInv, exportPrim));
                            }
                        }
                        return OSDSerializeMembers(taskInv);
                    }
                    newItem = Client.Inventory.Store[newItemID] as InventoryObject;
                }
                var rezedEvent = new ManualResetEvent(false);

                EventHandler<ObjectPropertiesEventArgs> rezedInWorld = (o, e) =>
                {
                    if (e.Properties.ItemID != newItemID) return;
                    objectID = e.Properties.ObjectID;
                    rezedEvent.Set();
                };

                Client.Objects.ObjectProperties += rezedInWorld;
                Client.Appearance.Attach(newItem, origAttach, true);
                bool success = rezedEvent.WaitOne(5000);
                Client.Objects.ObjectProperties -= rezedInWorld;
                newAssetID = objectID;
                if (!success)
                {
                    missing = true;
                    Failure("CANT ATTACH taskinv object " + ItemDesc(taskInv, exportPrim));
                    if (noCopyItem)
                    {
                        //Move back from Personal Inventory to TaskInv
                        if (newItem != null)
                        {
                            Client.Inventory.UpdateTaskInventory(exportPrim.LocalID, newItem);
                            return OSDSerializeMembers(taskInv);
                        }
                        else
                        {
                            Failure("Couldnt find it " + ItemDesc(taskInv, exportPrim));
                        }
                    }
                    return OSDSerializeMembers(taskInv);
                } else
                {
                    missing = false;
                }
            }
            newAssetID = objectID;
            SimObject O = GetSimObjectFromUUID(objectID);
            if (O == null)
            {
                missing = true;
                Failure("Cant FIND taskinv object " + ItemDesc(taskInv, exportPrim));
                return OSDSerializeMembers(taskInv);
            }
            //folderObject.Add(O);

            Primitive prim = O.Prim;
            uint unpackedLocalID = O.LocalID;
            Simulator simulator = O.GetSimulator();
            string taskInfo = "" + unpackedLocalID + "," + simulator.Handle + "," + objectID + "," + exportPrim.ID + "," +
                          taskInv.AssetUUID + "," + itemID;
            lock (fileWriterLock)
            {
                File.WriteAllText(repackFile, taskInfo);               
            }
            //taskInvAssetUUID = taskInv.AssetUUID = objectID;
            bool needsDrop = prim.ParentID != 0;
            if (needsDrop)
            {
                Client.Objects.DropObject(simulator, unpackedLocalID);
                // wait for drop
                DateTime waitUntil = DateTime.Now.AddSeconds(10);
                while (prim.ParentID != 0 && DateTime.Now < waitUntil)
                {
                    Thread.Sleep(250);
                }
                if (prim.ParentID != 0)
                {
                    Failure("Cant Drop! " + ItemDesc(taskInv, exportPrim) + " Obj=" + O);
                }
                Vector3 pos = O.SimPosition;
                Client.Objects.SetPosition(simulator, unpackedLocalID, exportPrim.SimPosition + (Vector3.UnitZ*0.5f));
                Client.Objects.RequestObject(simulator, unpackedLocalID);
                waitUntil = DateTime.Now.AddSeconds(10);
                while (O.SimPosition == pos && DateTime.Now < waitUntil)
                {
                    Thread.Sleep(250);
                }
                if (O.SimPosition == pos)
                {
                    Failure("Cant Move! " + ItemDesc(taskInv, exportPrim) + " Obj=" + O);
                }
            }
            if (IsSkipped(O, arglist))
            {
                Failure("IsSkipped " + O);
            }
            int saved = LocalFailures;
            LocalFailures = 0;
            var pda = PrimDepsAssets;
            bool usedWait = arglist.Contains("wait");
            if (!usedWait)
            {
                arglist.Add("wait");
            }
            //SaveLLSD(Client, dumpDir + O.ID, O, Failure);
            ExportPrim(Client, O, Failure, arglist);
            string subLLSD = dumpDir + O.ID.ToString() + ".llsd";
            if (!usedWait)
            {
                arglist.Remove("wait");
            }
            if (!File.Exists(subLLSD))
            {
                ExportPrim(Client, O, Failure, arglist);
                Failure("No LLSD file " + ItemDesc(taskInv, exportPrim) + " Obj=" + O);
                missing = true;
            }
            var areKilled = new ManualResetEvent(false);
            EventHandler<KillObjectEventArgs> onKill = (s, e) =>
                                                           {
                                                               if (e.ObjectLocalID != unpackedLocalID) return;
                                                               areKilled.Set();
                                                           };
            Client.Objects.KillObject += onKill;
            if (noCopyItem)
            {
                // back to the TaskInv it came from
                Client.Inventory.RequestDeRezToInventory(unpackedLocalID, DeRezDestination.TaskInventory,
                                                             exportPrim.ID,
                                                             UUID.Random());
            }
            else
            {
                // delete it
                Client.Inventory.RequestDeRezToInventory(unpackedLocalID, DeRezDestination.AgentInventoryTake,
                                                         FolderCalled("TaskInvDeRez"),
                                                         UUID.Random());
            }
            bool wasKilled = areKilled.WaitOne(TimeSpan.FromSeconds(5));
            if (!wasKilled && noCopyItem)
            {
                //Take back to the Personal Inv
                Client.Inventory.RequestDeRezToInventory(unpackedLocalID, DeRezDestination.AgentInventoryTake,
                                                         objectFolder,
                                                         UUID.Random());
                
                wasKilled = areKilled.WaitOne(TimeSpan.FromSeconds(5));

                //Move from Personal Inventory to TaskInv
                var invItem = GetInvItem(Client, taskInv.Name, objectFolder);
                if (invItem != null)
                {
                    Client.Inventory.UpdateTaskInventory(exportPrim.LocalID, invItem);
                } else
                {
                    Failure("Couldnt find it " + O);   
                }
            }
            Client.Objects.KillObject -= onKill;
            if (!wasKilled)
            {
                missing = true;
                Failure("Could not kill temp object " + O);
            }
            else
            {
                lock (fileWriterLock) File.Delete(repackFile);
                missing = false;
            }

            int subLocaL = LocalFailures;
            LocalFailures = saved;
            missing = missing || (subLocaL != 0);
            if (missing)
            {
                Failure("LocalFailures=" + subLocaL + " for " + ItemDesc(taskInv, exportPrim));
            } else
            {
                var OID = O.ID;
                if (!CogbotHelpers.IsNullOrZero(taskInv.AssetUUID))
                {
                    Failure("Might overwrite AssetID " + taskInv.AssetUUID + " with " + OID);
                } else
                {
                    taskInv.AssetUUID = OID;
                }
                TaskItemComplete(exportPrim.ID, itemID, OID , taskInv.AssetType);
                lock (fileWriterLock)
                    File.WriteAllText(taskObjFile, taskInfo);
            }
            taskInvAssetUUID = taskInv.AssetUUID = objectID;
            if (!CogbotHelpers.IsNullOrZero(taskInvAssetUUID))
            {
                lock (fileWriterLock)
                    File.WriteAllText(assetDumpDir + taskInvAssetUUID + ".object",
                                      taskInfo + "," + taskInvAssetUUID);
            }
            else
            {
               /// Success("Cant get UUID of Task?!");
            }
            return OSDSerializeMembers(taskInv);
        }

        public void KillAllUnpacked(OutputDelegate Failures)
        {

            UUID into = FolderCalled("TaskInvKilled") ?? Client.Inventory.FindFolderForType(AssetType.TrashFolder);
            lock (fileWriterLock)
            {
                foreach (var file in Directory.GetFiles(dumpDir, "*.repack"))
                {
                    string[] csv = File.ReadAllText(file).Split(new[] { ',' });
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
        static public string ItemDesc(InventoryItem I, SimObject O)
        {
            return I.Name + "(" + I.AssetType + " " + I.AssetUUID + ")@" + named(O);
        }

    }
}
