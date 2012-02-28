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

    public class ExportTaskAsset
    {
        public InventoryItem I;
        public int NumRequests = 0;
        public String Error = "";
        public SimObject O;
        public AutoResetEvent waiting;
        public override string ToString()
        {
            return ExportCommand.ItemDesc(I, O);
        }

        public void Asset_Received(AssetDownload trans, Asset asset)
        {
            var Running = ExportCommand.Running;
            var item = I;
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
            if (CogbotHelpers.IsNullOrZero(item.AssetUUID))
            {
                item.AssetUUID = asset.AssetID;
            }
            Running.Assets_OnReceived(trans, asset);
            //AddRelated(item.AssetUUID, item.AssetType);
            Running.TaskItemComplete(itemID);
            if (waiting != null) waiting.Set();
        }

        public void Request()
        {
            if (!string.IsNullOrEmpty(Error))
            {
                return;
            }
            var Running = ExportCommand.Running;
            InventoryItem item = I;
            UUID itemID = item.UUID;
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
                TaskItemComplete(taskInv.UUID);                
                WaitingFolderSimObject = GetSimObjectFromUUID(objid);
                WaitingFolderObjectBool = false;
                return;
            }
        }

        public void TaskItemComplete(UUID itemID)
        {
            lock (TaskAssetWaiting) TaskAssetWaiting.Remove(itemID);
            lock (CompletedTaskItem) CompletedTaskItem.Add(itemID);
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

        void SaveTaskInv(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".task";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            exportPrim.StartGetTaskInventory();
            bool mightHaveTaskInv = exportPrim.TaskInventoryLikely;

            bool canSee = checkPerms(Client, exportPrim, SilientFailure, false);
            bool canScript = checkPerms(Client, exportPrim, SilientFailure, true);

            if (!canSee)
            {
                if (!mightHaveTaskInv) return;
                Failure("Cant get/check TaskInv of " + named(exportPrim));
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

            OSDArray contents = new OSDArray();
            List<SimObject> foundObject = new List<SimObject>();
            List<InventoryObject> folderObject = new List<InventoryObject>();

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
            if (hasObjects || true) MoveCloseTo(exportPrim);
            foreach (InventoryBase b in ib)
            {
                bool missing;
                OSDMap was = SaveEachTaskItem(Client, exportPrim, b, Failure, folderObject, out missing);
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
            if (!string.IsNullOrEmpty(TaskInvFailures))
            {
                UUID objectFolder = FolderCalled(exportPrim.ID.ToString(), inventoryHolderUUID);
                foreach (InventoryBase inventoryBase in Client.Inventory.FolderContents(objectFolder,Client.Self.AgentID,false,true,InventorySortOrder.ByDate,10000))
                {
                    InventoryItem iback = inventoryBase as InventoryItem;
                    if (iback != null)
                    {
                        Client.Inventory.UpdateTaskInventory(exportPrim.LocalID, iback);
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

        OSDMap SaveEachTaskItem(BotClient Client, SimObject exportPrim, InventoryBase b, OutputDelegate Failure, List<InventoryObject> folderObject, out bool missing)
        {
            //string primName = " from " + named(exportPrim);
            //primName = "";
            InventoryFolder fldr = b as InventoryFolder;
            if (fldr != null)
            {
                if (fldr.Name == "Contents")
                {
                    missing = false;
                    return null;
                }
                //  Success("Folder " + fldr.Name + primName);

                //                List<InventoryBase> currentContents = Client.Inventory.GetContents(fldr);
                //              fldr
                missing = false;
                return OSDSerializeMembers(b);// b.UUID + ",Folder," + UUID.Zero + "," + fldr.Name + "\n";
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
                lock (CompletedTaskItem)
                {
                    UUID itemID = item.UUID;
                    if (CompletedTaskItem.Contains(itemID))
                    {
                        missing = false;
                        return OSDSerializeMembers(item);
                    }
                }
            if (item.InventoryType == InventoryType.Object)
            {
                return UnpackTaskObject(exportPrim, item as InventoryObject, folderObject, Client, Failure, "", out missing);
            }
            return UnpackTaskItem(Client, exportPrim, item, Failure, out missing);
        }

        OSDMap UnpackTaskItem(BotClient Client, SimObject exportPrim, InventoryItem item, OutputDelegate Failure, out bool missing)
        {
            UUID itemID = item.UUID;
            if (CompletedTaskItem.Contains(itemID))
            {
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
                TaskAssetWaiting[itemID] = ho = new ExportTaskAsset { I = item, O = exportPrim };
                ho.waiting = waitUntilDL;
                missing = true;
                // return itemEntry;
            }
            waitUntilDL = ho.waiting;
            if (!CogbotHelpers.IsNullOrZero(item.AssetUUID) && CompletedAssets.Contains(item.AssetUUID))
            {
                TaskItemComplete(itemID);
                missing = false;
                return OSDSerializeMembers(item);
            }
            SlowlyDo(ho.Request);
            missing = waitUntilDL == null || !waitUntilDL.WaitOne(4000);
            AddRelated(item.AssetUUID, item.AssetType);
            FindOrCreateAsset(item.AssetUUID, item.AssetType);
            return OSDSerializeMembers(item);
        }

        private OSDMap UnpackTaskObject(SimObject exportPrim, InventoryObject taskInv, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName, out bool missing)
        {
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
            if (showsMissingOnly)
            {
                needFiles++;
                Failure("NEED TASKOBJ: " + ItemDesc(taskInv, exportPrim));
                return OSDSerializeMembers(taskInv);
            }
            AutoResetEvent takeCopyEvent = new AutoResetEvent(false);
            AutoResetEvent rezedEvent = new AutoResetEvent(false);
            uint localID = 0;
            UUID objectID = UUID.Zero;
            UUID newItemID = UUID.Zero;
            AttachmentPoint origAttach = taskInv.AttachPoint;
            UUID objectFolder = FolderCalled(exportPrim.ID.ToString(), inventoryHolderUUID);

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
                if (e.CreatorID != taskInv.CreatorID || e.Type != taskInv.InventoryType || rItem.Name!=taskInv.Name)
                {
                    return;
                }                // if (inventoryHolderUUID != e.FolderID) return;
                newItemID = e.ItemID;
                takeCopyEvent.Set();
            };

            EventHandler<ObjectPropertiesEventArgs> rezedInWorld = (o, e) =>
            {
                if (e.Properties.ItemID != newItemID) return;
                objectID = e.Properties.ObjectID;
                rezedEvent.Set();
            };

            SimAvatarClient theAvatar = Client.TheSimAvatar;
            PermissionWho pw = theAvatar.EffectivePermissionWho(taskInv.OwnerID, taskInv.GroupID, taskInv.GroupOwned);
            PermissionMask pm = CogbotHelpers.PermMaskForWho(pw, taskInv.Permissions);
            PermissionMask pmo = CogbotHelpers.PermMaskForWho(pw, exportPrim.Properties.Permissions);
            bool canModify = Permissions.HasPermissions(pm, PermissionMask.Modify);
            bool canCopy = Permissions.HasPermissions(pm, PermissionMask.Copy);
            bool noCopyItem = !canCopy;
            bool canModifyObject = Permissions.HasPermissions(pmo, PermissionMask.Modify);
            if (noCopyItem)
            {
                if (!canModifyObject)
                {
                    missing = true;
                    Failure("Cant modify object to borrow out the nocopy object " + ItemDesc(taskInv, exportPrim));
                    return OSDSerializeMembers(taskInv);
                }
            }
            Client.Inventory.TaskItemReceived += copiedToInventory;

            Client.Inventory.MoveTaskInventory(exportPrim.LocalID, taskInv.UUID, objectFolder,
                                               exportPrim.GetSimulator());
            bool success = takeCopyEvent.WaitOne(TimeSpan.FromSeconds(10));
            Client.Inventory.TaskItemReceived -= copiedToInventory;
            if (!success)
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
            var newItem = Client.Inventory.Store[newItemID] as InventoryObject;

            Client.Objects.ObjectProperties += rezedInWorld;
            Client.Appearance.Attach(newItem, origAttach, true);

            success = rezedEvent.WaitOne(5000);
            Client.Objects.ObjectProperties -= rezedInWorld;
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
            }

            SimObject O = GetSimObjectFromUUID(objectID);
            if (O == null)
            {
                missing = true;
                Failure("Cant FIND taskinv object " + ItemDesc(taskInv, exportPrim));
                return OSDSerializeMembers(taskInv);
            }
            //folderObject.Add(O);
            Primitive prim = O.Prim;
            localID = localID > 0 ? localID : O.LocalID;
            Simulator simulator = O.GetSimulator();
            string taskInfo = "" + localID + "," + simulator.Handle + "," + objectID + "," + exportPrim.ID + "," +
                          taskInv.AssetUUID + "," + itemID;
            lock (fileWriterLock)
            {
                File.WriteAllText(repackFile, taskInfo);               
            }
            Client.Objects.DropObject(simulator, localID);
            // wait for drop
            DateTime waitUntil = DateTime.Now.AddSeconds(10);
            while (O.Prim.ParentID != 0 && DateTime.Now < waitUntil)
            {
                Thread.Sleep(250);
            }
            if (O.Prim.ParentID != 0)
            {
                Failure("Cant Drop! " + ItemDesc(taskInv, exportPrim) + " Obj=" + O);
            }
            Vector3 pos = O.SimPosition;
            Client.Objects.SetPosition(simulator, localID, exportPrim.SimPosition + (Vector3.UnitZ * 0.5f));
            Client.Objects.RequestObject(simulator, O.LocalID);
            waitUntil = DateTime.Now.AddSeconds(10);
            while (O.SimPosition == pos && DateTime.Now < waitUntil)
            {
                Thread.Sleep(250);
            }
            if (O.SimPosition == pos)
            {
                Failure("Cant Move! " + ItemDesc(taskInv, exportPrim) + " Obj=" + O);
            }
            if (IsSkipped(O))
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
            AutoResetEvent areKilled = new AutoResetEvent(false);
            EventHandler<KillObjectEventArgs> onKill = (s, e) =>
                                                           {
                                                               if (e.ObjectLocalID != localID) return;
                                                               areKilled.Set();
                                                           };
            Client.Objects.KillObject += onKill;
            if (noCopyItem)
            {
                // back to the TaskInv it came from
                Client.Inventory.RequestDeRezToInventory(localID, DeRezDestination.TaskInventory,
                                                             exportPrim.ID,
                                                             UUID.Random());
            }
            else
            {
                // delete it
                Client.Inventory.RequestDeRezToInventory(localID, DeRezDestination.AgentInventoryTake,
                                                         FolderCalled("TaskInvDeRez"),
                                                         UUID.Random());
            }
            bool wasKilled = areKilled.WaitOne(TimeSpan.FromSeconds(5));
            if (!wasKilled && noCopyItem)
            {
                //Take back to the Personal Inv
                Client.Inventory.RequestDeRezToInventory(localID, DeRezDestination.AgentInventoryTake,
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
                TaskItemComplete(itemID);
                lock (fileWriterLock)
                    File.WriteAllText(taskObjFile, taskInfo);
            }
            taskInvAssetUUID = taskInv.AssetUUID;
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
        private void UnpackTaskObjectP2(SimObject exportPrim, InventoryObject io, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName)
        {
            throw new NotImplementedException("UnpackTaskObjectP2");
            InventoryObject item = io;
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
            UUID itemID = item.UUID;
            Client.Inventory.MoveTaskInventory(exportPrim.LocalID, itemID, inventoryHolderUUID, exportPrim.GetSimulator());
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
            //foundObject.Add(O);
        }
        static public string ItemDesc(InventoryItem I, SimObject O)
        {
            return I.Name + "(" + I.AssetType + " " + I.AssetUUID + ")@" + named(O);
        }

        private void UnpackTaskObject0(SimObject exportPrim, InventoryObject taskInv, List<InventoryObject> folderObject, BotClient Client, OutputDelegate Failure, string primName)
        {
            throw new NotImplementedException("UnpackTaskObjectP2");
            folderObject.Add(taskInv);

            var itemID = taskInv.UUID;
            string exportFile = assetDumpDir + taskInv.AssetUUID + ".object";
            string repack = dumpDir + itemID + ".repack";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile) || File.Exists(repack)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED OBJ for " + named(exportPrim));
                return;
            }
            ExportTaskAsset ho;
            lock (TaskAssetWaiting)
                if (!TaskAssetWaiting.TryGetValue(itemID, out ho))
                {
                    TaskAssetWaiting[itemID] = ho = new ExportTaskAsset { I = taskInv, O = exportPrim };
                }
        }

        private string GetTaskInvFailures(OutputDelegate Failure, SimObject exportPrim, BotClient Client, List<InventoryObject> folderObject, string contents, string TaskInvFailures)
        {
            if (folderObject.Count > 0 && !taskobj)
            {
                // dont save it since we are skipping task objects
                string ObjectFailures = "Run with 'taskobj' for:\n" + contents + " for " + named(exportPrim) + "\n";
                TaskInvFailures += ObjectFailures;
                Failure(ObjectFailures);
                return TaskInvFailures;
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
            return TaskInvFailures;
        }
    }
}
