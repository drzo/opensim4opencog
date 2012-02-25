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
            lock (Running.TaskAssetWaiting) Running.TaskAssetWaiting.Remove(itemID);
            lock (Running.CompletedTaskItem) Running.CompletedTaskItem.Add(itemID);
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
            if (Running.CompletedTaskItem.Contains(itemID))
            {
                Running.TaskAssetWaiting.Remove(itemID);
                return;
            }
            lock(Running.TaskAssetWaiting)
            {
                if (!Running.TaskAssetWaiting.ContainsKey(itemID))
                {
                    if (!Running.CompletedTaskItem.Contains(itemID))
                    {
                        Running.CompletedTaskItem.Add(itemID);
                        return;
                    }
                    return;
                }
            }
            Running.Client.Assets.RequestInventoryAsset(item.AssetUUID, itemID, O.ID, item.OwnerID,
                                                        item.AssetType, true, Asset_Received);
            NumRequests++;
        }
    }

    public class ExportTaskObject
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

    public partial class ExportCommand : Command, RegionMasterCommand
    {
        public readonly Dictionary<UUID, ExportTaskAsset> TaskAssetWaiting = new Dictionary<UUID, ExportTaskAsset>();
        public readonly Dictionary<UUID, ExportTaskObject> TasksRezed = new Dictionary<UUID, ExportTaskObject>();
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
                lock (TaskAssetWaiting)
                {
                    ExportTaskAsset ho;
                    TaskAssetWaiting.TryGetValue(taskInv.UUID, out ho);
                    TaskAssetWaiting.Remove(taskInv.UUID);
                    lock (CompletedTaskItem) CompletedTaskItem.Add(taskInv.UUID);
                }
                WaitingFolderSimObject = GetSimObjectFromUUID(objid);
                WaitingFolderObjectBool = false;
                return;
            }
        }

        private bool checkTaskPerm(SimObject exportPrim, InventoryItem item, BotClient Client, OutputDelegate Failure, bool mustModify)
        {
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
            //if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED TASK for " + named(exportPrim));
                return;
            }
            exportPrim.StartGetTaskInventory();
            bool mightHaveTaskInv = false;
            const PrimFlags maybeScriptsInside = PrimFlags.AllowInventoryDrop | PrimFlags.Scripted | PrimFlags.Touch;
            if ((maybeScriptsInside & exportPrim.Prim.Flags) != 0)
            {
                mightHaveTaskInv = true;
            }
            else
            {
                var props = exportPrim.Properties;

                mightHaveTaskInv = (exportPrim.Prim.ClickAction == ClickAction.Sit)
                                   || !string.IsNullOrEmpty(props.SitName)
                                   || !string.IsNullOrEmpty(props.TouchName);
            }
            
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

            string contents = "";
            List<SimObject> foundObject = new List<SimObject>();
            List<InventoryObject> folderObject = new List<InventoryObject>();

            string TaskInvFailures = "";
            foreach (InventoryBase b in ib)
            {
                bool missing;
                string was = SaveEachTaskItem(Client, exportPrim, b, Failure, folderObject, out missing);
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

        string SaveEachTaskItem(BotClient Client, SimObject exportPrim, InventoryBase b, OutputDelegate Failure, List<InventoryObject> folderObject, out bool missing)
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
            bool exportable = checkTaskPerm(exportPrim, item, Client, Failure, false);
            lock (TaskAssetWaiting)
                lock (CompletedTaskItem)
                {
                    UUID itemID = item.UUID;
                    if (CompletedTaskItem.Contains(itemID))
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
            return UnpackTaskItem(Client, exportPrim, (InventoryItem)b, Failure, itemEntry, out missing);
        }
        string UnpackTaskItem(BotClient Client, SimObject exportPrim, InventoryItem item, OutputDelegate Failure, string itemEntry, out bool missing)
        {
            UUID itemID = item.UUID;
            if (CompletedTaskItem.Contains(itemID))
            {
                missing = false;
                return itemEntry;
            }
            ExportTaskAsset ho;
            lock (TaskAssetWaiting) if (!TaskAssetWaiting.TryGetValue(itemID, out ho))
            {
                TaskAssetWaiting[itemID] = ho = new ExportTaskAsset { I = item, O = exportPrim };
                missing = true;
                // return itemEntry;
            }
            if (!CogbotHelpers.IsNullOrZero(item.AssetUUID) && CompletedAssets.Contains(item.AssetUUID))
            {
                TaskAssetWaiting.Remove(itemID);
                CompletedTaskItem.Add(itemID);
                missing = false;
                return itemEntry;
            }
            AddRelated(item.AssetUUID, item.AssetType);

            SlowlyDo(ho.Request);
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
            ExportTaskAsset ho = new ExportTaskAsset { I = taskInv, O = exportPrim };

            lock (TaskAssetWaiting)
                TaskAssetWaiting.Add(taskInv.UUID, ho);
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
            var tiobj = new ExportTaskObject()
            {
                Task = taskInv,
                Inv = newItem,
                WasInside = exportPrim,
                ObjectID = objectID,
                LocalID = localID,
                LiveVersion = O,
            };
            TasksRezed[taskInv.UUID] = tiobj;
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
            foundObject.Add(O);
        }
        static public string ItemDesc(InventoryItem I, SimObject O)
        {
            return I.Name + "(" + I.AssetType + " " + I.AssetUUID + ")@" + named(O);
        }

    }
}
