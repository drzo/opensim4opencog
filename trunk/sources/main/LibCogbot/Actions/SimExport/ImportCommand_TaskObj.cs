using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.IO;
using cogbot.Actions.SimExport;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.StructuredData;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{

    public partial class ImportCommand
    {
        public List<Action> KillClosures = new List<Action>();
        public HashSet<UUID> MustExport = new HashSet<UUID>();
        public HashSet<uint> MustExportUINT = new HashSet<uint>();

        public class TaskItemToCreate
        {
            public override string ToString()
            {
                return AssetType + " " + OldAssetID + (string.IsNullOrEmpty(Error) ? " " : " " + Error + " ") + ItemName +
                       "@" + CreatedPrim;
            }
            public override bool Equals(object obj)
            {
                var other = obj as TaskItemToCreate;
                return other != null && other.OldItemID == OldItemID;
            }
            public override int GetHashCode()
            {
                return OldItemID.GetHashCode();
            } 
            //private ExportTaskAsset Exporter;
            public string ItemName;
            private UUID NewAgentItemID;
            public OSDMap TaskItemOSD
            {
                get
                {
                    if (_taskOSD == null)
                    {
                        CreatedPrim.LoadTaskOSD(Running.WriteLine);
                    }
                    return _taskOSD;
                    
                }
                set
                {
                    _taskOSD = value;
                    if (value == null) return;
                    var taskOSD = value;
                    ItemName = taskOSD["Name"];
                    OSD assType = taskOSD["AssetType"];
                    if (assType.Type == OSDType.Unknown)
                    {
                        throw new NullReferenceException("" + taskOSD);
                    }
                    AssetType = (AssetType) assType.AsInteger();
                    oneAssetID = taskOSD["AssetUUID"].AsUUID();
                    OldItemID = taskOSD["UUID"].AsUUID();
                }
            }
            private OSDMap _taskOSD;
            private int _taskOSDNumber;

            public InventoryItem AgentItem;
            public InventoryItem SourceTaskItem;

            private readonly PrimToCreate CreatedPrim;
            public AssetType AssetType;
            public bool AlreadyMovedToTask = false;
            private UUID oneAssetID = UUID.Zero;
            public UUID OldAssetID
            {
                get
                {
                    return oneAssetID;
                }
                set
                {
                    if (CogbotHelpers.IsNullOrZero(value)) return;
                    TaskItemOSD = null;
                    oneAssetID = value;
                }
            }
            public UUID OldItemID;

            protected InventoryManager Inventory
            {
                get { return CreatedPrim.Inventory; }
            }

            public TaskItemToCreate(PrimToCreate ptc, OSDMap taskOSD)
            {
                CreatedPrim = ptc;
                TaskItemOSD = taskOSD;
            }
            public bool FindAgentItem()
            {
                if (AlreadyMovedToTask) return true;
                if (IsLocalScene)
                {
                    NewAssetID = OldAssetID;
                    return true;
                }
                if (AgentItem != null) return true;
                foreach (InventoryBase content in CreatedPrim.AgentContents)
                {
                    if (content.Name == ItemName)
                    {
                        AgentItem = content as InventoryItem;
                        if (AgentItem != null) return true;
                    }
                }
                return false;
            }
            public bool FindTaskItem()
            {
                if (SourceTaskItem != null) return true;
                List<InventoryBase> taskInv = CreatedPrim.sourceObjectinventory;
                if (taskInv == null) return false;
                foreach (InventoryBase content in taskInv)
                {
                    if (content.Name == ItemName)
                    {
                        var item = content as InventoryItem;
                        if (item == null) continue;
                        if (item.AssetType != AssetType) continue;
                        SourceTaskItem = item;
                        return true;
                    }
                }
                return false;
            }


            private ManualResetEvent areItem;
            private short oldTaskSerial;
            private UUID NewAssetID;
            public int NumRequests = 0;
            public String Error = "";
            public ManualResetEvent waiting;

            public bool CreateAgentItem(OutputDelegate WriteLine, bool createObjects)
            {
                if (FindAgentItem()) return true;
                if (IsLocalScene)
                {
                    NewAssetID = OldAssetID;
                    return true;
                }
                if (AssetType == AssetType.Object)
                {
                    string taskFileName = ExportCommand.dumpDir + OldItemID + ".taskobj";
                    if (!File.Exists(taskFileName))
                    {
                        WriteLine("ERROR: Cant restore Object asset: " + ToString());
                        return false;
                    }
                    if (!createObjects)
                    {
                        WriteLine("Skipping for now Object asset: " + ToString());
                        return true;
                    }
                    else
                    {
                        string taskFileContents = File.ReadAllText(taskFileName);
                        PrimToCreate innerObject = FindObject(taskFileContents, WriteLine);
                        if (innerObject.PackedInsideNow) return true;
                        if (innerObject.Complete)
                        {
                            // null for a refresh
                            CreatedPrim._contents = null;
                            // Remove World Object and PUT in AgentInvenory
                            areItem = areItem ?? new ManualResetEvent(false);
                            areItem.Reset();
                            Inventory.ItemReceived += AgentInventoryOnItemReceived;
                            Inventory.RequestDeRezToInventory(innerObject.NewLocalID, DeRezDestination.AgentInventoryTake,
                                                                     CreatedPrim.AgentSyncFolder, OldItemID);
                            if (!areItem.WaitOne(TimeSpan.FromSeconds(5)))
                            {
                                Inventory.ItemReceived -= AgentInventoryOnItemReceived;
                                WriteLine("ERROR: Cant derez inventory object: " + ToString());
                                return false;
                            }
                            Inventory.ItemReceived -= AgentInventoryOnItemReceived;
                            SetItemFromOSD(AgentItem, true);
                            this.Inventory.RequestUpdateItem(AgentItem);
                            innerObject.PackedInsideNow = true;
                            innerObject.SaveProgressFile();
                            return FindAgentItem();
                        }
                        else
                        {
                            WriteLine("ERROR: Inner object not ready: " + innerObject);
                        }
                        return false;
                    }

                }

                UUID NewItemID;
                if (CogbotHelpers.IsNullOrZero(OldAssetID))
                {
                    WriteLine("FAILED: NO AssetID: " + ToString());
                    //skipped++;
                    var scriptsUUID = Exporting.FolderCalled("Scripts");
                    InventoryItem missingItem = Exporting.GetInvItem(Client, "MissingItemScript", scriptsUUID);
                    NewItemID = missingItem.UUID;
                    NewAssetID = missingItem.AssetUUID;
                }
                else
                {
                    ItemToCreate itc = Running.FindItemToCreate(OldAssetID, AssetType, false);
                    NewItemID = itc.NewItemID;
                }
                areItem = areItem ?? new ManualResetEvent(false);
                areItem.Reset();
                //Inventory.ItemReceived += AgentInventoryOnItemReceived;
                Inventory.RequestCopyItem(NewItemID, CreatedPrim.AgentSyncFolder, ItemName, AgentInventoryOnItemReceived);
                if (!areItem.WaitOne(TimeSpan.FromSeconds(5)))
                {
                    //Inventory.ItemReceived -= AgentInventoryOnItemReceived;
                    WriteLine("FAILED: Cant copy inventory asset: " + this);
                    return false;
                }
                //Inventory.ItemReceived -= AgentInventoryOnItemReceived;
                NewAssetID = AgentItem.AssetUUID;
                SetItemFromOSD(AgentItem, true);
                this.Inventory.RequestUpdateItem(AgentItem);
                return true;
            }

            private void AgentInventoryOnItemReceived(object sender, ItemReceivedEventArgs e)
            {
                var item = e.Item;
                if (item.ParentUUID != CreatedPrim.AgentSyncFolder) return;
                if (item.Name != ItemName) return;
                AgentInventoryOnItemReceived(e.Item);
            }

            private void AgentInventoryOnItemReceived(InventoryBase item)
            {
                if (item.Name != ItemName) return;
                NewAgentItemID = item.UUID;
                AgentItem = item as InventoryItem;
                areItem.Set();
            }

            public bool CreateTaskItem(OutputDelegate WriteLine, bool createObjects)
            {
                if (FindTaskItem()) return true;

                if (!FindAgentItem())
                {
                    WriteLine("NULL: FindAgentItem: " + ToString());
                    return false;
                }

                if (AssetType == AssetType.Object && !IsLocalScene)
                {
                    if (!createObjects) return true;
                }

                if (IsLocalScene)
                {
                    NewAssetID = OldAssetID;
                    lock (CreatedPrim.TaskItemsToCreate)
                        if (!CreatedPrim.TaskItemsToCreate.Contains(this))
                            CreatedPrim.TaskItemsToCreate.Add(this);
                    return true;
                }
                // Copy to Task
                SetItemFromOSD(AgentItem, true);
                areItem = areItem ?? new ManualResetEvent(false);
                areItem.Reset();
                oldTaskSerial = CreatedPrim.TaskSerial;
                if (oldTaskSerial == -1)
                {
                    SimObject createdPrimRezed = CreatedPrim.Rezed;
                    if (createdPrimRezed == null)
                    {
                        WriteLine("TIMEOUT: TaskSerial in " + ToString());
                    }
                    else
                    {
                        oldTaskSerial = createdPrimRezed.Properties.InventorySerial;
                    }
                }
                Client.Objects.ObjectProperties += TaskInventoryItemReceived;

                if (AgentItem.InventoryType == InventoryType.LSL)
                {
                    Inventory.CopyScriptToTask(CreatedPrim.NewLocalID, (InventoryItem)AgentItem, true);
                    Inventory.RequestSetScriptRunning(CreatedPrim.NewID, AgentItem.AssetUUID, true);
                }
                else
                {
                    Inventory.UpdateTaskInventory(CreatedPrim.NewLocalID, (InventoryItem)AgentItem);
                }
                if (!areItem.WaitOne(TimeSpan.FromSeconds(5)))
                {
                    WriteLine("TIMEOUT: UpdateTask in " + ToString());
                }
                Client.Objects.ObjectProperties -= TaskInventoryItemReceived;
                var revent = CreatedPrim.RequestNewTaskInventory();
                bool timedOut = false;
                if (!revent.WaitOne(TimeSpan.FromSeconds(10)))
                {
                    timedOut = true;
                }
                bool found = FindTaskItem(); ;
                if (AssetType == AssetType.Object)
                {
                    AlreadyMovedToTask = found;
                }
                if (!found)
                {
                    if (timedOut) WriteLine("TIMEOUT: RequestNewTaskInventory in " + ToString());
                }
                else
                {
                    WriteLine("SUCCESS: Found " + ToString());
                }
                return found;
            }

            private void TaskInventoryItemReceived(object sender, ObjectPropertiesEventArgs e)
            {
                if (e.Properties.ObjectID == CreatedPrim.NewID)
                {
                    if (oldTaskSerial < e.Properties.InventorySerial)
                    {
                        areItem.Set();
                    }
                }
            }

            private void SetItemFromOSD(InventoryItem item, bool restoreItemMeta)
            {
                var fid = item.ParentUUID;
                var iid = item.UUID;
                var aid = item.AssetUUID;
                var rid = item.AssetUUID;
                var ast = item.AssetType;
                if(TaskItemOSD["typeosd"].AsString().Contains("Folder"))
                {
                    OSD.SetObjectOSD(item, TaskItemOSD);   
                }
                OSD.SetObjectOSD(item, TaskItemOSD);
                ReplaceAllMembers(item, typeof(UUID), UUIDReplacer, MissingFromExport);
                if (!restoreItemMeta) return;
                item.ParentUUID = fid;
                item.UUID = iid;
                item.AssetUUID = aid;
                item.RezzID = rid;
                item.AssetType = ast;
            }

            protected BotClient Client
            {
                get { return CreatedPrim.Client; }
            }

            public PrimToCreate FindObject(string contents, OutputDelegate WriteLine)
            {
                var splt = contents.Split(',');
                var oldID = UUID.Parse(splt[3]);
                return Running.GetOldPrim(oldID);
            }

            public InventoryItem ToInventoryBase(out bool improvement, bool improve)
            {
                improvement = false;
                //OSDMap UnpackTaskItem(BotClient Client, SimObject exportPrim, InventoryItem item, OutputDelegate Failure, out bool missing)
                if (SourceTaskItem == null || CogbotHelpers.IsNullOrZero(SourceTaskItem.AssetUUID))
                {
                    if (SourceTaskItem == null)
                    {
                        int pn;
                        SourceTaskItem = InventoryManager.CreateInventoryItem(
                            ItemToCreate.AssetTypeToInventoryType(this.AssetType, out pn), OldItemID);
                        if (SourceTaskItem == null)
                        {
                            SourceTaskItem = new InventoryItem(OldItemID);
                        }
                        SetItemFromOSD(SourceTaskItem, false);
                    }
                    if (!improve) return SourceTaskItem;
                    if (CogbotHelpers.IsNullOrZero(OldAssetID))
                    {
                        if (SourceTaskItem.AssetType == AssetType.LSLText && !Exporting.settings.Contains("tasklsl"))
                        {
                            //RFailure("Use tasklsl for " + this);
                            return SourceTaskItem;
                        }
                        if (SourceTaskItem.AssetType != AssetType.Object)
                        {
                            if (false) ExportCommand.LogError(CreatedPrim.OldID,
                                                    "ASSET ZERO " + ToString() + " " + SourceTaskItem.Name + "/" +
                                                    SourceTaskItem.Description);
                            if (NumRequests < 3 && string.IsNullOrEmpty(Error))
                            {
                                waiting = new ManualResetEvent(false);
                                Request();
                                if (!waiting.WaitOne(TimeSpan.FromSeconds(1)))
                                {
                                    ExportCommand.LogError(CreatedPrim.OldID,
                                                           "ASSET ZERO1 " + ToString() + " " + SourceTaskItem.Name + "/" +
                                                           SourceTaskItem.Description);
                                }
                                else
                                {
                                    if (!string.IsNullOrEmpty(Error))
                                    {
                                        ExportCommand.LogError(CreatedPrim.OldID,
                                                               "ASSET ERROR " + ToString() + " " + SourceTaskItem.Name + "/" +
                                                               SourceTaskItem.Description);
                                        CreatedPrim.TaskInvComplete = false;
                                    }
                                    else
                                    {
                                        improvement = string.IsNullOrEmpty(Error) && !CogbotHelpers.IsNullOrZero(OldAssetID);
                                    }
                                }
                            }
                            else
                            {
                                ExportCommand.LogError(CreatedPrim.OldID,
                                                       "ASSET ZERO2 " + ToString() + " " + SourceTaskItem.Name + "/" +
                                                       SourceTaskItem.Description);
                            }
                            /* Running.Client.Assets.RequestInventoryAsset(_item.AssetUUID, _item.UUID, _item.ParentUUID, _item.OwnerID,
                                                                        _item.AssetType, true, Asset_Received);*/


                            //    OldAssetID = GetMissingFiller(_item.AssetType);
                        }
                    }
                    if (!CogbotHelpers.IsNullOrZero(OldAssetID)) SourceTaskItem.AssetUUID = OldAssetID;
                    if (CogbotHelpers.IsNullOrZero(SourceTaskItem.RezzID))
                    {
                        if (SourceTaskItem.AssetType != AssetType.Object)
                        {
                            SourceTaskItem.RezzID = SourceTaskItem.AssetUUID; 
                        } else
                        {
                            waiting = new ManualResetEvent(false);
                            RequestRezObject();
                            if (!waiting.WaitOne(TimeSpan.FromSeconds(1)))
                            {
                                ExportCommand.LogError(CreatedPrim.OldID,
                                                       "ASSET ZERO1 " + ToString() + " " + SourceTaskItem.Name + "/" +
                                                       SourceTaskItem.Description);
                            }
                        }
                    }
                }
                return SourceTaskItem;
            }

            public void Asset_Received(AssetDownload trans, Asset asset)
            {
                var Running = Exporting;
                var item = SourceTaskItem;
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
                            if (waiting != null) waiting.Set();
                            return;
                        }
                    }
                    if (waiting != null) waiting.Set();
                    return;
                }
                Error = "";
                TaskItemOSD = null;
                OldAssetID = asset.AssetID;
                if (SourceTaskItem != null) SourceTaskItem.AssetUUID = asset.AssetID;
                //AddRelated(item.AssetUUID, item.AssetType);
                if (false)
                {
                    Running.Assets_OnReceived(trans, asset);
                    Running.TaskItemComplete(CreatedPrim.OldID, itemID, asset.AssetID, asset.AssetType);
                }
                if (waiting != null) waiting.Set();
            }

            public void Request()
            {
                if (!string.IsNullOrEmpty(Error))
                {
                    if (waiting != null) waiting.Set();
                    return;
                }
                var Running = Exporting;
                InventoryItem item = SourceTaskItem;
                UUID itemID = item.UUID;
                if (AssetType == AssetType.Object)
                {
                    RequestRezObject();
                    return;
                    //UnpackTaskObject
                }
                if (!Exporting.settings.Contains("tasklsl"))
                {
                  //  RFailure("Use tasklsl for " + this);
                    return;
                }
                if (item.AssetType == AssetType.LSLText || item.AssetType == AssetType.Notecard)
                {
                    //Perhaps copy to AgentInventory first
                }
                Running.Client.Assets.RequestInventoryAsset(item.AssetUUID, itemID, CreatedPrim.OldID, item.OwnerID,
                                                            item.AssetType, true, Asset_Received);
                NumRequests++;
            }

            private void RequestRezObject()
            {
                RequestRezObject0();
                if (waiting != null) waiting.Set();
            }
            private void RequestRezObject0()
            {
                var Running = Exporting;
                InventoryItem item = SourceTaskItem;
                UUID itemID = item.UUID;
                if (!CogbotHelpers.IsNullOrZero(item.RezzID))
                {
                    return;
                }
                var tof = ExportCommand.dumpDir + itemID + ".taskobj";
                if (File.Exists(tof))
                {
                    OldAssetID = SourceTaskItem.RezzID = UUID.Parse(File.ReadAllText(tof).Split(',')[2]);
                    return;
                }

                if (NoCopyItem)
                {
                    if (!Running.settings.Contains("nctaskobjs"))
                    {
                        ExportCommand.LogError(CreatedPrim.OldID, "NoCopy TaskObj: " + this);
                        return;
                    }
                }
                var Incremental = Exporting.Incremental;
                var showsMissingOnly = Exporting.showsMissingOnly;
                string repackFile = ExportCommand.dumpDir + itemID + ".repack";
                if (File.Exists(repackFile) || File.Exists(tof))
                {
                    missing = false;
                    return;
                }
                if (!Exporting.settings.Contains("reztaskobjs") /*|| CreatedPrim.OldID.ToString() != "8a11c67d-edd3-d0b9-b4ec-c4e8f13f6875"*/)
                {
                    RFailure("Use reztaskobjs for " + this);
                    return;
                }

                var exportPrim = CreatedPrim.Rezed;
                if (exportPrim == null)
                {
                    uint exportLocalID = CreatedPrim.Prim.LocalID;
                    Client.Objects.RequestObject(Running.settings.CurSim, exportLocalID);
                    exportPrim = WorldObjects.GetSimObjectFromUUID(CreatedPrim.OldID);
                }
                if (exportPrim == null)
                {
                    if (CreatedPrim.IsAsset)
                    {
                        //return;
                    }
                    //Running.AttemptMoveTo(CreatedPrim.SimPosition);
                    RFailure("Cant get to export prim for " + this);
                    return;
                }
                Error = "";
                failures = new StringWriter();
                uint localID = exportPrim.LocalID;
                var sim = exportPrim.GetSimulator();
                Client.Objects.SelectObject(sim, localID);
                Running.MoveCloseTo(exportPrim);
                UUID newObjID;
                var settings = Running.settings;
                ExportCommand.IsExporting = true;
                UnpackTaskObject(settings, item as InventoryObject, RFailure, true, out newObjID);
                if (missing)
                {
                    Error = failures.ToString();
                    ExportCommand.LogError(CreatedPrim.OldID, "OBJMISSING: " + this + " " + Error.ToString().Replace('\n', ' '));
                }
                Client.Objects.DeselectObject(sim, localID);
                if (!CogbotHelpers.IsNullOrZero(newObjID))
                {
                    //TaskItemOSD["RezzID"] = newObjID;
                    OldAssetID = newObjID;
                    if (SourceTaskItem != null) SourceTaskItem.RezzID = newObjID;
                }
            }


            StringWriter failures = new StringWriter();
            
            private void RFailure(string s, params object[] args)
            {
                failures.WriteLine(s, args);
                Running.Failure(DLRConsole.SafeFormat(s, args));
            }

            internal void UnpackTaskObject(ImportSettings arglist, InventoryObject invObject,
                OutputDelegate Failure, bool dotaskobj, out UUID newAssetID)
            {
                var Incremental = Exporting.Incremental;
                var showsMissingOnly = Exporting.showsMissingOnly;

                var itemID = invObject.UUID;
                string exportFile = ExportCommand.dumpDir + OldAssetID + ".object";
                string taskObjFile = ExportCommand.dumpDir + itemID + ".taskobj";
                string repackFile = ExportCommand.dumpDir + itemID + ".repack";

                foreach (string file in new[] {exportFile, taskObjFile, repackFile})
                {
                    if (Incremental || showsMissingOnly)
                    {
                        if (File.Exists(file))
                        {
                            missing = false;
                            string[] conts = File.ReadAllText(taskObjFile).Split(',');
                            newAssetID = UUID.Parse(conts[2]);
                            return;
                        }
                    }
                }

                newAssetID = OldAssetID;

                if (showsMissingOnly || !dotaskobj)
                {
                    Exporting.needFiles++;
                    Failure("NEED TASKOBJ: " + this);
                    missing = true;
                    return;
                }

                UUID newItemID = UUID.Zero;

                if (File.Exists(repackFile))
                {
                    string[] conts = File.ReadAllText(repackFile).Split(',');
                    OldAssetID = newAssetID = UUID.Parse(conts[2]);
                    PostRezzed();
                    return;
                }
                if (RezzedO == null)
                {
                    //UUID objectID = UUID.Zero;
                    if (NoCopyItem)
                    {
                        var exportPrim = CreatedPrim.Rezed;
                        PermissionWho pwo = Running.TheSimAvatar.EffectivePermissionWho(exportPrim);
                        PermissionMask pmo = CogbotHelpers.PermMaskForWho(pwo, exportPrim.Properties.Permissions);
                        bool canModifyObject = Permissions.HasPermissions(pmo, PermissionMask.Modify);
                        if (!canModifyObject && !Exporting.settings.Contains("nctaskobjs"))
                        {
                            missing = true;
                            Failure("Cant modify object to borrow out the nocopy object " + this);
                            return;
                        }
                    }

                    if (CogbotHelpers.IsNullOrZero(OldAssetID))
                    {
                        EnsureAgentItem();
                        if (AgentItem == null)
                        {
                            EnsureAgentItem();
                            if (AgentItem == null)
                            {
                                Failure("Cant get agent Item");
                                return;
                            }
                        }
                        Client.Objects.ObjectProperties += rezedInWorld;
                        Error = "Awaiting Attach";
                        Client.Appearance.Attach(AgentItem, invObject.AttachPoint, true);
                        bool success = rezedEvent.WaitOne(TimeSpan.FromMinutes(1));
                        Client.Objects.ObjectProperties -= rezedInWorld;
                        newAssetID = OldAssetID;
                        if (!success)
                        {
                            missing = true;
                            Failure("CANT ATTACH taskinv object " + this);
                            if (NoCopyItem)
                            {
                                //Move back from Personal Inventory to TaskInv
                                if (AgentItem != null)
                                {
                                    var exportPrim = CreatedPrim.Rezed;
                                    Client.Inventory.UpdateTaskInventory(exportPrim.LocalID, AgentItem);
                                    return;
                                }
                                else
                                {
                                    Failure("Couldnt find it " + this);
                                }
                            }
                            return;
                        }
                        else
                        {
                            missing = false;
                        }
                    }
                    newAssetID = OldAssetID;
                    RezzedO = ExportCommand.GetSimObjectFromUUID(OldAssetID);
                }
                if (RezzedO == null)
                {
                    missing = true;
                    Failure("Cant FIND taskinv object " + this);
                    return;
                }
                PostRezzed();
            }

            private bool calledPostRez = false;
            void PostRezzed() {
                lock (WorkFlowLock)
                {
                    if (calledPostRez) return;
                    calledPostRez = true;
                }

                var invObject = SourceTaskItem;
                var itemID = SourceTaskItem.UUID;
                var Failure = (OutputDelegate)Running.WriteLine;
                if (CogbotHelpers.IsNullOrZero(OldAssetID))
                {
                    OldAssetID = RezzedO.ID;
                }
                //folderObject.Add(O);
                Exporting.TasksRezed[OldAssetID] = this;

                RezzedO = ExportCommand.GetSimObjectFromUUID(OldAssetID);
                if (RezzedO == null)
                {
                    RezzedO = ExportCommand.GetSimObjectFromUUID(OldAssetID);
                }
                if (RezzedO == null)
                {
                    RezzedO = ExportCommand.GetSimObjectFromUUID(OldAssetID);
                }
                if (RezzedO == null)
                {
                    RezzedO = ExportCommand.GetSimObjectFromUUID(OldAssetID);
                }

                Primitive prim = RezzedO.Prim;
                uint unpackedLocalID = RezzedO.LocalID;
                Running.MustExportUINT.Add(unpackedLocalID);

                Simulator simulator = RezzedO.GetSimulator();
                string taskInfo = "" + unpackedLocalID + "," + simulator.Handle + "," + OldAssetID + "," +
                                  CreatedPrim.OldID + "," + invObject.AssetUUID + "," + itemID + "," +
                                  NoCopyItem.ToString() + "," + ToString();
                string taskObjFile = ExportCommand.dumpDir + itemID + ".taskobj";
                string repackFile = ExportCommand.dumpDir + itemID + ".repack";
                string objectAssetFile = ExportCommand.dumpDir + RezzedO.ID.ToString() + ".objectAsset";
                lock (ExportCommand.fileWriterLock)
                {
                    File.WriteAllText(repackFile, taskInfo);
                    File.WriteAllText(objectAssetFile, taskInfo);
                }

                //taskInvAssetUUID = taskInv.AssetUUID = objectID;
                bool needsDrop = prim.ParentID != 0;
                Vector3 newPos = CreatedPrim.SimPosition + (Vector3.UnitZ * 0.5f);
                if (needsDrop)
                {
                    Client.Objects.DropObject(simulator, unpackedLocalID);
                    // wait for drop
                    DateTime waitUntil = DateTime.Now.AddSeconds(60);
                    while (prim.ParentID != 0 && DateTime.Now < waitUntil)
                    {
                        Thread.Sleep(250);
                    }
                    if (prim.ParentID != 0)
                    {
                        Failure("Cant Drop! " + this + " Obj=" + RezzedO);
                    }
                    Vector3 pos = RezzedO.SimPosition;

                    Client.Objects.SetPosition(simulator, unpackedLocalID, newPos);
                    Exporting.AddMoveTo(CreatedPrim.SimPosition);
                    Client.Objects.RequestObject(simulator, unpackedLocalID);
                    waitUntil = DateTime.Now.AddSeconds(10);
                    while (RezzedO.SimPosition == pos && DateTime.Now < waitUntil)
                    {
                        Thread.Sleep(250);
                    }
                    if (RezzedO.SimPosition == pos)
                    {
                        Failure("Cant Move! " + this + " Obj=" + RezzedO);
                    }
                } else
                {
                    Exporting.AddMoveTo(prim.Position);
                }
                if (ExportCommand.IsSkipped(RezzedO, Exporting.settings))
                {
                    Failure("IsSkipped " + RezzedO);
                }
                int saved = Exporting.LocalFailures;
                Exporting.LocalFailures = 0;
                var pda = Exporting.PrimDepsAssets;
                //CreatedPrim.SetIsAsset();

                AddKill(unpackedLocalID, simulator, repackFile, Failure);
                invObject.RezzID = OldAssetID;
                Exporting.TaskItemComplete(CreatedPrim.OldID, itemID, OldAssetID, invObject.AssetType);
                lock (ExportCommand.fileWriterLock) File.WriteAllText(taskObjFile, taskInfo);
                if (!CogbotHelpers.IsNullOrZero(OldAssetID))
                {
                    lock (ExportCommand.fileWriterLock)
                        File.WriteAllText(ExportCommand.dumpDir + OldAssetID + ".object",
                                          taskInfo + "," + OldAssetID);
                }
            }

            private void AddKill(uint unpackedLocalID, Simulator simulator, string repackFile, OutputDelegate Failure)
            {
                Running.KillClosures.Add(() => KillMade(Failure, unpackedLocalID, simulator, repackFile));
            }

            private void KillMade(OutputDelegate Failure, uint unpackedLocalID, Simulator simulator, string repackFile)
            {
                var areKilled = new ManualResetEvent(false);
                EventHandler<KillObjectEventArgs> onKill = (s, e) =>
                                                               {
                                                                   if (e.ObjectLocalID != unpackedLocalID) return;
                                                                   areKilled.Set();
                                                               };
                Client.Objects.KillObject += onKill;
                if (NoCopyItem)
                {
                    // back to the TaskInv it came from
                    Client.Inventory.RequestDeRezToInventory(unpackedLocalID, DeRezDestination.TaskInventory,
                                                             CreatedPrim.OldID, UUID.Random());
                }
                else
                {
                    // delete it
                    Client.Inventory.RequestDeRezToInventory(unpackedLocalID, DeRezDestination.AgentInventoryTake,
                                                             Exporting.FolderCalled("TaskInvDeRez"), UUID.Random());
                }
                bool wasKilled = areKilled.WaitOne(TimeSpan.FromMinutes(1));
                Client.Objects.KillObject -= onKill;
                if (!wasKilled && NoCopyItem)
                {
                    //Take back to the Personal Inv
                    Client.Inventory.RequestDeRezToInventory(unpackedLocalID, DeRezDestination.AgentInventoryTake,
                                                             AgentSyncFolder, UUID.Random());

                    wasKilled = areKilled.WaitOne(TimeSpan.FromSeconds(10));

                    //Move from Personal Inventory to TaskInv
                    var invItem = Exporting.GetInvItem(Client, ItemName, AgentSyncFolder);
                    if (invItem != null)
                    {
                        var exportPrim = CreatedPrim.Rezed;
                        Client.Inventory.UpdateTaskInventory(exportPrim.LocalID, invItem);
                    }
                    else
                    {
                        Failure("Couldnt find it " + RezzedO);
                    }
                }
                if (!wasKilled)
                {
                    missing = true;
                    Failure("Could not kill temp object " + RezzedO);
                }
                else
                {
                    lock (ExportCommand.fileWriterLock) File.Delete(repackFile);
                    missing = false;
                }
            }

            private void EnsureAgentItem()
            {
                if (AgentItem == null)
                {
                    var invObject = SourceTaskItem;
                    var conts = Client.Inventory.FolderContents(AgentSyncFolder, Exporting.TheSimAvatar.ID, false, true,
                                                                InventorySortOrder.ByDate, 10000);
                    //  InventoryObject AgentItem = null;
                    if (conts != null)
                    {
                        foreach (InventoryBase cont in conts)
                        {
                            InventoryItem it = cont as InventoryItem;
                            if (it == null) continue;
                            if (cont.Name == invObject.Name && it.AssetType == AssetType.Object)
                            {
                                AgentItem = it as InventoryObject;
                                return;
                            }
                        }
                    }

                    Inventory.TaskItemReceived += copiedToInventory;

                    Inventory.MoveTaskInventory(CreatedPrim.OldLocalID, invObject.UUID, AgentSyncFolder,
                                                CreatedPrim.GetSimulator());
                    bool successTC = takeCopyEvent.WaitOne(TimeSpan.FromSeconds(30));
                    Client.Inventory.TaskItemReceived -= copiedToInventory;
                    if (!successTC)
                    {
                        missing = true;
                        RFailure("Cant MOVE taskinv object " + this);
                        if (NoCopyItem)
                        {
                            //Move back from Personal Inventory to TaskInv (In case)
                            var invItem = Exporting.GetInvItem(Client, invObject.Name, AgentSyncFolder);
                            if (invItem != null)
                            {
                                Client.Inventory.UpdateTaskInventory(CreatedPrim.OldLocalID, invItem);
                            }
                            else
                            {
                                RFailure("Couldnt find it " + this);
                            }
                        }
                    }
                }
            }

            ManualResetEvent takeCopyEvent = new ManualResetEvent(false);
            private void copiedToInventory(object sender, TaskItemReceivedEventArgs e)
            {
                if (e.FolderID != AgentSyncFolder)
                {
                    return;
                }
                if (e.AssetID != OldAssetID && !CogbotHelpers.IsNullOrZero(e.AssetID) &&
                    !CogbotHelpers.IsNullOrZero(OldAssetID))
                {
                    return;
                }
                UUID newItemID = e.ItemID;
                var rItem = Client.Inventory.Store[newItemID];
                if (e.Type != SourceTaskItem.InventoryType || (rItem != null && rItem.Name != ItemName))
                {
                    return;
                }

                AgentItem = Client.Inventory.Store[newItemID] as InventoryObject;
                takeCopyEvent.Set();
            }

            protected bool NoCopyItem
            {
                get
                {
                    SimAvatarClient theAvatar = Client.TheSimAvatar;
                    PermissionWho pw = theAvatar.EffectivePermissionWho(SourceTaskItem.OwnerID, SourceTaskItem.GroupID, SourceTaskItem.GroupOwned);
                    PermissionMask pm = CogbotHelpers.PermMaskForWho(pw, SourceTaskItem.Permissions);
                    bool canModify = Permissions.HasPermissions(pm, PermissionMask.Modify);
                    bool canCopy = Permissions.HasPermissions(pm, PermissionMask.Copy);
                    if (canCopy) return false;
                    return true;
                }
            }

            protected UUID AgentSyncFolder
            {
                get
                {
                    return CreatedPrim.AgentSyncFolder;
                }
            }

            ManualResetEvent rezedEvent = new ManualResetEvent(false);
            private SimObject RezzedO;
            private bool missing;

            private void rezedInWorld(object o, ObjectPropertiesEventArgs e)
            {
                var iie = e.Properties.ItemID;
                if (CogbotHelpers.IsNullOrZero(iie)) return;
                if ((AgentItem == null || iie != AgentItem.UUID) && (SourceTaskItem == null || iie != SourceTaskItem.UUID))
                {
                    return;
                }
                Error = "";
                Client.Objects.ObjectProperties -= rezedInWorld;
                OldAssetID = e.Properties.ObjectID;
                Running.MustExport.Add(OldAssetID);
                ExportCommand.Exporting.TasksRezed[oneAssetID] = this;
                PostRezzed();
                rezedEvent.Set();
            }
        }

        private void OnObjectPropertiesNewesh(object sender, PrimEventArgs e)
        {
            Primitive prim = e.Prim;
            lock (MustExportUINT)
            {
                if (MustExportUINT.Contains(prim.ParentID))
                {
                    MustExportUINT.Add(prim.LocalID);
                    lock (MustExport) MustExport.Add(prim.ID);
                }
            }
            lock (MustExportUINT)
            {
                if (MustExportUINT.Contains(prim.LocalID))
                {
                    MustExport.Add(prim.ID);
                }
            }
        }

        private void CleanupPrims(ImportSettings settings)
        {
            lock(KillClosures)
            {
                foreach (Action action in LockInfo.CopyOf(KillClosures))
                {
                    try
                    {
                        action.Invoke();
                        KillClosures.Remove(action);
                    }
                    catch (Exception ee)
                    {

                    }
                }
            }
        }

        private void DoTodo(ImportSettings settings)
        {
            foreach (UUID mustExport in LockInfo.CopyOf(MustExport))
            {
                if (!MissingLINK(mustExport) && !MissingLLSD(mustExport) && !MissingTASK(mustExport))
                {
                    if (APrimToCreate(mustExport).EnsureTaskInv(true))
                    {
                        lock (MustExport)
                        {
                            MustExport.Remove(mustExport);
                            continue;
                        }
                    }
                }
                var o = ExportCommand.GetSimObjectFromUUID(mustExport);
                settings.Add("llsd");
                settings.Add("task");
                settings.Add("taskobj");
                settings.Add("dep");
                settings.Add("links");
                bool tlsls = settings.Contains("tasklsl");
                settings.Add("tasklsl");
                if (o!=null)Exporting.ExportPrim(Client, o, WriteLine, settings);
            }
        }

    }
}
