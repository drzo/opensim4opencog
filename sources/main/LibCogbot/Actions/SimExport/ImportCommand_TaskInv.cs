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
        private void ImportTaskFiles(ImportSettings importSettings, bool createObjects)
        {
            int incomplete = 1;
            int maxRetries = 10;
            while (incomplete > 0 && maxRetries-- > 0)
            {
                incomplete = ImportTaskFiles0(importSettings, createObjects);
                WriteLine("ImportTaskFiles Incomplete=" + incomplete);
            }
        }
        private int ImportTaskFiles0(ImportSettings importSettings, bool createObjects)
        {
            int incomplete = 0;
            var agentSyncFolderHolder = ExportCommand.Running.FolderCalled("TaskInvHolder");
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.task"))
            {
                string fileUUID = Path.GetFileNameWithoutExtension(Path.GetFileName(file));
                var ptc = APrimToCreate(UUID.Parse(fileUUID));
                if (ptc.TaskInvComplete) continue;
                string taskDataS = File.ReadAllText(file);
                if (string.IsNullOrEmpty(taskDataS))
                {
                    ptc.TaskInvComplete = true;
                    continue;
                }
                ptc.CreateWorkflow(agentSyncFolderHolder);
                if (!ptc.LoadOSD(taskDataS, WriteLine))
                {
                    Failure("FAILED: LoadOSD " + ptc);
                }
                if (!ptc.SyncToAgentFolder(WriteLine, createObjects))
                {
                    Failure("FAILED: SyncToAgentFolder " + ptc);
                }
                if (!ptc.SyncToObject(WriteLine, createObjects))
                {
                    Failure("FAILED: SyncToObject " + ptc);
                }
                if (!ptc.Complete)
                {
                    Failure("INComplete: " + ptc);
                    incomplete++;
                }
                else
                {
                    Success("COMPLETE: " + ptc);
                }
            }
            return incomplete;
        }

        public partial class PrimToCreate
        {
            internal List<InventoryBase> objectinventory;
            private ManualResetEvent taskInvRequestRelpy = null;
            private ulong _xferID;
            public List<InventoryBase> TaskInventory
            {
                get
                {
                    if (objectinventory == null)
                    {
                        if (!RequestNewTaskInventory().WaitOne(TimeSpan.FromSeconds(10)))
                        {
                            Running.WriteLine("Unable to retrieve TaskInv for " + ToString());
                        }
                    }
                    return objectinventory;
                }
            }

            public ManualResetEvent RequestNewTaskInventory()
            {
                objectinventory = null;
                taskInvRequestRelpy = taskInvRequestRelpy ?? new ManualResetEvent(false);
                taskInvRequestRelpy.Reset();
                var man = this.Inventory;
                man.TaskInventoryReply += ti_callback;
                man.RequestTaskInventory(NewLocalID);
                return taskInvRequestRelpy;

            }

            internal InventoryManager Inventory
            {
                get { return Client.Inventory; }
            }

            private void ti_callback(object sender, TaskInventoryReplyEventArgs e)
            {
                if (e.ItemID == NewID)
                {
                    InventoryManager man = Inventory;
                    String filename = e.AssetFilename;
                    man.TaskInventoryReply -= ti_callback;

                    if (!String.IsNullOrEmpty(filename))
                    {
                        Running.Client.Assets.XferReceived += xferCallback;

                        // Start the actual asset xfer
                        _xferID = Running.Client.Assets.RequestAssetXfer(filename, true, false, UUID.Zero, AssetType.Unknown,
                                                                 true);
                    }
                    else
                    {
                        Logger.DebugLog("Task is empty for " + NewID, Running.Client);
                        if (TaskInventoryLikely)
                        {
                            objectinventory = SimObjectImpl.ERROR_TASK_INV;
                        }
                        else
                        {
                            objectinventory = SimObjectImpl.EMPTY_TASK_INV;
                        }
                    }
                }
            }

            protected bool TaskInventoryLikely
            {
                get { return true; }
            }

            private void xferCallback(object sender, XferReceivedEventArgs e)
            {
                if (e.Xfer.XferID == _xferID)
                {
                    Running.Client.Assets.XferReceived -= xferCallback;
                    if (e.Xfer.Error != TransferError.None)
                    {
                        objectinventory = SimObjectImpl.ERROR_TASK_INV;
                        taskInvRequestRelpy.Set();
                        return;
                    }
                    String taskList = Utils.BytesToString(e.Xfer.AssetData);
                    objectinventory = InventoryManager.ParseTaskInventory(taskList);
                    taskInvRequestRelpy.Set();
                }
            }

            private int failed = 0;

            public BotClient Client
            {
                get { return Running.Client; }
            }

            public UUID AgentSyncFolder { get; set; }

            internal List<InventoryBase> _contents;
            public List<InventoryBase> AgentContents
            {
                get
                {
                    if (_contents == null)
                    {
                        _contents = Inventory.FolderContents(AgentSyncFolder, Client.Self.AgentID, false, true,
                                                                    InventorySortOrder.ByDate, 10000);

                        if (_contents == null) _contents = new List<InventoryBase>();
                    }
                    return _contents;
                }
            }

            public bool TaskInvComplete;
            public bool Complete
            {
                get
                {
                    if (TaskInvComplete) return true;
                    if (TaskItemsToCreate == null) return false;
                    foreach (var itemTask in TaskItemsToCreate)
                    {
                        if (!itemTask.FindTaskItem())
                        {
                            return false;
                        }
                    }
                    TaskInvComplete = true;
                    return true;
                }
            }

            public List<TaskItemToCreate> TaskItemsToCreate;
            public event Action TaskInvChanged;
            public short TaskSerial = -1;
            private void TaskInventoryItemReceived(object sender, ObjectPropertiesEventArgs e)
            {
                if (e.Properties.ObjectID != NewID) return;
                short newSerial = e.Properties.InventorySerial;
                if (TaskSerial < newSerial)
                {
                    objectinventory = null;
                    if (TaskInvChanged != null)
                    {
                        TaskInvChanged();
                    }
                }
                TaskSerial = newSerial;
            }

            public void CreateWorkflow(UUID agentSyncFolderHolder)
            {
                if (TaskItemsToCreate != null) return;
                TaskItemsToCreate = new List<TaskItemToCreate>();
                AgentSyncFolder = ExportCommand.Running.FolderCalled(OldID.ToString(), agentSyncFolderHolder);
                Client.Objects.ObjectProperties += TaskInventoryItemReceived;
            }

            public bool LoadOSD(string taskDataS, OutputDelegate WriteLine)
            {
                failed = 0;
                var taskData = OSDParser.DeserializeLLSDXml(taskDataS) as OSDArray;
                if (taskData == null)
                {
                    WriteLine("Cant read taskData: " + taskDataS);
                    return false;
                }

                if (taskData.Count == TaskItemsToCreate.Count) return true;
                // scan for existing source nodes
                foreach (OSDMap item in taskData)
                {
                    TaskItemsToCreate.Add(new TaskItemToCreate(this, item));
                }
                return failed == 0;
            }

            public bool SyncToAgentFolder(OutputDelegate WriteLine, bool createObjects)
            {
                failed = 0;
                // create missing source nodes
                foreach (var itemTask in TaskItemsToCreate)
                {
                    if (!itemTask.CreateAgentItem(WriteLine, createObjects))
                    {
                        failed++;
                        TaskInvComplete = false;
                    }
                }
                return failed == 0;
            }

            public bool SyncToObject(OutputDelegate WriteLine, bool createObjects)
            {
                failed = 0;
                foreach (var itemTask in TaskItemsToCreate)
                {
                    if (!itemTask.FindAgentItem())
                    {
                        WriteLine("FAILED FindAgentItem: " + itemTask);
                        failed++;
                        TaskInvComplete = false;
                        continue;
                    }
                    if (!itemTask.CreateTaskItem(WriteLine, createObjects))
                    {
                        WriteLine("FAILED CreateTaskItem: " + itemTask);
                        TaskInvComplete = false;
                        failed++;
                        continue;
                    }
                }
                return failed == 0;
            }
        }

        public class TaskItemToCreate
        {
            public override string ToString()
            {
                return AssetType + " " + OldAssetID + "  " + ItemName + "@" + CreatedPrim;
            }
            public readonly string ItemName;
            private UUID NewAgentItemID;
            public readonly OSDMap TaskOSD;
            public InventoryItem Item;
            public InventoryItem TaskItem;
            private readonly PrimToCreate CreatedPrim;
            public readonly AssetType AssetType;
            public UUID OldAssetID;
            private readonly UUID OldItemID;

            protected InventoryManager Inventory
            {
                get { return CreatedPrim.Inventory; }
            }

            public TaskItemToCreate(PrimToCreate ptc, OSDMap taskOSD)
            {
                CreatedPrim = ptc;
                TaskOSD = taskOSD;
                ItemName = taskOSD["Name"];
                AssetType = (AssetType)taskOSD["AssetType"].AsInteger();
                OldAssetID = taskOSD["AssetUUID"].AsUUID();
                OldItemID = taskOSD["UUID"].AsUUID();
            }
            public bool FindAgentItem()
            {
                if (Item != null) return true;
                foreach (InventoryBase content in CreatedPrim.AgentContents)
                {
                    if (content.Name == ItemName)
                    {
                        Item = content as InventoryItem;
                        if (Item != null) return true;
                    }
                }
                return false;
            }
            public bool FindTaskItem()
            {
                if (TaskItem != null) return true;
                List<InventoryBase> taskInv = CreatedPrim.TaskInventory;
                if (taskInv == null) return false;
                foreach (InventoryBase content in taskInv)
                {
                    if (content.Name == ItemName)
                    {
                        TaskItem = content as InventoryItem;
                        if (TaskItem != null) return true;
                    }
                }
                return false;
            }


            private ManualResetEvent areItem;
            private short oldTaskSerial;

            public bool CreateAgentItem(OutputDelegate WriteLine, bool createObjects)
            {
                if (FindAgentItem()) return true;

                if (AssetType == AssetType.Object)
                {
                    string taskFileName = ExportCommand.dumpDir + OldItemID + ".taskobj";
                    if (!File.Exists(taskFileName))
                    {
                        WriteLine("Cant restore Object asset: " + ToString());
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
                                WriteLine("Cant derez inventory object: " + ToString());
                                return false;                                
                            }
                            Inventory.ItemReceived -= AgentInventoryOnItemReceived;
                            SetItemFromOSD(Item);
                            this.Inventory.RequestUpdateItem(Item);

                            return FindAgentItem();
                        } else
                        {
                            WriteLine("Inner object not ready: " + innerObject);
                        }
                        return false;
                    }

                }

                if (CogbotHelpers.IsNullOrZero(OldAssetID))
                {
                    WriteLine("FAILED NO AssetID: " + this);
                    //skipped++;
                    return false;
                }
                ItemToCreate itc = Running.FindItemToCreate(OldAssetID, AssetType, false);

                areItem = areItem ?? new ManualResetEvent(false);
                areItem.Reset();
                Inventory.ItemReceived += AgentInventoryOnItemReceived;
                Inventory.RequestCopyItem(itc.NewItemID, CreatedPrim.AgentSyncFolder, ItemName, AgentInventoryOnItemReceived);
                if (!areItem.WaitOne(TimeSpan.FromSeconds(5)))
                {
                    WriteLine("Cant copy inventory asset: " + this);
                    return false;
                }
                SetItemFromOSD(Item);
                this.Inventory.RequestUpdateItem(Item);
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
                Item = item as InventoryItem;
                areItem.Set();
            }

            public bool CreateTaskItem(OutputDelegate WriteLine, bool createObjects)
            {
                if (FindTaskItem()) return true;

                if (!FindAgentItem())
                {
                    WriteLine("NULL FindAgentItem: " + this);
                    return false;
                }

                if (Item.InventoryType == InventoryType.Object)
                {
                    if (!createObjects) return true;
                }

                // Copy to Task
                SetItemFromOSD(Item);
                areItem = areItem ?? new ManualResetEvent(false);
                areItem.Reset();
                oldTaskSerial = CreatedPrim.TaskSerial;
                if (oldTaskSerial == -1)
                {
                    oldTaskSerial = CreatedPrim.Rezed.Properties.InventorySerial;
                }
                Client.Objects.ObjectProperties += TaskInventoryItemReceived;

                if (Item.InventoryType == InventoryType.LSL)
                {
                    Inventory.CopyScriptToTask(CreatedPrim.NewLocalID, (InventoryItem) Item, true);
                    Inventory.RequestSetScriptRunning(CreatedPrim.NewID, Item.AssetUUID, true);
                }
                else
                {
                    Inventory.UpdateTaskInventory(CreatedPrim.NewLocalID, (InventoryItem) Item);
                }
                if (areItem.WaitOne(TimeSpan.FromSeconds(5)))
                {
                    WriteLine("TIMEOUT: UpdateTask in " + ToString());
                }
                Client.Objects.ObjectProperties -= TaskInventoryItemReceived;
                var revent = CreatedPrim.RequestNewTaskInventory();
                if (!revent.WaitOne(TimeSpan.FromSeconds(10)))
                {
                    WriteLine("TIMEOUT: RequestNewTaskInventory in " + ToString());
                }
                return FindTaskItem();
            }

            private void TaskInventoryItemReceived(object sender, ObjectPropertiesEventArgs e)
            {
                if (e.Properties.ObjectID == CreatedPrim.NewID)
                {
                    if (oldTaskSerial + 1 == e.Properties.InventorySerial)
                    {
                        areItem.Set();
                    }
                }
            }

            private void SetItemFromOSD(InventoryItem item)
            {
                var fid = item.ParentUUID;
                var iid = item.UUID;
                OSD.SetObjectOSD(item, TaskOSD);
                ReplaceAllMembers(item, typeof(UUID), UUIDReplacer);
                item.ParentUUID = fid;
                item.UUID = iid;
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
        }
    }
}
