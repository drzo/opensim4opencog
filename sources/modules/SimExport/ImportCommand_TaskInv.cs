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
            foreach (PrimToCreate toCreate in parents)
            {
                toCreate.EnsureTaskInv(false);
            }
            foreach (PrimToCreate toCreate in childs)
            {
                toCreate.EnsureTaskInv(false);                
            }
        }
        private int ImportTaskFiles0(ImportSettings importSettings, bool createObjects)
        {
            //if (IsLocalScene) return 0;
            DateTime lastProgressNotice = DateTime.Now;
            int incomplete = 0;
            var agentSyncFolderHolder = Exporting.FolderCalled("TaskInvHolder");
            int created = 0;
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.task"))
            {
                string fileUUID = Path.GetFileNameWithoutExtension(Path.GetFileName(file));
                var ptc = APrimToCreate(UUID.Parse(fileUUID));
                if (++created % 25 == 0) WriteLine("tasked " + created);
                if (ptc.PackedInsideNow) continue;
              //  if (ptc.TaskInvComplete) continue;
                if ((ptc.Prim.RegionHandle != importSettings.CurSim.Handle) && importSettings.Contains("CheckRegion"))
                {
                    KillID(ptc.OldID);
                    continue;
                }
                string taskDataS = File.ReadAllText(file);
                if (string.IsNullOrEmpty(taskDataS) || taskDataS.Length < 30)
                {
                    ptc.TaskInvComplete = true;
                    continue;
                }
                ptc.CreateWorkflow(agentSyncFolderHolder);
                if (!ptc.LoadTaskOSD(WriteLine))
                {
                    //Failure("FAILED: LoadOSD " + ptc);
                    incomplete++;
                    continue;
                }
                if (!ptc.SyncToAgentFolder(WriteLine, createObjects))
                {
                    //Failure("FAILED: SyncToAgentFolder " + ptc);
                    if (ptc.succeeded == 0) continue;
                }
                if (!ptc.SyncToObject(WriteLine, createObjects))
                {
                    //Failure("FAILED: SyncToObject " + ptc);
                    if (ptc.succeeded == 0) continue;
                }
                if (!ptc.Complete)
                {
                    Failure("INCOMPLETE: " + ptc);
                    incomplete++;
                }
                else
                {
                    Success("COMPLETE: " + ptc);
                }
                if (!IsLocalScene) Success("............");
                if (lastProgressNotice.AddSeconds(30) < DateTime.Now)
                {
                    lastProgressNotice = DateTime.Now;
                    WriteLine("Task created " + created + " incomplete=" + incomplete);
                }
            }
            return incomplete;
        }

        public partial class PrimToCreate
        {
            internal List<InventoryBase> sourceObjectinventory;
            private ManualResetEvent taskInvRequestRelpy = null;
            private ulong _xferID;
            public List<InventoryBase> SourceTaskInventory(bool useCache)
            {
               // get
                {
                    List<InventoryBase> regenObjInv = useCache ? this.sourceObjectinventory : null;
                    bool improve = !useCache;
                    if (regenObjInv == null)
                    {
                        if (IsLocalScene)
                        {
                            if (TaskItemsToCreate == null)
                            {
                                
                                return null;
                            }
                            bool improvementM = false;
                            if (sourceObjectinventory != null)
                            {
                                //return objectinventory;
                            }
                            regenObjInv = new List<InventoryBase>();
                            lock (TaskItemsToCreate)
                            {
                                foreach (var toCreate in LockInfo.CopyOf(TaskItemsToCreate))
                                {
                                    bool improvement;
                                    var item = toCreate.ToInventoryBase(out improvement, improve);
                                    if (improvement)
                                    {
                                        improvementM = true;
                                    }
                                    regenObjInv.Add(item);
                                }
                                AssignObjectNums();
                            }
                            if (improvementM)
                            {
                                Exporting.SaveTaskOSD(OldID, regenObjInv);
                            }
                            return sourceObjectinventory = regenObjInv;
                        }
                        if (!RequestNewTaskInventory().WaitOne(TimeSpan.FromSeconds(10)))
                        {
                            Importing.WriteLine("Unable to retrieve TaskInv for " + ToString());
                        }
                    }
                    return sourceObjectinventory = regenObjInv;
                }
            }

            public ManualResetEvent RequestNewTaskInventory()
            {
                sourceObjectinventory = null;
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
                        Importing.Client.Assets.XferReceived += xferCallback;

                        // Start the actual asset xfer
                        _xferID = Importing.Client.Assets.RequestAssetXfer(filename, true, false, UUID.Zero, AssetType.Unknown,
                                                                 true);
                    }
                    else
                    {
                        Logger.DebugLog("Task is empty for " + NewID, Importing.Client);
                        if (TaskInventoryLikely)
                        {
                            sourceObjectinventory = SimObjectImpl.ERROR_TASK_INV;
                        }
                        else
                        {
                            sourceObjectinventory = SimObjectImpl.EMPTY_TASK_INV;
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
                    Importing.Client.Assets.XferReceived -= xferCallback;
                    if (e.Xfer.Error != TransferError.None)
                    {
                        sourceObjectinventory = SimObjectImpl.ERROR_TASK_INV;
                        taskInvRequestRelpy.Set();
                        return;
                    }
                    String taskList = Utils.BytesToString(e.Xfer.AssetData);
                    sourceObjectinventory = InventoryManager.ParseTaskInventory(taskList);
                    taskInvRequestRelpy.Set();
                }
            }

            public int succeeded = 0;
            public int failed = 0;

            public BotClient Client
            {
                get { return Importing.Client; }
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

            public bool TaskInvComplete
            {
                get
                {
                    return _taskInvComplete;
                }
                set
                {
                    _taskInvComplete = value;
                    if (value)
                    {
                        if (_contents == null) _contents = new List<InventoryBase>();
                        if (sourceObjectinventory == null) sourceObjectinventory = new List<InventoryBase>();
                        if (TaskItemsToCreate == null) TaskItemsToCreate = new List<TaskItemToCreate>();
                    }
                    SaveProgressFile();
                }
            }
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

            protected Simulator CurSim
            {
                get { return Exporting.settings.CurSim; }
            }

            public List<TaskItemToCreate> TaskItemsToCreate;
            public event Action TaskInvChanged;
            public short TaskSerial = -1;
            private bool _taskInvComplete;

            private void TaskInventoryItemReceived(object sender, ObjectPropertiesEventArgs e)
            {
                if (e.Properties.ObjectID != NewID) return;
                short newSerial = e.Properties.InventorySerial;
                if (TaskSerial < newSerial)
                {
                    sourceObjectinventory = null;
                    if (TaskInvChanged != null)
                    {
                        TaskInvChanged();
                    }
                }
                TaskSerial = newSerial;
            }

            public void CreateWorkflow(UUID agentSyncFolderHolder)
            {
                string taskFile = ExportCommand.dumpDir + OldID + ".task";
                if (File.Exists(taskFile))
                {
                    string taskDataS = File.ReadAllText(taskFile);
                    if (string.IsNullOrEmpty(taskDataS) || taskDataS.Length < 30)
                    {
                        sourceObjectinventory = new List<InventoryBase>();
                        TaskItemsToCreate = new List<TaskItemToCreate>();
                        TaskInvComplete = true;
                        return;
                    }
                }
                if (CogbotHelpers.IsNullOrZero(AgentSyncFolder))
                {
                    AgentSyncFolder = Exporting.FolderCalled(OldID.ToString(), agentSyncFolderHolder);
                }
                if (TaskItemsToCreate != null) return;
                TaskItemsToCreate = new List<TaskItemToCreate>();
                if (IsLocalScene) return;
                
                Client.Objects.ObjectProperties += TaskInventoryItemReceived;
            }

            public int TaskObjectCount = -1;
            public bool LoadTaskOSD(OutputDelegate WriteLine)
            {
                failed = 0;
                succeeded = 0;
                string taskFile = ExportCommand.dumpDir + OldID + ".task";
                string taskDataS = File.ReadAllText(taskFile);
                if (string.IsNullOrEmpty(taskDataS) || taskDataS.Length < 30)
                {
                    TaskInvComplete = true;
                    return true;
                }
                var taskData = OSDParser.DeserializeLLSDXml(taskDataS) as OSDArray;
                if (taskData == null)
                {
                    WriteLine("ERROR: Cant read taskData: " + taskDataS);
                    return false;
                }

                lock (TaskItemsToCreate) if (taskData.Count == TaskItemsToCreate.Count && taskData.Count > 0)
                {
                    succeeded = taskData.Count;
                    return true;
                }
                // scan for existing source nodes
                lock (TaskItemsToCreate)
                {
                    foreach (OSDMap item in taskData)
                    {
                        if (item["AssetType"].Type == OSDType.Unknown)
                        {
                            continue;
                        }
                        var itemID = item["UUID"].AsUUID();
                        TaskItemToCreate titc = null;

                        foreach (TaskItemToCreate itemToCreate in TaskItemsToCreate)
                        {
                            if (itemToCreate.OldItemID == itemID)
                            {
                                titc = itemToCreate;
                                break;
                            }
                        }
                        if (titc == null)
                        {
                            titc = new TaskItemToCreate(this, item);
                            TaskItemsToCreate.Add(titc);
                        }
                        else
                        {
                            titc.TaskItemOSD = item;
                        }
                        AssignObjectNums();
                        succeeded++;
                    }
                }
                return failed == 0;
            }

            public void AssignObjectNums()
            {
                TaskObjectCount = 0;
                foreach (TaskItemToCreate toCreate in TaskItemsToCreate)
                {
                    if (toCreate.AssetType == AssetType.Object)
                    {
                        toCreate.ObjectNum = TaskObjectCount;
                        TaskObjectCount++;
                    }
                    
                }
            }

            public void UnpackRTI()
            {
                if (TaskObjectCount < 1) return;
                if (!MissingRTI(OldID)) return;
                if (RTIRequested) return;
                RTIRequested = true;
                lock (ExportCommand.fileWriterLock) File.WriteAllText(ExportCommand.dumpDir + OldID + ".0.rti", "requested");
                Exporting.AttemptMoveTo(SimPosition);
                lock (Importing.ExportHolder) Importing.ExportHolder.Add(OldID);
                Thread.Sleep(3000);
                if (!Exporting.PutItemToTaskInv(Client, OldLocalID, _rezed , "SimExportUnpackCopy"))
                {
                    lock (ExportCommand.fileWriterLock)
                    {
                        string nomod = ExportCommand.dumpDir + OldID + ".rti_status"; 
                        if (!File.Exists(nomod))
                        {
                            File.WriteAllText(nomod, "nomod");
                            this.MustUseAgentCopy = true;
                        }
                    }
                }
                Thread.Sleep(3000);
            }
            public bool SyncToAgentFolder(OutputDelegate WriteLine, bool createObjects)
            {
                failed = 0;
                succeeded = 0;
                // create missing source nodes
                lock (TaskItemsToCreate) foreach (var itemTask in TaskItemsToCreate)
                {
                    if (!itemTask.CreateAgentItem(WriteLine, createObjects))
                    {
                        failed++;
                        TaskInvComplete = false;
                    }
                    else
                    {
                        succeeded++;
                    }
                }
                return failed == 0;
            }

            public bool SyncToObject(OutputDelegate WriteLine, bool createObjects)
            {
                failed = 0;
                bool invComplete = true;

                lock (TaskItemsToCreate) foreach (var itemTask in TaskItemsToCreate)
                {
                    if (!itemTask.FindAgentItem())
                    {
                        WriteLine("FAILED FindAgentItem: " + itemTask);
                        failed++;
                        invComplete = false;
                        continue;
                    }
                    if (!itemTask.CreateTaskItem(WriteLine, createObjects))
                    {
                        WriteLine("FAILED CreateTaskItem: " + itemTask);
                        invComplete = false;
                        failed++;
                        continue;
                    }
                    succeeded++;
                }
                if (invComplete) TaskInvComplete = true;
                return failed == 0;
            }

            public bool IsAsset = false;
            internal bool RTIRequested;
            public bool MustUseAgentCopy;

            public void SetIsAsset()
            {
                IsAsset = true;
                if (!IsLinkParent) return;
                foreach (PrimToCreate c in Childs)
                {
                    c.SetIsAsset();
                }
            }

            public bool EnsureTaskInv(bool useCache)
            {
                if (File.Exists(ExportCommand.assetDumpDir + OldID + ".object")) return true;

                var o = Rezed;
                if (MissingTASK(OldID) && o != null)
                {
                    var taskinv = o.TaskInventory;
                    if (taskinv == null) return false;
                    Exporting.SaveTaskOSD(OldID, taskinv);
                }
                string taskFile = ExportCommand.dumpDir + OldID + ".task";
                if (!File.Exists(taskFile)) return false;
                string taskDataS = File.ReadAllText(taskFile);
                if (string.IsNullOrEmpty(taskDataS) || taskDataS.Length < 30)
                {
                    sourceObjectinventory = new List<InventoryBase>();
                    TaskItemsToCreate = new List<TaskItemToCreate>();
                    TaskInvComplete = true;
                    return true;
                }
                var agentSyncFolderHolder = Exporting.FolderCalled("TaskInvHolder");
                CreateWorkflow(agentSyncFolderHolder);
                LoadTaskOSD(Importing.WriteLine);
                var ti = SourceTaskInventory(useCache);
                foreach (InventoryBase i in ti)
                {
                    InventoryItem item = i as InventoryItem;
                    if (item == null) continue;
                    if (CogbotHelpers.IsNullOrZero(item.AssetUUID)) return false;
                }
                return true;
            }
        }
    }
}
