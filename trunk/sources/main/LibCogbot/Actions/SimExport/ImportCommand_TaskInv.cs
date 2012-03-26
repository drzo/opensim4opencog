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
        public class TaskObject
        {
            public bool IsPlaced = false;
            public PrimToCreate Rezzed
            {
                get
                {
                    return Running.GetOldPrim(RezzedID);
                }
            }
            public PrimToCreate InsideOf
            {
                get
                {
                    return Running.GetOldPrim(OldParent);
                }
            }
            public override bool Equals(object obj)
            {
                TaskObject tob = obj as TaskObject;
                return tob != null && tob.TaskItemID == TaskItemID;
            }
            public override int GetHashCode()
            {
                return TaskItemID.GetHashCode();
            }
            public override string ToString()
            {
                return "TaskObj " + Rezzed + " " + TaskItemID + "/" + AssetUUID + " inside " + InsideOf;
            }
            public uint OldLid = 0;
            /// <summary>
            /// Prim it was inside
            /// </summary>
            public UUID OldParent = UUID.Zero;
            /// <summary>
            /// When we rezed it - therefore the LLSD File
            /// </summary>
            public UUID RezzedID = UUID.Zero;
            /// <summary>
            /// AssetUUID (found in TaskInv)
            /// </summary>
            public UUID AssetUUID = UUID.Zero;
            /// <summary>
            /// ItemID (folder Item in TaskInv)
            /// </summary>
            public UUID TaskItemID = UUID.Zero;
        }
        public void ImportTaskObjects(ImportSettings importSettings)
        {
            DateTime lastProgressNotice = DateTime.Now;
            int incomplete = 0;
            var agentSyncFolderHolder = Exporting.FolderCalled("TaskInvHolder");
            int created = 0;
            var tos = LocalScene.TaskObjects;
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.taskobj"))
            {
                string[] c = File.ReadAllText(file).Split(',');
                TaskObject to = new TaskObject()
                                    {
                                        OldLid = uint.Parse(c[0]),
                                        RezzedID = UUID.Parse(c[2]),
                                        OldParent = UUID.Parse(c[3]),
                                        AssetUUID = UUID.Parse(c[4]),
                                        TaskItemID = UUID.Parse(c[5])
                                    };
                tos.Add(to);
                created++;
            }
            foreach (TaskObject o in tos)
            {
                var r = o.Rezzed;
                if (r != null) r.SetIsAsset();
            }
            foreach (TaskObject o in tos)
            {
                foreach(var b in o.InsideOf.SourceTaskInventory(true))
                {
                    InventoryItem i = b as InventoryItem;
                    if (i == null) continue;
                    if (CogbotHelpers.IsNullOrZero(i.AssetUUID))
                    {
                        if (i.UUID == o.TaskItemID)
                        {
                            i.RezzID = o.RezzedID;
                            o.IsPlaced = true;
                            break;
                        }
                    }
                }
            }
            foreach (TaskObject o in tos)
            {
                if (!o.IsPlaced)
                {
                    Failure("UNPLACED: " + o);
                }
            }
            // this makes sure we know that late found childs are assets
            foreach (PrimToCreate parent in parents)
            {
                if (parent.IsAsset) parent.SetIsAsset();
            }
        }
        public void DivideTaskObjects(ImportSettings importSettings)
        {
        }

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
                            }
                            if (improvementM)
                            {
                                Exporting.SaveTaskOSD(OldID, regenObjInv);
                            }
                            return sourceObjectinventory = regenObjInv;
                        }
                        if (!RequestNewTaskInventory().WaitOne(TimeSpan.FromSeconds(10)))
                        {
                            Running.WriteLine("Unable to retrieve TaskInv for " + ToString());
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
                    Running.Client.Assets.XferReceived -= xferCallback;
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

            public bool TaskInvComplete
            {
                get
                {
                    return _taskInvComplete;
                }
                set
                {
                    _taskInvComplete = value;
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
                if (CogbotHelpers.IsNullOrZero(AgentSyncFolder))
                {
                    AgentSyncFolder = Exporting.FolderCalled(OldID.ToString(), agentSyncFolderHolder);
                }
                if (TaskItemsToCreate != null) return;
                TaskItemsToCreate = new List<TaskItemToCreate>();
                if (IsLocalScene) return;
                
                Client.Objects.ObjectProperties += TaskInventoryItemReceived;
            }

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

                lock (TaskItemsToCreate) if (taskData.Count == TaskItemsToCreate.Count)
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
                        succeeded++;
                    }
                }
                return failed == 0;
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
                var agentSyncFolderHolder = Exporting.FolderCalled("TaskInvHolder");
                CreateWorkflow(agentSyncFolderHolder);

                string taskFile = ExportCommand.dumpDir + OldID + ".task";
                if (!File.Exists(taskFile)) return false;
                string taskDataS = File.ReadAllText(taskFile);
                if (string.IsNullOrEmpty(taskDataS) || taskDataS.Length < 30)
                {
                    TaskInvComplete = true;
                    return true;
                }
                LoadTaskOSD(Running.WriteLine);
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
