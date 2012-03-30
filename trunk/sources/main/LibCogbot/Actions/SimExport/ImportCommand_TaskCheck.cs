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
        public class TaskFileInfo
        {
            public bool IsPlaced = false;
            public PrimToCreate Rezzed
            {
                get
                {
                    return Importing.GetOldPrim(RezzedID);
                }
            }
            public PrimToCreate InsideOf
            {
                get
                {
                    return Importing.GetOldPrim(OldTaskHolder);
                }
            }
            public override bool Equals(object obj)
            {
                TaskFileInfo tob = obj as TaskFileInfo;
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
            public UUID OldTaskHolder = UUID.Zero;
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

            public bool missingLLSD;
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
                TaskFileInfo to = new TaskFileInfo()
                {
                    OldLid = uint.Parse(c[0]),
                    RezzedID = UUID.Parse(c[2]),
                    OldTaskHolder = UUID.Parse(c[3]),
                    AssetUUID = UUID.Parse(c[4]),
                    TaskItemID = UUID.Parse(c[5])
                };
                tos.Add(to);
                created++;
            }
            foreach (TaskFileInfo o in tos)
            {
                var r = o.Rezzed;
                if (r != null) r.SetIsAsset();
            }
            
            foreach (TaskFileInfo o in tos)
            {
                PrimToCreate oInsideOf = o.InsideOf;
                if (oInsideOf == null) continue;
                List<InventoryBase> taskInv = oInsideOf.SourceTaskInventory(true);
                if (taskInv == null) continue;
                foreach (var b in taskInv)
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
            foreach (TaskFileInfo o in tos)
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

        private void CheckTasks(ImportSettings settings)
        {
            foreach (PrimToCreate parent in parents)
            {
                var ls = new Linkset()
                {
                    Parent = parent
                };
                parent.Link = ls;
                LocalScene.Links.Add(ls);
            }
            foreach (PrimToCreate ch in childs)
            {
                var pp = ch.ParentPrim;
                if (pp == null)
                {
                    continue;
                }
                pp.Link.ChildAdd(ch);
            }
            foreach (var ls in LockInfo.CopyOf(LocalScene.Links))
            {
                ls.Children.Sort(compareLocalIDs);
                if (ls.Parent.IsAsset)
                {
                    LocalScene.AssetLinks.Add(ls);
                    LocalScene.Links.Remove(ls);
                }
            }

            int found = 0;
            Dictionary<UUID, TaskFileInfo> item2TO = new Dictionary<UUID, TaskFileInfo>();
            List<TaskFileInfo> mssingTO = new List<TaskFileInfo>();
            List<TaskFileInfo> duped = new List<TaskFileInfo>();
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.objectAsset"))
            {
                found++;
                string fileString = File.ReadAllText(file);
                string[] c = fileString.Split(',');
                TaskFileInfo to = new TaskFileInfo()
                {
                    OldLid = uint.Parse(c[0]),
                    RezzedID = UUID.Parse(c[2]),
                    OldTaskHolder = UUID.Parse(c[3]),
                    AssetUUID = UUID.Parse(c[4]),
                    TaskItemID = UUID.Parse(c[5])
                };
                bool missing = false;
                if (MissingLLSD(to.RezzedID))
                {
                    Failure("Need LLSD: " + fileString);
                    to.missingLLSD = true;
                    mssingTO.Add(to);
                }
                TaskFileInfo old;
                if (item2TO.TryGetValue(to.TaskItemID, out old))
                {
                    if (old.missingLLSD)
                    {
                        item2TO[to.TaskItemID] = to;
                        duped.Add(old);
                    }
                    else
                    {
                        duped.Add(to);
                    }
                }
                else
                {
                    if (to.missingLLSD)
                    {
                        continue;
                    }
                    item2TO[to.TaskItemID] = to;
                }

            }
            Success("t=" + found + " m=" + mssingTO.Count + " td=" + item2TO.Count + " duped=" + duped.Count);
        }
    }
}
