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
        private void ImportTaskFiles()
        {
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.task"))
            {
                string fileUUID = Path.GetFileNameWithoutExtension(Path.GetFileName(file));
                var ptc = GetOldPrim(UUID.Parse(fileUUID));
                string taskDataS = File.ReadAllText(file);
                if (string.IsNullOrEmpty(taskDataS))
                {
                    ptc.TaskInvComplete = true;
                    continue;
                }
                var tihh = ExportCommand.Running.FolderCalled("TaskInvHolder");
                var tih = ExportCommand.Running.FolderCalled(fileUUID, tihh);
                List<InventoryBase> contents = Client.Inventory.FolderContents(tih, Client.Self.AgentID, false, true,
                                                               InventorySortOrder.ByDate, 10000);

                var taskData = OSDParser.DeserializeLLSDXml(taskDataS) as OSDArray;
                if (taskData == null)
                {
                    WriteLine("Cant read taskData: " + taskDataS); 
                    continue;
                }
                int created = 0;
                int failed = 0;
                int skipped = 0;
                List<TaskItemToCreate> taskItemsToCreate = new List<TaskItemToCreate>();
                foreach (OSDMap item in taskData)
                {
                    string itemName = item["Name"];
                    taskItemsToCreate.Add(new TaskItemToCreate()
                                              {
                                                  TaskOSD = item
                                              });
                    InventoryItem found = null;
                    foreach (InventoryBase content in contents)
                    {
                        if (content.Name == itemName)
                        {
                            found = content as InventoryItem;
                            break;
                        }
                    }
                }
#if FALSE
                foreach (var item in taskItemsToCreate)
                {
    
                    //if (string.IsNullOrEmpty(item)) continue;
                    //string[] fields = item.Split(',');
                    // From 
                    //  item.UUID + "," + item.AssetType + "," + item.AssetUUID + "," + item.OwnerID + "," + item.GroupID + "," + item.GroupOwned + "," + item.Permissions.ToHexString() + "," + item.Name + "\n";
                    AssetType assetType = (AssetType)item["AssetType"].AsInteger();
                    UUID itemID = item["UUID"];
                    UUID assetID = item["AssetUUID"];
                    Permissions perms = Permissions.FromOSD(item["Permissions"]);
                    string itemDebug = itemName + " " + assetType + " " + itemID + " " + assetID; 
                    if (CogbotHelpers.IsNullOrZero(assetID))
                    {
                        WriteLine("Cant create Item: " + itemDebug);
                        skipped++;
                        continue;
                    }
                    string sfile = ExportCommand.assetDumpDir + Path.GetFileName(SimAsset.CFileName(assetID, assetType));
                    if (!File.Exists(sfile))
                    {
                        WriteLine("Cant find asset: " + itemDebug);
                        skipped++;
                        continue;
                    }
                    if (assetType == AssetType.Object)
                    {
                        WriteLine("Cant create Object asset: " + itemDebug);
                        skipped++;
                        continue;
                    }
                    byte[] data = File.ReadAllBytes(sfile);

                    ItemToCreate itc = FindItemToCreate(assetID, assetType, false);
                    UUID newAssetID;
                    UUID newItemID = null;
                    string ErrorMsg = "";
                    AutoResetEvent areItem = new AutoResetEvent(false);
                    InventoryManager.ItemCreatedFromAssetCallback CreatedAsset0 =
                        (suc, st, itemid, assetid) =>
                        {
                            if (!suc) ErrorMsg = st;
                            newItemID = itemid;
                            newAssetID = assetid;
                            areItem.Set();
                        };
                    /*                     
                    Client.Inventory.RequestCreateItemFromAsset(data, itemName, item, assetType,
                                                                ItemToCreate.AssetTypeToInventoryType(assetType),
                                                                tih, perms, CreatedAsset0);
                     */
                    InventoryBase copy = null;
                    InventoryManager.ItemCopiedCallback IBHUPdate = (newItem) =>
                    {
                        newItemID = newItem.UUID;
                        copy = newItem;
                        areItem.Set();
                    };
                    Client.Inventory.RequestCopyItem(itc.NewItemID, tih, itemName, IBHUPdate);
                    //UUID itcNewItemID = itc.NewItemID;
                    if (!areItem.WaitOne(TimeSpan.FromSeconds(10)))
                    {
                        WriteLine("Cant copy inventory asset: " + itemDebug);
                        failed++;
                        continue;
                    }
                    var invItem = copy as InventoryItem;
                    if (invItem == null)
                    {
                        WriteLine("NULL inventory asset: " + itemDebug);
                        failed++;
                        continue;
                    }
                    //Client.Inventory.TaskItemReceived += tir;
                    if (invItem.InventoryType == InventoryType.LSL)
                    {
                        Client.Inventory.CopyScriptToTask(ptc.NewLocalID, (InventoryItem)invItem, true);
                        Client.Inventory.RequestSetScriptRunning(ptc.NewID, invItem.AssetUUID, true);
                    }
                    else
                    {
                        Client.Inventory.UpdateTaskInventory(ptc.NewLocalID, (InventoryItem)invItem);
                    }
                    created++;
                }
#endif
            }
        }

        private class TaskItemToCreate
        {
            public OSDMap TaskOSD;
        }
    }
}
