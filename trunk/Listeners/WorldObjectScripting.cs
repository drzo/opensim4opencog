using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;

namespace cogbot.Listeners
{

    public partial class WorldObjects : DebugAllEvents
    {

        public static bool ScriptHolderPrecreated = true;
        private Primitive ScriptHolder = null;
        private bool ScriptHolderAttached = false;
        private InventoryItem ScriptHolderItem = null;
        private readonly AutoResetEvent ScriptHolderAttachWaiting = new AutoResetEvent(true);


        public override void Self_OnScriptControlChange(ScriptControlChange controls, bool pass, bool take)
        {
            base.Self_OnScriptControlChange(controls, pass, take);
        }

        public override void Self_OnScriptSensorReply(UUID requestorID, UUID groupID, string name, UUID objectID, UUID ownerID, Vector3 position, float range, Quaternion rotation, ScriptSensorTypeFlags type, Vector3 velocity)
        {
            base.Self_OnScriptSensorReply(requestorID, groupID, name, objectID, ownerID, position, range, rotation, type, velocity);
        }

        public override void Inventory_OnTaskInventoryReply(UUID itemID, short serial, string assetFilename)
        {
            base.Inventory_OnTaskInventoryReply(itemID, serial, assetFilename);
        }

        /// <summary>
        /// eval (thisClient.WorldSystem.GetScriptHolder "ScriptHolder")
        /// eval (thisClient.WorldSystem.ExecuteLSL "llOwnerSay(llGetObjectName());")
        /// </summary>
        /// <param name="s"></param>
        public void ExecuteLSL(String s)
        {
            LoadLSL(String.Format("default {{ state_entry() {{ {0} }} }}", s));
        }


 

        readonly static List<InventoryItem> TempScripts = new List<InventoryItem>();
        public void LoadLSL(string script)
        {

            InventoryItem scriptItem = null;
            lock (TempScripts)
            {
                scriptItem = NewInventoryScript(script, "tempScript" + TempScripts.Count);
                TempScripts.Add(scriptItem);
            }
            ScriptHolderItem = GetInventoryObject("ScriptHolder");
            if (ScriptHolder==null)
            {
                ScriptHolder = GetScriptHolder("ScriptHolder");
            }

            // we save this to clean out the object later of running scripts
            ShutdownHooks.Add(() =>
                                  {
                                      Simulator sim = GetSimulator(ScriptHolder.RegionHandle);
                                      client.Inventory.RemoveItem(scriptItem.UUID);
                                      List<InventoryBase> ibs = client.Inventory.GetTaskInventory(ScriptHolder.ID, 
                                          ScriptHolder.LocalID, 10000);
                                      foreach (InventoryBase ib in ibs)
                                      {
                                          if (ib.Name.StartsWith("tempScript"))
                                          {
                                              client.Inventory.RemoveTaskInventory(ScriptHolder.LocalID, ib.UUID, sim);
                                          }
                                      }
                                  }
                );


            // eval (thisClient.WorldSystem.ExecuteLSL "llOwnerSay(llGetObjectName());")
            UUID Transaction = client.Inventory.CopyScriptToTask(ScriptHolder.LocalID, scriptItem,true);
            Thread.Sleep(5000);
            client.Inventory.SetScriptRunning(ScriptHolder.ID, scriptItem.AssetUUID, true);
            return;
            Thread.Sleep(5000);
            if (ScriptHolderAttached)
            {
                client.Appearance.Detach(ScriptHolder.ID);
            }
            client.Appearance.Attach(ScriptHolderItem.UUID, ScriptHolderItem.OwnerID,
                         ScriptHolderItem.Name, ScriptHolderItem.Description,
                         ScriptHolderItem.Permissions,
                         ScriptHolderItem.Flags,
                         AttachmentPoint.HUDTop);
            //client.Appearance.Detach(prim.LocalID);
            //todo clean these up afterwards? or is this too soon?
            //client.Inventory.RemoveItem(scriptItem.UUID);
        }

        public Primitive GetScriptHolder(string primName)
        {
            if (ScriptHolder != null) return ScriptHolder;
            lock (ScriptHolderAttachWaiting)
            {
                if (ScriptHolder != null) return ScriptHolder;

                ScriptHolderItem = GetInventoryObject(primName);
                ScriptHolderAttachWaiting.Reset();
                if (!ScriptHolderAttached)
                {
                    if (ScriptHolderItem != null)
                    {
                        client.Appearance.Attach(ScriptHolderItem.UUID, ScriptHolderItem.OwnerID,
                                                 ScriptHolderItem.Name, ScriptHolderItem.Description,
                                                 ScriptHolderItem.Permissions,
                                                 ScriptHolderItem.Flags,
                                                 AttachmentPoint.HUDTop);
                    }
                }

                if (ScriptHolder != null) return ScriptHolder;

                if (ScriptHolderAttachWaiting.WaitOne(10000, true))
                {
                    return ScriptHolder;
                }
                Debug("Timed out waiting for script holder " + ScriptHolderItem);
                return ScriptHolder;
            }
        }

        /// <summary>
        /// first look in our folder then the the shared folder
        /// </summary>
        /// <param name="primName"></param>
        /// <returns></returns>
        public InventoryItem GetInventoryObject(string primName)
        {
            return EvalOnFolders(client.Inventory.Store.RootFolder, item =>
                ((item.AssetType == AssetType.Object || item.InventoryType == InventoryType.Object) && item.Name == primName))
                ??
                EvalOnFolders(client.Inventory.Store.LibraryFolder, item =>
                    ((item.AssetType == AssetType.Object || item.InventoryType == InventoryType.Object) && item.Name == primName));
        }


        public InventoryItem EvalOnFolders(InventoryFolder folder, Predicate<InventoryItem> when)
        {
            try
            {
                List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                                                                        true, true, InventorySortOrder.ByName, 3000);
                if (folderContents != null)
                {
                    foreach (InventoryBase ib in folderContents)
                    {
                        if (ib is InventoryItem)
                        {
                            InventoryItem ii = ib as InventoryItem;
                            if (when(ii)) return ii;
                        }
                    }
                    // now run any sub folders
                    foreach (InventoryBase ib in folderContents)
                    {
                        if (ib is InventoryFolder)
                        {
                            InventoryItem ii = EvalOnFolders(ib as InventoryFolder, when);
                            if (ii != null) return ii;
                        }

                    }
                }
            }
            catch (Exception)
            {
            }
            return null;
        }

        public override void Inventory_OnTaskItemReceived(UUID itemID, UUID folderID, UUID creatorID, UUID assetID, InventoryType type)
        {
            if (type != InventoryType.LSL)
            {
                base.Inventory_OnTaskItemReceived(itemID, folderID, creatorID, assetID, type);
                return;
            }
            ScriptHolderUploadWait.Set();
        }

        private AutoResetEvent ScriptHolderUploadWait = new AutoResetEvent(false);
        private UUID WaitingForTransaction = UUID.Zero;

        // eval (thisClient.WorldSystem.ExecuteLSL "llSay(llGetObjectName());")
        public InventoryItem NewInventoryScript(string body, string name)
        {
            lock (ScriptHolderUploadWait)
            {
                InventoryItem created = null;
                // FIXME: Upload the script asset first. When that completes, call RequestCreateItem
                try
                {
                    string desc = String.Format("{0} created by OpenMetaverse Cogbot {1}", name, DateTime.Now);
                    // create the asset
                    ScriptHolderUploadWait.Reset();
                    WaitingForTransaction = UUID.Random();
                    client.Inventory.RequestCreateItem(client.Inventory.FindFolderForType(AssetType.LSLText), name, desc, AssetType.LSLText, WaitingForTransaction, InventoryType.LSL, WearableType.Shape, PermissionMask.All,
                        (bool success, InventoryItem item) =>
                        {
                            if (success) // Save the folder Item
                            {
                                if (item == null)
                                {
                                    Debug("ITem was null??!!!");
                                }
                                created = item;
                            }
                            else
                            {
                                Debug("RequestCreateItem unsuccessful, Name {0} Data {1} Created {2}", name, body, item);
                            }
                        }
                        );
                    if (ScriptHolderUploadWait.WaitOne(30000, true))
                    {
                        // upload the asset
                        AutoResetEvent UpdateEvent = new AutoResetEvent(false);
                        client.Inventory.RequestUpdateScriptAgentInventory(Utils.StringToBytes(body), created.UUID, false,
                            (success1, status, itemid, assetid) =>
                            {
                                UpdateEvent.Set();
                                if (!success1)
                                {
                                    Debug("Update error for {0} {1}", name, status);
                                    return;
                                }
                                Debug("Script successfully uploaded, ItemID {0} AssetID {1} Created {2}", itemid, assetid, created);
                            });

                        if (UpdateEvent.WaitOne(30000, true))
                        {
                            return created;
                        }

                    }
                }

                catch (System.Exception e)
                {
                    Logger.Log(e.ToString(), Helpers.LogLevel.Error, client);
                }
                return created;
            }
        }
    }

}
