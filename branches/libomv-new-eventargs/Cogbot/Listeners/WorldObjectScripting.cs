using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Listeners
{

    public partial class WorldObjects
    {

        public static bool ScriptHolderPrecreated = true;
        private Primitive ScriptHolder = null;
        private bool ScriptHolderAttached = false;
        private readonly AutoResetEvent ScriptHolderAttachWaiting = new AutoResetEvent(false);

        public static bool AcceptOffersAnimationsObjects = true;

        public override void Self_OnScriptQuestion(Simulator simulator, UUID taskID, UUID itemID, string objectName,
                                                   string objectOwner, ScriptPermission questions)
        {
            EventQueue.Enqueue(
                () =>
                {
                    /*
                        TaskID: 552f9165-0dd8-9124-f9bb-20fa3cb18382
                        ItemID: 8fe015cb-bf46-5e1c-8975-f2cbca4762d9
                        Questions: 16
                        ObjectName: DanceBall
                        ObjectOwner: Serena Vale
                     */

                    client.SendPersonalEvent(SimEventType.SCRIPT, "On-Script-Question", simulator, taskID, itemID, objectName, objectOwner,
                                  questions);
                    /*
                         TaskID: 552f9165-0dd8-9124-f9bb-20fa3cb18382
                         ItemID: 8fe015cb-bf46-5e1c-8975-f2cbca4762d9
                         Questions: 16
                     */
                    if (AcceptOffersAnimationsObjects)
                        client.Self.ScriptQuestionReply(simulator, itemID, taskID, questions);
                }
                );
        }

        public override void Self_OnScriptDialog(string message, string objectName, UUID imageID, UUID objectID,
                                                 string firstName, string lastName, int chatChannel,
                                                 List<string> buttons)
        {
            EventQueue.Enqueue(
                () =>
                {
                    client.SendPersonalEvent(SimEventType.SCRIPT, "On-Script-Dialog", message, objectName, imageID, objectID, firstName,
                                 lastName,
                                 chatChannel, buttons);
                    if (AcceptOffersAnimationsObjects && buttons.Count > 0)
                    {
                        int buttonIndex = (new Random()).Next(buttons.Count);
                        string buttonlabel = buttons[buttonIndex];

                        int maxTries = buttons.Count * 2;
                        string buttonlabelToLower = buttonlabel.ToLower();
                        while (maxTries-- > 0 && (buttonlabelToLower.Contains("cancel") || buttonlabelToLower.Contains("ignore") || buttonlabelToLower.Contains("mute")))
                        {
                            buttonIndex = (new Random()).Next(buttons.Count);
                            buttonlabel = buttons[buttonIndex];
                            buttonlabelToLower = buttonlabel.ToLower();
                        }

                        client.Self.ReplyToScriptDialog(chatChannel, buttonIndex, buttonlabel, objectID);
                    }
                }
                );
        }

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

        public void Inventory_OnScriptRunning(UUID objectID0, UUID sctriptID, bool isMono, bool isRunning)
        {
            Console.WriteLine("On-Script-Running ObjectID: {0} ItemID: {1} IsMono: {2} IsRunning: {3}", objectID0, sctriptID, isMono, isRunning);
        }

        /// <summary>
        /// eval (thisClient.WorldSystem.GetScriptHolder "ScriptHolder")
        /// eval (thisClient.WorldSystem.ExecuteLSL "llOwnerSay(llGetObjectName());")
        /// </summary>
        /// <param name="s"></param>
        public void ExecuteLSL(String s)
        {
            LoadLSL(String.Format("default {{ state_entry() {{ {0}  llRemoveInventory(llGetScriptName()); }} }}", s), false);
        }

        readonly static List<InventoryItem> TempScripts = new List<InventoryItem>();
        public void LoadLSL(string script, bool daemonize)
        {
            InventoryItem scriptItem = null;
            string newVariablename = null;
            lock (TempScripts)
            {
                newVariablename = "tempScript" + TempScripts.Count;
                scriptItem = NewInventoryScript(script, newVariablename);
                TempScripts.Add(scriptItem);
            }

            ScriptHolder = GetScriptHolder("ScriptHolder");

            // we save this to clean out the object later of running scripts
            ShutdownHooks.Add(() =>
                                  {
                                      Simulator sim = GetSimulator(ScriptHolder.RegionHandle);
                                      List<InventoryBase> ibs = client.Inventory.GetTaskInventory(ScriptHolder.ID,
                                                                                                  ScriptHolder.LocalID,
                                                                                                  10000);
                                      foreach (InventoryBase ib in ibs)
                                      {
                                          if (!(ib is InventoryItem)) continue; // skip folders
                                          if (ib.Name == newVariablename)
                                          {
                                              if (GetScriptRunning(ScriptHolder.ID, ib.UUID))
                                              {
                                                  SetScriptRunning(ScriptHolder.ID, ib.UUID, false);
                                                  Console.WriteLine(String.Format("{0}", GetScriptRunning(ScriptHolder.ID, ib.UUID)));
                                              }
                                              break;
                                          }
                                          client.Inventory.RemoveTaskInventory(ScriptHolder.LocalID, ib.UUID, sim);
                                      }
                                  }
                );


            // eval (thisClient.WorldSystem.ExecuteLSL "llOwnerSay(llGetObjectName());")
            using (AutoResetEvent LocalReset = new AutoResetEvent(false))
            {
                InventoryManager.TaskInventoryReplyCallback callback
                    = (UUID itemID, short serial, string assetFilename) =>
                    {
                        if (itemID == ScriptHolder.ID)
                        {
                            LocalReset.Set();
                        }
                    };
                client.Inventory.OnTaskInventoryReply += callback;
                client.Inventory.CopyScriptToTask(ScriptHolder.LocalID, scriptItem, true);
                if (LocalReset.WaitOne(10000, false))
                {
                    Debug("time out on CopyScriptToTask");
                }
                client.Inventory.OnTaskInventoryReply -= callback;
            }
            // remove the arifact in agent inventory
            client.Inventory.RemoveItem(scriptItem.UUID);
        }

        public void SetScriptRunning(UUID objectID, UUID itemID, bool enableScript)
        {
            using (AutoResetEvent LocalReset = new AutoResetEvent(false))
            {
                InventoryManager.ScriptRunningCallback callback
                    = ((UUID objectID0, UUID sctriptID, bool isMono, bool isRunning) =>
                    {
                        if (objectID0 == objectID)
                        {
                            LocalReset.Set();
                        }
                    });
                client.Inventory.OnScriptRunning += callback;
                client.Inventory.SetScriptRunning(objectID, itemID, enableScript);
                LocalReset.WaitOne(10000, true);
                client.Inventory.OnScriptRunning -= callback;
            }
        }

        public bool GetScriptRunning(UUID objectID, UUID itemID)
        {
            bool wasRunning = false;
            using (AutoResetEvent LocalReset = new AutoResetEvent(false))
            {
                InventoryManager.ScriptRunningCallback callback
                    = ((UUID objectID0, UUID sctriptID, bool isMono, bool isRunning) =>
                    {
                        if (objectID0 == objectID && sctriptID == itemID)
                        {
                            wasRunning = isRunning;
                            LocalReset.Set();
                        }
                    });
                client.Inventory.OnScriptRunning += callback;
                client.Inventory.GetScriptRunning(objectID, itemID);
                LocalReset.WaitOne(10000, true);
                client.Inventory.OnScriptRunning -= callback;
            }
            return wasRunning;
        }

        public Primitive GetScriptHolder(string primName)
        {
            if (ScriptHolder != null) return ScriptHolder;
            lock (ScriptHolderAttachWaiting)
            {
                if (ScriptHolder != null) return ScriptHolder;

                ScriptHolderAttachWaiting.Reset();
                InventoryItem ScriptHolderItem = GetInventoryObject(primName);
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
            base.Inventory_OnTaskItemReceived(itemID, folderID, creatorID, assetID, type);
        }

        private UUID WaitingForTransaction = UUID.Zero;


        // eval (thisClient.WorldSystem.ExecuteLSL "llSay(llGetObjectName());")
        public InventoryItem NewInventoryScript(string body, string name)
        {
            InventoryItem created = null;
            try
            {

                // FIXME: Upload the script asset first. When that completes, call RequestCreateItem
                using (AutoResetEvent LocalReset = new AutoResetEvent(false))
                {
                    string desc = String.Format("{0} created by OpenMetaverse Cogbot {1}", name, DateTime.Now);
                    // create the asset
                    WaitingForTransaction = UUID.Random();
                    client.Inventory.RequestCreateItem(client.Inventory.FindFolderForType(AssetType.LSLText), name, desc, AssetType.LSLText, WaitingForTransaction, InventoryType.LSL, WearableType.Shape, PermissionMask.All,
                        (bool success, InventoryItem item) =>
                        {
                            LocalReset.Set();
                            if (success) // Save the folder Item
                            {
                                created = item;
                                Debug("RequestCreateItem Successful, Name {0} Data {1} Created {2}", name, body, item);
                            }
                            else
                            {
                                Debug("RequestCreateItem unsuccessful, Name {0} Data {1} Created {2}", name, body, item);
                            }
                        }
                        );
                    LocalReset.WaitOne(10000, true);
                    // upload the asset
                    using (AutoResetEvent UpdateEvent = new AutoResetEvent(false))
                    {
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
            }
            catch (System.Exception e)
            {
                Logger.Log(e.ToString(), Helpers.LogLevel.Error, client);
            }
            return created;
        }
    }
}
