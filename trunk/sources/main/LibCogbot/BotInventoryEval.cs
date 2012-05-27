using System;
using System.Collections.Generic;
using System.Collections;
using System.Threading;
using System.Windows.Forms;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using Radegast;

namespace cogbot
{
    public class BotInventoryEval
    {

        // recursive evaluator
        public string current_operation = "";
        public string current_itemName = "";
        //   protected TextForm botclient;
        protected BotClient client;
        public Hashtable hooked = new Hashtable();

        //private InventoryManager Manager;

        public BotInventoryEval(BotClient _c)
        {
            client = _c;
            if (Inventory != null)
            {
                Init1();
            }
            else
            {
                client.WorldSystem.OnConnectedQueue.Enqueue(Init1);
            }

            // Callbacks

        }


        public void regFolderHook(InventoryFolder folder)
        {
            if (!hooked.ContainsKey(folder.UUID))
            {
                hooked.Add(folder.UUID, folder.Name);
                WriteLine("  regFolderHook " + folder.Name);
            }

        }

        public void appendFolderHook(InventoryFolder folder)
        {
            if (!hooked.ContainsKey(folder.UUID))
            {
                hooked.Add(folder.UUID, folder.Name);
                //    botclient.Inventory.OnContentsRetrieved += new InventoryFolder.ContentsRetrieved(myfolder_OnContentsRetrieved);
                WriteLine("  appendFolderHook " + folder.Name);
            }

        }

        public void myfolder_OnContentsRetrieved(InventoryFolder folder)
        {
            regFolderHook(folder);
            // folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(60));
            // WriteLine("    myfolder_OnContentsRetrieved [" + folder.Name + "] = " + folder.UUID.ToString()+ " with count="+folder.Contents.Count.ToString());
            List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                                                                    true, true, InventorySortOrder.ByName, 10000);
            if (folderContents != null)
            {

                foreach (InventoryBase ib in folderContents)
                {
                    if (ib is InventoryItem)
                    {
                        InventoryItem ii = ib as InventoryItem;
                        if (current_operation == "print")
                        {
                            //int indent = 1;
                            //StringBuilder result = new StringBuilder();
                            //result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), ii.Name, ii.UUID.ToString());
                            //WriteLine(result.ToString());
                            WriteLine("   [Inventory Item] Name: " + ii.Name + " <==> " + ii.UUID.ToString() + " in folder[" + folder.Name + "]");
                        }


                        if (ii.Name == current_itemName)
                        {
                            // we found a matcher so lets do our ops
                            if (current_operation == "wear") client.Appearance.AddToOutfit(new List<InventoryItem> { ii });
                            if (current_operation == "animationStart") client.Self.AnimationStart(ii.UUID, false);
                            if (current_operation == "animationStop") client.Self.AnimationStop(ii.UUID, false);
                            if (current_operation == "attach") client.Appearance.Attach(ii, AttachmentPoint.Default);
                            if (current_operation == "rez")
                            {
                                client.Inventory.RequestRestoreRezFromInventory(client.Network.CurrentSim, ii,
                                                                                   UUID.Random());
                            }
                        }
                    }
                }
                // now run any sub folders
                foreach (InventoryBase ib in folderContents)
                {
                    if (ib is InventoryFolder)
                    {
                        InventoryFolder fld = (InventoryFolder)ib;

                        WriteLine(String.Format(" [Folder] Name: {0} <==> {1} in folder[{2}] RECIEVED", ib.Name, ib.UUID, folder.Name));

                        //evalOnFolders(ib as InventoryFolder, operation, itemName);
                        appendFolderHook(fld);
                        //fld.RequestContents();

                    }
                }
            }

        }

        public void evalOnFolders(InventoryFolder folder, string operation, string itemName)
        {

            current_itemName = itemName;
            current_operation = operation;

            try
            {
                /*
             //   WriteLine("examining folder [" + folder.Name + "] = " + folder.UUID.ToString());
                bool success = false;
                if (folder.IsStale)
                {
                    for (int wait = 5; ((wait < 10) && (!success)&&(folder.Contents.Count==0)); wait += 5)
                    {
                        success = folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(wait));
                        //success = folder.DownloadContents(TimeSpan.FromSeconds(wait));
                        WriteLine(" DownloadContentets returned " + success.ToString());
                        WriteLine(" Contents.count = " + folder.Contents.Count.ToString());
                    }
                //appendFolderHook(folder);
                //folder.RequestContents();
                }
                //else
                //{
                //    WriteLine(" Claim is folder is fresh...");
                //}
                */

                List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                                                                        true, true, InventorySortOrder.ByName, 3000);
                if (folderContents != null)
                {

                    // first scan this folder for objects

                    foreach (InventoryBase ib in folderContents)
                    {
                        //WriteLine(" [InvAll] Name: " + ib.Name + " <==> " + ib.ToString());
                        if (ib is InventoryItem)
                        {
                            InventoryItem ii = ib as InventoryItem;
                            if (operation == "print")
                            {
                                //int indent = 1;
                                //StringBuilder result = new StringBuilder();
                                //result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), ii.Name, ii.UUID.ToString());
                                //WriteLine(result.ToString());
                                WriteLine(String.Format(" [Inventory Item] Name: {0} <==> {1} OP:{2} ITEM:{3}", ii.Name, ii.UUID, operation, itemName));
                            }


                            if (String.Compare(ii.Name, itemName, true) == 0)
                            {
                                // we found a matcher so lets do our ops
                                if (operation == "wear") client.Appearance.AddToOutfit(new List<InventoryItem>{ii});
                                if (operation == "animationStart") client.Self.AnimationStart(ii.UUID, false);
                                if (operation == "animationStop") client.Self.AnimationStop(ii.UUID, false);
                                if (operation == "attach") client.Appearance.Attach(ii, AttachmentPoint.Default);
                                return;
                            }
                        }
                    }

                    // now run any sub folders
                    foreach (InventoryBase ib in folderContents)
                    {

                        if (ib is InventoryFolder)
                        {
                            WriteLine(String.Format(" [Folder] Name: {0} <==> {1} OP:{2} ITEM:{3}", ib.Name, ib.UUID, operation, itemName));
                            InventoryFolder fld = (InventoryFolder)ib;
                            //appendFolderHook(fld);
                            //fld.RequestContents();
                            //if ((operation == "wear") && (ib.Name == itemName))
                            //{
                            //    botclient.Appearance.AddToOutfit(botclient.Inventory.(), false);
                            //    WriteLine(" [WEAR] Name: " + ib.Name + " <==> " + ib.UUID.ToString());
                            //    return;
                            //}
                            evalOnFolders(ib as InventoryFolder, operation, itemName);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                WriteLine("Search Exception :" + e.StackTrace);
                WriteLine("  msg:" + e.Message);

            }
        }

        public void TraverseNodes(InventoryNode start)
        {
            TraverseNodes(start,TimeSpan.FromSeconds(10));
        }
        public void TraverseNodes(InventoryNode start, TimeSpan maxTime)
        {
            TraverseNodesUnsorted(start, maxTime);
            start.Sort();
        }
        public void TraverseNodesUnsorted(InventoryNode start, TimeSpan maxTime)
        {
            var Inventory = client.Inventory.Store;
            bool has_items = false;

            foreach (InventoryNode node in start.Nodes.Values)
            {
                if (node.Data is InventoryItem)
                {
                    has_items = true;
                    break;
                }
            }

            if (!has_items || start.NeedsUpdate)
            {
                InventoryFolder f = (InventoryFolder)start.Data;
                AutoResetEvent gotFolderEvent = new AutoResetEvent(false);
                bool success = false;

                EventHandler<FolderUpdatedEventArgs> callback = delegate(object sender, FolderUpdatedEventArgs ea)
                {
                    if (f.UUID == ea.FolderID)
                    {
                        if (((InventoryFolder)Inventory.Items[ea.FolderID].Data).DescendentCount <= Inventory.Items[ea.FolderID].Nodes.Count)
                        {
                            success = true;
                            gotFolderEvent.Set();
                        }
                    }
                };

                client.Inventory.FolderUpdated += callback;
                fetchFolder(f.UUID, f.OwnerID, true);
                gotFolderEvent.WaitOne(maxTime, false);
                client.Inventory.FolderUpdated -= callback;

                if (!success)
                {
                    Logger.Log(string.Format("Failed fetching folder {0}, got {1} items out of {2}", f.Name, Inventory.Items[f.UUID].Nodes.Count, ((InventoryFolder)Inventory.Items[f.UUID].Data).DescendentCount), Helpers.LogLevel.Error, client);
                }
            }

            foreach (InventoryBase item in Inventory.GetContents((InventoryFolder)start.Data))
            {
                if (item is InventoryFolder)
                {
                    TraverseNodes(Inventory.GetNodeFor(item.UUID), maxTime);
                }
            }
        }
        bool UseInventoryCaps
        {
            get
            {
                bool res =
                    client.Network.CurrentSim != null
                    && client.Network.CurrentSim.Caps != null
                    && client.Network.CurrentSim.Caps.CapabilityURI("FetchInventoryDescendents2") != null
                    && !DLRConsole.IsOnMonoUnix;
                return res;
            }
        }
        private void fetchFolder(UUID folderID, UUID ownerID, bool force)
        {
            if (force || !fetchedFolders.Contains(folderID))
            {
                if (!fetchedFolders.Contains(folderID))
                {
                    fetchedFolders.Add(folderID);
                }

                if (!UseInventoryCaps)
                {
                    client.Inventory.RequestFolderContents(folderID, ownerID, true, true, InventorySortOrder.ByDate);
                }
                else
                {
                    client.Inventory.RequestFolderContentsCap(folderID, ownerID, true, true, InventorySortOrder.ByDate);
                }
            }
        }

        private void WriteLine(string p)
        {
            client.WriteLine(p);
        }



        // Recursively generates lispTasks for items that match itemName in the inventory
        // like evalLispOnFolders(root,"(Console.Write 'OBJNAME is OBJUUID')","Shoes")
        public void evalLispOnFolders(InventoryFolder folder, string lispOperation, string itemName)
        {
            try
            {
                //if (folder.IsStale)
                //    folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(10));
                // first scan this folder for text
                List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                                                                        true, true, InventorySortOrder.ByName, 3000);
                if (folderContents != null)
                {

                    foreach (InventoryBase ib in folderContents)
                    {
                        if (ib is InventoryItem)
                        {
                            InventoryItem ii = ib as InventoryItem;
                            if (ii.Name == itemName)
                            {
                                String lispCode = lispOperation;
                                lispCode.Replace("OBJUUID", ii.UUID.ToString());
                                lispCode.Replace("OBJNAME", ii.Name);
                                client.evalLispString(lispCode);
                            }
                        }
                    }
                    // now run any sub folders
                    foreach (InventoryBase ib in folderContents)
                    {
                        if (ib is InventoryFolder)
                            evalLispOnFolders(ib as InventoryFolder, lispOperation, itemName);
                    }
                }
            }
            catch (Exception)
            {
            }
        }


        // recursive finder
        public UUID findInFolders(InventoryFolder folder, string itemName)
        {
            try
            {

                List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                                                                        true, true, InventorySortOrder.ByName, 3000);
                //if (folder.IsStale)
                //    folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(10));
                // first scan this folder for text
                if (folderContents != null)
                {
                    foreach (InventoryBase ib in folderContents)
                    {
                        if (ib is InventoryItem)
                        {
                            InventoryItem ii = ib as InventoryItem;
                            if (ii.Name == itemName)
                            {
                                return ii.UUID;
                            }
                        }
                    }
                    // now run any subfolders
                    foreach (InventoryBase ib in folderContents)
                    {
                        if (ib is InventoryFolder)
                        {
                            UUID ANS = findInFolders(ib as InventoryFolder, itemName);
                            if (ANS != UUID.Zero) return ANS;
                        }
                    }
                }

            }
            catch (Exception)
            {
            }
            return UUID.Zero;

        }
        public bool findInFolders(InventoryFolder folder, Predicate<InventoryBase> itemTest)
        {
            try
            {

                List<InventoryBase> folderContents = client.Inventory.FolderContents(folder.UUID, client.Self.AgentID,
                                                                                        true, true, InventorySortOrder.ByName, 6000);
                //if (folder.IsStale)
                //    folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(10));
                // first scan this folder for text
                if (folderContents != null)
                {
                    // now run any subfolders
                    foreach (InventoryBase ib in folderContents)
                    {
                        if (itemTest(ib)) return true;
                        if (ib is InventoryFolder)
                        {
                            var ANS = findInFolders(ib as InventoryFolder, itemTest);
                            if (ANS) return ANS;
                        }
                    }
                }

            }
            catch (Exception)
            {
            }
            return false;

        }

        //RadegastInstance instance;
        //Dictionary<UUID, TreeNode> FolderNodes = new Dictionary<UUID, TreeNode>();

        private InventoryManager Manager
        {
            get
            {
                return client.Inventory;
            }           
        }
        private OpenMetaverse.Inventory Inventory
        {
            get
            {
                return Manager.Store;
            }
        }
        //private TreeNode invRootNode;
        private string newItemName = string.Empty;
        private List<UUID> fetchedFolders = new List<UUID>();
        //private System.Threading.Timer _EditTimer;
        //private TreeNode _EditNode;
        private Dictionary<UUID, AttachmentInfo> attachments = new Dictionary<UUID, AttachmentInfo>();
        //private System.Timers.Timer TreeUpdateTimer;
        private Queue<InventoryBase> ItemsToAdd = new Queue<InventoryBase>();
        private Queue<InventoryBase> ItemsToUpdate = new Queue<InventoryBase>();
        //private bool TreeUpdateInProgress = false;
        private int updateInterval = 1000;
        private Thread InventoryUpdate;
        private List<UUID> WornItems = new List<UUID>();
        private bool appearnceWasBusy;
        //private InvNodeSorter sorter;
        private List<UUID> QueuedFolders = new List<UUID>();
        private Dictionary<UUID, int> FolderFetchRetries = new Dictionary<UUID, int>();

        #region Construction and disposal

        public void Init1()
        {
            
 
            InventoryUpdate = new Thread(new ThreadStart(StartTraverseNodes));
            InventoryUpdate.Name = "InventoryUpdate";
            InventoryUpdate.IsBackground = true;
            InventoryUpdate.Start();


            // Callbacks
            Inventory.InventoryObjectAdded += new EventHandler<InventoryObjectAddedEventArgs>(Inventory_InventoryObjectAdded);
            Inventory.InventoryObjectUpdated += new EventHandler<InventoryObjectUpdatedEventArgs>(Inventory_InventoryObjectUpdated);
            Inventory.InventoryObjectRemoved += new EventHandler<InventoryObjectRemovedEventArgs>(Inventory_InventoryObjectRemoved);

            client.Objects.ObjectUpdate += new EventHandler<PrimEventArgs>(Objects_AttachmentUpdate);
            client.Objects.KillObject += new EventHandler<KillObjectEventArgs>(Objects_KillObject);
            client.Appearance.AppearanceSet += new EventHandler<AppearanceSetEventArgs>(Appearance_AppearanceSet);
        }

        public void Dispose()
        {
            if (InventoryUpdate != null)
            {
                if (InventoryUpdate.IsAlive)
                    InventoryUpdate.Abort();
                InventoryUpdate = null;
            }

            Inventory.InventoryObjectAdded -= new EventHandler<InventoryObjectAddedEventArgs>(Inventory_InventoryObjectAdded);
            Inventory.InventoryObjectUpdated -= new EventHandler<InventoryObjectUpdatedEventArgs>(Inventory_InventoryObjectUpdated);
            Inventory.InventoryObjectRemoved -= new EventHandler<InventoryObjectRemovedEventArgs>(Inventory_InventoryObjectRemoved);

            client.Objects.ObjectUpdate -= new EventHandler<PrimEventArgs>(Objects_AttachmentUpdate);
            client.Objects.KillObject -= new EventHandler<KillObjectEventArgs>(Objects_KillObject);
            client.Appearance.AppearanceSet -= new EventHandler<AppearanceSetEventArgs>(Appearance_AppearanceSet);
        }
        #endregion

        #region Network callbacks
        void Appearance_AppearanceSet(object sender, AppearanceSetEventArgs e)
        {
            if (appearnceWasBusy)
            {
                appearnceWasBusy = false;
                client.Appearance.RequestSetAppearance(true);
            }
        }

        void Objects_KillObject(object sender, KillObjectEventArgs e)
        {
            AttachmentInfo attachment = null;
            lock (attachments)
            {
                foreach (AttachmentInfo att in attachments.Values)
                {
                    if (att.Prim != null && att.Prim.LocalID == e.ObjectLocalID)
                    {
                        attachment = att;
                        break;
                    }
                }

                if (attachment != null)
                {
                    attachments.Remove(attachment.InventoryID);
                }
            }
        }

        void Objects_AttachmentUpdate(object sender, PrimEventArgs e)
        {
            Primitive prim = e.Prim;

            if (client.Self.LocalID == 0 ||
                prim.ParentID != client.Self.LocalID ||
                prim.NameValues == null) return;

            for (int i = 0; i < prim.NameValues.Length; i++)
            {
                if (prim.NameValues[i].Name == "AttachItemID")
                {
                    AttachmentInfo attachment = new AttachmentInfo();
                    attachment.Prim = prim;
                    attachment.InventoryID = new UUID(prim.NameValues[i].Value.ToString());
                    attachment.PrimID = prim.ID;

                    lock (attachments)
                    {
                        // Add new attachment info
                        if (!attachments.ContainsKey(attachment.InventoryID))
                        {
                            attachments.Add(attachment.InventoryID, attachment);

                        }
                        else
                        {
                            attachment = attachments[attachment.InventoryID];
                            if (attachment.Prim == null)
                            {
                                attachment.Prim = prim;
                            }
                        }

                        // Don't update the tree yet if we're still updating invetory tree from server
                        //if (!TreeUpdateInProgress)
                        {
                            if (Inventory.Contains(attachment.InventoryID))
                            {
                                if (attachment.Item == null)
                                {
                                    InventoryItem item = (InventoryItem)Inventory[attachment.InventoryID];
                                    attachment.Item = item;
                                }
                                if (!attachment.MarkedAttached)
                                {
                                    attachment.MarkedAttached = true;
                                }
                            }
                            else
                            {
                                client.Inventory.RequestFetchInventory(attachment.InventoryID, client.Self.AgentID);
                            }
                        }
                    }
                    break;
                }
            }
        }

        void Inventory_InventoryObjectAdded(object sender, InventoryObjectAddedEventArgs e)
        {
            if (false)
            {
                lock (ItemsToAdd)
                {
                    ItemsToAdd.Enqueue(e.Obj);
                }
            }
            else
            {
                Exec_OnInventoryObjectAdded(e.Obj);
            }
        }

        void Exec_OnInventoryObjectAdded(InventoryBase obj)
        {

            lock (attachments)
            {
                if (attachments.ContainsKey(obj.UUID))
                {
                    attachments[obj.UUID].Item = (InventoryItem)obj;
                }
            }



            newItemName = string.Empty;
        }

        void Inventory_InventoryObjectRemoved(object sender, InventoryObjectRemovedEventArgs e)
        {

            lock (attachments)
            {
                if (attachments.ContainsKey(e.Obj.UUID))
                {
                    attachments.Remove(e.Obj.UUID);
                }
            }

        }

        void Inventory_InventoryObjectUpdated(object sender, InventoryObjectUpdatedEventArgs e)
        {
            if (false)
            {
                lock (ItemsToUpdate)
                {


                    if (!ItemsToUpdate.Contains(e.NewObject))
                    {
                        ItemsToUpdate.Enqueue(e.NewObject);
                    }
                }
            }
            else
            {
                Exec_OnInventoryObjectUpdated(e.OldObject, e.NewObject);
            }
        }

        void Exec_OnInventoryObjectUpdated(InventoryBase oldObject, InventoryBase newObject)
        {
            if (newObject == null) return;

            lock (attachments)
            {
                if (attachments.ContainsKey(newObject.UUID))
                {
                    attachments[newObject.UUID].Item = (InventoryItem)newObject;
                }
            }

        }
        #endregion




 
        #region Private methods


        private void TraverseAndQueueNodes(InventoryNode start)
        {
            bool has_items = false;

            foreach (InventoryNode node in start.Nodes.Values)
            {
                if (node.Data is InventoryItem)
                {
                    has_items = true;
                    break;
                }
            }

            if (!has_items || start.NeedsUpdate)
            {
                lock (QueuedFolders)
                {
                    lock (FolderFetchRetries)
                    {
                        int retries = 0;
                        FolderFetchRetries.TryGetValue(start.Data.UUID, out retries);
                        if (retries < 3)
                        {
                            if (!QueuedFolders.Contains(start.Data.UUID))
                            {
                                QueuedFolders.Add(start.Data.UUID);
                            }
                        }
                        FolderFetchRetries[start.Data.UUID] = retries + 1;
                    }
                }
            }

            foreach (InventoryBase item in Inventory.GetContents((InventoryFolder)start.Data))
            {
                if (item is InventoryFolder)
                {
                    TraverseAndQueueNodes(Inventory.GetNodeFor(item.UUID));
                }
            }
        }



        private void StartTraverseNodes()
        {
            if (!client.Network.CurrentSim.Caps.IsEventQueueRunning)
            {
                AutoResetEvent EQRunning = new AutoResetEvent(false);
                EventHandler<EventQueueRunningEventArgs> handler = (sender, e) =>
                {
                    EQRunning.Set();
                };
                client.Network.EventQueueRunning += handler;
                EQRunning.WaitOne(10 * 1000, false);
                client.Network.EventQueueRunning -= handler;
            }

            if (!client.Network.CurrentSim.Caps.IsEventQueueRunning)
            {
                return;
            }

            if (!UseInventoryCaps)
            {
                TraverseNodes(Inventory.RootNode);
            }
            else
            {
                lock (FolderFetchRetries)
                {
                    FolderFetchRetries.Clear();
                }

                do
                {
                    lock (QueuedFolders)
                    {
                        QueuedFolders.Clear();
                    }
                    TraverseAndQueueNodes(Inventory.RootNode);
                    if (QueuedFolders.Count == 0) break;
                    Logger.DebugLog(string.Format("Queued {0} folders for update", QueuedFolders.Count));

                    Parallel.ForEach<UUID>(Math.Min(QueuedFolders.Count, 16), QueuedFolders, folderID =>
                    {
                        bool success = false;

                        AutoResetEvent gotFolder = new AutoResetEvent(false);
                        EventHandler<FolderUpdatedEventArgs> handler = (sender, ev) =>
                        {
                            if (ev.FolderID == folderID)
                            {
                                success = ev.Success;
                                gotFolder.Set();
                            }
                        };

                        client.Inventory.FolderUpdated += handler;
                        client.Inventory.RequestFolderContentsCap(folderID, client.Self.AgentID, true, true, InventorySortOrder.ByDate);
                        if (!gotFolder.WaitOne(15 * 1000, false))
                        {
                            success = false;
                        }
                        client.Inventory.FolderUpdated -= handler;
                    });
                }
                while (QueuedFolders.Count > 0);
            }


            // Update attachments now that we are done
            lock (attachments)
            {
                foreach (AttachmentInfo a in attachments.Values)
                {
                    if (a.Item == null)
                    {
                        if (Inventory.Contains(a.InventoryID))
                        {
                            a.MarkedAttached = true;
                            a.Item = (InventoryItem)Inventory[a.InventoryID];
                        }
                        else
                        {
                            client.Inventory.RequestFetchInventory(a.InventoryID, client.Self.AgentID);
                            return;
                        }
                    }
                }
            }

        }

        public void ReloadInventory()
        {

    

            Inventory.Items = new Dictionary<UUID, InventoryNode>();
            Inventory.RootFolder = Inventory.RootFolder;
            Inventory.RootNode.NeedsUpdate = true;
            InventoryUpdate = new Thread(new ThreadStart(StartTraverseNodes));
            InventoryUpdate.Name = "InventoryUpdate";
            InventoryUpdate.IsBackground = true;
            InventoryUpdate.Start();
        }

        #endregion


        public bool IsWorn(InventoryItem item)
        {
            bool worn = client.Appearance.IsItemWorn(item) != WearableType.Invalid;

            lock (WornItems)
            {
                if (worn && !WornItems.Contains(item.UUID))
                    WornItems.Add(item.UUID);
            }
            return worn;
        }

        public AttachmentPoint AttachedTo(InventoryItem item)
        {
            lock (attachments)
            {
                if (attachments.ContainsKey(item.UUID))
                {
                    return attachments[item.UUID].Point;
                } else
                {
                    GetAttachments();
                    if (attachments.ContainsKey(item.UUID))
                    {
                        return attachments[item.UUID].Point;
                    }
                }
            }

            return AttachmentPoint.Default;
        }

        public object AttachesTo(InventoryItem item)
        {
            if (item is InventoryAttachment)
            {
                var i = (InventoryAttachment)item;
                return i.AttachmentPoint;
            }
            if (item is InventoryWearable)
            {
                var i = (InventoryWearable)item;
                return i.WearableType;
            }
            lock (attachments)
            {
                if (attachments.ContainsKey(item.UUID))
                {
                    return attachments[item.UUID].Point;
                }
                else
                {
                    GetAttachments();
                    if (attachments.ContainsKey(item.UUID))
                    {
                        return attachments[item.UUID].Point;
                    }
                }
            }

            return AttachmentPoint.Default;
        }


        public bool IsAttached(InventoryItem item)
        {
            List<Primitive> myAtt = client.Network.CurrentSim.ObjectsPrimitives.FindAll((Primitive p) => p.ParentID == client.Self.LocalID);
            foreach (Primitive prim in myAtt)
            {
                if (prim.NameValues == null) continue;
                UUID invID = UUID.Zero;
                for (int i = 0; i < prim.NameValues.Length; i++)
                {
                    if (prim.NameValues[i].Name == "AttachItemID")
                    {
                        invID = (UUID)prim.NameValues[i].Value.ToString();
                        break;
                    }
                }
                if (invID == item.UUID)
                {
                    lock (attachments)
                    {
                        AttachmentInfo inf = new AttachmentInfo();
                        inf.InventoryID = item.UUID;
                        inf.Item = item;
                        inf.MarkedAttached = true;
                        inf.Prim = prim;
                        inf.PrimID = prim.ID;
                        attachments[invID] = inf;
                    }
                    return true;
                }
            }

            return false;
        }


        public ICollection<AttachmentInfo> GetAttachments()
        {
            List<Primitive> myAtt = client.Network.CurrentSim.ObjectsPrimitives.FindAll((Primitive p) => p.ParentID == client.Self.LocalID);
            foreach (Primitive prim in myAtt)
            {
                if (prim.NameValues == null) continue;
                UUID invID = UUID.Zero;
                for (int i = 0; i < prim.NameValues.Length; i++)
                {
                    if (prim.NameValues[i].Name == "AttachItemID")
                    {
                        invID = (UUID)prim.NameValues[i].Value.ToString();
                        break;
                    }
                }
                var item = Inventory.GetNodeFor(invID).Data as InventoryItem;
                {
                    lock (attachments)
                    {
                        AttachmentInfo inf = new AttachmentInfo();
                        inf.InventoryID = invID;
                        inf.Item = item;
                        inf.MarkedAttached = true;
                        inf.Prim = prim;
                        inf.PrimID = prim.ID;
                        attachments[invID] = inf;
                    }
                }
            }

            return attachments.Values;
        }

        public InventoryItem AttachmentAt(AttachmentPoint point)
        {
            lock (attachments)
            {
                foreach (KeyValuePair<UUID, AttachmentInfo> att in attachments)
                {
                    if (att.Value.Point == point)
                    {
                        return att.Value.Item;
                    }
                }
            }
            return null;
        }


        public static bool IsFullPerm(InventoryItem item)
        {
            if (
                ((item.Permissions.OwnerMask & PermissionMask.Modify) != 0) &&
                ((item.Permissions.OwnerMask & PermissionMask.Copy) != 0) &&
                ((item.Permissions.OwnerMask & PermissionMask.Transfer) != 0)
                )
            {
                return true;
            }
            else
            {
                return false;
            }
        }

     

    }

    public class AttachmentInfo
    {
        public Primitive Prim;
        public InventoryItem Item;
        public UUID InventoryID;
        public UUID PrimID;
        public bool MarkedAttached = false;

        public AttachmentPoint Point
        {
            get
            {
                if (Prim != null)
                {
                    return Prim.PrimData.AttachmentPoint;
                }
                else
                {
                    return AttachmentPoint.Default;
                }
            }
        }
    }

}
