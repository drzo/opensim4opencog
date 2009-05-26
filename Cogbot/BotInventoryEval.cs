using System;
using System.Collections.Generic;
using System.Collections;
using OpenMetaverse;

namespace cogbot
{
    public class BotInventoryEval
    {
        // recursive evaluator
        public string current_operation = "";
        public string current_itemName = "";
        //   protected TextForm botclient;
        protected BotClient botclient;
        public Hashtable hooked = new Hashtable();
        //private Inventory Inventory;
        //private InventoryManager Manager;

        public BotInventoryEval(BotClient _c)
        {
            //  botclient = _c.botclient;
            botclient = _c;// botclient.CurrentClient;

        }

        public void regFolderHook(InventoryFolder folder)
        {
            if (!hooked.ContainsKey(folder.UUID))
            {
                hooked.Add(folder.UUID, folder.Name);
                botclient.output("  regFolderHook " + folder.Name);
            }

        }

        public void appendFolderHook(InventoryFolder folder)
        {
            if (!hooked.ContainsKey(folder.UUID))
            {
                hooked.Add(folder.UUID, folder.Name);
                //    botclient.Inventory.OnContentsRetrieved += new InventoryFolder.ContentsRetrieved(myfolder_OnContentsRetrieved);
                botclient.output("  appendFolderHook " + folder.Name);
            }

        }

        public void myfolder_OnContentsRetrieved(InventoryFolder folder)
        {
            regFolderHook(folder);
            // folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(60));
            // botclient.output("    myfolder_OnContentsRetrieved [" + folder.Name + "] = " + folder.UUID.ToString()+ " with count="+folder.Contents.Count.ToString());
            List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
                                                                                    true, true, InventorySortOrder.ByName, 3000);
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
                            //output(result.ToString());
                            botclient.output("   [Inventory Item] Name: " + ii.Name + " <==> " + ii.UUID.ToString() + " in folder[" + folder.Name + "]");
                        }


                        if (ii.Name == current_itemName)
                        {
                            // we found a matcher so lets do our ops
                            if (current_operation == "wear") botclient.Appearance.WearOutfit(ii.UUID, false);
                            if (current_operation == "animationStart") botclient.Self.AnimationStart(ii.UUID, false);
                            if (current_operation == "animationStop") botclient.Self.AnimationStop(ii.UUID, false);
                            if (current_operation == "attach") botclient.Appearance.Attach(ii, AttachmentPoint.Default);
                        }
                    }
                }
                // now run any sub folders
                foreach (InventoryBase ib in folderContents)
                {
                    if (ib is InventoryFolder)
                    {
                        InventoryFolder fld = (InventoryFolder)ib;

                        botclient.output(String.Format(" [Folder] Name: {0} <==> {1} in folder[{2}] RECIEVED", ib.Name, ib.UUID, folder.Name));

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
             //   botclient.output("examining folder [" + folder.Name + "] = " + folder.UUID.ToString());
                bool success = false;
                if (folder.IsStale)
                {
                    for (int wait = 5; ((wait < 10) && (!success)&&(folder.Contents.Count==0)); wait += 5)
                    {
                        success = folder.DownloadContentsOpenSim(TimeSpan.FromSeconds(wait));
                        //success = folder.DownloadContents(TimeSpan.FromSeconds(wait));
                        botclient.output(" DownloadContentets returned " + success.ToString());
                        botclient.output(" Contents.count = " + folder.Contents.Count.ToString());
                    }
                //appendFolderHook(folder);
                //folder.RequestContents();
                }
                //else
                //{
                //    output(" Claim is folder is fresh...");
                //}
                */

                List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
                                                                                        true, true, InventorySortOrder.ByName, 3000);
                if (folderContents != null)
                {

                    // first scan this folder for objects

                    foreach (InventoryBase ib in folderContents)
                    {
                        //botclient.output(" [InvAll] Name: " + ib.Name + " <==> " + ib.ToString());
                        if (ib is InventoryItem)
                        {
                            InventoryItem ii = ib as InventoryItem;
                            if (operation == "print")
                            {
                                //int indent = 1;
                                //StringBuilder result = new StringBuilder();
                                //result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), ii.Name, ii.UUID.ToString());
                                //output(result.ToString());
                                botclient.output(String.Format(" [Inventory Item] Name: {0} <==> {1} OP:{2} ITEM:{3}", ii.Name, ii.UUID, operation, itemName));
                            }


                            if (String.Compare(ii.Name, itemName, true) == 0)
                            {
                                // we found a matcher so lets do our ops
                                if (operation == "wear") botclient.Appearance.WearOutfit(ii.UUID, false);
                                if (operation == "animationStart") botclient.Self.AnimationStart(ii.UUID, false);
                                if (operation == "animationStop") botclient.Self.AnimationStop(ii.UUID, false);
                                if (operation == "attach") botclient.Appearance.Attach(ii, AttachmentPoint.Default);
                                return;
                            }
                        }
                    }

                    // now run any sub folders
                    foreach (InventoryBase ib in folderContents)
                    {

                        if (ib is InventoryFolder)
                        {
                            botclient.output(String.Format(" [Folder] Name: {0} <==> {1} OP:{2} ITEM:{3}", ib.Name, ib.UUID, operation, itemName));
                            InventoryFolder fld = (InventoryFolder)ib;
                            //appendFolderHook(fld);
                            //fld.RequestContents();
                            if ((operation == "wear") && (ib.Name == itemName))
                            {
                                botclient.Appearance.WearOutfit(ib.UUID, false);
                                botclient.output(" [WEAR] Name: " + ib.Name + " <==> " + ib.UUID.ToString());
                                return;
                            }
                            evalOnFolders(ib as InventoryFolder, operation, itemName);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                botclient.output("Search Exception :" + e.StackTrace);
                botclient.output("  msg:" + e.Message);

            }
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
                List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
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
                                botclient.evalLispString(lispCode);
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

                List<InventoryBase> folderContents = botclient.Inventory.FolderContents(folder.UUID, botclient.Self.AgentID,
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
    }

}
