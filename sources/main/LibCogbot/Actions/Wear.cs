using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    class Wear : Command, BotPersonalCommand
    {
        public Wear(BotClient Client)
            : base(Client)
        {
            Description = "Usage: wear [outfit name] ";
            Usage = Description + "\r\n you can type  'wear [bake] /My Outfit/Dance Party";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //AutoResetEvent are = new AutoResetEvent(false);
           // AppearanceManager.AppearanceUpdatedCallback callback = (Primitive.TextureEntry te) => are.Set();
            try
            {
                //Client.Appearance.OnAppearanceUpdated += callback;
                // base.acceptInput(verb, args);
                string target = String.Empty;
                if (args.Length == 0) return ShowUsage();
                bool bake = true;
                string wear = args.str.Trim();
                if (args.IsTrue("nobake"))
                {
                    bake = false;
                    wear = wear.Substring(6).Trim();
                }
                if (args.IsTrue("test"))
                {
                    bake = true;
                    wear = wear.Substring(4).Trim();
                    TheBotClient.wearFolder(wear);
                   // if (!are.WaitOne(WEARABLE_TIMEOUT * 2))
                     //   return Success("Timeout wearing " + wear + " " + (bake ? " (baked)" : " (not baked)");
                   // else
                    return Success("wearing folder: " + wear + " " + (bake ? " (baked)" : " (not baked)"));
                }
                try
                {
                    WriteLine("wearing folder: " + wear + " " + (bake ? " (baked)" : " (not baked)"));
                    List<InventoryItem> outfit = Client.GetFolderItems(wear);
                    if (outfit != null)
                    {
                        Client.Appearance.ReplaceOutfit(outfit);
                        return Success(wear);
                    }
                    WriteLine("no folder found attaching item: " + wear);
                    string lwear = wear;
                    BotInventoryEval searcher = new BotInventoryEval(Client);
                          InventoryFolder rootFolder = Client.Inventory.Store.RootFolder;
                    bool found = searcher.findInFolders(rootFolder, (ib)=>
                                                           {
                                                               InventoryItem item = ib as InventoryItem;
                                                               if (item != null && item.Name.ToLower() == lwear)
                                                               {

                                                                   Client.Appearance.Attach(item,
                                                                                            AttachmentPoint.Default);
                                                                   return true;
                                                               }
                                                               return false;
                                                           });
                    if (found) return Success("attaching " + wear);
                    return Failure("did not find " + wear);
                    //  if (!are.WaitOne(WEARABLE_TIMEOUT * 2))
                   //     return Success("Timeout wearing " + wear + " " + (bake ? " (baked)" : " (not baked)");
                   // else

                }
                catch (Exception ex)
                {
                    return Failure( "(Invalid outfit (" + ex.Message + ")" + args.str + ".");
                }
            }
            finally
            {
               // Client.Appearance.OnAppearanceUpdated -= callback;
            }
        }

        protected int WEARABLE_TIMEOUT
        {
            get { return 20000; }
        }
    }
}
