using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Wear : Command
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
                if (args.Length == 0) return Failure(Usage);
                bool bake = true;
                string wear = args.str.Trim();
                if (args[0] == "nobake")
                {
                    bake = false;
                    wear = wear.Substring(6).Trim();
                }
                if (args[0] == "test")
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
                    Client.Appearance.ReplaceOutfit(outfit);
                  //  if (!are.WaitOne(WEARABLE_TIMEOUT * 2))
                   //     return Success("Timeout wearing " + wear + " " + (bake ? " (baked)" : " (not baked)");
                   // else
                        return Success(wear);
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
