using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Wear : Action
    {
        public Wear(BotClient Client)
            : base(Client)
        {
            helpString = "Usage: wear [outfit name] ";
            usageString = helpString + "\r\n you can type  'wear [bake] /My Outfit/Dance Party";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            AutoResetEvent are = new AutoResetEvent(false);
            AppearanceManager.AppearanceUpdatedCallback callback = (Primitive.TextureEntry te) => are.Set();
            try
            {
                Client.Appearance.OnAppearanceUpdated += callback;
                // base.acceptInput(verb, args);
                string target = String.Empty;
                if (args.Length == 0) return usageString;
                bool bake = false;
                string wear = args.str.Trim();
                if (args[0] == "bake")
                {
                    bake = true;
                    wear = wear.Substring(4).Trim();
                }
                if (args[0] == "test")
                {
                    bake = true;
                    wear = wear.Substring(4).Trim();
                    TheBotClient.wearFolder(wear);
                    if (!are.WaitOne(WEARABLE_TIMEOUT * 2))
                        return "Timeout wearing " + wear + " " + (bake ? " (baked)" : " (not baked)");
                    else
                        return ("wearing folder: " + wear + " " + (bake ? " (baked)" : " (not baked)"));
                }
                try
                {
                    WriteLine("wearing folder: " + wear + " " + (bake ? " (baked)" : " (not baked)"));
                    Client.Appearance.WearOutfit(wear.Split('/'));
                    if (!are.WaitOne(WEARABLE_TIMEOUT * 2))
                        return "Timeout wearing " + wear + " " + (bake ? " (baked)" : " (not baked)");
                    else
                        return wear;
                }
                catch (Exception ex)
                {
                    return "(Invalid outfit (" + ex.Message + ")" + args.str + ".";
                }
            }
            finally
            {
                Client.Appearance.OnAppearanceUpdated -= callback;
            }
        }

        protected int WEARABLE_TIMEOUT
        {
            get { return 20000; }
        }
    }
}
