using System;
using System.Collections.Generic;
using System.Text;
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
                return ("wearing folder: " + wear + " " + (bake ? " (baked)" : " (not baked)"));
            } 
            try
            {
                WriteLine("wearing folder: " + wear + " " +(bake?" (baked)":" (not baked)"));
                Client.Appearance.WearOutfit(args.str.Split('/'), bake);
                return wear;
            }
            catch (InvalidOutfitException ex)
            {
                return "(Invalid outfit (" + ex.Message + ")" + args.str+".";
            }



        }
    }
}
