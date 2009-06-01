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
            usageString = helpString + "\r\n you can type  'wear /My Outfit/Dance Party";
        }

        public override string acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);
            string target = String.Empty;
            bool bake = true;
            try
            {
                WriteLine("wear args =(" + args.str + ").");
                Client.Appearance.WearOutfit(args.str.Split('/'), bake);
                return "worn " + args.str;
            }
            catch (InvalidOutfitException ex)
            {
                return "(Invalid outfit (" + ex.Message + ")" + args.str+".";
            }



        }
    }
}
