using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Wear : Action
    {
        public Wear(TextForm parent)
            : base(parent)
        {
            helpString = "Usage: wear [outfit name] ";
            usageString = helpString + "\r\n you can type  'wear /My Outfit/Dance Party";
        }

        public override void acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);
            string target = String.Empty;
            bool bake = true;
            try
            {
                parent.output("wear args =(" + args.str + ").");
                client.Appearance.WearOutfit(args.str.Split('/'), bake);
            }
            catch (InvalidOutfitException ex)
            {
                parent.output("Invalid outfit (" + ex.Message + ")" + args.str+".");
            }



        }
    }
}
