using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class AnimCommand : Command
    {
        public AnimCommand(BotClient testClient)
        {
            Client = testClient;
            Name = "anim";
            Description = "Do a amination or gesture.  Usage:  anim [1-10] aminname";
            Category = CommandCategory.Appearance;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length < 1)
                return  "Usage:  anim [1-10] HOVER";
            int time = 1;
            for (int i = 0; i < args.Length; i++)
            {

            }
            string target = String.Empty;
            bool bake = true;

            for (int ct = 0; ct < args.Length; ct++)
            {
                if (args[ct].Equals("nobake"))
                    bake = false;
                else
                    target = target + args[ct] + " ";
            }

            target = target.TrimEnd();

            try
            {
                Client.Appearance.WearOutfit(target.Split('/'), bake);
            }
            catch (InvalidOutfitException ex)
            {
                return "Invalid outfit (" + ex.Message + ")";
            }

            return String.Empty;
        }
    }
}
