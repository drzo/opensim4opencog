using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class WearCommand : Command
    {
        public WearCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "wear";
            Description = "Wear an outfit folder from inventory. Usage: wear [outfit name] [nobake]";
            Category = CommandCategory.Appearance;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return "Usage: wear [outfit name] eg: 'wear /My Outfit/Dance Party";

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
            catch (Exception ex)
            {
                return "Invalid outfit (" + ex.Message + ")";
            }

            return String.Empty;
        }
    }
}
