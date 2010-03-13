using System;
using OpenMetaverse;

namespace cogbot.Actions.System
{
    public class ToBotCommand : Command, SystemApplicationCommand
    {
        public ToBotCommand(BotClient testClient)
        {
            Name = "tobot";
            Description = "Send a command only to one bot.  Usage: tobot \"Nephrael Rae\" anim KISS";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2) return ShowUsage();
            BotClient oBotClient = ClientManager.GetBotByName(args[0]);
            if (oBotClient == null) return Success("not for me");
            string botcmd = String.Join(" ", args, 1, args.Length - 1).Trim();
            return Success("tobot " + oBotClient + " " + oBotClient.ExecuteCommand(botcmd, WriteLine));
        }
    }
}
