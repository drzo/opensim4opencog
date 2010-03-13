using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.System
{
    public class LogoutCommand : Command, BotSystemCommand
    {
        public LogoutCommand(BotClient testClient)
        {
            Name = "logout";
            Description = "Log this avatar out";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string name = Client.ToString();
			TheBotClient.ClientManager.Logout(TheBotClient);
            return Success("Logged " + name + " out");
        }
    }
}
