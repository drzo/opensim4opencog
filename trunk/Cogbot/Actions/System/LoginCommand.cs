using System;
using System.Collections.Generic;
using System.Reflection;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class LoginCommand : Command, BotSystemCommand
    {
        public LoginCommand(BotClient testClient)
        {
            Name = "login";
            Description = "Logs in another avatar. Usage: login firstname lastname password [simname] [loginuri]";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            // This is a dummy command. Calls to it should be intercepted and handled specially
            return Success("This command should not be executed directly");
        }
    }
}
