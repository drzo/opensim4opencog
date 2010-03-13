using System;
using System.Collections.Generic;
using System.Reflection;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.System
{
    public class AtCommand : Command, BotSystemCommand
    {
        public AtCommand(BotClient testClient)
        {
            Name = "@";
            Description = "Restrict the following commands to one or all avatars. Usage: @ [firstname lastname]";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            // This is a dummy command. Calls to it should be intercepted and handled specially
            return Success("This command should not be executed directly");            
        }
    }
}
