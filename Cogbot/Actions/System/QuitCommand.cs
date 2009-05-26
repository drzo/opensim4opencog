using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class QuitCommand : Command, SystemApplicationCommand
    {
        public QuitCommand(BotClient testClient)
		{
			Name = "quit";
			Description = "Log all avatars out and shut down";
            Category = CommandCategory.TestClient;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
            // This is a dummy command. Calls to it should be intercepted and handled specially
            return "This command should not be executed directly";
		}
    }
}
