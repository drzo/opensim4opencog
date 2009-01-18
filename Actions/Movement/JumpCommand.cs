using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class JumpCommand: Command
    {
        public JumpCommand(BotClient testClient)
		{
			Name = "jump";
			Description = "Jumps or flies up";
            Category = CommandCategory.Movement;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
            Client.Self.Jump(true);
            return "Jumped";
		}
    }
}
