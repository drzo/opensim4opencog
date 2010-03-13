using System;
using OpenMetaverse;

namespace cogbot.Actions.Movement
{
    public class JumpCommand : Command, BotPersonalCommand
    {
        public JumpCommand(BotClient testClient)
		{
			Name = "jump";
			Description = "Jumps or flies up";
            Category = CommandCategory.Movement;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            Client.Self.Jump(true);
            return Success("Jumped");
		}
    }
}
