using System;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

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

        public override CmdResult ExecuteRequest(CmdRequest args)
		{
            Client.Self.Jump(true);
            return Success("Jumped");
		}
    }
}
