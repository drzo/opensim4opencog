using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    public class SetHomeCommand : Command, BotPersonalCommand
    {
		public SetHomeCommand(BotClient testClient)
        {
            Name = "sethome";
            Description = "Sets home to the current location.";
            Category = CommandCategory.Movement;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
			Client.Self.SetHome();
            return Success("Home Set");
        }
    }
}
