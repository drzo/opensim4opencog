using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    public class StandCommand : Command, BotPersonalCommand
    {
        public StandCommand(BotClient testClient)
	{
		Name = "Stand";
		Description = "Stand";
        Category = CommandCategory.Movement;
	}
	
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
	    {
            Client.Self.Stand();
		    return Success("Standing up.");  
	    }
    }
}
