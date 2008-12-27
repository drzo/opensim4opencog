using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class StandCommand: Command
    {
        public StandCommand(cogbot.TextForm testClient)
	{
		Name = "stand";
		Description = "Stand";
        Category = CommandCategory.Movement;
	}
	
        public override string Execute(string[] args, UUID fromAgentID)
	    {
            client.Self.Stand();
		    return "Standing up.";  
	    }
    }
}
