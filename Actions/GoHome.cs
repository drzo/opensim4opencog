using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class GoHomeCommand : Command
    {
		public GoHomeCommand(cogbot.TextForm testClient)
        {
            Name = "gohome";
            Description = "Teleports home";
            Category = CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
			if ( client.Self.GoHome() ) {
				return "Teleport Home Succesful";
			} else {
				return "Teleport Home Failed";
			}
        }
    }
}
