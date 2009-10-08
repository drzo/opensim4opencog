using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class GoHomeCommand : Command
    {
		public GoHomeCommand(BotClient testClient)
        {
            Name = "gohome";
            Description = "Teleports home";
            Category = CommandCategory.Movement;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
			if ( Client.Self.GoHome() ) {
				return "Teleport Home Succesful";
			} else {
				return "Teleport Home Failed";
			}
        }
    }
}
