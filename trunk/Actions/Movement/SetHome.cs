using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SetHomeCommand : Command
    {
		public SetHomeCommand(cogbot.TextForm testClient)
        {
            Name = "sethome";
            Description = "Sets home to the current location.";
            Category = CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
			client.Self.SetHome();
            return "Home Set";
        }
    }
}
