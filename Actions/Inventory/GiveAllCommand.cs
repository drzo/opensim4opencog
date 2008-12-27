using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class GiveAllCommand: Command
    {
		public GiveAllCommand(cogbot.TextForm testClient)
		{
			Name = "giveAll";
			Description = "Gives you all it's money.";
            Category = CommandCategory.Other;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
			if (fromAgentID == UUID.Zero)
				return "Unable to send money to console.  This command only works when IMed.";

		    int amount = client.Self.Balance;
		    client.Self.GiveAvatarMoney(fromAgentID, client.Self.Balance, "cogbot.TextForm.GiveAll");
		    return "Gave $" + amount + " to " + fromAgentID;
		}
    }
}
