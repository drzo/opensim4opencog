using System;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Money
{
    public class GiveAllCommand : Command, BotPersonalCommand
    {
		public GiveAllCommand(BotClient testClient)
		{
			Name = "Give All Money";
			Description = "Gives you all it's money.";
            Category = CommandCategory.Money;
            Parameters = CreateParams("targets", typeof(PrimSpec), "The targets of " + Name);
		}

        public override CmdResult ExecuteRequest(CmdRequest args)
		{
            var fromAgentID = CogbotHelpers.NonZero((UUID)BotClient.SessionToCallerId(args.CallerAgent), UUID.Zero);
            if (fromAgentID == UUID.Zero)
                return Failure("Unable to send money to console.  This command only works when IMed.");
            if (fromAgentID == Client.Self.AgentID)
                return Failure("Unable to send money to self.");

		    int amount = Client.Self.Balance;
		    Client.Self.GiveAvatarMoney(fromAgentID, Client.Self.Balance, "BotClient.GiveAll");
		    return Success("Gave $" + amount + " to " + fromAgentID);
		}
    }
}
