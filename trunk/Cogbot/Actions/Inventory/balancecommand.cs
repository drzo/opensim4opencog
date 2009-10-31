using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class BalanceCommand : Command, BotPersonalCommand
    {
        public BalanceCommand(BotClient testClient)
		{
			Name = "balance";
			Description = "Shows the amount of L$.";
            Category = CommandCategory.Other;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            System.Threading.AutoResetEvent waitBalance = new System.Threading.AutoResetEvent(false);
            
            EventHandler<BalanceEventArgs> del = delegate(object sender, BalanceEventArgs e) { waitBalance.Set(); };
            Client.Self.MoneyBalance += del;            
            Client.Self.RequestBalance();
            String result = "Timeout waiting for balance reply";
            if (waitBalance.WaitOne(10000, false))
            {
                result = Client.ToString() + " has L$: " + Client.Self.Balance;
            }            
            Client.Self.MoneyBalance -= del;
            return Success(result);

		}
    }
}
