using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.Money
{
    public class BalanceCommand : Command, BotPersonalCommand
    {
        public BalanceCommand(BotClient testClient)
		{
			Name = "balance";
			Description = "Shows the amount of L$.";
            Category = CommandCategory.Money;
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            AutoResetEvent waitBalance = new AutoResetEvent(false);
            
            EventHandler<BalanceEventArgs> del = delegate(object sender, BalanceEventArgs e) { waitBalance.Set(); };
            Client.Self.MoneyBalance += del;            
            Client.Self.RequestBalance();
            String result = "Timeout waiting for balance reply";
            if (waitBalance.WaitOne(10000, false))
            {
                result = Client.GetName() + " has L$: " + Client.Self.Balance;
            }            
            Client.Self.MoneyBalance -= del;
            return Success(result);

		}
    }
}
