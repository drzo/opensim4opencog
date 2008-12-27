using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class BalanceCommand: Command
    {
        public BalanceCommand(cogbot.TextForm testClient)
		{
			Name = "balance";
			Description = "Shows the amount of L$.";
            Category = CommandCategory.Other;
		}

        public override string Execute(string[] args, UUID fromAgentID)
		{
            System.Threading.AutoResetEvent waitBalance = new System.Threading.AutoResetEvent(false);
            AgentManager.BalanceCallback del = delegate(int balance) { waitBalance.Set(); };
            client.Self.OnBalanceUpdated += del;
            client.Self.RequestBalance();
            String result = "Timeout waiting for balance reply";
            if (waitBalance.WaitOne(10000, false))
            {
                result = client.ToString() + " has L$: " + client.Self.Balance;
            }            
            client.Self.OnBalanceUpdated -= del;
            return result;

		}
    }
}
