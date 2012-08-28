using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Money
{
    public class BalanceCommand : Command, BotPersonalCommand, AsynchronousCommand
    {
        public BalanceCommand(BotClient testClient)
        {
            Name = "balance";
        }

        public override void MakeInfo()
        {
            Description = "Shows the amount of L$.";
            Category = CommandCategory.Money;
            Parameters = CreateParams();
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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