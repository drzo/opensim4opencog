using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class WaitForLoginCommand : Command, BotSystemCommand
    {
        public WaitForLoginCommand(BotClient testClient)
        {
            Name = "waitforlogin";
            Description = "Waits until all bots that are currently attempting to login have succeeded or failed";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            foreach (var bot in ClientManager.SingleInstance.BotClients)
            {
                var net = bot.gridClient.Network;
                int retries = 10;
                while (net.CurrentSim == null)
                {
                    WriteLine("Pending logins: " + bot.GetName());
                    System.Threading.Thread.Sleep(1000);
                    retries--;
                    if (retries < 1) break;
                }
            }

            return Success("All pending logins have completed, currently tracking " + ClientManager.SingleInstance.BotClients.Count + " bots");
        }
    }
}
