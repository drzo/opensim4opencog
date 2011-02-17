using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class WaitForLoginCommand : Command, BotSystemCommand
    {
        public WaitForLoginCommand(BotClient testClient)
        {
            Name = "waitforlogin";
            Description = "Waits until all bots that are currently attempting to login have succeeded or failed";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int time = 10;
            if (args.Length > 0)
            {
                if (!int.TryParse(args[0], out time))
                {
                    time = 10;
                }
            }
            bool all = (args.Length > 1 && args[1].ToLower() == "all");

            var LoginList = all ? ClientManager.SingleInstance.BotClients : new List<BotClient> {TheBotClient};            

            int retries = time;
            while (retries > 0)
            {
                bool someoneNotLoggedIn = false;
                foreach (var bot in LoginList)
                {
                    if (bot.IsLoggedInAndReady) continue;
                    WriteLine("Pending login: " + bot.GetName());
                    someoneNotLoggedIn = true;
                }
                if (!someoneNotLoggedIn) break;               
                Thread.Sleep(1000);
                retries--;
            }
            foreach (var bot in LoginList)
            {
                var net = bot.gridClient.Network;
                if (net.CurrentSim == null)
                {
                    Failure("Still Pending logins: " + bot.GetName());
                }
                else
                {
                    Success(bot.GetName() + ": " + net.CurrentSim);
                }
            }
            return SuccessOrFailure();
        }
    }
}
