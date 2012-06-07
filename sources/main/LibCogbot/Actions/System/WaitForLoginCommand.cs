using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;

using MushDLR223.ScriptEngines;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif

namespace Cogbot.Actions.System
{
    public class WaitForLoginCommand : Command, BotSystemCommand
    {
        public WaitForLoginCommand(BotClient testClient)
        {
            Name = "waitforlogin";
            Description = "Waits until bots that are currently attempting to login have succeeded or failed." + 
                "If 'all' is included it waits for all bots. By default it waits only for the currently targeted bot.";
            Details = @"<p>waitforlogin  -   <i>time out after 10 sec</i></p>
<p>waitforlogin 20  -  <i>time out after 20 sec</i></p>
<p>waitforlogin all 20   - <i>wait up to 20 sec for all bots to log in</i></p>";
            Category = CommandCategory.BotClient;
            Parameters = CreateParams(
                Optional(
                "timeout", typeof(int), "max seconds to wait"),
                Optional(
                "all", typeof(bool), "if true, wait for all bots"));
            ResultMap = CreateParams(
     "message", typeof(string), "if we timed out, who we're waiting for",
     "success", typeof(bool), "true if bots logged in");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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
                    AddSuccess(bot.GetName() + ": " + net.CurrentSim);
                }
            }
            return SuccessOrFailure();
        }
    }
}
