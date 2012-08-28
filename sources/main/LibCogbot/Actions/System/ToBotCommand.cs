using System;
using System.Threading;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class ToBotCommand : Command, SystemApplicationCommand
    {
        public ToBotCommand(BotClient testClient)
        {
            Name = "tobot";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description =
                "Send a command only to one bot.  This is useful when more than one bot is listening to your botcommands on open channel." +
                "The bot must be logged on from the same Cogbot instance";
            AddUsage("tobot <avatar> <command>", "Send the command only to avatar");
            AddExample("tobot \"Nephrael Rae\" anim KISS", "Make Nephrael Rae play the kiss animation");
            Parameters = CreateParams(
                "avatar", typeof (AgentSpec), "the avatar to perform the command on",
                "command", typeof (BotCommand), "the command to perform");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 2) return ShowUsage();
            BotClient oBotClient = ClientManager.GetBotByName(args.GetString("avatar"));
            if (oBotClient != TheBotClient) return Success("not for me");
            string botcmd = args.GetString("command");
            return
                Success("tobot " + oBotClient + " " +
                        oBotClient.ExecuteCommand(botcmd, args.CallerAgent, args.Output, args.CmdFlags));
        }
    }

    public class AllBotsCommand : Command, SystemApplicationCommand
    {
        public AllBotsCommand(BotClient testClient)
        {
            Name = "AllBots";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description =
                "Send a command to all bots.  This is useful when more than one bot is listening to your botcommands on open channel." +
                "The bots must be logged on from the same Cogbot instance";
            AddUsage("allbots <botcmd>", "Send the command only to avatar");
            AddExample("allbots anim KISS", "Make all bots play the kiss animation");
            Parameters = CreateParams(
                "command", typeof (BotCommand), "the command to perform");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1) return ShowUsage();
            string cmd = args.GetString("command");
            //return ClientManager.DoCommandAll()
            // Make an immutable copy of the Clients dictionary to safely iterate over
            int[] completed = {0};
            var BotClients = ClientManager.BotClients;
            int count = BotClients.Count;
            var fromAgentID = args.CallerAgent;
            if (count == 0) return ClientManager.ExecuteSystemCommand(cmd, fromAgentID, WriteLine, args.CmdFlags);
            CmdResult[] results = new CmdResult[count];
            int[] clientNum = {0};
            foreach (BotClient client in BotClients)
            {
                clientNum[0]++;
                ThreadPool.QueueUserWorkItem(
                    (WaitCallback)
                    delegate(object state)
                        {
                            BotClient testClient = (BotClient) state;
                            results[clientNum[0]] = testClient.ExecuteCommand(cmd, fromAgentID, WriteLine, args.CmdFlags);
                            ++completed[0];
                        },
                    client);
            }
            while (completed[0] < count)
                Thread.Sleep(50);
            foreach (var r in results)
            {
                if (r != null) return r;
            }
            return results[0];
        }
    }
}