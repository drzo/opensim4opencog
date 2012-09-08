using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class SetBotCommand : Command, SystemApplicationCommand
    {
        public SetBotCommand(BotClient testClient)
        {
            Name = "setbot";
        }

        public override void MakeInfo()
        {
            Description = "Sets the current bot for subsequent botcmd REPL commands.";
            Details =
                AddExample(
                    @"
... log on two bots, Ima Bot and Another Bot
/setbot Ima Bot
/say hi, I am Ima Bot
... Ima Bot says hi, I am Ima Bot in chat
/setbot Another Bot
/say hi, I'm not Ima
... Another Bot says I'm not Ima  in chat
",
                    "first one bot, then a different one, chats");
            Parameters = CreateParams("bot", typeof (AgentSpec), "name of bot");
            AddUsage("setbot <none|<name>>", "Sets the bot by user name");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string botname = String.Join(" ", args).Trim();
            object key = args.Output;
            ClientManager.SetOnlyOneCurrentREPLBotClient(key, botname);
            // This is a dummy command. Calls to it should be intercepted and handled specially
            return Success("SetOnlyOneCurrentBotClient=" + TheBotClient.ClientManager.GetCurrentBotClient(key));
        }
    }
}