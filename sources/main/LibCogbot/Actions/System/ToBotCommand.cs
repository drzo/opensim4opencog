using System;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class ToBotCommand : Command, SystemApplicationCommand
    {
        public ToBotCommand(BotClient testClient)
        {
            Name = "tobot";
            Description =
                "Send a command only to one bot.  This is useful when more than one bot is listening to your botcommands on open channel." +
                "The bot must be logged on from the same Cogbot instance";
            Details = 
                AddUsage("tobot <avatar> <botcmd>", "Send the command only to avatar") +
                Example("tobot \"Nephrael Rae\" anim KISS", "Make Nephrael Rae play the kiss animation");
            Parameters = CreateParams(
                "avatar", typeof(AgentSpec), "the avatar to perform the command on",
                "command", typeof(BotCommand), "the command to perform");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 2) return ShowUsage();
            BotClient oBotClient = ClientManager.GetBotByName(args[0]);
            if (oBotClient != TheBotClient) return Success("not for me");
            string botcmd = String.Join(" ", args, 1, args.Length - 1).Trim();
            return Success("tobot " + oBotClient + " " + oBotClient.ExecuteCommand(botcmd, fromAgentID, WriteLine));
        }
    }
}
