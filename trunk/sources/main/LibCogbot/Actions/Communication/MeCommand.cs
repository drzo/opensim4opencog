using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Communication
{
    public class MeCommand : Command, BotPersonalCommand
    {
        public MeCommand(BotClient testClient)
        {
            Name = "me";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "Emote something.";
            Details = AddUsage(Name + " (optional channel) whatever", "emotes whatever (optionally to channel)");
            Category = CommandCategory.Communication;
            Parameters =
                CreateParams(
                    Optional("channel", typeof (int), "the optional channel in which the message goes out"),
                    "message", typeof (string), "what you output to the simulator");
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int channel = 0;
            int startIndex = 0;

            if (args.Length < 1)
            {
                return ShowUsage(); // " me (optional channel) whatever";
            }
            else if (args.Length > 1)
            {
                if (Int32.TryParse(args[0], out channel))
                    startIndex = 1;
            }

            StringBuilder message = new StringBuilder();

            for (int i = startIndex; i < args.Length; i++)
            {
                message.Append(args[i]);
                if (i != args.Length - 1) message.Append(" ");
            }

            Client.Self.Chat("/me " + message.ToString(), channel, ChatType.Normal);

            return Success("Emoted " + message.ToString());
        }
    }
}