using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Communication
{
    public class SayCommand : Command, BotPersonalCommand
    {
        public SayCommand(BotClient testClient)
		{
			Name = "say";
            Description = Name + " something.";
            Usage = Name + " (optional channel) whatever";
            Category = CommandCategory.Communication;
            Parameters =
                NamedParam.CreateParams(
                            NamedParam.Optional("channel", typeof(int), "the optional channel in which the message goes out"),
                            "message", typeof(string), "what you output to the simulator");
            ResultMap = NamedParam.CreateParams(
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if command was successful");
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            int channel = 0;
            int startIndex = 0;
            
            if (args.Length < 1)
            {
                return ShowUsage();// " say (optional channel) whatever";
            }
            else if (args.Length > 1 && (args[0].StartsWith("/") || args[0].StartsWith("#")))
            {
                if (Int32.TryParse(args[0].Substring(1), out channel))
					startIndex = 1;
            }

            StringBuilder message = new StringBuilder();

			for (int i = startIndex; i < args.Length; i++)
            {
                message.Append(args[i]);
                if (i != args.Length - 1) message.Append(" ");
            }

			Client.Self.Chat(message.ToString(), channel, ChatType.Normal);

            return Success("Said " + message.ToString());
		}
    }
}
