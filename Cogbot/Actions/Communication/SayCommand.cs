using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class SayCommand : Command, BotPersonalCommand
    {
        public SayCommand(BotClient testClient)
		{
			Name = "say";
			Description = "Say something.  Usage: say (optional channel) whatever)";
            Category = CommandCategory.Communication;
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
