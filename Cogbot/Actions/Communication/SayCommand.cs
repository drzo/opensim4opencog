using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class SayCommand: Command
    {
        public SayCommand(BotClient testClient)
		{
			Name = "say";
			Description = "Say something.  (usage: say (optional channel) whatever)";
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

			Client.Self.Chat(message.ToString(), channel, ChatType.Normal);

            return Success("Said " + message.ToString());
		}
    }
}
