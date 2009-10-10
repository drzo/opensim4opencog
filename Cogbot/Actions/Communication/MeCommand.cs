using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class MeCommand: Command
    {
        public MeCommand(BotClient testClient)
		{
			Name = "me";
			Description = "Emote something.  (usage: me (optional channel) whatever)";
            Category = CommandCategory.Communication;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            int channel = 0;
            int startIndex = 0;
            
            if (args.Length < 1)
            {
                return Failure(Usage);// " me (optional channel) whatever";
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
