using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
using OpenMetaverse.StructuredData;

namespace Cogbot.Actions.Communication
{
    public class SayCommand : Command, BotPersonalCommand
    {
        public SayCommand(BotClient testClient)
		{
			Name = "say";
            Description = "chat a message.  If the message starts with #&lt;integer&gt; it is chatted on a channel." +
                          "see <a href='wiki/BotCommands#shout'>shout</a> and <a href='wiki/BotCommands#whisper'>whisper</a> to " +
                          "increase or decrease range. If the message is surrounded by &lt; and &gt; it is interpreted as passed " +
                          "to a physical robot body. See BotClient.cs for details";
            Category = CommandCategory.Communication;
            AddVersion(CreateParams(
                           Optional("#channel", typeof (int), "the optional channel in which the message goes out"),
                           "message", typeof (string), "Message to chat. If it starts with # followed by an integer, chats on channel"),
                           Description);
            ResultMap = CreateParams(
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if command was successful");
		}

        public override CmdResult ExecuteRequest(CmdRequest args)
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
            string text = message.ToString();
            if (text.StartsWith("<") && text.EndsWith(">"))
            {
                TheBotClient.XmlTalk(text, WriteLine);
                return Success("xmlsaid: " + text);
            }
            if (channel == 0)
            {
                TheBotClient.Talk(text);
            }
            else
            {
                TheBotClient.Talk(text, channel, ChatType.Normal);
            }
            return new CmdResult("said: " + text, true, Results);
        }
    }
}
