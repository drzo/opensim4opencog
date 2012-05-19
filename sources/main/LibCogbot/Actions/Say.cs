using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
using OpenMetaverse.StructuredData;

namespace cogbot.Actions
{
    class Say : Command, BotPersonalCommand
    {
        public Say(BotClient Client) 
            : base(Client) 
        {
            Description = "chat a message.  If the message starts with #&lt;integer&gt; it is chatted on a channel." +
                "see <a href='wiki/BotCommands#shout'>shout</a> and <a href='wiki/BotCommands#whisper'>whisper</a> to " +
                "increase or decrease range. If the message is surrounded by &lt; and &gt; it is interpreted as passed " +
                "to a physical robot body. See BotClient.cs for details";
            Usage = "say &lt;message&gt;";
            Parameters = NamedParam.CreateParams("message", typeof(string),
    "Message to chat. If it starts with # followed by an integer, chats on channel");
            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if we could not chat, the reason why",
                 "success", typeof(bool), "true if the chat succeeded");
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
          //  base.acceptInput(verb, args);
           
            string text = args.str;
            if (args.Length == 1)
            {
                text = args[0];
            }

            if (args.str.Length > 0)
            {
                if (text.StartsWith("<") && text.EndsWith(">"))
                {
                    TheBotClient.XmlTalk(text, WriteLine);
                    return Success("xmlsaid: " + text);
                }
                if (text.StartsWith("#"))
                {
                    int channel; 
                    int fi = text.IndexOf(' ');
                    if (Int32.TryParse(text.Substring(1, fi), out channel))
                    {
                        text = text.Substring(fi);
                        TheBotClient.Talk(text, channel, ChatType.Normal);
                        return Success("Said channel(" + channel + "): " + text);
                    }
                }
                TheBotClient.Talk(text);
            }
            return new CmdResult("said: " + text, true, new OSDMap());
        }
    }
}
