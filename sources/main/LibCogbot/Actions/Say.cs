using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    class Say : Command, BotPersonalCommand
    {
        public Say(BotClient Client) 
            : base(Client) 
        {
            Name = GetType().Name.ToLower().Replace("command", "");
            Description = "Say a message for everyone to hear.";
            Usage = "To communicate to everyone, type \"say <message>\"";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
          //  base.acceptInput(verb, args);
           
            string text = args.str;

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
            return Success("said: " + text);
        }
    }
}
