using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

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

            if (args.str.Length > 0)
            {
                TheBotClient.Talk(args.str);
            }
            return Success("said: " + args.str);
        }
    }
}
