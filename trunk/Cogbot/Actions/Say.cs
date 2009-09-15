using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    class Say : Action
    {
        public Say(BotClient Client) 
            : base(Client) 
        {
            Name = GetType().Name.ToLower().Replace("command", "");
            helpString = "Say a message for everyone to hear.";
            usageString = "To communicate to everyone, type \"say <message>\"";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
          //  base.acceptInput(verb, args);

            if (args.str.Length > 0)
            {
                TheBotClient.Talk(args.str);
            }
            return "said: " + args.str;
        }
    }
}
