using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    class Say : Action
    {
        public Say(TextForm parent) 
            : base(parent) 
        {
            helpString = "Say a message for everyone to hear.";
            usageString = "To communicate to everyone, type \"say <message>\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            base.acceptInput(verb, args);

            if (args.str.Length > 0)
            {
                client.Self.Chat(args.str, 0, ChatType.Normal);
            }
        }
    }
}
