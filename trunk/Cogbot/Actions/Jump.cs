using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Jump : Action
    {
        public Jump(BotClient Client)
            : base(Client)
        {
            helpString = "Jump for 500ms.";
            usageString = "to Jump type \"jump\""; ;
        }

        public override string acceptInput(string verb, Parser args)
        {
          //  base.acceptInput(verb, args);

            Client.Self.Jump(true);
            System.Threading.Thread.Sleep(500);
            Client.Self.Jump(false);

            TheBotClient.describeNext = true;
            return "You jumped.";
        }
    }
}
