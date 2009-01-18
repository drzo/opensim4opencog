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
            helpString = "Jump.";
            usageString = "to Jump type \"jump\""; ;
        }

        public override void acceptInput(string verb, Parser args)
        {
          //  base.acceptInput(verb, args);

            WriteLine("You jumped.");
            Client.Self.Jump(true);
            System.Threading.Thread.Sleep(500);
            Client.Self.Jump(false);

            Client.describeNext = true;
        }
    }
}
