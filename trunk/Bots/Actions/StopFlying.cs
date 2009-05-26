using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class StopFlying : Action
    {
        public StopFlying(BotClient Client)
            : base(Client)
        {
            helpString = "You stop flying.";
            usageString = "To stop flying type: \"stop-flying\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
         //   base.acceptInput(verb, args);

            WriteLine("You stopped flying.");
            Client.Self.Fly(false);

            TheBotClient.describeNext = true;
        }
    }
}
