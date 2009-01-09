using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class StopFlying : Action
    {
        public StopFlying(TextForm parent)
            : base(parent)
        {
            helpString = "You stop flying.";
            usageString = "To stop flying type: \"stop-flying\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
         //   base.acceptInput(verb, args);

            parent.output("You stopped flying.");
            Client.Self.Fly(false);

            parent.describeNext = true;
        }
    }
}
