using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Jump : Action
    {
        public Jump(TextForm parent)
            : base(parent)
        {
            helpString = "Jump.";
            usageString = "to Jump type \"jump\""; ;
        }

        public override void acceptInput(string verb, Parser args)
        {
            base.acceptInput(verb, args);

            parent.output("You jumped.");
            client.Self.Jump(true);

            parent.describeNext = true;
        }
    }
}
