using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Crouch : Action
    {
        public Crouch(TextForm parent)
            : base(parent)
        {
            helpString = "Crouch.";
            usageString = "To Crouch type \"crouch\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            //base.acceptInput(verb, args);

            parent.output("You crouched.");
            Client.Self.Crouch(true);

            parent.describeNext = true;
        }
    }
}
