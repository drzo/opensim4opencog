using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Stand : Action
    {
        public Stand(TextForm parent)
            : base(parent)
        {
            helpString = "Stand up.";
            usageString = "To Stand up, type \"stand\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            //base.acceptInput(verb, args);

            Sit sit = (Sit)parent.actions["sit"];
            if (Client.Self.SittingOn == 0 && !sit.sittingOnGround)
            {
                parent.output("You are already standing.");
            }
            else
            {
                Client.Self.Crouch(false);
                Client.Self.Stand();
                parent.output("You stood up.");
                sit.sittingOnGround = false;
            }

            parent.describeNext = true;
        }
    }
}
