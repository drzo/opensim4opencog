using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Stand : Action
    {
        public Stand(BotClient Client)
            : base(Client)
        {
            helpString = "Stand up.";
            usageString = "To Stand up, type \"stand\"";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            Sit sit = (Sit)Client.Commands["sit"];
            if (Client.Self.SittingOn == 0 && !sit.sittingOnGround)
            {
                return ("$bot is already standing.");
            }
            else
            {
                Client.Self.Crouch(false);
                Client.Self.Stand();
                sit.sittingOnGround = false;
            }

            Client.describeNext = true;
            return("$bot stood up.");
        }
    }
}
