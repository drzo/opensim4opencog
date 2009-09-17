using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Stand : Command
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

            SimActor sitter = WorldSystem.TheSimAvatar;
            if (!sitter.IsSitting)
            {
                return ("$bot is already standing.");
            }
            else
            {
                sitter.StandUp();
            }

            Client.describeNext = true;
            return("$bot stood up.");
        }
    }
}
