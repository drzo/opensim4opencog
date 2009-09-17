using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{
    class StopFlying : Command
    {
        public StopFlying(BotClient Client)
            : base(Client)
        {
            helpString = "You stop flying.";
            usageString = "To stop flying type: \"stop-flying\"";
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
         //   base.acceptInput(verb, args);
            Client.Self.Fly(false);

            TheBotClient.describeNext = true;
            return "$bot stopped flying";
        }
    }
}
