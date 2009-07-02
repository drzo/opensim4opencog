using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Locate : Action
    {
        public Locate(BotClient Client)
            : base(Client)
        {
            helpString = "Gives the coordinates of where $bot is.";
            usageString = "To locate the coordinates of yourself, type in \"locate\"";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
           // base.acceptInput(verb, args);

            return ("$bot is in " + Client.Network.CurrentSim.Name + "/" + (int)GetSimPosition().X + "/" + (int)GetSimPosition().Y + "/" + (int)GetSimPosition().Z);
        }
    }
}
