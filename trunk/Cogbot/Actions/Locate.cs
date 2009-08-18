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
            if (Client.Network.CurrentSim == null) return "$bot is nowhere.";
            return (string.Format("$bot is in {0}/{1}/{2}/{3}", Client.Network.CurrentSim.Name, (int)GetSimPosition().X, (int)GetSimPosition().Y, (int)GetSimPosition().Z));
        }
    }
}
