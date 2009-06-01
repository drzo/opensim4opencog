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
            helpString = "Gives the coordinates of where you are.";
            usageString = "To locate the coordinates of yourself, type in \"locate\"";
        }

        public override string acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);

            return ("You are in " + Client.Network.CurrentSim.Name + "/" + (int)Client.Self.SimPosition.X + "/" + (int)Client.Self.SimPosition.Y + "/" + (int)Client.Self.SimPosition.Z);
        }
    }
}
