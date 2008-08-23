using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Locate : Action
    {
        public Locate(TextForm parent)
            : base(parent)
        {
            helpString = "Gives the coordinates of where you are.";
            usageString = "To locate the coordinates of yourself, type in \"locate\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            base.acceptInput(verb, args);

            parent.output("You are in " + client.Network.CurrentSim.Name + " " + (int)client.Self.SimPosition.X + "," + (int)client.Self.SimPosition.Y + "," + (int)client.Self.SimPosition.Z);
        }
    }
}
