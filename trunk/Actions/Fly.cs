using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading; //using libsecondlife;

namespace cogbot.Actions
{
    class Fly : Action
    {
        public Fly(BotClient Client)
            : base(Client)
        {
            Name = "fly";
            helpString = "You start flying.";
            usageString = "To start flying type: \"fly\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
          //  base.acceptInput(verb, args);

            WriteLine("You are flying.");

            if (args.str == "up")
            {
                Client.Self.Movement.UpPos = true;
                Client.Self.Movement.SendUpdate(true);
                Thread.Sleep(1000);
                Client.Self.Movement.UpPos = false;
                Client.Self.Movement.SendUpdate(true);
            }
            else if (args.str == "down")
            {
                Client.Self.Movement.UpNeg = true;
                Client.Self.Movement.SendUpdate(true);
                Thread.Sleep(1000);
                Client.Self.Movement.UpNeg = false;
                Client.Self.Movement.SendUpdate(true);
            }
            else
            {
                Client.Self.Fly(true);
            }

            Client.describeNext = true;
        }
    }
}

