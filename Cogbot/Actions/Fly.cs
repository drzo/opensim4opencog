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
            helpString = "To start flying type: \"fly\"";
            usageString = "fly [up|down] 'no_argument= start flying";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
          //  base.acceptInput(verb, args);

            Client.describeNext = true;

            if (args.str == "up")
            {
                Client.Self.Movement.UpPos = true;
                Client.Self.Movement.SendUpdate(true);
                Thread.Sleep(1000);
                Client.Self.Movement.UpPos = false;
                Client.Self.Movement.SendUpdate(true);
                return "flew up";
            }
            else if (args.str == "down")
            {
                Client.Self.Movement.UpNeg = true;
                Client.Self.Movement.SendUpdate(true);
                Thread.Sleep(1000);
                Client.Self.Movement.UpNeg = false;
                Client.Self.Movement.SendUpdate(true);
                return "flew down";
            }
            else
            {
                Client.Self.Fly(true);
                return "now flying";
            }

        }
    }
}

