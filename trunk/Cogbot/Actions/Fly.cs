using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class Fly : Command, BotPersonalCommand
    {
        public Fly(BotClient Client)
            : base(Client)
        {
            Name = "fly";
            Description = "To start flying type: \"fly\"";
            Usage = "fly [up|down] 'no_argument= start flying";
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
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
                return Success("flew up");
            }
            else if (args.str == "down")
            {
                Client.Self.Movement.UpNeg = true;
                Client.Self.Movement.SendUpdate(true);
                Thread.Sleep(1000);
                Client.Self.Movement.UpNeg = false;
                Client.Self.Movement.SendUpdate(true);
                return Success("flew down");
            }
            else
            {
                Client.Self.Fly(true);
                return Success("now flying");
            }

        }
    }
}

