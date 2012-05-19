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
            Description = "Makes the avatar fly";
            Usage = @"<p>fly  - <i>start flying</i></p>
<p>fly up  - <i>increase height by about 50 meters (one second key press), or jump if on ground</i></p>
<p>fly down - <i>decrease height by about 50 meters (one second key press). Will not auto-land</i></p>
<p>stop-flying  - <i>stop flying. Bot will fall if not near ground</i></p>";
            ParameterVersions = NamedParam.CreateParamVersions(
                NamedParam.CreateParams(),
                NamedParam.CreateParams("up", typeof(bool), "increase height by about 50 meters (one second key press), or jump if on ground"),
                NamedParam.CreateParams("down", typeof(bool), "decrease height by about 50 meters (one second key press). Will not auto-land")
               );
            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we flew");
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

