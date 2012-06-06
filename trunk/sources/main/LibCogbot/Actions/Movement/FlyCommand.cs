using System;
using System.Threading;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    public class FlyCommand : Command, BotPersonalCommand
    {
        public FlyCommand(BotClient testClient)
        {
            Name = "fly";
            Description = "Makes the avatar fly";
            AddVersion(CreateParams(Optional("stop", typeof(bool), "if true stops flying")), "start flying unless stop is specified, Bot will fall if not near ground");
            AddVersion(CreateParams("up", typeof(bool), ""),
                       "increase height by about 50 meters (one second key press), or jump if on ground");
            AddVersion(CreateParams("down", typeof(bool), ""),
                       "decrease height by about 50 meters (one second key press). Will not auto-land");
            ResultMap = CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we flew");
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //  base.acceptInput(verb, args);

            Client.describeNext = true;
            if (args.str == "stop")
            {
                Client.Self.Fly(false);
                return Success("stopped flying");
            }
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
