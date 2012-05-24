using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;


using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class Jump : Command, BotPersonalCommand
    {
        public Jump(BotClient Client)
            : base(Client)
        {
            Description = "Jump for 1/2 second.";
            Details = "jump";
            Name = "Jump";
            Parameters = CreateParams();
            ResultMap = CreateParams(
                 "message", typeof(string), "if we could not why, why (shouldnt happen)",
                 "success", typeof(bool), "true if we stood up");

            Category = CommandCategory.Movement;
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
          //  base.acceptInput(verb, args);

            Client.Self.Jump(true);
            Thread.Sleep(500);
            Client.Self.Jump(false);

            TheBotClient.describeNext = true;
            return Success("$bot jumped.");
        }
    }
}
