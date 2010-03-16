using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;


namespace cogbot.Actions.Movement
{
    class Jump : Command, BotPersonalCommand
    {
        public Jump(BotClient Client)
            : base(Client)
        {
            Description = "Jump for 500ms.";
            Usage = "to Jump type \"jump\"";
            Name = "Jump";
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
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
