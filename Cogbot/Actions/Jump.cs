using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Jump : Command
    {
        public Jump(BotClient Client)
            : base(Client)
        {
            Description = "Jump for 500ms.";
            Usage = "to Jump type \"jump\"";
            Name = "Jump";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
          //  base.acceptInput(verb, args);

            Client.Self.Jump(true);
            System.Threading.Thread.Sleep(500);
            Client.Self.Jump(false);

            TheBotClient.describeNext = true;
            return Success("$bot jumped.");
        }
    }
}
