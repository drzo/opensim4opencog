using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions.Movement
{
    class StopFlying : Command, BotPersonalCommand
    {
        public StopFlying(BotClient Client)
            : base(Client)
        {
            Description = "You stop flying.";
            Usage = "To stop flying type: \"stop-flying\"";
            Category = CommandCategory.Movement;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
         //   base.acceptInput(verb, args);
            Client.Self.Fly(false);

            TheBotClient.describeNext = true;
            return Success("$bot stopped flying");
        }
    }
}
