using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class StopFlying : Command, BotPersonalCommand
    {
        public StopFlying(BotClient Client)
            : base(Client)
        {
            Description = "Stop flying. If the bot is in midair it will fall. Will succeed if we weren't flying";
            Details = "stop-flying";
            Parameters = CreateParams();
            ResultMap = CreateParams(
                 "message", typeof(string), "if we could not stop flying, the reason why (shouldnt happen)",
                 "success", typeof(bool), "true if we stopped flying");
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
