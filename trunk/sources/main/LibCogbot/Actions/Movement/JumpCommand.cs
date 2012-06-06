using System;
using System.Threading;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    public class JumpCommand : Command, BotPersonalCommand
    {
        public JumpCommand(BotClient testClient)
		{
           Description = "Jump for 1/2 second.";
            Name = "Jump";
            AddVersion(CreateParams(), Description);
            ResultMap = CreateParams(
                 "message", typeof(string), "if we could not why, why (shouldnt happen)",
                 "success", typeof(bool), "true if we jumped up");

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
