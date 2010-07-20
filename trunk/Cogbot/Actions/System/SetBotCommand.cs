using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{

        public class SetBotCommand : Command, SystemApplicationCommand
        {
            public SetBotCommand(BotClient testClient)
            {
                Name = "setbot";
                Description = "Sets one current bot for subsequent textform commands";
                Category = CommandCategory.BotClient;
            }

            public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
            {
                string botname = String.Join(" ",args).Trim();
                TheBotClient.ClientManager.SetOnlyOneCurrentBotClient(botname);
                // This is a dummy command. Calls to it should be intercepted and handled specially
                return Success("SetOnlyOneCurrentBotClient=" + TheBotClient.ClientManager.OnlyOneCurrentBotClient);
            }
        }
    
}
