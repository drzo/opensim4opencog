using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions
{

        public class SetBotCommand : Command, SystemApplicationCommand
        {
            public SetBotCommand(BotClient testClient)
            {
                Name = "setbot";
                Description = "Sets one current bot for subsequent textform commands";
                Category = CommandCategory.TestClient;
            }

            public override string Execute(string[] args, UUID fromAgentID)
            {
                string botname = String.Join(" ",args).Trim();
                TheBotClient.ClientManager.SetOnlyOneCurrentBotClient(botname);
                // This is a dummy command. Calls to it should be intercepted and handled specially
                return "SetOnlyOneCurrentBotClient=" + TheBotClient.ClientManager.OnlyOneCurrentBotClient;
            }
        }
    
}
