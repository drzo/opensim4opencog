using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Logout : Action
    {
        public Logout(BotClient Client)
            : base(Client)
        {
            helpString = "Logout from Secondlife";
            usageString = "To Logout from Second Life, type \"logout\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            if (Client.Network.Connected)
            {
                Client.Network.Logout();
            }
        }

    }
}
