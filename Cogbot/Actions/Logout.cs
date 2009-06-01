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

        public override string acceptInput(string verb, Parser args)
        {
            if (Client.Network.Connected)
            {
                Client.Network.Logout();
                return "Logged out " + Client;
            }
            return "Was Logged out " + Client;
        }

    }
}
