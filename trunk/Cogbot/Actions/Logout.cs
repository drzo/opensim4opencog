using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Logout : Command
    {
        public Logout(BotClient Client)
            : base(Client)
        {
            Name = "Logout";
            helpString = "Logout from Secondlife";
            usageString = "To Logout from Second Life, type \"logout\"";
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
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
