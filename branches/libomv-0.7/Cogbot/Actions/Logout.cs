using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Logout : Command, BotSystemCommand
    {
        public Logout(BotClient Client)
            : base(Client)
        {
            Name = "Logout";
            Description = "Logout from Secondlife";
            Usage = "To Logout from Second Life, type \"logout\"";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            if (Client.Network.Connected)
            {
                Client.Network.Logout();
                return Success("Logged out " + Client);
            }
            return Success("Was Logged out " + Client);
        }

    }
}
