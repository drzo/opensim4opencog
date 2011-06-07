using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

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
            Category = CommandCategory.BotClient;
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            if (Client.Network.Connected)
            {
                ClientManager.Logout(Client);
                return Success("Logged out " + Client);
            }
            return Success("Was Logged out " + Client);
        }

    }
}
