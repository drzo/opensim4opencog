using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class UptimeCommand : Command, BotSystemCommand
    {
        public DateTime Created = DateTime.Now;

        public UptimeCommand(BotClient testClient)
        {
            Name = "uptime";
        }

        public override void MakeInfo()
        {
            Description = "Shows the login name, login time and length of time logged on.";
            Category = CommandCategory.Simulator;
            Parameters = CreateParams();
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string name = Client.ToString();
            return Success("I am " + name + ", Up Since: " + Created + " (" + (DateTime.Now - Created) + ")");
        }
    }
}