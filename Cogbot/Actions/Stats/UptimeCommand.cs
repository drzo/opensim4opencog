using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.System
{
    public class UptimeCommand : Command, BotSystemCommand
    {
        public DateTime Created = DateTime.Now;

        public UptimeCommand(BotClient testClient)
        {
            Name = "uptime";
            Description = "Shows the login name, login time and length of time logged on.";
            Category = CommandCategory.Simulator;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string name = Client.ToString();
            return Success("I am " + name + ", Up Since: " + Created + " (" + (DateTime.Now - Created) + ")");
        }
    }
}