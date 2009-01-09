using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class UptimeCommand : Command
    {
        public DateTime Created = DateTime.Now;

        public UptimeCommand(cogbot.TextForm testClient)
        {
            Name = "uptime";
            Description = "Shows the login name, login time and length of time logged on.";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            string name = Client.ToString();
            return "I am " + name + ", Up Since: " + Created + " (" + (DateTime.Now - Created) + ")";
        }
    }
}