using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class LogoutCommand : Command
    {
        public LogoutCommand(cogbot.TextForm testClient)
        {
            Name = "logout";
            Description = "Log this avatar out";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            string name = client.ToString();
            parent.LogOut(client);
            return "Logged " + name + " out";
        }
    }
}
