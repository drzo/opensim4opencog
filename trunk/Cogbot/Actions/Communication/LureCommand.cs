using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions.Communication
{
    class LureCommand    :Command
    {
        public LureCommand(BotClient testClient)
        {
            Name = "lure";
            Description = "Send a lure to a user. Usage: lure FirstName LastName";
            Category = CommandCategory.Communication;
        }


        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string user = string.Join(" ", args);
            UUID id = WorldSystem.GetUserID(user);
            if (id==UUID.Zero) return "cannot find " + user;
            Client.Self.SendTeleportLure(id);
            return "teleport Lure sent to " + user;
        }
    }
}
