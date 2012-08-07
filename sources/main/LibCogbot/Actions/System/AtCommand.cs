using System;
using System.Collections.Generic;
using System.Reflection;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class AtCommand : Command, SystemApplicationCommand
    {
        public AtCommand(BotClient testClient)
        {
            Name = "@";
            Description = "Restrict the following commands to one or all avatars. Usage: @ [firstname lastname]";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            // This is a dummy command. Calls to it should be intercepted and handled specially
            return Success("This command should not be executed directly");            
        }
    }
}
