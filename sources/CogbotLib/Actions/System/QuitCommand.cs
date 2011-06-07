using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class QuitCommand : Command, SystemApplicationCommand
    {
        public QuitCommand(BotClient testClient)
		{
			Name = "quit";
			Description = "Log all avatars out and shut down";
            Category = CommandCategory.BotClient;
		}

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            // This is a dummy command. Calls to it should be intercepted and handled specially
            CmdResult r = Success("This command should not be executed directly");
            try
            {
                Client.Dispose();
            }
            catch (Exception e0)
            {

            }
            return r;

        }
    }
}
