using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class ThreadCommand : Command
    {
        public ThreadCommand(BotClient testClient)
        {
            Name = "thread";
            Description = "executes a command in its own thread. Type \"thread\" for usage.";
            Category = CommandCategory.Other;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length < 1)
            {
                return "Usage: thread anim 30 crouch";
            }

            Thread thread = new Thread(new ThreadStart(delegate()
            {
                String cmd = String.Join(" ", args);
                Client.ExecuteCommand(cmd);
                WriteLine("done with " + cmd);
            }));
            thread.Start();
            return String.Empty;
        }
    }
}
