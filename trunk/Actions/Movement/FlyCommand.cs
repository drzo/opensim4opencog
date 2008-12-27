using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class FlyCommand : Command
    {
        public FlyCommand(cogbot.TextForm testClient)
        {
            Name = "fly";
            Description = "Starts or stops flying. Usage: fly [start/stop]";
            Category = CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            bool start = true;

            if (args.Length == 1 && args[0].ToLower() == "stop")
                start = false;

            if (start)
            {
                client.Self.Fly(true);
                return "Started flying";
            }
            else
            {
                client.Self.Fly(false);
                return "Stopped flying";
            }
        }
    }
}
