using System;
using OpenMetaverse;

namespace cogbot.Actions.Movement
{
    public class FlyCommand : Command, BotPersonalCommand
    {
        public FlyCommand(BotClient testClient)
        {
            Name = "fly";
            Description = "Starts or stops flying. Usage: fly [start/stop]";
            Category = CommandCategory.Movement;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            bool start = true;

            if (args.Length == 1 && args[0].ToLower() == "stop")
                start = false;

            if (start)
            {
                Client.Self.Fly(true);
                return Success("Started flying");
            }
            else
            {
                Client.Self.Fly(false);
                return Success("Stopped flying");
            }
        }
    }
}
