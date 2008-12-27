using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class CrouchCommand : Command
    {
        public CrouchCommand(cogbot.TextForm testClient)
        {
            Name = "crouch";
            Description = "Starts or stops crouching. Usage: crouch [start/stop]";
            Category = CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            bool start = true;

            if (args.Length == 1 && args[0].ToLower() == "stop")
                start = false;

            if (start)
            {
                client.Self.Crouch(true);
                return "Started crouching";
            }
            else
            {
                client.Self.Crouch(false);
                return "Stopped crouching";
            }
        }
    }
}
