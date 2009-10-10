using System;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class CrouchCommand : Command
    {
        public CrouchCommand(BotClient testClient)
        {
            Name = "crouch";
            Description = "Starts or stops crouching. Usage: crouch [start/stop]";
            Category = CommandCategory.Movement;
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            bool start = true;

            if (args.Length == 1 && args[0].ToLower() == "stop")
                start = false;

            if (start)
            {
                Client.Self.Crouch(true);
                return Success("Started crouching");
            }
            else
            {
                Client.Self.Crouch(false);
                return Success("Stopped crouching");
            }
        }
    }
}
