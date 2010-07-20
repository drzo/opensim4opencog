using System;
using MushDLR223.ScriptEngines;
using OpenMetaverse;

namespace cogbot.Actions.Movement
{
    public class GoHomeCommand : Command, BotPersonalCommand
    {
        public GoHomeCommand(BotClient testClient)
        {
            Name = "gohome";
            Description = "Teleports home";
            Category = CommandCategory.Movement;
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (Client.Self.GoHome())
            {
                return Success("Teleport Home Succesful");
            }
            else
            {
                return Failure("Teleport Home Failed");
            }
        }
    }
}
