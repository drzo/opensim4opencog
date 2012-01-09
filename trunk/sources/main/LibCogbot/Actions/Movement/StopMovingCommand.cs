using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class StopMovingCommand : Command, BotPersonalCommand
    {
        public StopMovingCommand(BotClient client)
        {
            Name = "stopmoving";
            Description = "stops all movement threads";
            Category = CommandCategory.Movement;
            Parameters = new[] { new NamedParam(typeof(GridClient), typeof(GridClient)) };

        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            TheSimAvatar.StopMoving();
            return Success(string.Format("Stopped moving"));
        }
    }
}
