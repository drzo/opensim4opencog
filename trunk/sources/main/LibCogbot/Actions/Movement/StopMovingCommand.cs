using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    class StopMovingCommand : Command, BotPersonalCommand
    {
        public StopMovingCommand(BotClient client)
        {
            Name = "stopmoving";
            Description = "stops all movement threads";
            Category = CommandCategory.Movement;
            Parameters = CreateParams();

        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argsUsed;
            TheSimAvatar.StopMoving();
            return Success(string.Format("Stopped moving"));
        }
    }
}
