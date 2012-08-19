using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    class TurnToCommand : Cogbot.Actions.Command, BotPersonalCommand
    {
        public TurnToCommand(BotClient client)
        {
            Name = "turnto";
            Description = "turn the avatar toward the specified position for a maximum of seconds. turnto [prim | [x y [z]]";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams("position", typeof (SimPosition), "the location you wish to " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argsUsed;
            SimPosition simObject = WorldSystem.GetVector(args, out argsUsed);
            WriteLine("turnto {0}", simObject);
            WorldSystem.TheSimAvatar.TurnToward(simObject);
            return Success(WorldSystem.TheSimAvatar.DistanceVectorString(simObject));
        }
    }
}