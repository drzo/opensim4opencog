using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Movement
{
    class TurnToCommand : cogbot.Actions.Command
    {
        public TurnToCommand(BotClient client)
        {
            Name = "turnto";
            Description = "turn the avatar toward the specified position for a maximum of seconds. turnto [prim | [x y [z]]";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            SimPosition simObject = WorldSystem.GetVector(args, out argsUsed);
            WriteLine("turnto {0}", simObject);
            WorldSystem.TheSimAvatar.TurnToward(simObject);
            return WorldSystem.TheSimAvatar.DistanceVectorString(simObject);
        }
    }
}