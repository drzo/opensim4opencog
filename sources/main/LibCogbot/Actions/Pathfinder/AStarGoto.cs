using System;
using Cogbot.Actions.Movement;
using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;
using Cogbot.World;

namespace Cogbot.Actions.Pathfinder
{
    public class AStarGoto : Cogbot.Actions.Command, BotPersonalCommand
    {
        public AStarGoto(BotClient client)
        {
            Name = GetType().Name;
        }

        public override void MakeInfo()
        {
            Description = "Use A* Pathfinding to get to object";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams("to", typeof (SimPosition), "the location you wish to " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            args.SetValue("sproc", MovementProceedure.AStar);
            return MoveToCommand.ExecuteRequestProc(args, this);
        }
    }
}