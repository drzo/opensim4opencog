using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions.Pathfinder
{
    class meshinfo : cogbot.Actions.Command, SystemApplicationCommand
    {
        public meshinfo(BotClient client)
        {
            Name = GetType().Name;
            Description = "Reads the sim prims for improving routes";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            IEnumerable<SimObject> objs = WorldSystem.GetPrimitives(args, out argsUsed);
            if (argsUsed == 0) objs = WorldSystem.GetAllSimObjects();
            foreach (SimObject o in objs)
            {
                WriteLine("MeshInfo: " + o);
                WriteLine(o.Mesh.DebugString());
            }
            return Success("Ran " + Name);
        }
    }
}