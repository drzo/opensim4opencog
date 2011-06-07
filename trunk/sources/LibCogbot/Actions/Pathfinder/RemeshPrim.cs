using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Pathfinder
{
    public class RemeshPrim : cogbot.Actions.Command, SystemApplicationCommand
    {
        public RemeshPrim(BotClient client)
        {
            Name = GetType().Name;
            Description = "Reads the sim prims for improving routes then bakes the region (was called srprim). Usage: remeshprim [prims] ";
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
                o.UpdateOccupied();
            }
            SimRegion.BakeRegions();
            return Success("Ran " + Name);
        }
    }
}