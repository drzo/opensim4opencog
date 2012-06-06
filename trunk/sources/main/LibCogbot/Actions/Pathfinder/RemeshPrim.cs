using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Pathfinder
{
    public class RemeshPrim : Cogbot.Actions.Command, BotPersonalCommand
    {
        public RemeshPrim(BotClient client)
        {
            Name = GetType().Name;
            Description = "Reads the sim prims for improving routes then bakes the region (was called srprim). Usage: remeshprim [prims] ";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argsUsed;
            ICollection<SimObject> objs = WorldSystem.GetPrimitives(args, out argsUsed);
            bool rightNow = true;
            if (argsUsed == 0)
            {
                objs = (ICollection<SimObject>) WorldSystem.GetAllSimObjects();
                rightNow = false;
            }
            WriteLine("Meshing " + objs.Count);
            foreach (SimObject o2 in objs)
            {
                SimObjectPathFinding o = o2.PathFinding;
                o.IsWorthMeshing = true;
                if (rightNow)
                {
                    o.AddCollisionsNow();
                }
                else
                {
                    o.AddCollisions();
                }
            }
            if (rightNow)
            {
                SimRegion.BakeRegions();
            } else
            {
                Cogbot.WorldPathSystem.MeshingQueue.Enqueue(SimRegion.BakeRegions);
            }

            return TheBotClient.ExecuteCommand("meshinfo", fromAgentID, WriteLine);
        }
    }
}