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
        }

        public override void MakeInfo()
        {
            Description =
                "Reads the sim prims for improving routes then bakes the region (was called srprim). Usage: remeshprim [prims] ";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams("targets", typeof (PrimSpec), "The targets of " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int argsUsed;
            var keyargs = args.GetProperty("targets");
            ICollection<SimObject> objs = WorldSystem.GetPrimitives(keyargs, out argsUsed);
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
            }
            else
            {
                Cogbot.WorldPathSystem.MeshingQueue.Enqueue(SimRegion.BakeRegions);
            }

            return TheBotClient.ExecuteCommand("meshinfo", args.CallerAgent, args.Output, args.CmdFlags);
        }
    }
}