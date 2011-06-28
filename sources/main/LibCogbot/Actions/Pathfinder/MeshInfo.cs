using System.Collections;
using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

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
            if (!WorldObjects.MaintainMeshes) return Success("WorldObjects.MaintainMeshes = false for " + Name);
            IEnumerable<SimObject> objs = WorldSystem.GetPrimitives(args, out argsUsed);
            if (argsUsed == 0)
            {

                objs = WorldSystem.GetAllSimObjects();
                int meshed = 0;
                int unmeshed = 0;
                foreach (var o in objs)
                {
                    if (o.IsMeshed)
                    {
                        meshed++;
                        continue;
                    }
                    unmeshed++;
                }
                int total = meshed + unmeshed;

                return
                    Success(string.Format("Is/UnMeshed = {0}/{1} {2:#.1}% {3}", meshed, unmeshed, 100*meshed/total, Name));
            }
            foreach (SimObject o in objs)
            {
                WriteLine("MeshInfo: " + o);              
                WriteLine(o.Mesh.DebugString());
            }
            return Success("Ran " + Name);
        }
    }
}