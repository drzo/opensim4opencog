using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Pathfinder
{
    public class UnmeshPrim : cogbot.Actions.Command, SystemApplicationCommand
    {
        public UnmeshPrim(BotClient client)
        {
            Name = GetType().Name;
            Description = "Unmeshes all prims(was called srprim). Usage: remeshprim [prims] ";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            ICollection<SimObject> objs = WorldSystem.GetPrimitives(args, out argsUsed);
            bool rightNow = true;
            if (argsUsed == 0)
            {
                objs = (ICollection<SimObject>) WorldSystem.GetAllSimObjects();
                rightNow = false;
            }
            WriteLine("Unmeshing " + objs.Count);
            foreach (SimObject o in objs)
            {
                o.IsWorthMeshing = true;
                if (rightNow)
                {
                    o.RemoveCollisions();
                }
                else
                {
                    o.RemoveCollisions();
                }
            }
            if (rightNow)
            {
                TheSimAvatar.GetSimRegion().GetPathStore(TheSimAvatar.SimPosition).RemoveAllCollisionPlanes();
            }
            else
            {
                TheSimAvatar.GetSimRegion().GetPathStore(TheSimAvatar.SimPosition).RemoveAllCollisionPlanes();
            }

            return TheBotClient.ExecuteCommand("meshinfo", fromAgentID, WriteLine);
        }
    }
}