using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class MovePrimCommand : Cogbot.Actions.Command, RegionMasterCommand
    {
        public MovePrimCommand(BotClient client)
        {
            Name = "moveprim";
            Description = "move prim to the relative specified position. Usage: moveprim <prim> <position>";
            Category = Cogbot.Actions.CommandCategory.Objects;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {

            if (args.Length < 2)
                return ShowUsage();// " moveprim prim [x y [z]]";

            int used;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out used);
            if (IsEmpty(PS)) return Failure("Cannot find prim: " + args.str);
            string[] to = Parser.SplitOff(args, used);
            SimPosition aPos = WorldSystem.GetVector(to, out used);
            if (aPos == null) return Failure("Cannot find position: " + string.Join(" ", to));
            if (!aPos.IsRegionAttached) return Failure("!IsRegionAttached: " + aPos);
            List<SimObject> TODO = new List<SimObject>();
            foreach (var O in PS)
            {
                if (!O.IsRegionAttached) return Failure("!IsRegionAttached: " + O);
                TODO.Add(O);
            }
            foreach (var O in TODO)
            {
                SimPosition localPos = WorldSystem.GetVector(to, out used, O);
                Vector3d local = localPos.GlobalPosition;
                O.SetObjectPosition(local);
            }
            return Success("acted on " + PS.Count);
        }
    }
}