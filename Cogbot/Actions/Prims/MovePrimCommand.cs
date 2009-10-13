using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions
{
    public class MovePrimCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public MovePrimCommand(BotClient client)
        {
            Name = "moveprim";
            Description = "move prim to the relative specified position. Usage: moveprim <prim> <position>";
            Category = cogbot.Actions.CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 2)
                return ShowUsage();// " moveprim prim [x y [z]]";

            int used;
            List<Primitive> PS = WorldSystem.GetPrimitives(args, out used);
            if (IsEmpty(PS)) return Failure("Cannot find prim: " + string.Join(" ", args));
            string[] to = Parser.SplitOff(args, used);
            SimPosition aPos = WorldSystem.GetVector(to, out used, TheSimAvatar);
            if (aPos == null) return Failure("Cannot find position: " + string.Join(" ", to));
            if (!aPos.IsRegionAttached) return Failure("!IsRegionAttached: " + aPos);
            List<SimObject> TODO = new List<SimObject>();
            foreach (var P in PS)
            {
                SimObject O = WorldSystem.GetSimObject(P);
                if (!O.IsRegionAttached) return Failure("!IsRegionAttached: " + O);
                TODO.Add(O);
            }
            foreach (var O in TODO)
            {
                SimPosition localPos = WorldSystem.GetVector(to, out used, O);
                Vector3d local = localPos.GlobalPosition;
                O.MoveTo(local, 1f, 10);
            }
            return Success("acted on " + PS.Count);
        }
    }
}