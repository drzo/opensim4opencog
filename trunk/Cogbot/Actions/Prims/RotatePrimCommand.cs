using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Objects
{
    public class RotatePrimCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public RotatePrimCommand(BotClient client)
        {
            Name = "Rotateprim";
            Description = "Rotate prim to the relative specified position. Usage: Rotateprim <prim> <position>";
            Category = CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 2)
                return ShowUsage();// " Rotateprim prim [x y [z]]";

            int used;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out used);
            if (IsEmpty(PS)) return Failure("Cannot find prim: " + string.Join(" ", args));
            string[] to = Parser.SplitOff(args, used);

            Quaternion aPos;
            if (!Quaternion.TryParse(string.Join(" ",to),out aPos))
            {
                return Failure("Cannot find position: " + string.Join(" ", to));
            }
            List<SimObject> TODO = new List<SimObject>();
            foreach (var O in PS)
            {
                if (!O.IsRegionAttached) return Failure("!IsRegionAttached: " + O);
                TODO.Add(O);
            }

            List<SimObject> RTODO = new List<SimObject>();
            foreach (var O in TODO)
            {
                if (!O.IsRoot && TODO.Contains(O.Parent))
                {
                    WriteLine("Already rotating parent of " + O);
                    continue;
                }
                RTODO.Add(O);
            }

            foreach (var O in RTODO)
            {
                O.SetObjectRotation(aPos);
            }
            return Success("acted on " + PS.Count);
        }
    }
}