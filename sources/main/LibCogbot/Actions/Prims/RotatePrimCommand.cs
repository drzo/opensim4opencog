using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class RotatePrimCommand : Cogbot.Actions.Command, RegionMasterCommand
    {
        public RotatePrimCommand(BotClient client)
        {
            Name = "Rotateprim";
            Description = "Rotate prim to the relative specified position. Usage: Rotateprim <prim> <position>";
            Category = CommandCategory.Objects;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {

            if (args.Length < 2)
                return ShowUsage();// " Rotateprim prim [x y [z]]";

            int used;
            List<SimObject> PS = WorldSystem.GetSingleArg(args, out used);
            if (IsEmpty(PS)) return Failure("Cannot find prim: " + args.str);
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