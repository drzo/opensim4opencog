using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;
using PathSystem3D.Navigation;

namespace cogbot.Actions
{
    public class PointAtCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public PointAtCommand(BotClient client)
        {
            Name = "PointAt";
            Description = "PointAts from a prim. Usage: PointAt [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        private SimPosition pointing;
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            RadegastInstance instance = TheBotClient.TheRadegastInstance;
            int used;
            if (args.Length == 0)
            {
                instance.State.UnSetPointing();
                TheSimAvatar.SelectedBeam = !TheSimAvatar.SelectedBeam;
                return Success("SelectedBeam = " + TheSimAvatar.SelectedBeam);
            }
            SimObject o = WorldSystem.GetSimObjectS(args, out used);
            if (o == null) return Failure(string.Format("Cant find {0}", string.Join(" ", args)));
            Primitive currentPrim = o.Prim;
            if (pointing != null)
            {
                pointing = null;
            }
            else
            {
                pointing = o;
                instance.State.SetPointing(currentPrim, 3);
            }
            return Success(Name + " on " + o);
        }
    }
}