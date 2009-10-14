using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using cogbot.Utilities;
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

        ListAsSet<EffectBeamInfo> BeamInfos = new ListAsSet<EffectBeamInfo>();
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            RadegastInstance instance = TheBotClient.TheRadegastInstance;

            foreach (var set in BeamInfos)
            {
                set.UnSetPointing();
            }
            BeamInfos.Clear();
            int used;
            if (args.Length == 0)
            {
                instance.State.UnSetPointing();
                TheSimAvatar.SelectedBeam = !TheSimAvatar.SelectedBeam;
                return Success("SelectedBeam = " + TheSimAvatar.SelectedBeam);
            }
            List<Primitive> PS = WorldSystem.GetPrimitives(args, out used);
            if (PS.Count==0) return Failure(string.Format("Cant find {0}", string.Join(" ", args)));
            GridClient grc = TheBotClient;
            foreach (var currentPrim in PS)
            {
                SimObject o = WorldSystem.GetSimObject(currentPrim);
                EffectBeamInfo info = new EffectBeamInfo(grc);
                info.SetPointing(o, 3);
                BeamInfos.AddTo(info);
                instance.State.SetPointing(currentPrim, 3);                
            }
            return Success(Name + " on " + PS.Count);
        }
    }
}