using System.Collections.Generic;
using Cogbot;
using Cogbot.World;
using Cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using Radegast;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Agent
{
    public class PointAtCommand : Cogbot.Actions.Command, BotPersonalCommand
    {
        public PointAtCommand(BotClient client)
        {
        }

        public override void MakeInfo()
        {
            Name = "PointAt";
            Description = "PointAts from a prim. Usage: PointAt [prim]";
            Category = Cogbot.Actions.CommandCategory.Objects;
            Parameters = CreateParams(
                Optional("--stop", typeof (bool), "stop previous pointing"),
                "targets", typeof (PrimSpec), "The targets of " + Name);
        }

        ListAsSet<EffectBeamInfo> BeamInfos = new ListAsSet<EffectBeamInfo>();
        public override CmdResult ExecuteRequest(CmdRequest args)
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
                if (instance != null) instance.State.UnSetPointing();
                TheSimAvatar.SelectedBeam = !TheSimAvatar.SelectedBeam;
                return Success("SelectedBeam = " + TheSimAvatar.SelectedBeam);
            }
            var targets = args.GetProperty("targets");
            List<SimObject> PS = WorldSystem.GetPrimitives(targets, out used);
            GridClient grc = TheBotClient;
            if (PS.Count==0)
            {
                SimPosition pos = WorldSystem.GetVector(targets, out used);
                if (pos!=null)
                {
                    EffectBeamInfo info = new EffectBeamInfo(grc);
                    info.SetPointing(pos, 3);
                    BeamInfos.AddTo(info);
                    return Success(Name + " on " + pos);
                }
                return Failure(string.Format("Cant find {0}", string.Join(" ", args)));
            }
            foreach (var o in PS)
            {             
                EffectBeamInfo info = new EffectBeamInfo(grc);
                info.SetPointing(o, 3);
                BeamInfos.AddTo(info);
                Primitive p = o.Prim;
                if (p != null && instance != null) instance.State.SetPointing(p, 3);                
            }
            return Success(Name + " on " + PS.Count);
        }
    }
}