using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Objects
{
    public class SelectObjectCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public SelectObjectCommand(BotClient client)
        {
            Name = "selectobject";
            Description = "Re selectobject [re|de] [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
            //client.RegisterCommand("deselect", this);
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                WorldObjects.ResetSelectedObjects();
                return Success("ResetSelectedObjects");
            }
            string note = "";
            var autoDeselect = (args[0] == "re");
            if (autoDeselect)
            {
                note += "re";
                args = Parser.SplitOff(args, 1);
            }
            var deSelect = (args[0] == "de");
            if (deSelect)
            {
                note += "de";
                args = Parser.SplitOff(args, 1);
            }
            int used;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out used);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            List<uint> selectobjs = new List<uint>();
            var fp = PS[0].RegionHandle;           
            foreach (var P in PS)
            {
                if (fp != P.RegionHandle)
                {
                    SelectObjects(deSelect, fp, selectobjs, autoDeselect);
                    fp = P.RegionHandle;
                    selectobjs = new List<uint>();
                }
                selectobjs.Add(P.LocalID);
                uint pid = P.ParentID;
                if (pid != 0)
                {
                    selectobjs.Add(pid);
                }
            }
            SelectObjects(deSelect, fp, selectobjs, autoDeselect);
            return Success("objects " + note + "selected " + PS.Count);
        }

        private void SelectObjects(bool deSelect, ulong fp, List<uint> selectobjs, bool autoDeselect)
        {
            if (selectobjs.Count == 0) return;
            Simulator sim = WorldSystem.GetSimulator(fp);
            while (selectobjs.Count > 25)
            {
                var so = selectobjs.GetRange(0, 40);
                selectobjs.RemoveRange(0, 40);
                if (deSelect)
                {
                    Client.Objects.DeselectObjects(sim, so.ToArray());
                }
                else
                {
                    Client.Objects.SelectObjects(sim, so.ToArray(), autoDeselect);
                }
            }
            if (deSelect)
            {
                Client.Objects.DeselectObjects(sim, selectobjs.ToArray());
            }
            else
            {
                Client.Objects.SelectObjects(sim, selectobjs.ToArray(), autoDeselect);
            }
        }
    }
}