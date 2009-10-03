using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions
{
    public class PrimWorkshopCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public PrimWorkshopCommand(BotClient client)
        {
            Name = "Prim Workshop";
            Description = "Runs PrimWorkshop on a prim. Usage: PrimWorkshop [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0) {
                return Usage;
            }
            int used;
            List<Primitive> prims = new List<Primitive>();
            SimObject o = WorldSystem.GetSimObject(args, out used);
            if (o == null) return string.Format("Cant find {0}", string.Join(" ", args));
            if (!o.HasPrim) return string.Format("No prim for {0}", string.Join(" ", args));
            prims.Add(o.Prim);
            foreach (var child in o.Children)
            {
                if (child.HasPrim) prims.Add(child.Prim);
            }
            TheBotClient.Invoke(() =>
                                    {
                                        frmPrimWorkshop pw = new frmPrimWorkshop(TheBotClient.TheRadegastInstance);
                                        pw.loadPrims(prims);
                                        pw.Show();
                                    });
            return Name + " on " + o;
        }
    }
}