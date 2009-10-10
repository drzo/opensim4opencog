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

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0) {
                return Failure(Usage);
            }
            int used;
            List<Primitive> prims = new List<Primitive>();
            SimObject o = WorldSystem.GetSimObject(args, out used);
            if (o == null) return Failure(string.Format("Cant find {0}", string.Join(" ", args)));
            Primitive currentPrim = o.Prim;
            if (currentPrim == null) return Failure("Still waiting on Prim for " + o);
            prims.Add(currentPrim);
            foreach (var child in o.Children)
            {
                currentPrim = child.Prim;
                if (currentPrim != null) prims.Add(currentPrim);
            }
            TheBotClient.Invoke(() =>
                                    {
                                        frmPrimWorkshop pw = new frmPrimWorkshop(TheBotClient.TheRadegastInstance);
                                        pw.loadPrims(prims);
                                        pw.Show();
                                    });
            return Success(Name + " on " + o);
        }
    }
}