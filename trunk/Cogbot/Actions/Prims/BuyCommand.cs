using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions
{
    public class BuyCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public BuyCommand(BotClient client)
        {
            Name = "Buy";
            Description = "Buys from a prim. Usage: Buy [prim]";
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
            Primitive currentPrim = o.Prim;
            GridClient client = TheBotClient;
            client.Objects.BuyObject(o.GetSimulator(), currentPrim.LocalID, currentPrim.Properties.SaleType, currentPrim.Properties.SalePrice, client.Self.ActiveGroup, client.Inventory.FindFolderForType(AssetType.Object));
            return Name + " on " + o;
        }
    }
}