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

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0) {
                return ShowUsage();
            }
            int used;
            int argsUsed;
            List<Primitive> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            foreach (var currentPrim in PS)
            {
                SimObject o = WorldSystem.GetSimObject(currentPrim);
                Primitive.ObjectProperties Properties = o.Properties;
                if (Properties == null)
                {
                    Failure("Still waiting on properties for " + o);
                    continue;
                }
                GridClient client = TheBotClient;
                client.Objects.BuyObject(o.GetSimulator(), currentPrim.LocalID, Properties.SaleType,
                                         Properties.SalePrice, client.Self.ActiveGroup,
                                         client.Inventory.FindFolderForType(AssetType.Object));
                Success(Name + " on " + o);
            }
            return SuccessOrFailure();
        }
    }
}