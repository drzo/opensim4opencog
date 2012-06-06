using System.Collections.Generic;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Money
{
    public class BuyCommand : Cogbot.Actions.Command, RegionMasterCommand
    {
        public BuyCommand(BotClient client)
        {
            Name = "Buy";
            Description = "Buys from a prim. Usage: Buy [prim]";
            Category = CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length==0) {
                return ShowUsage();
            }
            int used;
            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + args.str);
            foreach (var o in PS)
            {
                //SimObject o = WorldSystem.GetSimObject(currentPrim);
                Primitive.ObjectProperties Properties = o.Properties;
                if (Properties == null)
                {
                    Failure("Still waiting on properties for " + o);
                    continue;
                }
                GridClient client = TheBotClient;
                client.Objects.BuyObject(o.GetSimulator(), o.LocalID, Properties.SaleType,
                                         Properties.SalePrice, client.Self.ActiveGroup,
                                         client.Inventory.FindFolderForType(AssetType.Object));
                Success(Name + " on " + o);
            }
            return SuccessOrFailure();
        }
    }
}