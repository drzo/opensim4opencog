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
        }

        public override void MakeInfo()
        {
            Description = "Buys from a prim. Usage: Buy [prim]";
            Category = CommandCategory.Objects;
            Parameters = CreateParams("targets", typeof (PrimSpec), "The targets of " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }

            List<SimObject> PS;
            if (!args.TryGetValue("targets", out PS) || IsEmpty(PS))
            {
                return Failure("Cannot find objects from " + args.GetString("targets"));
            }
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
                AddSuccess(Name + " on " + o);
            }
            return SuccessOrFailure();
        }
    }
}