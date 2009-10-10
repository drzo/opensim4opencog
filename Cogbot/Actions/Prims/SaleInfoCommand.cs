using System.Reflection;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class SaleInfoCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public SaleInfoCommand(BotClient client)
        {
            Name = "SaleInfo";
            Description = "sets or prints SaleInfo on a prim. Usage: SaleInfo [prim] [amount] [saletype]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0) {
                return Failure(Usage);
            }
            int used;
            SimObject o = WorldSystem.GetSimObject(args, out used);
            if (o == null) return Failure(string.Format("Cant find {0}", string.Join(" ", args)));
            Primitive currentPrim = o.Prim;
            if (currentPrim == null) return Failure(string.Format("No Prim localId for {0}", string.Join(" ", args)));
            Primitive.ObjectProperties props = o.Properties;
            if (used == args.Length)
            {
                WorldObjects.EnsureSelected(currentPrim.LocalID, WorldSystem.GetSimulator(currentPrim));
                if (props == null) return Failure( "no props on " + o + " yet try again");
                return Success(string.Format("saletype {0} {1} {2}", o.ID, props.SalePrice, props.SaleType));
            }
            int amount;
            string strA = args[used].Replace("$", "").Replace("L", "");
            if (!int.TryParse(strA, out amount))
            {
                return Failure("Cant determine amount: " + strA);
            }
            used++;
            strA = args[used];
            FieldInfo fi = typeof (SaleType).GetField(strA);
            SaleType saletype = SaleType.Original;
            if (fi==null)
            {
                int st;
                if (!int.TryParse(strA, out st))
                {
                    return Failure("Cant determine SaleType: " + strA);
                }
                saletype = (SaleType) st;
            } else
            {
                saletype = (SaleType) fi.GetValue(null);
            }

            WriteLine("Setting Ammount={0} SaleType={1} for {2}", saletype, amount, o);
            TheBotClient.Objects.SetSaleInfo(currentPrim.LocalID,saletype,amount);

            return Success(Name + " on " + o);
        }
    }
}