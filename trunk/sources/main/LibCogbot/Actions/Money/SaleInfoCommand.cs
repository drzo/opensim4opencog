using System.Reflection;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Money
{
    public class SaleInfoCommand : Cogbot.Actions.Command, RegionMasterCommand, AsynchronousCommand
    {
        public SaleInfoCommand(BotClient client)
        {
            Name = "SaleInfo";
            Description = "sets or prints SaleInfo on a prim. Usage: SaleInfo [prim] [amount] [saletype]";
            Category = CommandCategory.Objects;
            Parameters = CreateParams("targets", typeof(PrimSpec), "The targets of " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length==0) {
                return ShowUsage();
            }
            int used;
            SimObject o = WorldSystem.GetSimObjectS(args, out used);
            if (o == null) return Failure(string.Format("Cant find {0}", args.str));
            Primitive currentPrim = o.Prim;
            if (currentPrim == null) return Failure(string.Format("No Prim localId for {0}", args.str));
            Primitive.ObjectProperties props = o.Properties;
            if (used == args.Length)
            {
                WorldObjects.EnsureSelected(currentPrim, WorldSystem.GetSimulator(currentPrim));
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
            TheBotClient.Objects.SetSaleInfo(WorldSystem.GetSimulator(currentPrim),currentPrim.LocalID,saletype,amount);

            return Success(Name + " on " + o);
        }
    }
}