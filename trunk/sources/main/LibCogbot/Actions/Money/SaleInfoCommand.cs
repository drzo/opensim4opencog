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
            Description = "sets or prints SaleInfo on a prim. Usage: SaleInfo <prim> [amount] [saletype]";
            Category = CommandCategory.Objects;
            Parameters = CreateParams("target", typeof (PrimSpec), "The target(s) of the " + Name,
                                      "ammount", typeof (string), "The ammount to pay",
                                      Optional("saletype", typeof (string), "The ammount to pay"));
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            SimObject o;
            if (!args.TryGetValue("target", out o))
            {
                return Failure(string.Format("Cant find {0}", args.str));
            }
            bool isObject = !(o is SimAvatar);
            UUID target = o.ID;
            Primitive currentPrim = o.Prim;
            if (currentPrim == null) return Failure(string.Format("No Prim localId for {0}", args.str));
            Primitive.ObjectProperties props = o.Properties;
            WorldObjects.EnsureSelected(currentPrim, WorldSystem.GetSimulator(currentPrim));
            if (props == null) return Failure("no props on " + o + " yet try again");
            GridClient client = TheBotClient;
            string strA;
            if (!args.TryGetValue("ammount", out strA))
            {
                return Success(string.Format("saletype {0} {1} {2}", o.ID, props.SalePrice, props.SaleType));
            }
            int amount;
            strA = strA.Replace("$", "").Replace("L", "");
            if (!int.TryParse(strA, out amount))
            {
                return Failure("Cant determine amount from: " + strA);
            }
            SaleType saletype;
            if (!args.TryGetValue("saletype", out saletype))
            {
                saletype = SaleType.Original;
                //return Failure("Cant determine SaleType: " + strA);
            }

            WriteLine("Setting Ammount={0} SaleType={1} for {2}", saletype, amount, o);            
            TheBotClient.Objects.SetSaleInfo(WorldSystem.GetSimulator(currentPrim), currentPrim.LocalID, saletype, amount);

            return Success(Name + " on " + o);
        }
    }
}