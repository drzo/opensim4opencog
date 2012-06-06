using System.Collections.Generic;
using Cogbot;
using Cogbot.World;
using OpenMetaverse;
using Radegast;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Money
{
    public class PayCommand : Cogbot.Actions.Command, RegionMasterCommand
    {
        public PayCommand(BotClient client)
        {
            Name = "Pay";
            Description = "Pays a prim. Usage: Pay [prim] [amount]";
            Category = CommandCategory.Money;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length==0) {
                return ShowUsage();
            }
            int used;
            SimObject o = WorldSystem.GetSimObjectS(args, out used);
            if (o == null) return Failure(string.Format("Cant find {0}", string.Join(" ", args)));

            bool isObject = !(o is SimAvatar);
            UUID target = o.ID;
            GridClient client = TheBotClient;
            if (used == args.Length) (new frmPay(TheBotClient.TheRadegastInstance, o.ID, o.GetName(), isObject)).ShowDialog();
            //else
            {
                int amount;
                string strA = args[used].Replace("$","").Replace("L","");               
                if (!int.TryParse(strA, out amount))
                {
                    return Failure("Cant determine amount from: " + args[used]);
                }
                if (!isObject)
                {
                    client.Self.GiveAvatarMoney(target, amount);
                }
                else
                {
                    client.Self.GiveObjectMoney(target, amount, o.Properties.Name);
                }
            }
            return Success(Name + " on " + o);
        }
    }
}