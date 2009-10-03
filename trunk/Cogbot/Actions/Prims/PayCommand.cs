using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions
{
    public class PayCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public PayCommand(BotClient client)
        {
            Name = "Pay";
            Description = "Pays a prim. Usage: Pay [prim] [amount]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0) {
                return Usage;
            }
            int used;
            SimObject o = WorldSystem.GetSimObject(args, out used);
            if (o == null) return string.Format("Cant find {0}", string.Join(" ", args));

            bool isObject = !(o is SimAvatar);
            UUID target = o.ID;
            GridClient client = TheBotClient;
            if (used == args.Length) (new frmPay(TheBotClient.TheRadegastInstance, o.ID, o.GetName(), isObject)).ShowDialog();
            else
            {
                int amount;
                string strA = args[used].Replace("$","").Replace("L","");               
                if (!int.TryParse(strA, out amount))
                {
                    return "Cant determine amount from: " + args[used];
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
            return Name + " on " + o;
        }
    }
}