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
            Name = "GUIPay";
            Description = "Pays a prim. Usage: Pay [prim] [amount]";
            Category = CommandCategory.Money;
            Parameters = CreateParams("target", typeof(PrimSpec), "The target(s) of the " + Name,
                                      "ammount", typeof(string), "The ammount to pay");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            SimObject o;
            if (!args.TryGetValue("target", out o)) return Failure(string.Format("Cant find {0}", args.str));

            bool isObject = !(o is SimAvatar);
            UUID target = o.ID;
            GridClient client = TheBotClient;
            string strA;
            if (!args.TryGetValue("ammount", out strA))
            {
                return Failure("Cant determine amount from: " + strA);
            }
            else
            {
                int amount;
                strA = strA.Replace("$", "").Replace("L", "");
                if (!int.TryParse(strA, out amount))
                {
                    return Failure("Cant determine amount from: " + strA);
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