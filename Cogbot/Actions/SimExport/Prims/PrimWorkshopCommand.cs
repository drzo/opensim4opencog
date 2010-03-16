using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions
{
    public class PrimWorkshopCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public PrimWorkshopCommand(BotClient client)
        {
            Name = "Prim Workshop";
            Description = "Runs PrimWorkshop on a prim. Usage: PrimWorkshop [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }

            int argsUsed;
            List<string> searchArgs = new List<string> {"family"};
            searchArgs.AddRange(args);
            List<SimObject> PSO = WorldSystem.GetPrimitives(searchArgs.ToArray(), out argsUsed);
            List<Primitive> PS = new List<Primitive>();
            WorldSystem.AsPrimitives(PS,PSO);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            TheBotClient.Invoke(() =>
                                    {
                                        frmPrimWorkshop pw = new frmPrimWorkshop(TheBotClient.TheRadegastInstance);
                                        pw.loadPrims(PS);
                                        pw.Show();
                                    });
            return Success(Name + " on " + PS.Count);
        }
    }
}