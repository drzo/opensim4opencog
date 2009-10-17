using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions
{
    public class TakeCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public TakeCommand(BotClient client)
        {
            Name = "Take";
            Description = "Takes from a prim. Usage: Take [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            GridClient client = TheBotClient;
            foreach (var currentPrim in PS)
            {
                Success(Name + " on " + currentPrim);
                client.Inventory.RequestDeRezToInventory(currentPrim.LocalID, DeRezDestination.AgentInventoryTake,
                                                         client.Inventory.FindFolderForType(AssetType.Object), UUID.Zero);
            }
            return SuccessOrFailure();
        }
    }
}