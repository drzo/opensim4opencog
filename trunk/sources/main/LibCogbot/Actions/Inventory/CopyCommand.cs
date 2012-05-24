using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Inventory
{
    public class CopyCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public CopyCommand(BotClient client)
        {
            Name = "Copy";
            Description = "Copys from a prim. Usage: Copy [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + args.str);
            GridClient client = TheBotClient;
            foreach (var currentPrim in PS)
            {
                Success(Name + " on " + currentPrim);
                client.Inventory.RequestDeRezToInventory(currentPrim.LocalID, DeRezDestination.AgentInventoryCopy,
                                                         client.Inventory.FindFolderForType(AssetType.Object), UUID.Zero);
            }
            return SuccessOrFailure();
        }
    }
}