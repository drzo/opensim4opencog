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
            if (args.Length==0) {
                return Failure(Usage);
            }
            int used;
            List<Primitive> prims = new List<Primitive>();
            SimObject o = WorldSystem.GetSimObject(args, out used);
            if (o == null) return Failure(string.Format("Cant find {0}", string.Join(" ", args)));
            Primitive currentPrim = o.Prim;
            if (currentPrim == null) return Failure("Still waiting on Prim for " + o);
            GridClient client = TheBotClient;
            client.Inventory.RequestDeRezToInventory(currentPrim.LocalID, DeRezDestination.AgentInventoryTake,
                                                     client.Inventory.FindFolderForType(AssetType.Object), UUID.Zero);
            return Success(Name + " on " + o);
        }
    }
}