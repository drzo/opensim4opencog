using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions
{
    public class WearPrimCommand : cogbot.Actions.Command, RegionMasterCommand
    {

        public WearPrimCommand(BotClient client)
        {
            Name = "WearPrim";
            Description = "Takes and wears a prim. Usage: wearprim [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }


        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                return ShowUsage();
            }
            List<UUID> Received = new List<UUID>();
            ManualResetEvent ItemsRecieved = new ManualResetEvent(false);

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            GridClient client = TheBotClient;

            EventHandler<ItemReceivedEventArgs> onItemReceived = (sender, e) => {
                                                                       var item = e.Item;
                                                                       lock (Received) if (Received.Contains(item.AssetUUID))
                                                                       {
                                                                           ItemsRecieved.Set();
                                                                           Received.Remove(item.AssetUUID);
                                                                           List<InventoryItem> newList = new List<InventoryItem> { item };
                                                                           Client.Appearance.AddToOutfit(newList);
                                                                       }
            };

           ;
            Client.Inventory.ItemReceived += onItemReceived;

            foreach (var currentPrim in PS)
            {
                Success(Name + " on " + currentPrim);
                lock (Received) Received.Add(currentPrim.ID);
                client.Inventory.RequestDeRezToInventory(currentPrim.LocalID, DeRezDestination.AgentInventoryTake,
                                                         client.Inventory.FindFolderForType(AssetType.Object), UUID.Zero);
            }
            try
            {
                //30 secs
                if (!ItemsRecieved.WaitOne(30000))
                {
                    return Failure("Timeout on reciveing items");
                }
                return SuccessOrFailure();

            }
            finally
            {
                Client.Inventory.ItemReceived -= onItemReceived;
            }

        }
    }
}