using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class ObjectInventoryCommand : Command, RegionMasterCommand
    {
        public ObjectInventoryCommand(BotClient testClient)
        {
            Name = "objectinventory";
            Description = "Retrieves a listing of items inside an object (task inventory). Usage: objectinventory [objectID]";
            Category = CommandCategory.Inventory;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " objectinventory [objectID]";

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            foreach (var found in PS)
            {


                uint objectLocalID = found.LocalID;
                UUID objectID = found.ID;

                List<InventoryBase> items = Client.Inventory.GetTaskInventory(objectID, objectLocalID, 1000 * 30);

                if (items != null)
                {
                    string result = String.Empty;

                    for (int i = 0; i < items.Count; i++)
                    {
                        if (items[i] is InventoryFolder)
                        {
                            result += String.Format("[Folder] Name: {0}", items[i].Name) + Environment.NewLine;
                        }
                        else
                        {
                            InventoryItem item = (InventoryItem)items[i];
                            result += String.Format("[Item] Name: {0} Desc: {1} Type: {2}", item.Name, item.Description,
                                                    item.AssetType) + Environment.NewLine;
                        }
                    }

                    Success(result);
                }
                else
                {
                    Failure("failed to download task inventory for " + objectLocalID);
                }
            }
            return SuccessOrFailure();
        }
    }
}
