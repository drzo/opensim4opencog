using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class ObjectInventoryCommand : Command
    {
        public ObjectInventoryCommand(BotClient testClient)
        {
            Name = "objectinventory";
            Description = "Retrieves a listing of items inside an object (task inventory). Usage: objectinventory [objectID]";
            Category = CommandCategory.Inventory;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return "Usage: objectinventory [objectID]";

            int argsUsed;
            Primitive found = WorldSystem.GetPrimitive(args, out argsUsed);
            if (found == null) return "Couldn't find object " + String.Join(" ", args);

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

                return result;
            }
            else
            {
                return "Failed to download task inventory for " + objectLocalID;
            }
        }
    }
}
