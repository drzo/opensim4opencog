using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Actions.Inventory.Shell
{
    class GiveItemCommand : Command
    {

        const string nl = "\n";
        public GiveItemCommand(BotClient client)
        {
            Name = "give";
            Description = "Gives items from the current working directory to an avatar.";
            Category = CommandCategory.Inventory;
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2)
            {
                return Failure(Usage);// " give <agent uuid> <item1> [item2] [item3] [...]";
            }
            UUID dest;
            if (!UUIDTryParse(args[0], out dest))
            {
                return Failure( "First argument expected agent UUID.");
            }
            InventoryManager Manager = Client.Inventory;
            if (Client.CurrentDirectory == null)
            {
                Client.CurrentDirectory = Manager.Store.RootFolder;
            }
            string ret = "";
            for (int i = 1; i < args.Length; ++i)
            {
                string inventoryName = args[i];
                // WARNING: Uses local copy of inventory contents, need to download them first.
                String found = GiveMatches(Manager, inventoryName,
                                           FolderContents(Manager, Client.CurrentDirectory.UUID), dest);
                if (!string.IsNullOrEmpty(found))
                    ret += "No inventory item named " + inventoryName + " found." + nl;
            }
            return Success(ret);
        }

        private string GiveMatches(InventoryManager manager, string inventoryName, IEnumerable<InventoryBase> contents, UUID dest)
        {
            string found = "";
            if (contents != null)
                foreach (InventoryBase b in contents)
                {
                    if (inventoryName == b.Name || inventoryName == b.UUID.ToString())
                    {
                        found += GiveAll(manager, b, dest);
                    }
                    else if (b is InventoryFolder)
                    {
                        found += GiveMatches(manager, inventoryName, FolderContents(manager, b.UUID), dest);
                    }
                }
            return found;
        }

        private List<InventoryBase> FolderContents(InventoryManager manager, UUID fid)
        {
            return manager.FolderContents(fid, Client.Self.AgentID, true, true, InventorySortOrder.ByName, 10000);
        }

        private string GiveAll(InventoryManager manager, InventoryBase b, UUID dest)
        {
            string ret = "";
            
            if (b is InventoryItem)
            {
                InventoryItem item = b as InventoryItem;
                ret += item.Name + nl;
                manager.GiveItem(item.UUID, item.Name, item.AssetType, dest, true);
            }
            else if (b is InventoryFolder)
            {
                InventoryFolder folder = b as InventoryFolder;
                List<InventoryBase> folderContents = FolderContents(manager,folder.UUID);
                if (folderContents != null)
                    foreach (InventoryBase list in folderContents)
                    {
                        ret += GiveAll(manager, list, dest);
                    }
            }
            return ret;
        }
    }
}
