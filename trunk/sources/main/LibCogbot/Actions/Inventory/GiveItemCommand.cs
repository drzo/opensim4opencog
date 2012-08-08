using System;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Inventory.Shell
{
    class GiveItemCommand : Command, BotPersonalCommand
    {

        const string nl = "\n";
        public GiveItemCommand(BotClient client)
        {
            Name = "give";
            Description = "Gives items from the current working directory to an avatar or object.";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string[] toks = (string[]) args.tokens.Clone();
            if (args.Length < 2)
            {
                return ShowUsage(); // "give <agent/primSpec> <item1> [item2] [item3] [...]";
            }

            string[] objects;
            if (!args.GetAfter("to", out objects) || objects.Length == 0)
            {
                return ShowUsage();
            }
            ListAsSet<SimObject> allTargets = GetPrimitiveFromList(objects);

            int prepLocatedAt = args.IndexOf("to");
            Array.Resize(ref toks, prepLocatedAt);

            int given = 0;
            foreach (var dest in allTargets)
            {
                given += GiveTo(dest, toks, dest.IsAvatar);
            }
            return SuccessOrFailure();
        }

        public int GiveTo(SimObject dest, string[] args, bool moveInsteadOfCopy)
        {
            InventoryManager Manager = Client.Inventory;
            if (Client.CurrentDirectory == null)
            {
                Client.CurrentDirectory = Manager.Store.RootFolder;
            }
            string ret = "";
            for (int i = 0; i < args.Length; ++i)
            {
                string inventoryName = args[i];
                // WARNING: Uses local copy of inventory contents, need to download them first.
                String found = GiveMatches(Manager, inventoryName,
                                           FolderContents(Manager, Client.CurrentDirectory.UUID), dest, moveInsteadOfCopy);
                if (!string.IsNullOrEmpty(found))
                    ret += "No inventory item named " + inventoryName + " found." + nl;
            }
            return args.Length;
        }

        private string GiveMatches(InventoryManager manager, string inventoryName, IEnumerable<InventoryBase> contents, SimObject dest, bool moveInsteadOfCopy)
        {
            string found = "";
            if (contents != null)
                foreach (InventoryBase b in contents)
                {
                    if (inventoryName == b.Name || inventoryName == b.UUID.ToString())
                    {
                        found += GiveAll(manager, b, dest, moveInsteadOfCopy);
                    }
                    else if (b is InventoryFolder)
                    {
                        found += GiveMatches(manager, inventoryName, FolderContents(manager, b.UUID), dest, moveInsteadOfCopy);
                    }
                }
            return found;
        }

        private List<InventoryBase> FolderContents(InventoryManager manager, UUID fid)
        {
            return manager.FolderContents(fid, Client.Self.AgentID, true, true, InventorySortOrder.ByName, 10000);
        }

        private string GiveAll(InventoryManager manager, InventoryBase b, SimObject dest, bool moveInsteadOfCopy)
        {
            string ret = "";

            if (b is InventoryItem)
            {
                InventoryItem item = b as InventoryItem;
                ret += item.Name + nl;
                if (dest.IsAvatar)
                {
                    manager.GiveItem(item.UUID, item.Name, item.AssetType, dest.ID, true);
                }
                else
                {
                    throw new NotImplementedException("giving items to objects");
                }
            }
            else if (b is InventoryFolder)
            {
                InventoryFolder item = b as InventoryFolder;
                if (dest.IsAvatar)
                {
                    manager.GiveFolder(item.UUID, item.Name, AssetType.Folder, dest.ID, true);
                    return item.Name + nl;
                }
                InventoryFolder folder = b as InventoryFolder;
                List<InventoryBase> folderContents = FolderContents(manager, folder.UUID);
                if (folderContents != null)
                    foreach (InventoryBase list in folderContents)
                    {
                        ret += GiveAll(manager, list, dest, moveInsteadOfCopy);
                    }
            }
            return ret;
        }
    }
}
