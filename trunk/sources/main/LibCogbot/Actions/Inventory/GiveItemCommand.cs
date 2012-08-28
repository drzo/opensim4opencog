using System;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using MushDLR223.Utilities;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Inventory.Shell
{
    internal class GiveItemCommand : Command, BotPersonalCommand
    {
        private const string nl = "\n";

        public GiveItemCommand(BotClient client)
        {
            Name = "give";
        }

        public override void MakeInfo()
        {
            Description = "Gives items from the current working directory to an avatar or object.";
            Details = "give items <item1> [item2] [item3] [...] to <agent/primSpec1> [agent/primSpec2] [...] ";
            Parameters =
                CreateParams(
                    Optional("--move", typeof (bool), "To move instead of copy"),
                    "items", typeof (List<string>), "the inventoiry items",
                    "to", typeof (string[]), "the receivers list");
            Category = CommandCategory.Inventory;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            bool moveInsteadOfCopy = args.IsTrue("--move");
            if (!args.ContainsKey("items") || !args.ContainsKey("to"))
            {
                return ShowUsage();
            }
            int argsUsed;
            List<SimObject> allTargets;
            if (!args.TryGetValue("to", out allTargets))
            {
                return Failure("Cannot find avatar/objects 'to' give to");
            }

            Success("Going to give to " + allTargets.Count + " avatar/objects");

            var man = Client.BotInventory;
            var found = man.FindAll(args.GetProperty("items"), false,
                                    inventoryName => Failure("No inventory item named " + inventoryName + " found."));

            int given = 0;
            foreach (var dest in allTargets)
            {
                foreach (InventoryBase item in found)
                {
                    GiveAll(man, item, dest, moveInsteadOfCopy);
                }
            }
            return SuccessOrFailure();
        }

        public string GiveAll(BotInventoryEval man, InventoryBase b, SimObject dest, bool moveInsteadOfCopy)
        {
            string ret = "";
            var manager = man.Manager;
            if (b is InventoryItem)
            {
                InventoryItem item = b as InventoryItem;

                bool canCopy = (item.Permissions.OwnerMask & PermissionMask.Copy) == PermissionMask.Copy;

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
                List<InventoryBase> folderContents = man.FolderContents(folder.UUID);
                if (folderContents != null)
                    foreach (InventoryBase list in folderContents)
                    {
                        ret += GiveAll(man, list, dest, moveInsteadOfCopy);
                    }
            }
            return ret;
        }
    }
}