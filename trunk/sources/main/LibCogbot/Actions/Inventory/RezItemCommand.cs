using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;
using PathSystem3D.Navigation;

namespace Cogbot.Actions.Inventory.Shell
{
    internal class RezItemCommand : Command, BotPersonalCommand
    {
        private const string nl = "\n";

        public RezItemCommand(BotClient client)
        {
            Name = "rezitem";
        }

        public override void MakeInfo()
        {
            Description = "Rezs items from the current working directory to an avatar.";
            Details = "rezitem [$self|Primspec|--prev] <item1> [item2] [item3] [...]";
            AddVersion(
                CreateParams(OneOf(
                                 Optional("--prev", typeof (bool), "use previous rezzed location"),
                                 Optional("location", typeof (SimPosition), "rez point")),
                             "items", typeof (string[]), "inventory matches"), Description);
            Category = CommandCategory.Inventory;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
            {
                return ShowUsage();
                    // "rezitem [avatar5,prev,sim/123/232/23@360] "fairyverse Goodies" [item2] [item3] [...]";
            }

            int argsUsed = 0;
            string lowerMatch = args[0].ToLower();
            SimPosition dest = null;
            if (!args.ContainsFlag("--prev"))
            {
                dest = WorldSystem.GetVector(args.GetProperty("location"), out argsUsed);
            }
            var man = Client.BotInventory;
            var found = man.FindAll(args.GetProperty("items"), true,
                                    inventoryName => Failure("No inventory item named " + inventoryName + " found."));
            foreach (InventoryBase ib in man.ItemsOnly(found))
            {
                RezAll(man, ib, dest);
            }
            return Success("found.Count=" + found.Count);
        }

        public string RezAll(BotInventoryEval man, InventoryBase b, SimPosition dest)
        {
            string ret = "";
            var manager = man.Manager;
            if (b is InventoryItem)
            {
                InventoryItem item = b as InventoryItem;

                bool canCopy = (item.Permissions.OwnerMask & PermissionMask.Copy) == PermissionMask.Copy;

                ret += item.Name + nl;
                if (dest == null)
                {
                    Simulator sim = Client.Network.CurrentSim;
                    Client.Inventory.RequestRestoreRezFromInventory(sim, item, item.UUID);
                }
                else
                {
                    Simulator sim = SimRegion.GetRegion(dest.GlobalPosition).TheSimulator;
                    Client.Inventory.RequestRezFromInventory(sim, dest.SimRotation, dest.SimPosition, item, UUID.Zero);
                }
            }
            else if (b is InventoryFolder)
            {
                InventoryFolder folder = b as InventoryFolder;
                List<InventoryBase> folderContents = man.FolderContents(folder.UUID);
                if (folderContents != null)
                    foreach (InventoryBase list in folderContents)
                    {
                        ret += RezAll(man, list, dest);
                    }
            }
            return ret;
        }

        private string Fullpath(InventoryManager manager, InventoryItem item)
        {
            if (item == null) return "null";
            return Fullpath(manager, manager.Store.GetNodeFor(item.ParentUUID)) + "/" + item.Name;
        }

        private string Fullpath(InventoryManager manager, InventoryNode item)
        {
            if (item == null) return "";
            return Fullpath(manager, item.Parent) + "/" + item.Data.Name;
        }
    }
}