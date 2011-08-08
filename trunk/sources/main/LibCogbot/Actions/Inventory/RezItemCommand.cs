using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Inventory.Shell
{
    class RezItemCommand : Command, BotPersonalCommand
    {

        const string nl = "\n";
        public RezItemCommand(BotClient client)
        {
            Name = "rezitem";
            Description = "Rezs items from the current working directory to an avatar.";
            Usage = "rezitem [avatar5,prev,sim/123/232/23@360] <item1> [item2] [item3] [...]";
            Category = CommandCategory.Inventory;
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2)
            {
                return ShowUsage();// "rezitem [avatar5,prev,sim/123/232/23@360] <item1> [item2] [item3] [...]";
            }

            int argsUsed = 0;
            SimPosition dest = null;
            if (args[0]=="avatar5")
            {
                argsUsed++;
                dest = TheSimAvatar.ApproachPosition;
                
            } else if (args[0]=="prev")
            {
                argsUsed++;
            } else
            {
               dest = WorldSystem.GetVector(args, out argsUsed);
            }

            InventoryManager Manager = Client.Inventory;
            if (Client.CurrentDirectory == null)
            {
                Client.CurrentDirectory = Manager.Store.RootFolder;
            }
            string ret = "";
            for (int i = argsUsed; i < args.Length; ++i)
            {
                string inventoryName = args[i];
                // WARNING: Uses local copy of inventory contents, need to download them first.
                String found = RezMatches(Manager, inventoryName,
                                           FolderContents(Manager, Client.CurrentDirectory.UUID), dest);
                if (!string.IsNullOrEmpty(found))
                    ret += "No inventory item named " + inventoryName + " found." + nl;
            }
            return Success(ret);
        }

        private string RezMatches(InventoryManager manager, string inventoryName, IEnumerable<InventoryBase> contents, SimPosition dest)
        {
            string found = "";
            if (contents != null)
                foreach (InventoryBase b in contents)
                {
                    if (inventoryName == b.Name || inventoryName == b.UUID.ToString())
                    {
                        found += RezAll(manager, b, dest);
                    }
                    else if (b is InventoryFolder)
                    {
                        found += RezMatches(manager, inventoryName, FolderContents(manager, b.UUID), dest);
                    }
                }
            return found;
        }

        private List<InventoryBase> FolderContents(InventoryManager manager, UUID fid)
        {
            return manager.FolderContents(fid, Client.Self.AgentID, true, true, InventorySortOrder.ByName, 10000);
        }

        private string RezAll(InventoryManager manager, InventoryBase b, SimPosition dest)
        {
            string ret = "";
            
            if (b is InventoryItem)
            {
                InventoryItem item = b as InventoryItem;
                ret += item.Name + nl;
                manager.RequestRezFromInventory(Client.Network.CurrentSim, dest.SimRotation, dest.SimPosition, item);
            }
            else if (b is InventoryFolder)
            {
                InventoryFolder folder = b as InventoryFolder;
                List<InventoryBase> folderContents = FolderContents(manager,folder.UUID);
                if (folderContents != null)
                    foreach (InventoryBase list in folderContents)
                    {
                        ret += RezAll(manager, list, dest);
                    }
            }
            return ret;
        }
    }
}
