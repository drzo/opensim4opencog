using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using cogbot.TheOpenSims;
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
                return ShowUsage();// "rezitem [avatar5,prev,sim/123/232/23@360] "fairyverse Goodies" [item2] [item3] [...]";
            }

            int argsUsed = 0;
            string lowerMatch = args[0].ToLower();
            SimPosition dest = null;
            if (lowerMatch == "avatar")
            {
                argsUsed++;
                dest = SimRegion.GetWaypoint(TheSimAvatar.ApproachVector3D);

            }
            else if (lowerMatch == "prev")
            {
                argsUsed++;
            }
            else
            {
                dest = WorldSystem.GetVector(args, out argsUsed);
            }
            
            lowerMatch = args[argsUsed];

            InventoryManager Manager = Client.Inventory;

            string folderPrefix = "/";
            if (Client.CurrentDirectory == null)
            {
                Client.CurrentDirectory = Manager.Store.RootFolder;
            }

            var startAt = Client.CurrentDirectory;
            
            if (lowerMatch.StartsWith("//"))
            {
                startAt = Manager.Store.LibraryFolder;
                args[argsUsed] = lowerMatch.Substring(2);
            } else if (lowerMatch.StartsWith("/"))
            {
                startAt = Manager.Store.RootFolder;
                args[argsUsed] = lowerMatch.Substring(1);
            }

            folderPrefix = "";

            int expected = args.Length - argsUsed;

            List<InventoryItem> found = new List<InventoryItem>();
            for (int i = argsUsed; i < args.Length; ++i)
            {
                var inventoryName = new Regex("^" + args[i] + "$", RegexOptions.IgnoreCase);
                // WARNING: Uses local copy of inventory contents, need to download them first.
                RezMatches(Manager, folderPrefix, inventoryName, FolderContents(Manager, startAt.UUID), found);              
            }

            foreach (var v in found)
            {
                Success("found=" + Fullpath(Manager, v));
            }
            if (found.Count != expected)
            {
                return Failure("expected " + expected + " item matched not " + found.Count);
            }

            Simulator sim = Client.Network.CurrentSim;
            foreach (var v in found)
            {
                Success("found=" + v.Name);
                if (dest == null)
                {
                  //  Manager.RequestRestoreRezFromInventory(sim, v, UUID.Random());
                }
                else
                {
                  //  Manager.RequestRezFromInventory(sim, dest.SimRotation, dest.SimPosition, v);
                }
            }
            return Success("found.Count=" + found.Count);
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
        private void RezMatches(InventoryManager manager, String folderPrefix, Regex inventoryName, IEnumerable<InventoryBase> contents, List<InventoryItem> dest)
        {
            if (contents != null)
                foreach (InventoryBase b in contents)
                {
                    if (inventoryName.IsMatch(b.Name) ||
                        inventoryName.IsMatch(folderPrefix + b.Name) ||
                        inventoryName.IsMatch(b.UUID.ToString()))
                    {
                        RezAll(manager, b, dest);
                    }
                    else if (b is InventoryFolder)
                    {
                        RezMatches(manager, folderPrefix + b.Name + "/", inventoryName, FolderContents(manager, b.UUID),
                                   dest);
                    }
                    else if (b is InventoryItem)
                    {
                        InventoryItem ii = (InventoryItem)b;
                        if (inventoryName.IsMatch(ii.AssetUUID.ToString()))
                        {
                            RezAll(manager, b, dest);
                        }
                    }
                }
        }

        private List<InventoryBase> FolderContents(InventoryManager manager, UUID fid)
        {
            var f = manager.Store.GetContents(fid);
            if (f != null && f.Count > 0) return f;
            var contents = manager.FolderContents(fid, Client.Self.AgentID, true, true, InventorySortOrder.ByName, 5);
            if (contents != null) return contents;
            contents = manager.FolderContents(fid, UUID.Zero , true, true, InventorySortOrder.ByName, 5);
            if (contents != null) return contents;
            return null;
        }

        private void RezAll(InventoryManager manager, InventoryBase b, List<InventoryItem> dest)
        {
            List<InventoryItem> ret = new List<InventoryItem>();
            
            if (b is InventoryItem)
            {
                InventoryItem item = b as InventoryItem;
                dest.Add(item);
            }
            else if (b is InventoryFolder)
            {
                InventoryFolder folder = b as InventoryFolder;
                List<InventoryBase> folderContents = FolderContents(manager,folder.UUID);
                if (folderContents != null)
                    foreach (InventoryBase list in folderContents)
                    {
                        RezAll(manager, list, dest);
                    }
            }
        }
    }
}
