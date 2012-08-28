using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Serialization;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Inventory.Shell
{
    public class InventoryLintCommand : Command, BotPersonalCommand
    {
        private OpenMetaverse.Inventory Inventory;
        private InventoryManager Manager;

        public InventoryLintCommand(BotClient testClient)
        {
            Name = "ilint";
        }

        public override void MakeInfo()
        {
            Description = "Prints out inventory.";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            Manager = Client.Inventory;
            Inventory = Manager.Store;

            InventoryFolder rootFolder = Inventory.RootFolder;
            int found = 0;
            int items = 0;
            int remed = 0;
            foreach (KeyValuePair<UUID, InventoryNode> node in MushDLR223.Utilities.LockInfo.CopyOf(Inventory.Items))
            {
                found++;
                var n = node.Value;
                if (n.Parent == null || n.Parent.Data.Name == "TaskInvHolder")
                {
                    var d = n.Data;
                    items++;
                    if (d is InventoryItem) continue;
                    if (n.ParentID == UUID.Zero || (n.Parent != null && n.Parent.Data.Name == "TaskInvHolder"))
                    {
                        UUID uuid;
                        if (UUID.TryParse(d.Name, out uuid) || d.Name.ToLower().Contains("taskinv"))
                        {
                            remed++;
                            Client.Inventory.MoveFolder(node.Key,
                                                        Client.Inventory.FindFolderForType(AssetType.TrashFolder));
                            /*
                            Client.Inventory.RemoveDescendants(node.Key);
                            Client.Inventory.RemoveFolder(node.Key);
                            Client.Inventory.RemoveItem(node.Key);
                             */
                        }
                    }
                }
            }

            return Success("remed=" + remed + " items=" + items + " total=" + found);
            ;
        }

        private void PrintFolder(InventoryFolder f, OutputDelegate result, int indent)
        {
            List<InventoryBase> contents = Manager.FolderContents(f.UUID, Client.Self.AgentID,
                                                                  true, true, InventorySortOrder.ByName, 10000);

            if (contents != null)
            {
                foreach (InventoryBase i in contents)
                {
                    result("{0}{1} ({2})\n", new String(' ', indent*2), i.Name, i.UUID);
                    if (i is InventoryFolder)
                    {
                        InventoryFolder folder = (InventoryFolder) i;
                        PrintFolder(folder, result, indent + 1);
                    }
                }
            }
        }
    }
}