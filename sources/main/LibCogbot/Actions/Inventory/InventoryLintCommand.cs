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

namespace cogbot.Actions.Inventory.Shell
{
    public class InventoryLintCommand : Command, BotPersonalCommand
    {
        private OpenMetaverse.Inventory Inventory;
        private InventoryManager Manager;

        public InventoryLintCommand(BotClient testClient)
        {
            Name = "ilint";
            Description = "Prints out inventory.";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            Manager = Client.Inventory;
            Inventory = Manager.Store;
            StringBuilder result = new StringBuilder();

            InventoryFolder rootFolder = Inventory.RootFolder;
            foreach (KeyValuePair<UUID, InventoryNode> node in MushDLR223.Utilities.LockInfo.CopyOf(Inventory.Items))
            {
                var n = node.Value;
                if (n.Parent==null)
                {
                    var d = n.Data;
                    if (d is InventoryItem) continue;
                    if (n.ParentID==UUID.Zero)
                    {
                        UUID uuid;
                        if (UUID.TryParse(d.Name,out uuid))
                        {
                            Client.Inventory.RemoveFolder(node.Key);
                            Client.Inventory.RemoveItem(node.Key);
                        }
                    }
                }

            }

            return Success(result.ToString());;
        }

        void PrintFolder(InventoryFolder f, StringBuilder result, int indent)
        {
            List<InventoryBase> contents = Manager.FolderContents(f.UUID, Client.Self.AgentID,
                true, true, InventorySortOrder.ByName, 10000);

            if (contents != null)
            {
                foreach (InventoryBase i in contents)
                {
                    result.AppendFormat("{0}{1} ({2})\n", new String(' ', indent * 2), i.Name, i.UUID);
                    if (i is InventoryFolder)
                    {
                        InventoryFolder folder = (InventoryFolder)i;
                        PrintFolder(folder, result, indent + 1);
                    }
                }
            }
        }
    }
}