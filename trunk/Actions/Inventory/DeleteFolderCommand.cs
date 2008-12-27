using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Serialization;
using OpenMetaverse;

namespace cogbot.Actions
{
    /// <summary>
    /// Inventory Example, Moves a folder to the Trash folder
    /// </summary>
    public class DeleteFolderCommand : Command
    {
        public DeleteFolderCommand(cogbot.TextForm testClient)
        {
            Name = "deleteFolder";
            Description = "Moves a folder to the Trash Folder";
            Category = CommandCategory.Inventory;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            // parse the command line
            string target = String.Empty;
            for (int ct = 0; ct < args.Length; ct++)
                target = target + args[ct] + " ";
            target = target.TrimEnd();

            // initialize results list
            List<InventoryBase> found = new List<InventoryBase>();
            try
            {
                // find the folder
                found = client.Inventory.LocalFind(client.Inventory.Store.RootFolder.UUID, target.Split('/'), 0, true);
                if (found.Count.Equals(1))
                {
                    // move the folder to the trash folder
                    client.Inventory.MoveFolder(found[0].UUID, client.Inventory.FindFolderForType(AssetType.TrashFolder));
                    return String.Format("Moved folder {0} to Trash", found[0].Name);
                }
            }
            catch (InvalidOutfitException ex)
            {
                return "Folder Not Found: (" + ex.Message + ")";
            }
            return string.Empty;
        }
    }
}