using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Serialization;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Inventory.Shell
{
    /// <summary>
    /// Inventory Example, Moves a folder to the Trash folder
    /// </summary>
    public class DeleteFolderCommand : Command, BotPersonalCommand
    {
        public DeleteFolderCommand(BotClient testClient)
        {
            Name = "deleteFolder";
            Description = "Moves a folder to the Trash Folder";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
                found = Client.Inventory.LocalFind(Client.Inventory.Store.RootFolder.UUID, target.Split('/'), 0, true);
                if (found.Count.Equals(1))
                {
                    // move the folder to the trash folder
                    Client.Inventory.MoveFolder(found[0].UUID, Client.Inventory.FindFolderForType(AssetType.TrashFolder));
                    return Success(string.Format("Moved folder {0} to Trash", found[0].Name));
                }
                return Success(string.Empty);
            }
            catch (Exception ex)
            {
                return Failure("Folder Not Found: (" + ex.Message + ")");
            }
        }
    }
}