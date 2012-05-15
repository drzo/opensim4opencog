using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Inventory.Shell
{
    public class PwdCommand : Command, BotPersonalCommand
    {
        private InventoryManager Manager;
        private OpenMetaverse.Inventory Inventory;

        public PwdCommand(BotClient client)
        {
            Name = "pwd";
            Description = "Displays the current working inventory folder.";
            Category = CommandCategory.Inventory;
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            Manager = Client.Inventory;
            Inventory = Client.Inventory.Store;

            if (args.Length > 0)
                return ShowUsage();// " pwd";
            string pathStr = "";
            string[] path = null;

            InventoryFolder currentFolder = Client.CurrentDirectory;

            if (currentFolder == null) // We need this to be set to something. 
                return Failure("Error: Client not logged in.");

            return Success("Current folder: " + currentFolder.Name);
        }
    }
}