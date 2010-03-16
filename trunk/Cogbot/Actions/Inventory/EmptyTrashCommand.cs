using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.Inventory
{
    public class EmptyTrashCommand : Command, BotPersonalCommand
    {
        /// <summary>
        /// BotClient command to download and display a notecard asset
        /// </summary>
        /// <param name="testClient"></param>
        public EmptyTrashCommand(BotClient testClient)
        {
            Name = "emptytrash";
            Description = "Empty inventory Trash folder";
            Category = CommandCategory.Inventory;
        }

        /// <summary>
        /// Exectute the command
        /// </summary>
        /// <param name="args"></param>
        /// <param name="fromAgentID"></param>
        /// <returns></returns>
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            Client.Inventory.EmptyTrash();
            return Success("Trash Emptied");
        }
    }
}
