using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Inventory
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
        }

        public override void MakeInfo()
        {
            Description = "Empty inventory Trash folder";
            Category = CommandCategory.Inventory;
        }

        /// <summary>
        /// Exectute the command
        /// </summary>
        /// <param name="args"></param>
        /// <param name="fromAgentID"></param>
        /// <returns></returns>
        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            Client.Inventory.EmptyTrash();
            return Success("Trash Emptied");
        }
    }
}