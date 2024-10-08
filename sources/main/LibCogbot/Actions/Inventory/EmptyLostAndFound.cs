using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Inventory
{
    public class EmptyLostAndCommand : Command, BotPersonalCommand
    {
        /// <summary>
        /// BotClient command to download and display a notecard asset
        /// </summary>
        /// <param name="testClient"></param>
        public EmptyLostAndCommand(BotClient testClient)
        {
            Name = "emptylostandfound";
        }

        public override void MakeInfo()
        {
            Description = "Empty inventory Lost And Found folder";
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
            Client.Inventory.EmptyLostAndFound();
            return Success("Lost And Found Emptied");
        }
    }
}