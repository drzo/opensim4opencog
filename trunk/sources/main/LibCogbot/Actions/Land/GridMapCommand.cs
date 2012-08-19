using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Land
{
    public class GridMapCommand : Command, GridMasterCommand
    {
        public GridMapCommand(BotClient testClient)
        {
            Name = "gridmap";
            Description = "Downloads all visible information about the grid map";
            Category = CommandCategory.Simulator;
            Parameters = CreateParams();
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            //if (args.Length < 1)
            //    return Success("";

            Client.Grid.RequestMainlandSims(GridLayerType.Objects);
            
            return Success("Sent " + Name);
        }
    }
}
