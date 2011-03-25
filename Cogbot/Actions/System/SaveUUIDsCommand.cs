using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using cogbot.TheOpenSims;
using OpenMetaverse;
using cogbot.Listeners;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Search
{
    public class SaveUUIDsCommand : Command, GridMasterCommand
    {
        public SaveUUIDsCommand(BotClient testClient)
        {
            Name = "Save UUIDs";
            Description = "Saves resolution of UUID types.  Usage: SaveUUIDs [AssetMapping3.xml]";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string filename = "AssetMapping3.xml";
            if (args.Length >0) filename = String.Join(" ", args, 0, args.Length).Trim();
            return Success("Done with UUIDs " + SimAssetStore.SaveAssetFile(filename, false));
        }
    }
}
