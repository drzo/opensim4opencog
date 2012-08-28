using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class AnimInfoCommand : Command, GridMasterCommand, FFINOUSE
    {
        public AnimInfoCommand(BotClient testClient)
        {
            Name = "assetinfo";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "Show debug info about assets.";
            AddExample(Name + " stand1", "shows that it loops and durration");
            Category = CommandCategory.Appearance;
            AddVersion(
                CreateParams(Optional("assets", typeof(List<SimAsset>),
                                      "the asset you want info about such as duration")),
                "shows the info about animation");
            ResultMap = CreateParams(
                "message", typeof(string), "debug infos about the asset",
                "success", typeof(bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            int count = 0;
            List<SimAsset> As;
            if (args.TryGetValue("assets", out As))
            {
                foreach (var A in As)
                {
                    WriteLine(A.DebugInfo());
                    count++;
                    continue;
                }
            }
            return Success("Shown " + count + " assets");
        }
    }
}