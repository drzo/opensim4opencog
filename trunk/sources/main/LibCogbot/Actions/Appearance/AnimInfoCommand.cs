using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class AnimInfoCommand : Command, GridMasterCommand
    {
        public AnimInfoCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "animinfo";
            Description = "Show debug info about anims.";
            Details = AddUsage(Name + " <match>", "shows the info about animation") +
                    Example(Name + " stand1", "shows that it loops and durration");
            Category = CommandCategory.Appearance;
            Parameters = CreateParams(Optional("anim", typeof (SimAnimation), "the animation you want info about such as duration"));
            ResultMap = CreateParams(
                "message", typeof(string), "debug infos about the animations",
                "success", typeof(bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {

            ICollection<SimAsset> list = SimAssetStore.GetAssets(AssetType.Animation);
            int count = 0;
            string alist = String.Empty;
            lock (list)
            foreach (SimAsset A in list)
            {
                foreach (string s in args.tokens)
                {
                   if (A.Matches(s))
                   {
                       alist += " ";
                       alist += A.DebugInfo();
                       alist += Environment.NewLine;
                       count++;
                       continue;
                   } 
                }
            }
            WriteLine("Currently: {0}", alist);

            return Success("Shown " + count + " amins");
        }
    }
}
