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
            Name = "animinfo";
            TheBotClient = testClient;
        }

        override public void MakeInfo()
        {
            Description = "Show debug info about anims.";                    
            AddExample(Name + " stand1", "shows that it loops and durration");
            Category = CommandCategory.Appearance;
            AddVersion(
                CreateParams(Optional("anim", typeof (SimAnimation),
                                      "the animation you want info about such as duration")),
                "shows the info about animation");
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
