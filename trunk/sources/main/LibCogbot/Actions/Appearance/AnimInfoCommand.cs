using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Appearance
{
    public class AnimInfoCommand : Command, GridMasterCommand
    {
        public AnimInfoCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "animinfo";
            Description = "Show debug info about anims.";
            Usage = Htmlize.Usage(Name + " <match>", "shows the info about animation") +
                    Htmlize.Example(Name + " stand1", "shows that it loops and durration");
            Category = CommandCategory.Appearance;
            Parameters = NamedParam.CreateParams(NamedParam.Optional("anim", typeof (SimAnimation), "the animation you want info about such as duration"));
            ResultMap = NamedParam.CreateParams(
                "message", typeof(string), "debug infos about the animations",
                "success", typeof(bool), "true if command was successful");
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            ICollection<SimAsset> list = SimAssetStore.GetAssets(AssetType.Animation);
            int count = 0;
            string alist = String.Empty;
            lock (list)
            foreach (SimAsset A in list)
            {
                foreach (string s in args)
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
