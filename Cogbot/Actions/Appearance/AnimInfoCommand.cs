using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;

namespace cogbot.Actions
{
    public class AnimInfoCommand : Command
    {
        public AnimInfoCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "animinfo";
            Description = "Show debug info about anims.  Usage:  animinfo [match]";
            Category = CommandCategory.Appearance;
            Parameters = new[] { new NamedParam(typeof(SimAnimation), typeof(UUID)) };
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
