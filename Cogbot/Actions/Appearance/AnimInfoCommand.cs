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
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            ICollection<SimAnimation> list = SimAnimationStore.SimAnimations;
            int count = 0;
            string alist = String.Empty;
            lock (list)
            foreach (SimAnimation A in list)
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

            return "Shown " + count + " amins";
        }
    }
}
