using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;

namespace cogbot.Actions
{
    public class AnimCommand : Command
    {
        public AnimCommand(BotClient testClient)
        {
            Client = testClient;
            Name = "anim";
            Description = "Do a amination or gesture.  Usage:  anim [1-10] aminname";
            Category = CommandCategory.Appearance;
        }
       
        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length < 1)
            {
                ICollection<string> list = Listeners.WorldObjects.GetAnimationList();
               WriteLine(Client.argsListString(list));
               return "Usage:  anim [seconds] HOVER [seconds] CLAP JUMP STAND";
           }
            int time = 1300; //should be long enbough for most animations
            List<KeyValuePair<UUID, int>> amins = new List<KeyValuePair<UUID, int>>();
            for (int i = 0; i < args.Length; i++)
            {
                string a = args[i];
                try
                {
                    float ia = float.Parse(a);
                    if (ia > 0.0)
                    {
                        time = (int)(ia * 1000);
                        continue;
                    }
                }
                catch (Exception) { }
                UUID anim = Listeners.WorldObjects.GetAnimationUUID(a);

                if (anim == UUID.Zero)
                {
                    if (a.Contains("-")) 
                    anim = UUID.Parse(a);
                }
                if (anim == UUID.Zero)
                {
                    WriteLine("unknown animation " + a);
                    continue;
                }
                amins.Add(new KeyValuePair<UUID,int>(anim,time));
            }
            foreach(KeyValuePair<UUID,int> anim in amins) {
                Client.Self.AnimationStart(anim.Key, true);
                WriteLine("Run anim " + Listeners.WorldObjects.GetAnimationName(anim.Key) + " for " + anim.Value/1000 + " seconds.");
                Thread.Sleep(anim.Value);
                Client.Self.AnimationStop(anim.Key, true);
            }
            return "Ran "+amins.Count+" amins";
        }
    }
}
