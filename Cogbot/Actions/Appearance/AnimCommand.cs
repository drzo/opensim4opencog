using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;

namespace cogbot.Actions
{
    public class AnimCommand : Command
    {
        public AnimCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "anim";
            Description = "Do a animation or gesture.  Usage:  anim [seconds] HOVER [seconds] 23423423423-4234234234-234234234-23423423  +CLAP -JUMP STAND";
            Category = CommandCategory.Appearance;
        }
       
        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
            {
                ICollection<string> list = WorldSystem.SimAssetSystem.GetAssetNames(AssetType.Animation);
                WriteLine(TheBotClient.argsListString(list));
                IDictionary<UUID, int> anims = WorldSystem.TheSimAvatar.GetCurrentAnimDict();
                string alist = String.Empty;
                foreach (UUID id in anims.Keys)
                {
                    alist += WorldSystem.GetAnimationName(id);
                    alist += " ";
                    alist += anims[id];
                    alist += Environment.NewLine;
                }
                WriteLine("Currently: {0}", alist);
                return "Usage: anim [seconds] HOVER [seconds] 23423423423-4234234234-234234234-23423423  +CLAP -JUMP STAND";
           }
            int time = 1300; //should be long enough for most animations
            List<KeyValuePair<UUID, int>> amins = new List<KeyValuePair<UUID, int>>();
            for (int i = 0; i < args.Length; i++)
            {
                string a = args[i];
                if (String.IsNullOrEmpty(a)) continue;
                if (time < 1) time = 1300;
                try
                {
                    float ia;
                    if (float.TryParse(a, out ia))
                    {
                        if (ia > 0.0)
                        {
                            time = (int)(ia * 1000);
                            continue;
                        }
                    }
                }
                catch (Exception) { }
                char c = a.ToCharArray()[0];
                if (c == '-')
                {
                    time = -1;
                    a = a.Substring(1);
                }
                else if (c == '+')
                {
                    time = 0;
                    a = a.Substring(1);
                }
                UUID anim = WorldSystem.GetAssetUUID(a, AssetType.Animation);

                if (anim == UUID.Zero)
                {
                    try
                    {
                        if (a.Substring(2).Contains("-"))
                            anim = UUIDParse(a);
                    }
                    catch (Exception) { }
                }
                if (anim == UUID.Zero)
                {
                    WriteLine("unknown animation " + a);
                    continue;
                }
                amins.Add(new KeyValuePair<UUID,int>(anim,time));
            }
            foreach (KeyValuePair<UUID, int> anim in amins)
            {
                try
                {
                    int val = anim.Value;
                    switch (val)
                    {
                        case -1:
                            Client.Self.AnimationStop(anim.Key, true);
                            WriteLine("Stop anim " + WorldSystem.GetAnimationName(anim.Key));
                            continue;
                        case 0:
                            Client.Self.AnimationStart(anim.Key, true);
                            WriteLine("Start anim " + WorldSystem.GetAnimationName(anim.Key));
                            continue;
                        default:
                            try
                            {
                                Client.Self.AnimationStart(anim.Key, true);
                                WriteLine("Ran anim " + WorldSystem.GetAnimationName(anim.Key) + " for " + val/1000 +
                                          " seconds.");
                                Thread.Sleep(val);
                            }
                            finally
                            {
                                Client.Self.AnimationStop(anim.Key, true);
                            }
                            continue;
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine("" + e);
                }
            }
            return "Ran "+amins.Count+" amins";
        }
    }
}
