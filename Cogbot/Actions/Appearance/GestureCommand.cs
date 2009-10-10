using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;

namespace cogbot.Actions
{
    public class GestureCommand : Command
    {
        public GestureCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "gesture";
            Description = "Do a gesture.  Usage:  gesture [seconds] BOW [seconds] 23423423423-4234234234-234234234-23423423  +CLAP -JUMP STAND";
            Category = CommandCategory.Appearance;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
            {
                ICollection<string> list = WorldSystem.SimAssetSystem.GetAssetNames(AssetType.Gesture);
                WriteLine(TheBotClient.argsListString(list));
                IDictionary<UUID, int> gestures = WorldSystem.TheSimAvatar.GetCurrentAnimDict();
                string alist = String.Empty;
                foreach (UUID id in gestures.Keys)
                {
                    alist += WorldSystem.GetAnimationName(id);
                    alist += " ";
                    alist += gestures[id];
                    alist += Environment.NewLine;
                }
                WriteLine("Currently: {0}", alist);
                return base.Failure(Usage);// " gesture  23423423423-4234234234-234234234-23423423";
            }
            string a = args[0];
            UUID gesture = WorldSystem.GetAssetUUID(a, AssetType.Gesture);
            if (gesture == UUID.Zero)
            {
                try
                {
                    if (a.Substring(2).Contains("-"))
                        gesture = UUIDParse(a);
                }
                catch (Exception)
                {
                }
            }
            if (gesture == UUID.Zero)
            {
                return Failure("Unknown gesture " + a);
            }
            Client.Self.PlayGesture(gesture);
            return Success("Started gesture: " + WorldSystem.SimAssetSystem.GetAssetName(gesture));
;
        }
    }
}
