using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class GestureCommand : Command, BotPersonalCommand
    {
        public GestureCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "gesture";
            Description = "List or do animation or gesture on Simulator.";
            Details = AddUsage("gesture", "just lists anims currently running") +
                    Example("gesture 5 23423423423-4234234234-234234234-23423423 10 CLAP",
                                  "wait 5 seconds, play hiyah (used uuid), wait 10 seconds, play hiyah (used name)");

            Category = CommandCategory.Appearance;
            ParameterVersions = CreateParamVersions(
                CreateParams(),
                CreateParams(
                    Optional("stopall", typeof (bool), "stops all current anims"),
                    Optional("anim_0-N", typeof (SimAnimation), "+/-animuuid"),
                    Optional("seconds", typeof (int), "how long to pause for"),
                    Optional("gesture_0-N", typeof (SimAnimation), "gesture to play at this step")));
            ResultMap = CreateParams(
                "ranSteps", typeof (List<string>), "list of ran steps",
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
            {
                ICollection<string> list = WorldSystem.SimAssetSystem.GetAssetNames(AssetType.Gesture);
                WriteLine(TheBotClient.argsListString(list));
                Dictionary<UUID,int> gestures = WorldSystem.TheSimAvatar.GetCurrentAnimDict();
                string alist = String.Empty;
                foreach (var anim in gestures)
                {
                    alist += WorldSystem.GetAnimationName(anim.Key);
                    alist += " ";
                    alist += anim.Value;
                    alist += Environment.NewLine;
                }
                WriteLine("Currently: {0}", alist);
                return base.ShowUsage();// " gesture  23423423423-4234234234-234234234-23423423";
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
            a = WorldSystem.SimAssetSystem.GetAssetName(gesture) + " " + a;
            if (!Client.Network.Connected)
            {
                return Success("ERROR NotConnected gesture " + a);
            }
            Client.Self.PlayGesture(gesture);
            return Success("Started gesture: " + a);
;
        }
    }
}
