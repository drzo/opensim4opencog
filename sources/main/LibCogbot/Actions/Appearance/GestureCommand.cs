using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Appearance
{
    public class GestureCommand : Command, BotPersonalCommand
    {
        public GestureCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "gesture";
            Description = "List or do animation or gesture on Simulator.";
            Usage =
                @"Usage: gesture // just lists anims currently ran
                  Usage: gesture stopall +HOVER 5 +23423423423-4234234234-234234234-23423423 10 -CLAP  " +
                "// stop all current anims, begin hover, wait 5 seconds, begin clapping, wait 10 seconds, stop clapping ";

            Category = CommandCategory.Appearance;
            ParameterVersions = NamedParam.CreateParamVersions(
                NamedParam.CreateParams(),
                NamedParam.CreateParams(
                    NamedParam.Optional("stopall", typeof (bool), "stops all current anims"),
                    NamedParam.Optional("anim_0-N", typeof (SimAnimation), "+/-animuuid"),
                    NamedParam.Optional("seconds", typeof (int), "how long to pause for"),
                    NamedParam.Optional("gesture_0-N", typeof (SimAnimation), "gesture to play at this step")));
            ResultMap = NamedParam.CreateParams(
                "ranSteps", typeof (List<string>), "list of ran steps",
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
