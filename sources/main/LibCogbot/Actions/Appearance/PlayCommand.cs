using System;
using OpenMetaverse;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class PlayCommand : Command, BotPersonalCommand, FFIComplete
    {
        public static bool NOSEARCH_ANIM = false;

        public PlayCommand(BotClient testClient)
        {
            Name = "anim";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "List or do animation or gesture on Simulator.";
            Details = AddUsage("anim", "just lists anims currently running") +
                      AddUsage("anim stopall +HOVER 5 +23423423423-4234234234-234234234-23423423 10 -CLAP",
                               "stop all current anims, begin hover, wait 5 seconds, begin clapping (used uuid), wait 10 seconds, stop clapping (used name)");

            //// we dont use sequenceOf here... for ideas
            NamedParam sequenceOf = SequenceOf("doList",
                                               OneOf(
                                                   Required("asset_0-N", typeof (SimAsset), "+/-animuuid"),
                                                   Required("stopall", typeof (bool),
                                                            "stops all current anims/played items"),
                                                   Required("seconds", typeof (int), "how long to pause for")));
            Category = CommandCategory.Appearance;

            AddVersion(CreateParams("dolist", typeof (string[]), "asset play list"), "run the anim list");
            AddVersion(CreateParams(), "just lists anims currently running");

            ResultMap = CreateParams(
                "assets", typeof (List<SimAnimation>), "list of assets like animations and gestures played",
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (NOSEARCH_ANIM)
            {
                String str = args.str;
                WriteLine("PLAY ECHO " + str);
                return Success("\nStart assets " + str + "\n");
            }
            bool writeInfo = !args.IsFFI;
            if (args.Length < 1)
            {
                Dictionary<UUID, int> gestures = WorldSystem.TheSimAvatar.GetCurrentAnimDict();
                string alist = String.Empty;
                foreach (var anim in gestures)
                {
                    AppendItem("assets", WorldSystem.GetAsset(anim.Key));
                    if (!writeInfo) continue;
                    alist += WorldSystem.GetAnimationName(anim.Key);
                    alist += " ";
                    alist += anim.Value;
                    alist += Environment.NewLine;
                }
                if (writeInfo) WriteLine("Currently: {0}", alist);
                return SuccessOrFailure();
                    // " anim [seconds] HOVER [seconds] 23423423423-4234234234-234234234-23423423  +CLAP -JUMP STAND";           
            }
            int argStart = 0;

            int time = 1300; //should be long enough for most animations
            base.SetWriteLine("message");
            string directive = args[0].ToLower();
            int mode = 0;
            int defaultAnimTime = 2;
            int animsRan = 0;
            AssetType defaultSearch = AssetType.Gesture;
            for (int i = argStart; i < args.Length; i++)
            {
                string a = args[i];
                if (String.IsNullOrEmpty(a)) continue;
                try
                {
                    float ia;
                    if (float.TryParse(a, out ia))
                    {
                        if (ia > 0.0)
                        {
                            time = (int) (ia*1000);
                            Thread.Sleep(time);
                            continue;
                        }
                    }
                }
                catch (Exception)
                {
                }
                char c = a.ToCharArray()[0];
                if (c == '-')
                {
                    mode = -1;
                    a = a.Substring(1);
                }
                else if (c == '+')
                {
                    mode = 1;
                    a = a.Substring(1);
                }
                else
                {
                    mode = 0;
                }
                if (a == "") continue;

                if (a.ToLower() == "stopall")
                {
                    Dictionary<UUID, bool> animations = new Dictionary<UUID, bool>();
                    var anims = TheSimAvatar.GetCurrentAnims();
                    foreach (var ani in anims)
                    {
                        animations[ani] = false;
                    }
                    int knownCount = animations.Count;

                    Client.Self.Animate(animations, true);
                    WriteLine("stopping all");
                    continue;
                }

                try
                {
                    defaultSearch = (AssetType) Enum.Parse(typeof (AssetType), a, true);
                    continue;
                }
                catch
                {
                }

                UUID anim = WorldSystem.GetAssetUUID(a, defaultSearch);

                if (anim == UUID.Zero)
                {
                    try
                    {
                        if (a.Substring(2).Contains("-"))
                            anim = UUIDParse(a);
                    }
                    catch (Exception)
                    {
                    }
                }
                if (anim == UUID.Zero)
                {
                    anim = WorldSystem.GetAssetUUID(a, AssetType.Unknown);
                }
                if (anim == UUID.Zero)
                {
                    Failure("skipping unknown animation/gesture " + a);
                    continue;
                }
                SimAsset asset = SimAssetStore.FindAsset(anim);
                animsRan++;
                AppendItem("assets", WorldSystem.GetAsset(anim));
                if (asset is SimGesture)
                {
                    Client.Self.PlayGesture(asset.AssetID);
                    continue;
                }
                if (asset is SimSound)
                {
                    Client.Sound.PlaySound(asset.AssetID);
                    continue;
                }
                if (asset != null && !(asset is SimAnimation))
                {
                    Failure("Dont know how to play " + asset);
                    continue;
                }
                try
                {
                    switch (mode)
                    {
                        case -1:
                            Client.Self.AnimationStop(anim, true);
                            if (writeInfo) WriteLine("\nStop anim " + WorldSystem.GetAnimationName(anim));
                            continue;
                        case +1:
                            Client.Self.AnimationStart(anim, true);
                            if (writeInfo) WriteLine("\nStart anim " + WorldSystem.GetAnimationName(anim));
                            continue;
                        default:
                            try
                            {
                                int val = time;
                                Client.Self.AnimationStart(anim, true);
                                if (writeInfo)
                                    WriteLine("\nRan anim " + WorldSystem.GetAnimationName(anim) + " for " + val/1000 +
                                              " seconds.");
                                Thread.Sleep(val);
                            }
                            finally
                            {
                                Client.Self.AnimationStop(anim, true);
                            }
                            continue;
                    }
                }
                catch (Exception e)
                {
                    return Failure("\nRan " + animsRan + " asserts but " + e);
                }
            }
            return SuccessOrFailure();
        }
    }
}