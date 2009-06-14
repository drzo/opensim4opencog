using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    public class AnimThread
    {
        private readonly SimAnimation anim;
        private readonly AgentManager ClientSelf;
        private Thread animLoop;
        private bool repeat = true;

        public AnimThread(AgentManager c, SimAnimation amin0)
        {
            ClientSelf = c; //.Self;
            //c.Client
            //if (WorldObjects.Master.GetAnimationName(amin0).StartsWith("S"))
            //{
             //   repeat = false;
           // }
            anim = amin0;
        }

        public override string ToString()
        {
            return String.Format("AnimLoop {0} of {1}", anim, ClientSelf);
        }

        public void Start()
        {
            animLoop = new Thread(LoopAnim) {Name = string.Format("Thread for {0}", ToString())};
            animLoop.Start();
        }

        private void LoopAnim()
        {
            try
            {
                ClientSelf.AnimationStart(AnimationID, true);
                while (NeedsLooping)
                {
                    // some anims will only last a short time so we have to 
                    // remind the server we still want to be using it 
                    // like Laugh .. lasts for about .9 seconds
                    //12000 is a estimate average
                    Thread.Sleep((int)(anim.Length*1000));
                    ClientSelf.AnimationStop(AnimationID, true);
                    ClientSelf.AnimationStart(AnimationID, true);
                }
            }
            catch (Exception)
            {
            } // for the Abort 
        }

        protected bool NeedsLooping
        {
            get
            {
                if (repeat) return true;
                if (!anim.IsLoop) return true;
                return false;
            }
        }

        protected UUID AnimationID
        {
            get { return anim.AnimationID; }
        }

        public void Stop()
        {
            repeat = false;
            if (animLoop != null)
            {
                try
                {
                    if (animLoop.IsAlive) animLoop.Abort();
                }
                catch (Exception)
                {
                }
                animLoop = null;
            }
            ClientSelf.AnimationStop(AnimationID, true);
        }
    }

    public class SimAnimationStore
    {

        internal static readonly Dictionary<UUID, SimAnimation> uuidAnim = new Dictionary<UUID, SimAnimation>();
        internal static readonly Dictionary<string, SimAnimation> nameAnim = new Dictionary<string, SimAnimation>();

        readonly BotClient Client;
        public SimAnimationStore(BotClient GC)
        {
            Client = GC;            
        }

        void DownloadAnimFolder()
        {
            Client.AnimationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);
            List<InventoryBase> contents = Client.Inventory.FolderContents(Client.AnimationFolder, Client.Self.AgentID,
                true, true, InventorySortOrder.ByName, 3000);
            foreach (InventoryBase IB in contents)
            {
                if (IB is InventoryItem)
                {
                    InventoryItem II = (InventoryItem)IB;
                    if (II.AssetType == AssetType.Animation)
                    {
                        SetAnimationName(II.AssetUUID, II.Name);
                    }
                }
            }
        }

        public bool SameAnims(BinBVHAnimationReader bvh1, BinBVHAnimationReader bvh2)
        {
            if (bvh1 == bvh2) return true;
            if (bvh1.joints.Length != bvh2.joints.Length) return false;
            if (bvh1.Loop != bvh2.Loop) return false;           
            return false;
        }

        internal void OnAnimDownloaded(UUID uUID, AssetAnimation asset)
        {
            SimAnimation A = FindOrCreateAnimation(uUID);
            A.BvhData = asset.AssetData;



            if (false)
            {
                AutoResetEvent UploadCompleteEvent = new AutoResetEvent(false);
                if (Client.AnimationFolder == UUID.Zero)
                    Client.AnimationFolder = Client.Inventory.FindFolderForType(AssetType.Animation);

                DateTime start = new DateTime();
                Client.Inventory.RequestCreateItemFromAsset(asset.AssetData, A.Name, "Anim captured " + uUID,
                                                            AssetType.Animation,
                                                            InventoryType.Animation, Client.AnimationFolder,
                                                            delegate(bool success, string status, UUID itemID,
                                                                     UUID assetID)
                                                                {
                                                                    Console.WriteLine(
                                                                        String.Format(
                                                                            "RequestCreateItemFromAsset() returned: Success={0}, Status={1}, ItemID={2}, AssetID={3}",
                                                                            success, status, itemID, assetID));
                                                                    Console.WriteLine(String.Format("Upload took {0}",
                                                                                                    DateTime.Now.
                                                                                                        Subtract(start)));
                                                                    UploadCompleteEvent.Set();
                                                                });

                UploadCompleteEvent.WaitOne();

                //A.Name
                //SetAnimationName(asset.AssetID, s);
            }
            //              Debug(s);
            //                        RegisterUUID(asset.AssetID, s);

        }


        internal void FillAnimationNames()
        {
            lock (uuidAnim)
            {
                if (uuidAnim.Count > 0) return;

                AnimMap("aim_bazooka_r", "avatar_aim_R_bazooka");
                AnimMap("aim_bow_l", "avatar_aim_L_bow");
                AnimMap("aim_handgun_r", "avatar_aim_R_handgun");
                AnimMap("aim_rifle_r", "avatar_aim_R_rifle");
                AnimMap("medium_land", "avatar_soft_land");
                AnimMap("muscle_beach", "avatar_musclebeach");
                AnimMap("no", "avatar_no_head");
                AnimMap("nyah_nyah", "avatar_nyanya");
                AnimMap("onetwo_punch", "avatar_punch_onetwo");
                AnimMap("pre_jump", "avatar_prejump");
                AnimMap("punch_left", "avatar_punch_L");
                AnimMap("punch_right", "avatar_punch_R");
                AnimMap("roundhouse_kick", "avatar_kick_roundhouse_R");
                AnimMap("shoot_bow_l", "avatar_shoot_L_bow");
                AnimMap("sit_ground_staticrained", "avatar_sit_ground_constrained");
                AnimMap("sword_strike", "avatar_sword_strike_R");
                AnimMap("tantrum", "avatar_angry_tantrum");
                AnimMap("yes", "avatar_yes_head");
                AnimMap("blow_kiss", "avatar_blowkiss");
                AnimMap("busy", "avatar_away");
                AnimMap("finger_wag", "avatar_angry_fingerwag");
                AnimMap("hold_bazooka_r", "avatar_hold_R_bazooka");
                AnimMap("hold_bow_l", "avatar_hold_L_bow");
                AnimMap("hold_handgun_r", "avatar_hold_R_handgun");
                AnimMap("hold_rifle_r", "avatar_hold_R_rifle");
                AnimMap("jump_for_joy", "avatar_jumpforjoy");
                AnimMap("kiss_my_butt", "avatar_kissmybutt");
                // TODO: these animations need corrected likely
                AnimMap("express_embarrassed", "avatar_express_embarrased");
                AnimMap("embarrassed", "avatar_express_embarrased");
                AnimMap("belly_laugh", "avatar_express_laugh"); 
                AnimMap("angry", "avatar_angry_tantrum");
                /*
                 * need UUIDS for
                        customize.animation
                        customize_done.animation
                        express_disdain.animation
                        express_frown.animation
                        express_kiss.animation
                        express_open_mouth.animation
                        express_smile.animation
                        express_tongue_out.animation
                        express_toothsmile.animation
                 */
                AnimUUID("AIM_L_BOW", "46bb4359-de38-4ed8-6a22-f1f52fe8f506");
                AnimUUID("AIM_R_BAZOOKA", "b5b4a67d-0aee-30d2-72cd-77b333e932ef");
                AnimUUID("AIM_R_HANDGUN", "3147d815-6338-b932-f011-16b56d9ac18b");
                AnimUUID("AIM_R_RIFLE", "ea633413-8006-180a-c3ba-96dd1d756720");
                AnimUUID("ANGRY_FINGERWAG", "c1bc7f36-3ba0-d844-f93c-93be945d644f");
                AnimUUID("ANGRY_TANTRUM", "11000694-3f41-adc2-606b-eee1d66f3724");
                AnimUUID("AWAY", "fd037134-85d4-f241-72c6-4f42164fedee");
                AnimUUID("BACKFLIP", "c4ca6188-9127-4f31-0158-23c4e2f93304");
                AnimUUID("BLOWKISS", "db84829b-462c-ee83-1e27-9bbee66bd624");
                AnimUUID("BOW", "82e99230-c906-1403-4d9c-3889dd98daba");
                AnimUUID("BRUSH", "349a3801-54f9-bf2c-3bd0-1ac89772af01");
                AnimUUID("BUSY", "efcf670c-2d18-8128-973a-034ebc806b67");
                AnimUUID("CLAP", "9b0c1c4e-8ac7-7969-1494-28c874c4f668");
                AnimUUID("COURTBOW", "9ba1c942-08be-e43a-fb29-16ad440efc50");
                AnimUUID("CROUCH", "Crouching", "201f3fdf-cb1f-dbec-201f-7333e328ae7c");
                AnimUUID("CROUCHWALK", "CrouchWalking", "47f5f6fb-22e5-ae44-f871-73aaaf4a6022");
                AnimUUID("DANCE1", "b68a3d7c-de9e-fc87-eec8-543d787e5b0d");
                AnimUUID("DANCE2", "928cae18-e31d-76fd-9cc9-2f55160ff818");
                AnimUUID("DANCE3", "30047778-10ea-1af7-6881-4db7a3a5a114");
                AnimUUID("DANCE4", "951469f4-c7b2-c818-9dee-ad7eea8c30b7");
                AnimUUID("DANCE5", "4bd69a1d-1114-a0b4-625f-84e0a5237155");
                AnimUUID("DANCE6", "cd28b69b-9c95-bb78-3f94-8d605ff1bb12");
                AnimUUID("DANCE7", "a54d8ee2-28bb-80a9-7f0c-7afbbe24a5d6");
                AnimUUID("DANCE8", "b0dc417c-1f11-af36-2e80-7e7489fa7cdc");
                AnimUUID("DEAD", "57abaae6-1d17-7b1b-5f98-6d11a6411276");
                AnimUUID("DRINK", "0f86e355-dd31-a61c-fdb0-3a96b9aad05f");
                AnimUUID("EXPRESS_AFRAID", "6b61c8e8-4747-0d75-12d7-e49ff207a4ca");
                AnimUUID("EXPRESS_AFRAID_EMOTE", "aa2df84d-cf8f-7218-527b-424a52de766e");
                AnimUUID("EXPRESS_ANGER", "5747a48e-073e-c331-f6f3-7c2149613d3e");
                AnimUUID("EXPRESS_ANGER_EMOTE", "1a03b575-9634-b62a-5767-3a679e81f4de");
                AnimUUID("EXPRESS_BORED", "b906c4ba-703b-1940-32a3-0c7f7d791510");
                AnimUUID("EXPRESS_BORED_EMOTE", "214aa6c1-ba6a-4578-f27c-ce7688f61d0d");
                AnimUUID("EXPRESS_CRY", "92624d3e-1068-f1aa-a5ec-8244585193ed");
                AnimUUID("EXPRESS_CRY_EMOTE", "d535471b-85bf-3b4d-a542-93bea4f59d33");
                AnimUUID("EXPRESS_DISDAIN", "d4416ff1-09d3-300f-4183-1b68a19b9fc1");
                AnimUUID("EXPRESS_EMBARRASED", "514af488-9051-044a-b3fc-d4dbf76377c6");
                AnimUUID("EXPRESS_EMBARRASSED_EMOTE", "0b8c8211-d78c-33e8-fa28-c51a9594e424");
                AnimUUID("EXPRESS_FROWN", "fee3df48-fa3d-1015-1e26-a205810e3001");
                AnimUUID("EXPRESS_KISS", "1e8d90cc-a84e-e135-884c-7c82c8b03a14");
                AnimUUID("EXPRESS_LAUGH", "18b3a4b5-b463-bd48-e4b6-71eaac76c515");
                AnimUUID("EXPRESS_LAUGH_EMOTE", "62570842-0950-96f8-341c-809e65110823");
                AnimUUID("EXPRESS_OPEN_MOUTH", "d63bc1f9-fc81-9625-a0c6-007176d82eb7");
                AnimUUID("EXPRESS_REPULSED", "36f81a92-f076-5893-dc4b-7c3795e487cf");
                AnimUUID("EXPRESS_REPULSED_EMOTE", "f76cda94-41d4-a229-2872-e0296e58afe1");
                AnimUUID("EXPRESS_SAD", "0eb702e2-cc5a-9a88-56a5-661a55c0676a");
                AnimUUID("EXPRESS_SAD_EMOTE", "eb6ebfb2-a4b3-a19c-d388-4dd5c03823f7");
                AnimUUID("EXPRESS_SHRUG", "70ea714f-3a97-d742-1b01-590a8fcd1db5");
                AnimUUID("EXPRESS_SHRUG_EMOTE", "a351b1bc-cc94-aac2-7bea-a7e6ebad15ef");
                AnimUUID("EXPRESS_SMILE", "b7c7c833-e3d3-c4e3-9fc0-131237446312");
                AnimUUID("EXPRESS_SURPRISE", "313b9881-4302-73c0-c7d0-0e7a36b6c224");
                AnimUUID("EXPRESS_SURPRISE_EMOTE", "728646d9-cc79-08b2-32d6-937f0a835c24");
                AnimUUID("EXPRESS_TOOTHSMILE", "b92ec1a5-e7ce-a76b-2b05-bcdb9311417e");
                AnimUUID("EXPRESS_TONGUE_OUT", "835965c6-7f2f-bda2-5deb-2478737f91bf");
                AnimUUID("EXPRESS_WINK", "869ecdad-a44b-671e-3266-56aef2e3ac2e");
                AnimUUID("EXPRESS_WINK_EMOTE", "da020525-4d94-59d6-23d7-81fdebf33148");
                AnimUUID("EXPRESS_WORRY", "9f496bd2-589a-709f-16cc-69bf7df1d36c");
                AnimUUID("EXPRESS_WORRY_EMOTE", "9c05e5c7-6f07-6ca4-ed5a-b230390c3950");
                AnimUUID("FALLDOWN", "Falling", "666307d9-a860-572d-6fd4-c3ab8865c094");
                AnimUUID("FEMALE_WALK", "Walking", "f5fc7433-043d-e819-8298-f519a119b688");
                AnimUUID("FIST_PUMP", "7db00ccd-f380-f3ee-439d-61968ec69c8a");
                AnimUUID("FLY", "Flying", "aec4610c-757f-bc4e-c092-c6e9caf18daf");
                AnimUUID("FLYSLOW", "FlyingSlow", "2b5a38b2-5e00-3a97-a495-4c826bc443e6");
                AnimUUID("HELLO", "9b29cd61-c45b-5689-ded2-91756b8d76a9");
                AnimUUID("HOLD_L_BOW", "8b102617-bcba-037b-86c1-b76219f90c88");
                AnimUUID("HOLD_R_BAZOOKA", "ef62d355-c815-4816-2474-b1acc21094a6");
                AnimUUID("HOLD_R_HANDGUN", "efdc1727-8b8a-c800-4077-975fc27ee2f2");
                AnimUUID("HOLD_R_RIFLE", "3d94bad0-c55b-7dcc-8763-033c59405d33");
                AnimUUID("HOLD_THROW_R", "7570c7b5-1f22-56dd-56ef-a9168241bbb6");
                AnimUUID("HOVER", "Hovering", "4ae8016b-31b9-03bb-c401-b1ea941db41d");
                AnimUUID("HOVER_DOWN", "Hovering Down", "20f063ea-8306-2562-0b07-5c853b37b31e");
                AnimUUID("HOVER_UP", "Hovering Up", "62c5de58-cb33-5743-3d07-9e4cd4352864");
                AnimUUID("IMPATIENT", "5ea3991f-c293-392e-6860-91dfa01278a3");
                AnimUUID("JUMP", "Jumping", "2305bd75-1ca9-b03b-1faa-b176b8a8c49e");
                AnimUUID("JUMPFORJOY", "709ea28e-1573-c023-8bf8-520c8bc637fa");
                AnimUUID("KICK_ROUNDHOUSE_R", "49aea43b-5ac3-8a44-b595-96100af0beda");
                AnimUUID("KISSMYBUTT", "19999406-3a3a-d58c-a2ac-d72e555dcf51");
                AnimUUID("LAND", "Landing", "7a17b059-12b2-41b1-570a-186368b6aa6f");
                AnimUUID("LAUGH_SHORT", "ca5b3f14-3194-7a2b-c894-aa699b718d1f");
                AnimUUID("MOTORCYCLE_SIT", "08464f78-3a8e-2944-cba5-0c94aff3af29");
                AnimUUID("MUSCLEBEACH", "315c3a41-a5f3-0ba4-27da-f893f769e69b");
                AnimUUID("NO_HEAD", "5a977ed9-7f72-44e9-4c4c-6e913df8ae74");
                AnimUUID("NO_UNHAPPY", "d83fa0e5-97ed-7eb2-e798-7bd006215cb4");
                AnimUUID("NYANYA", "f061723d-0a18-754f-66ee-29a44795a32f");
                AnimUUID("PEACE", "b312b10e-65ab-a0a4-8b3c-1326ea8e3ed9");
                AnimUUID("POINT_ME", "17c024cc-eef2-f6a0-3527-9869876d7752");
                AnimUUID("POINT_YOU", "ec952cca-61ef-aa3b-2789-4d1344f016de");
                AnimUUID("PREJUMP", "PreJumping", "7a4e87fe-de39-6fcb-6223-024b00893244");
                AnimUUID("PUNCH_L", "f3300ad9-3462-1d07-2044-0fef80062da0");
                AnimUUID("PUNCH_ONETWO", "eefc79be-daae-a239-8c04-890f5d23654a");
                AnimUUID("PUNCH_R", "c8e42d32-7310-6906-c903-cab5d4a34656");
                AnimUUID("RPS_COUNTDOWN", "35db4f7e-28c2-6679-cea9-3ee108f7fc7f");
                AnimUUID("RPS_PAPER", "0836b67f-7f7b-f37b-c00a-460dc1521f5a");
                AnimUUID("RPS_ROCK", "42dd95d5-0bc6-6392-f650-777304946c0f");
                AnimUUID("RPS_SCISSORS", "16803a9f-5140-e042-4d7b-d28ba247c325");
                AnimUUID("RUN", "Running", "05ddbff8-aaa9-92a1-2b74-8fe77a29b445");
                AnimUUID("SALUTE", "cd7668a6-7011-d7e2-ead8-fc69eff1a104");
                AnimUUID("SHOOT_L_BOW", "e04d450d-fdb5-0432-fd68-818aaf5935f8");
                AnimUUID("SHOUT", "6bd01860-4ebd-127a-bb3d-d1427e8e0c42");
                AnimUUID("SIT", "Sitting", "1a5fe8ac-a804-8a5d-7cbd-56bd83184568");
                AnimUUID("SIT_FEMALE", "Sitting", "b1709c8d-ecd3-54a1-4f28-d55ac0840782");
                AnimUUID("SIT_GENERIC", "Sitting", "245f3c54-f1c0-bf2e-811f-46d8eeb386e7");
                AnimUUID("SIT_GROUND", "Sitting on Ground", "1c7600d6-661f-b87b-efe2-d7421eb93c86");
                AnimUUID("SIT_GROUND_CONSTRAINED", "Sitting on Ground", "1a2bd58e-87ff-0df8-0b4c-53e047b0bb6e");
                AnimUUID("SIT_TO_STAND", "a8dee56f-2eae-9e7a-05a2-6fb92b97e21e");
                AnimUUID("SLEEP", "f2bed5f9-9d44-39af-b0cd-257b2a17fe40");
                AnimUUID("SMOKE_IDLE", "d2f2ee58-8ad1-06c9-d8d3-3827ba31567a");
                AnimUUID("SMOKE_INHALE", "6802d553-49da-0778-9f85-1599a2266526");
                AnimUUID("SMOKE_THROW_DOWN", "0a9fb970-8b44-9114-d3a9-bf69cfe804d6");
                AnimUUID("SNAPSHOT", "eae8905b-271a-99e2-4c0e-31106afd100c");
                AnimUUID("SOFT_LAND", "Soft Landing", "f4f00d6e-b9fe-9292-f4cb-0ae06ea58d57");
                AnimUUID("STAND", "Standing", "2408fe9e-df1d-1d7d-f4ff-1384fa7b350f");
                AnimUUID("STAND_1", "Standing", "15468e00-3400-bb66-cecc-646d7c14458e");
                AnimUUID("STAND_2", "Standing", "370f3a20-6ca6-9971-848c-9a01bc42ae3c");
                AnimUUID("STAND_3", "Standing", "42b46214-4b44-79ae-deb8-0df61424ff4b");
                AnimUUID("STAND_4", "Standing", "f22fed8b-a5ed-2c93-64d5-bdd8b93c889f");
                AnimUUID("STANDUP", "Standing Up", "3da1d753-028a-5446-24f3-9c9b856d9422");
                AnimUUID("STRETCH", "80700431-74ec-a008-14f8-77575e73693f");
                AnimUUID("STRIDE", "Striding", "1cb562b0-ba21-2202-efb3-30f82cdf9595");
                AnimUUID("SURF", "41426836-7437-7e89-025d-0aa4d10f1d69");
                AnimUUID("SWORD_STRIKE_R", "85428680-6bf9-3e64-b489-6f81087c24bd");
                AnimUUID("TALK", "5c682a95-6da4-a463-0bf6-0f5b7be129d1");
                AnimUUID("THROW_R", "aa134404-7dac-7aca-2cba-435f9db875ca");
                AnimUUID("TRYON_SHIRT", "83ff59fe-2346-f236-9009-4e3608af64c1");
                AnimUUID("TURN_180", "038fcec9-5ebd-8a8e-0e2e-6e71a0a1ac53");
                AnimUUID("TURNBACK_180", "6883a61a-b27b-5914-a61e-dda118a9ee2c");
                AnimUUID("TURNLEFT", "Turning Left", "56e0ba0d-4a9f-7f27-6117-32f2ebbf6135");
                AnimUUID("TURNRIGHT", "Turning Right", "2d6daa51-3192-6794-8e2e-a15f8338ec30");
                AnimUUID("TYPE", "c541c47f-e0c0-058b-ad1a-d6ae3a4584d9");
                AnimUUID("WALK", "Walking", "6ed24bd8-91aa-4b12-ccc7-c97c857ab4e0");
                AnimUUID("WHISPER", "7693f268-06c7-ea71-fa21-2b30d6533f8f");
                AnimUUID("WHISTLE", "b1ed7982-c68e-a982-7561-52a88a5298c0");
                AnimUUID("WINK_HOLLYWOOD", "c0c4030f-c02b-49de-24ba-2331f43fe41c");
                AnimUUID("YES_HAPPY", "b8c8b2a3-9008-1771-3bfc-90924955ab2d");
                AnimUUID("YES_HEAD", "15dd911d-be82-2856-26db-27659b142875");
                AnimUUID("YOGA_FLOAT", "42ecd00b-9947-a97c-400a-bbc9174c7aeb");
                AnimUUID("DO_CRUCIFIX_BOOK", "6a492cee-d738-455c-7042-6ef1e52c8ad4");
                AnimUUID("WRITING", "25a57306-f2b8-4821-a28e-23bac5eb5c4a");
                AnimUUID("TYPING", "944daa15-4be4-4319-9568-14ce5d1fcf01");
                

                // from http://wiki.secondlife.com/wiki/Internal_Animations#Viewer-generated_motions
                AnimUUID("body_noise", "9aa8b0a6-0c6f-9518-c7c3-4f41f2c001ad", "LLBodyNoiseMotion in newview/llvoavatar.cpp", "2", "Minor body turns. Joints moved: torso.");
                AnimUUID("breathe_rot", "4c5a103e-b830-2f1c-16bc-224aa0ad5bc8", "LLBreatheMotionRot in newview/llvoavatar.cpp", "1", "Breathing simulation. Joints moved: chest");
                AnimUUID("editing", "2a8eba1d-a7f8-5596-d44a-b4977bf8c8bb", "LLEditingMotion in llcharacter/lleditingmotion.cpp", "2", "Left arm follows selected and edited objects. Joints moved: left shoulder, left elbow, left wrist, torso.");
                AnimUUID("eye", "5c780ea8-1cd1-c463-a128-48c023f6fbea", "LLEyeMotion in llcharacter/llheadrotmotion.cpp", "1", "Eye rotation and blinking. Joints moved: head, left eye, right eye");
                AnimUUID("fly_adjust", "db95561f-f1b0-9f9a-7224-b12f71af126e", "LLFlyAdjustMotion in llcharacter/llkeyframewalkmotion.cpp", "3", "Add body roll during flight. Joints moved: pelvis");
                AnimUUID("hand_motion", "ce986325-0ba7-6e6e-cc24-b17c4b795578", "LLHandMotion in llcharacter/llhandmotion.cpp", "1", "Sets standard hand poses defined in other animations");
                AnimUUID("head_rot", "e6e8d1dd-e643-fff7-b238-c6b4b056a68d", "LLHeadRotMotion in llcharacter/llheadrotmotion.cpp", "1", "Moves head and torso to follow the avatar's look at position (cursor, camera). Joints moved: torso, neck, head");
                AnimUUID("pelvis_fix", "0c5dd2a2-514d-8893-d44d-05beffad208b", "LLPelvisFixMotion in newview/llvoavatar.cpp", "0", "Makes corrections to keep avatar standing upright. Joints moved: pelvis");
                AnimUUID("target", "0e4896cb-fba4-926c-f355-8720189d5b55", "LLTargetingMotion in llcharacter/lltargetingmotion.cpp", "2", "Move body with look at position, used during aim_* animations above. Joints moved: pelvis, torso, right wrist");
                AnimUUID("walk_adjust", "829bc85b-02fc-ec41-be2e-74cc6dd7215d", "LLWalkAdjustMotion in llcharacter/llkeyframewalkmotion.cpp", "2", "Makes walking corrections for terrain, turns. Joints moved: pelvis, left ankle, right ankle");
                

                foreach (FieldInfo fi in typeof (Animations).GetFields())
                {
                    AnimUUID(fi.Name, ((UUID) fi.GetValue(null)).ToString());
                    ////string uids = uid.ToString();
                    //SetAnimationName(uid, fName);
                    ////                    uuidAnim[uid] = fName;
                    //WorldObjects.RegisterUUID(uid, fName);
                    //                  nameAnim[fName] = uid;
                }
                if (Directory.Exists("bvh_files/"))
                {
                    foreach (string files in Directory.GetFiles("bvh_files/"))
                    {
                        if (!files.EndsWith("animation")) continue;
                        byte[] bs = File.ReadAllBytes(files);
                        string name = Path.GetFileNameWithoutExtension(Path.GetFileName(files)).ToLower();
                        if (nameAnim.ContainsKey(name)) continue;
                        Console.WriteLine("Anim w/o UUID " + name);
                        SimAnimation anim = new SimAnimation(UUID.Zero, name);
                        nameAnim[name] = anim;
                    }
                }

                ClassifyAnims();
                foreach (SimAnimation A in SimAnimations)
                {
                   if (A.IsIncomplete())  Console.WriteLine("Animation: " + A.ToString());
                }
            }
        }

        private void AnimUUID(string p, string p_2, string p_3, string p_4, string p_5)
        {
            AnimUUID(p, p_2);
        }

        private void AnimUUID(string p,string p_2,string p_3)
        {
            AnimUUID(p, p_3);
        }


        private void AnimUUID(string fName, string id)
        {
            fName = fName.ToLower();
            UUID uid = UUID.Parse(id);
            SimAnimation anim = FindOrCreateAnimation(uid);
            anim.Name = fName;
            WorldObjects.RegisterUUID(uid, anim);
            if (Directory.Exists("bvh_files/"))
            {
                byte[] bytes;
                string usedName;
                if (BytesFromFile(fName, out bytes, out usedName))
                {
                    anim.BvhData = bytes;
                    anim.Name = usedName;
                }
                else
                {
                    Console.WriteLine("Anim w/o BVH " + fName + " " + uid);
                }
            }
        }

        public static bool BytesFromFile(string fName, out byte[] bytes,out string usedName)
        {
            usedName = fName;
            if (File.Exists("bvh_files/" + usedName + ".animation"))
            {
                bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                return true;
            }
            usedName = "avatar_" + fName;
            if (File.Exists("bvh_files/" + usedName + ".animation"))
            {
                bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                return true;
            }
            usedName = "avatar_express_" + fName;
            if (File.Exists("bvh_files/" + usedName + ".animation"))
            {
                bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                return true;
            }
            if (nameNameMap.ContainsKey(fName))
            {
                usedName = nameNameMap[fName];
                if (File.Exists("bvh_files/" + usedName + ".animation"))
                {
                    bytes = File.ReadAllBytes("bvh_files/" + usedName + ".animation");
                    return true;
                }
            }
            bytes = null;
            return false;
        }

        private void AnimMap(string name, string file)
        {
            nameNameMap[name.ToLower()] = file.ToLower();
        }

        public ICollection<string> GetAnimationList()
        {
            FillAnimationNames();
            List<String> names = new List<String>();
            foreach (var list in SimAnimations)
            {
                names.Add(list.Name); 
            }
            return names;
        }

        public String GetAnimationName(UUID uuid)
        {
            FillAnimationNames();
            String name;
            SimAnimation anim;
            if (uuidAnim.TryGetValue(uuid, out anim))
            {
                return anim.Name;
            }
            return null;
        }


        public UUID GetAnimationUUID(string a)
        {
            a = a.ToLower();
            FillAnimationNames();
            UUID partial = default(UUID);
            foreach (String name in nameAnim.Keys)
            {
                String sname = name.ToLower();
                if (sname.Equals(a))
                {
                    return nameAnim[name].AnimationIDs[0];
                }
                if (sname.Contains(a))
                {
                    partial = nameAnim[name].AnimationIDs[0];
                }
            }
            return partial;
        }

        internal void SetAnimationName(UUID uUID, string s)
        {
            SimAnimation anim = FindOrCreateAnimation(uUID);
            anim.Name = s;
        }

        static SimAnimation FindAnimation(UUID uUID)
        {
            SimAnimation anim;
            if (!uuidAnim.TryGetValue(uUID, out anim))
            {
                foreach (var A in SimAnimations)
                {
                    if (A.AnimationIDs.Contains(uUID))
                    {
                        return anim;
                    }
                }
            }
            return anim;
        }

        public static List<UUID> Matches(String name)
        {            
            List<UUID> matches = new List<UUID>();
            foreach (var A in SimAnimations)
            {
                if (A.Matches(name))
                {
                    matches.Add(A.AnimationID);
                }
            }            
            return matches;   
        }


        public static SimAnimation GetAminFromAssest(AssetAnimation asset)
        {
            SimAnimation A = FindOrCreateAnimation(asset.AssetID);
            A.BvhData = asset.AssetData;
            return A;
        }


        void ClassifyAnims()
        {
            string type = "Sitting";
            PutType(type, Animations.SIT_GROUND);
            PutType(type,Animations.SIT);
            PutType(type,Animations.SIT_GENERIC);
            PutType(type,Animations.SIT_FEMALE);
            PutType(type,Animations.CROUCH);
            PutType(type,Animations.MOTORCYCLE_SIT);
            PutType(type,Animations.SIT_GROUND_staticRAINED);

            type = "Flying";

            PutType(type, Animations.FLYSLOW);
            PutType(type,Animations.FLY);
            PutType(type,FLY_ADJUST);
            PutType(type,Animations.HOVER);
            PutType(type,Animations.HOVER_DOWN);
            PutType(type,Animations.HOVER_UP);
            PutType(type,Animations.YOGA_FLOAT);

            type = "Walking";
            PutType(type, Animations.WALK);
            PutType(type, Animations.TURNLEFT);
            PutType(type, Animations.TURNRIGHT);
            PutType(type, Animations.STRIDE);
            PutType(type, Animations.RUN);
            PutType(type, Animations.FEMALE_WALK);
            PutType(type, WALK_ADJUST);
            PutType(type, Animations.CROUCHWALK);


            type = "Stopping";
            PutType(type,Animations.SIT_TO_STAND);
            PutType(type,Animations.FALLDOWN);
            PutType(type,Animations.STANDUP);
            PutType(type,Animations.MEDIUM_LAND);
            PutType(type,Animations.LAND);
                            
            type = "Laying";
            PutType(type,Animations.SLEEP);
            PutType(type,Animations.DEAD);
            
            type = "Standing";
            PutType(type,Animations.STAND);
            PutType(type,Animations.STAND_1);
            PutType(type,Animations.STAND_2);
            PutType(type,Animations.STAND_3);
            PutType(type,Animations.STAND_4);
            PutType(type,Animations.STANDUP);
            PutType(type,Animations.AWAY);
            PutType(type,Animations.BUSY);  ;

            type = "Jumping";
            PutType(type,Animations.JUMP);
            PutType(type,Animations.PRE_JUMP);

            type = "Communicating";
            PutType(type,Animations.TALK);
            PutType(type,Animations.TYPE);
            PutType(type,Animations.SHRUG);
            PutType(type,Animations.YES);
            PutType(type,Animations.YES_HAPPY);
            PutType(type,Animations.NO);
            PutType(type,Animations.NO_UNHAPPY);
            PutType(type,Animations.ANGRY);
            PutType(type,Animations.LAUGH_SHORT);
            PutType(type,Animations.CRY);
            PutType(type,Animations.WINK);
            PutType(type,Animations.WHISTLE);
            PutType(type,Animations.SHOUT);
        }

        

        static Dictionary<string,List<UUID>> stringList = new Dictionary<string, List<UUID>>();

        public static List<UUID> MeaningUUIDs(string name)
        {     
            lock (stringList)
            {
                if (stringList.ContainsKey(name))
                {
                    return stringList[name];
                }
                return stringList[name] = new List<UUID>();
            }
        }

        private void PutType(string anims, UUID uUID)
        {
            SimAnimation a = FindOrCreateAnimation(uUID);
            a.AddType(anims);
        }

        static public bool IsSitAnim(ICollection<UUID> anims)
        {
            return false;
        }

        readonly static UUID WALK_ADJUST = new UUID("829bc85b-02fc-ec41-be2e-74cc6dd7215d");
        readonly static UUID FLY_ADJUST = new UUID("db95561f-f1b0-9f9a-7224-b12f71af126e");

        public static List<UUID> Matches(ICollection<UUID> anims, String name)
        {
            List<UUID> matches = new List<UUID>();
            foreach (UUID uuid in anims)
            {
                SimAnimation A = FindAnimation(uuid); 
                if (A==null) continue;
                if (A.Matches(name))
                {
                    matches.Add(uuid);
                }
            }
            return matches;
        }

        public static SimAnimation FindOrCreateAnimation(UUID uUID)
        {
            SimAnimation anim = FindAnimation(uUID);
            if (anim == null)
            {
                anim = new SimAnimation(uUID, null);
               // WorldObjects.RequestAsset(uUID, AssetType.Animation, true);
                uuidAnim[uUID] = anim;
            }
            return anim;
        }

        internal readonly static List<SimAnimation> SimAnimations = new List<SimAnimation>();
        internal readonly static Dictionary<string,string> nameNameMap = new Dictionary<string, string>();

    }


    public class SimAnimation : BotMentalAspect
    {

        public bool Matches(String s)
        {
            return ToString().ToLower().Contains(s.ToLower());
        }

        public override string ToString()
        {
            string s = String.Empty;
            foreach (string n in _Name)
            {
                s += " " + n;
            }
            foreach (UUID n in AnimationIDs)
            {
                s += " " + n;
            }
            if (_reader == null && (_BvhData == null || _BvhData.Length == 0)) s += " NOBVH";
            return s.TrimStart();

        }
        public List<UUID> AnimationIDs = new List<UUID>();

        public List<string> _Name = new List<string>();
        public string Name
        {
            get
            {
                if (_Name.Count == 0)
                {
                    // InventoryFolder AF = (InventoryFolder) Client.Inventory.Store[Client.AnimationFolder];

                    //InventoryItem item = InventoryManager.CreateInventoryItem(InventoryType.Animation, uUID);
                    //item.InventoryType = InventoryType.Animation;
                    //item.AssetUUID = uUID;
                    //item.AssetType = AssetType.Animation;
                    //item.Name = "Animation" + uUID.ToString();
                    //item.ParentUUID = Client.AnimationFolder; // FindFolderForType(item.AssetType);
                    //item.CreationDate = new DateTime();
                    BinBVHAnimationReader bvh = Reader;
                    if (bvh != null && !string.IsNullOrEmpty(bvh.ExpressionName))
                    {
                        string n = bvh.ExpressionName;
                        _Name.Add(n);
                        return n;
                    }
                    string tmpname = "" + AnimationIDs[0];
                    if (AnimationIDs.Count==1)
                    {
                        byte[] bs = BvhData;
                        if (bs!=null && bs.Length>0)
                        {
                            File.WriteAllBytes(tmpname+".anim",BvhData);
                        }
                        //WorldObjects.Master.GetModuleName()
                    }
                    return tmpname;
                }
                return _Name[0];
            }
            set
            {
                if (value == null) return;
                if (!_Name.Contains(value))
                    _Name.Add(value);
                if (!SimAnimationStore.nameAnim.ContainsKey(value))
                    SimAnimationStore.nameAnim[value] = this;
            }
        }

        public byte[] _BvhData;
        public byte[] BvhData
        {
            get
            {
                if (_BvhData == null)
                {
                    foreach (string n in _Name)
                    {
                        byte[] N_BvhData;
                        string usedName;
                        if (SimAnimationStore.BytesFromFile(n, out N_BvhData, out usedName))
                        {
                            _BvhData = N_BvhData;
                            Name = usedName;
                            break;
                        }

                    }

                }
                return _BvhData;
            }
            set
            {
                if (_BvhData == value)
                    return;
                _BvhData = value;
                try
                {
                    _reader = new BinBVHAnimationReader(_BvhData);
                    AnalyzeBvh();
                }
                catch (System.Exception ex)
                {
                    Console.WriteLine("" + ex);
                    //_BvhData = null;
                }
            }
        }

        public Single Length
        {
            get
            {
                if (!HasReader()) return 2;
                return Reader.Length;
            }
        }

        private bool HasReader()
        {
            return _reader != null;
        }

        public bool IsLoop
        {
            get
            {
                if (!HasReader()) return false;
                return Reader.Loop;
            }
        }

        public BinBVHAnimationReader _reader;
        public BinBVHAnimationReader Reader
        {
            get
            {
                if (_reader == null)
                {
                    try
                    {
                        byte[] tryb = BvhData;
                        if (tryb != null && tryb.Length > 0)
                            _reader = new BinBVHAnimationReader(tryb);
                        AnalyzeBvh();

                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("" + e);
//                        _BvhData = null;
                    }
                }
                return _reader;
            }
            set
            {
                if (_reader == value)
                    return;
                _reader = value;
                AnalyzeBvh();
            }
        }

       
        
        private void AnalyzeBvh()
        {
            if (!HasReader()) return;
            BinBVHAnimationReader r = _reader;
            foreach (SimAnimation animation in SimAnimationStore.SimAnimations)
            {
                if (animation == this) continue;
                if (!animation.HasReader()) continue;
                if (r == animation.Reader)
                {
                    Console.WriteLine("Found dup " + animation);
                    foreach (string name in animation._Name)
                    {
                        AddName(name + "_" + AnimationID);
                    }
                    foreach (string meaning in animation.Meanings)
                    {
                        AddType(meaning);
                    }
                    return;
                }
            }
        }
   
        private void AddName(string n)
        {
            if (!string.IsNullOrEmpty(n))
            {
                if (!_Name.Contains(n)) _Name.Add(n);
            }
        }

        public string DebugInfo()
        {
            string st = ToString();
            if (!IsIncomplete())
            {
                BinBVHAnimationReader r = Reader;
                AddName(r.ExpressionName);


                foreach (var c in r.joints)
                {
                    st += c.Name;
                    st += "\n rotationkeys: ";
                    foreach (var s in c.rotationkeys)
                    {
                        st += " " + s.key_element;
                    }
                    st += "\n positionkeys:";
                    foreach (var s in c.positionkeys)
                    {
                        st += " " + s.key_element;
                    }
                }
            }
            return st;
        }

        public SimAnimation(UUID anim, String name)
        {
            AnimationID = anim;
            Name = name;
            SimAnimationStore.SimAnimations.Add(this);
        }

        public UUID AnimationID
        {
            get
            {
                if (AnimationIDs.Count == 0) return UUID.Zero;
                return AnimationIDs[0];
            }
            set
            {
                if (value == UUID.Zero) return;
                SimAnimationStore.uuidAnim[value] = this;
                if (AnimationIDs.Contains(value)) return;
                AnimationIDs.Add(value);
            }
        }

        public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        internal bool IsIncomplete()
        {
            return !HasReader() || AnimationIDs.Count == 0 || _Name.Count == 0;
        }

        readonly List<string> Meanings = new List<string>();
        internal void AddType(string anims)
        {
            lock (Meanings)
            {
                if (!Meanings.Contains(anims))
                {
                    Meanings.Add(anims);
                    List<UUID> AMeanings = SimAnimationStore.MeaningUUIDs(anims);
                    if (!AMeanings.Contains(AnimationID))
                    {
                        AMeanings.Add(AnimationID);
                    }

                }
            }
        }
    }

#if PORTIT

    /// <summary>
    /// Asset request download handler, allows a configurable number of download slots
    /// </summary>
    public class AssetPipeline
    {
        class TaskInfo
        {
            public UUID RequestID;
            public int RequestNbr;
            public AssetType Type;

            public TaskInfo(UUID reqID, int reqNbr, AssetType type)
            {
                RequestID = reqID;
                RequestNbr = reqNbr;
                Type = type;
            }
        }

        public delegate void DownloadFinishedCallback(UUID id, bool success);
        public delegate void DownloadProgressCallback(UUID image, int recieved, int total);

        /// <summary>Fired when a texture download completes</summary>
        public event DownloadFinishedCallback OnDownloadFinished;
        /// <summary>Fired when some texture BvhData is received</summary>
        public event DownloadProgressCallback OnDownloadProgress;

        public int CurrentCount { get { return currentRequests.Count; } }
        public int QueuedCount { get { return requestQueue.Count; } }

        GridClient client;
        /// <summary>Maximum concurrent texture requests</summary>
        int maxAssetRequests;
        /// <summary>Queue for image requests that have not been sent out yet</summary>
        List<TaskInfo> requestQueue;
        /// <summary>Current texture downloads</summary>
        Dictionary<UUID, int> currentRequests;
        /// <summary>Storage for completed texture downloads</summary>
        Dictionary<UUID, ImageDownload> completedDownloads;
        AutoResetEvent[] resetEvents;
        int[] threadpoolSlots;
        Thread downloadMaster;
        bool running;
        object syncObject = new object();

        /// <summary>
        /// Default constructor
        /// </summary>
        /// <param name="client">Reference to <code>SecondLife</code> client</param>
        /// <param name="maxRequests">Maximum number of concurrent texture requests</param>
        public AssetPipeline(GridClient client, int maxRequests)
        {
            running = true;
            this.client = client;
            maxAssetRequests = maxRequests;

            requestQueue = new List<TaskInfo>();
            currentRequests = new Dictionary<UUID, int>(maxAssetRequests);
            completedDownloads = new Dictionary<UUID, ImageDownload>();
            resetEvents = new AutoResetEvent[maxAssetRequests];
            threadpoolSlots = new int[maxAssetRequests];

            // Pre-configure autoreset events/download slots
            for (int i = 0; i < maxAssetRequests; i++)
            {
                resetEvents[i] = new AutoResetEvent(false);
                threadpoolSlots[i] = -1;
            }

            client.Assets.OnImageReceived += Assets_OnImageReceived;
            client.Assets.OnImageReceiveProgress += Assets_OnImageReceiveProgress;

            // Fire up the texture download thread
            downloadMaster = new Thread(new ThreadStart(DownloadThread));
            downloadMaster.Start();
        }

        public void Shutdown()
        {
            client.Assets.OnImageReceived -= Assets_OnImageReceived;
            client.Assets.OnImageReceiveProgress -= Assets_OnImageReceiveProgress;

            requestQueue.Clear();

            for (int i = 0; i < resetEvents.Length; i++)
                if (resetEvents[i] != null)
                    resetEvents[i].Set();

            running = false;
        }

        /// <summary>
        /// Request a texture be downloaded, once downloaded OnImageRenderReady event will be fired
        /// containing texture key which can be used to retrieve texture with GetAssetToRender method
        /// </summary>
        /// <param name="textureID">Asset to request</param>
        /// <param name="type">Type of the requested texture</param>
        public void RequestAsset(UUID textureID, AssetType type0)
        {
            lock (syncObject)
            {
                if (client.Assets.Cache.HasImage(textureID))
                {
                    // Add to rendering dictionary
                    if (!completedDownloads.ContainsKey(textureID))
                    {
                        completedDownloads.Add(textureID, client.Assets.Cache.GetCachedImage(textureID));

                        // Let any subscribers know about it
                        if (OnDownloadFinished != null)
                            OnDownloadFinished(textureID, true);
                    }
                    else
                    {
                        // This image has already been served up, ignore this request
                    }
                }
                else
                {
                    // Make sure the request isn't already queued up
                    foreach (TaskInfo task in requestQueue)
                    {
                        if (task.RequestID == textureID)
                            return;
                    }

                    // Make sure we aren't already downloading the texture
                    if (!currentRequests.ContainsKey(textureID))
                        requestQueue.Add(new TaskInfo(textureID, 0, type0));
                }
            }
        }

        /// <summary>
        /// retrieve texture information from dictionary
        /// </summary>
        /// <param name="textureID">Asset ID</param>
        /// <returns>ImageDownload object</returns>
        public ImageDownload GetAssetToRender(UUID textureID)
        {
            lock (syncObject)
            {
                if (completedDownloads.ContainsKey(textureID))
                {
                    return completedDownloads[textureID];
                }
                else
                {
                    Logger.Log("Requested texture data for texture that does not exist in dictionary", Helpers.LogLevel.Warning);
                    return null;
                }
            }
        }

        /// <summary>
        /// Remove no longer necessary texture from dictionary
        /// </summary>
        /// <param name="textureID"></param>
        public bool RemoveFromPipeline(UUID textureID)
        {
            lock (syncObject)
                return completedDownloads.Remove(textureID);
        }

        public void AbortDownload(UUID textureID)
        {
            lock (syncObject)
            {
                for (int i = 0; i < requestQueue.Count; i++)
                {
                    TaskInfo task = requestQueue[i];

                    if (task.RequestID == textureID)
                    {
                        requestQueue.RemoveAt(i);
                        --i;
                    }
                }

                int current;
                if (currentRequests.TryGetValue(textureID, out current))
                {
                    currentRequests.Remove(textureID);
                    resetEvents[current].Set();

                    // FIXME: Send an abort packet
                }
            }
        }

        /// <summary>
        /// Master Download Thread, Queues up downloads in the threadpool
        /// </summary>
        private void DownloadThread()
        {
            int reqNbr;

            while (running)
            {
                if (requestQueue.Count > 0)
                {
                    reqNbr = -1;
                    // find available slot for reset event
                    for (int i = 0; i < threadpoolSlots.Length; i++)
                    {
                        if (threadpoolSlots[i] == -1)
                        {
                            threadpoolSlots[i] = 1;
                            reqNbr = i;
                            break;
                        }
                    }

                    if (reqNbr != -1)
                    {
                        TaskInfo task = null;
                        lock (syncObject)
                        {
                            if (requestQueue.Count > 0)
                            {
                                task = requestQueue[0];
                                requestQueue.RemoveAt(0);
                            }
                        }

                        if (task != null)
                        {
                            task.RequestNbr = reqNbr;

                            Logger.DebugLog(String.Format("Sending Worker thread new download request {0}", reqNbr));
                            ThreadPool.QueueUserWorkItem(AssetRequestDoWork, task);
                            continue;
                        }
                    }
                }

                // Queue was empty, let's give up some CPU time
                Thread.Sleep(500);
            }

            Logger.Log("Asset pipeline shutting down", Helpers.LogLevel.Info);
        }

        private void AssetRequestDoWork(Object threadContext)
        {
            TaskInfo ti = (TaskInfo)threadContext;

            lock (syncObject)
            {
                if (currentRequests.ContainsKey(ti.RequestID))
                {
                    threadpoolSlots[ti.RequestNbr] = -1;
                    return;
                }
                else
                {
                    currentRequests.Add(ti.RequestID, ti.RequestNbr);
                }
            }

            Logger.DebugLog(String.Format("Worker {0} Requesting {1}", ti.RequestNbr, ti.RequestID));

            resetEvents[ti.RequestNbr].Reset();
            client.Assets.RequestAsset(ti.RequestID, ti.Type,true);

            // don't release this worker slot until texture is downloaded or timeout occurs
            if (!resetEvents[ti.RequestNbr].WaitOne(45 * 1000, false))
            {
                // Timed out
                Logger.Log("Worker " + ti.RequestNbr + " Timeout waiting for Asset " + ti.RequestID + " to Download", Helpers.LogLevel.Warning);

                lock (syncObject)
                    currentRequests.Remove(ti.RequestID);

                if (OnDownloadFinished != null)
                    OnDownloadFinished(ti.RequestID, false);
            }

            // free up this download slot
            threadpoolSlots[ti.RequestNbr] = -1;
        }

        private void Assets_OnImageReceived(ImageDownload image, Asset asset)
        {
            int requestNbr;
            bool found;

            lock (syncObject)
                found = currentRequests.TryGetValue(image.ID, out requestNbr);

            if (asset != null && found)
            {
                Logger.DebugLog(String.Format("Worker {0} Downloaded texture {1}", requestNbr, image.ID));

                // Free up this slot in the ThreadPool
                lock (syncObject)
                    currentRequests.Remove(image.ID);

                resetEvents[requestNbr].Set();

                if (image.Success)
                {
                    // Add to the completed texture dictionary
                    lock (syncObject)
                        completedDownloads[image.ID] = image;
                }
                else
                {
                   // Logger.Log(String.Format("Download of texture {0} failed. NotFound={1}", image.ID, image.NotFound),
                     //   Helpers.LogLevel.Warning);
                }

                // Let any subscribers know about it
                if (OnDownloadFinished != null)
                    OnDownloadFinished(image.ID, image.Success);
            }
        }

        private void Assets_OnImageReceiveProgress(UUID image, int lastPacket, int recieved, int total)
        {
            if (OnDownloadProgress != null && currentRequests.ContainsKey(image))
                OnDownloadProgress(image, recieved, total);
        }
    }
#endif
}