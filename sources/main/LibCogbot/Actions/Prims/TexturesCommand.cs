using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Assets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Inventory
{
    public class TexturesCommand : Command, RegionMasterCommand, AsynchronousCommand
    {
        private Dictionary<UUID, UUID> alreadyRequested = new Dictionary<UUID, UUID>();
        private bool enabled = false;
        private bool registered = false;

        public TexturesCommand(BotClient testClient)
        {
            enabled = testClient.ClientManager.GetTextures;

            Name = "textures";
        }

        public override void MakeInfo()
        {
            Description = "Turns automatic texture downloading on or off. Usage: textures [on/off]";
            Category = CommandCategory.Objects;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage(); // " textures [on/off]";

            if (args[0].ToLower() == "on")
            {
                if (!registered)
                {
                    registered = true;
                    Client.Objects.ObjectUpdate += Objects_OnNewPrim;
                    Client.Objects.AvatarUpdate += Objects_OnNewAvatar;
                }
                Client.ClientManager.GetTextures = enabled = true;
                return Success("Texture downloading is on");
            }
            else if (args[0].ToLower() == "off")
            {
                Client.ClientManager.GetTextures = enabled = false;
                return Success("Texture downloading is off");
            }
            else
            {
                return ShowUsage(); // " textures [on/off]";
            }
        }

        private void Objects_OnNewAvatar(object sender, AvatarUpdateEventArgs e)
        {
            Avatar avatar = e.Avatar;
            if (enabled)
            {
                // Search this avatar for textures
                for (int i = 0; i < avatar.Textures.FaceTextures.Length; i++)
                {
                    Primitive.TextureEntryFace face = avatar.Textures.FaceTextures[i];

                    if (face != null)
                    {
                        if (!alreadyRequested.ContainsKey(face.TextureID))
                        {
                            alreadyRequested[face.TextureID] = face.TextureID;

                            // Determine if this is a baked outfit texture or a normal texture
                            ImageType type = ImageType.Normal;
                            AvatarTextureIndex index = (AvatarTextureIndex) i;
                            switch (index)
                            {
                                case AvatarTextureIndex.EyesBaked:
                                case AvatarTextureIndex.HeadBaked:
                                case AvatarTextureIndex.LowerBaked:
                                case AvatarTextureIndex.SkirtBaked:
                                case AvatarTextureIndex.UpperBaked:
                                    type = ImageType.Baked;
                                    break;
                            }

                            Client.Assets.RequestImage(face.TextureID, type, Assets_OnImageReceived);
                        }
                    }
                }
            }
        }

        private void Objects_OnNewPrim(object sender, PrimEventArgs e)
        {
            Primitive prim = e.Prim;

            if (enabled)
            {
                // Search this prim for textures
                for (int i = 0; i < prim.Textures.FaceTextures.Length; i++)
                {
                    Primitive.TextureEntryFace face = prim.Textures.FaceTextures[i];

                    if (face != null)
                    {
                        if (!alreadyRequested.ContainsKey(face.TextureID))
                        {
                            alreadyRequested[face.TextureID] = face.TextureID;
                            Client.Assets.RequestImage(face.TextureID, ImageType.Normal, Assets_OnImageReceived);
                        }
                    }
                }
            }
        }

        private void Assets_OnImageReceived(TextureRequestState state, AssetTexture asset)
        {
            if (state == TextureRequestState.Finished && enabled && alreadyRequested.ContainsKey(asset.AssetID))
            {
                if (state == TextureRequestState.Finished)
                    Logger.DebugLog(String.Format("Finished downloading texture {0} ({1} bytes)", asset.AssetID,
                                                  asset.AssetData.Length));
                else
                    Logger.Log("Failed to download texture " + asset.AssetID + ": " + state, Helpers.LogLevel.Warning);
            }
        }
    }
}