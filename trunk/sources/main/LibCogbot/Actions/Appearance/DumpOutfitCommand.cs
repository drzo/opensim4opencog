using System;
using System.Text;
using System.IO;
using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Imaging;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class DumpOutfitCommand : Command, BotPersonalCommand, FFIComplete, AsynchronousCommand
    {

        public DumpOutfitCommand(BotClient testClient)
        {
            Name = "dumpoutfit";
            TheBotClient = testClient;
        }

        override public void MakeInfo()
        {
            Description = "Dumps all of the textures from an avatars outfit to the hard drive.";
            Category = CommandCategory.Inventory;
            Details = AddUsage(Name + " [agent-spec]", "may use $self");
            Parameters = CreateParams("target", typeof (AgentSpec), "the agents you wish to see " + Name);
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage(); // " dumpoutfit [avatar-uuid]";
            bool writeInfo = !args.IsFFI;
            //UUID target = UUID.Zero;

            //if (!UUIDTryParse(args, 0 , out target))
            //    return ShowUsage();// " dumpoutfit [avatar-uuid]";

            //lock (Client.Network.Simulators)
            {
                //for (int i = 0; i < Client.Network.Simulators.Count; i++)
                {


                    int argsUsed;
                    List<SimObject> PS; args.TryGetValue("target", out PS);
                    if (IsEmpty(PS)) return Failure("Cannot find objects from " + args.str);
                    List<UUID> OutfitAssets = new List<UUID>();
                    foreach (var O in PS)
                    {
                        Primitive targetAv = O.Prim;
                        StringBuilder output = new StringBuilder("Downloading ");
                        if (targetAv.Textures==null)
                        {
                            Failure("no textures " + targetAv);
                            continue;
                        }
                        for (int j = 0; j < targetAv.Textures.FaceTextures.Length; j++)
                        {
                            Primitive.TextureEntryFace face = targetAv.Textures.FaceTextures[j];

                            if (face != null)
                            {
                                ImageType type = ImageType.Normal;

                                switch ((AvatarTextureIndex) j)
                                {
                                    case AvatarTextureIndex.HeadBaked:
                                    case AvatarTextureIndex.EyesBaked:
                                    case AvatarTextureIndex.UpperBaked:
                                    case AvatarTextureIndex.LowerBaked:
                                    case AvatarTextureIndex.SkirtBaked:
                                        type = ImageType.Baked;
                                        break;
                                }

                                OutfitAssets.Add(face.TextureID);
                                Client.Assets.RequestImage(face.TextureID, type,
                                                           delegate(TextureRequestState state, AssetTexture assettexture)
                                                               {
                                                                   lock (OutfitAssets)
                                                                   {
                                                                       if (OutfitAssets.Contains(assettexture.AssetID))
                                                                       {
                                                                           if (state == TextureRequestState.Finished)
                                                                           {

                                                                               try
                                                                               {
                                                                                   string newVariable = assettexture.AssetID + ".jp2";
                                                                                   File.WriteAllBytes(newVariable, assettexture.AssetData);
                                                                                   AddSuccess("Wrote JPEG2000 image " + newVariable);

                                                                                   ManagedImage imgData;
                                                                                   if (OpenJPEG.DecodeToImage(assettexture.AssetData, out imgData))
                                                                                   {
                                                                                       byte[] tgaFile = imgData.ExportTGA();
                                                                                       File.WriteAllBytes(assettexture.AssetID + ".tga", tgaFile);
                                                                                       AddSuccess("Wrote TGA image " + assettexture.AssetID + ".tga");
                                                                                   } else
                                                                                   {
                                                                                       Failure("Failed decode of " + newVariable);
                                                                                   }
                                                                               }
                                                                               catch (Exception e)
                                                                               {
                                                                                   Failure(e.ToString());
                                                                               }
                                                                           }
                                                                           else
                                                                           {
                                                                               Failure("Failed to download image " + assettexture.AssetID);
                                                                           }

                                                                           OutfitAssets.Remove(assettexture.AssetID);
                                                                       }
                                                                   }
                                                               });
                                output.Append(((AvatarTextureIndex) j).ToString());
                                output.Append(" ");
                            }
                        }

                        AddSuccess(output.ToString());
                    }
                }
            }
            return SuccessOrFailure();
        }
    }
}
