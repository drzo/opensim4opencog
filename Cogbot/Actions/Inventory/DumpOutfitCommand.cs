using System;
using System.Text;
using System.IO;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Imaging;

namespace cogbot.Actions.Appearance
{
    public class DumpOutfitCommand : Command, BotPersonalCommand
    {

        public DumpOutfitCommand(BotClient testClient)
        {
            Name = "dumpoutfit";
            Description = "Dumps all of the textures from an avatars outfit to the hard drive. Usage: dumpoutfit [avatar-uuid]";
            Category = CommandCategory.Inventory;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage(); // " dumpoutfit [avatar-uuid]";

            //UUID target;

            //if (!UUIDTryParse(args, 0 , out target))
            //    return ShowUsage();// " dumpoutfit [avatar-uuid]";

            //lock (Client.Network.Simulators)
            {
                //for (int i = 0; i < Client.Network.Simulators.Count; i++)
                {


                    int argsUsed;
                    List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
                    if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
                    List<UUID> OutfitAssets = new List<UUID>();
                    foreach (var O in PS)
                    {
                        Primitive targetAv = O.Prim;
                        StringBuilder output = new StringBuilder("Downloading ");                        

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
                                                                                   Success("Wrote JPEG2000 image " + newVariable);

                                                                                   ManagedImage imgData;
                                                                                   if (OpenJPEG.DecodeToImage(assettexture.AssetData, out imgData))
                                                                                   {
                                                                                       byte[] tgaFile = imgData.ExportTGA();
                                                                                       File.WriteAllBytes(assettexture.AssetID + ".tga", tgaFile);
                                                                                       Success("Wrote TGA image " + assettexture.AssetID + ".tga");
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

                        Success(output.ToString());
                    }
                }
            }
            return SuccessOrFailure();
        }
    }
}
