using System;
using System.IO;
using System.Threading;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Assets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.SimExport
{
    public class DownloadTextureCommand : Command, RegionMasterCommand, AsynchronousCommand
    {
        private UUID TextureID = UUID.Zero;
        private AutoResetEvent DownloadHandle = new AutoResetEvent(false);
        private AssetTexture Asset;
        private TextureRequestState resultState;

        public DownloadTextureCommand(BotClient testClient)
        {
            Name = "downloadtexture";
        }

        public override void MakeInfo()
        {
            Description = "Downloads the specified texture. " +
                          "Usage: downloadtexture [texture-uuid] [discardlevel]";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage(); // " downloadtexture [texture-uuid] [discardlevel]";

            TextureID = UUID.Zero;
            DownloadHandle.Reset();
            Asset = null;

            if (UUID.TryParse(args[0], out TextureID))
            {
                int discardLevel = 0;

                if (args.Length > 1)
                {
                    if (!Int32.TryParse(args[1], out discardLevel))
                        return ShowUsage(); // " downloadtexture [texture-uuid] [discardlevel]";
                }

                Client.Assets.RequestImage(TextureID, ImageType.Normal, Assets_OnImageReceived);

                if (DownloadHandle.WaitOne(120*1000, false))
                {
                    if (resultState == TextureRequestState.Finished)
                    {
                        if (Asset != null && SimAsset.Decode(Asset))
                        {
                            try
                            {
                                File.WriteAllBytes(Asset.AssetID + ".jp2", Asset.AssetData);
                            }
                            catch (Exception ex)
                            {
                                Logger.Log(ex.Message, Helpers.LogLevel.Error, Client, ex);
                            }

                            return
                                Success(string.Format("Saved {0}.jp2 ({1}x{2})", Asset.AssetID, Asset.Image.Width,
                                                      Asset.Image.Height));
                        }
                        else
                        {
                            return Failure("failed to decode texture " + TextureID.ToString());
                        }
                    }
                    else if (resultState == TextureRequestState.NotFound)
                    {
                        return Failure("Simulator reported texture not found: " + TextureID.ToString());
                    }
                    else
                    {
                        return Failure("Download failed for texture " + TextureID + " " + resultState);
                    }
                }
                else
                {
                    return Failure("Timed out waiting for texture download");
                }
            }
            else
            {
                return ShowUsage(); // " downloadtexture [texture-uuid]";
            }
        }

        private void Assets_OnImageReceived(TextureRequestState state, AssetTexture asset)
        {
            resultState = state;
            Asset = asset;

            DownloadHandle.Set();
        }
    }
}