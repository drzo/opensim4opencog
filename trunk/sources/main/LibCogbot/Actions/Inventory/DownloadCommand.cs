using System;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Assets;

using MushDLR223.ScriptEngines;
using OpenMetaverse.Imaging;

namespace cogbot.Actions.SimExport
{
    public class DownloadCommand : Command, GridMasterCommand
    {

        public DownloadCommand(BotClient testClient)
        {
            Name = "download";
            Description = "Downloads the specified asset. Usage: download [uuid] [assetType] [filename]";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2) return ShowUsage();
            AssetType assetType;

            FieldInfo fi = typeof(AssetType).GetField(args[1]);
            if (fi == null)
            {
                int typeInt;
                if (int.TryParse(args[1], out typeInt))
                {
                    assetType = (AssetType)typeInt;
                }
                else
                {
                    WriteLine(typeof(AssetType).GetFields().ToString());
                    return Failure("Unknown asset type");
                }
            }
            else
            {
                assetType = (AssetType)fi.GetValue(null);
            }
            //var v = WorldObjects.uuidTypeObject;
            UUID AssetID = UUID.Zero;
            int argsUsed;
            UUIDTryParse(args, 0, out AssetID, out argsUsed);
            AutoResetEvent DownloadHandle = new AutoResetEvent(false);
            Client.Assets.RequestAsset(AssetID, assetType, true, delegate(AssetDownload transfer, Asset asset)
            {
                WriteLine("Status: " + transfer.Status + "  Asset: " + asset);

                if (transfer.Success) try
                    {
                        string filename = String.Format("{0}.{1}", AssetID, assetType.ToString().ToLower());
                        string decodeType = "none";
                        if (args.Length > 2)
                        {
                            filename = args[2];
                        }
                        if (args.Length > 3)
                        {
                            decodeType = args[3];
                        }
                        DecodeFile(filename, decodeType, asset);
                    }
                    catch (Exception ex)
                    {
                        Logger.Log(ex.Message, Helpers.LogLevel.Error, ex);
                    }
                DownloadHandle.Set();
            });
            DownloadHandle.WaitOne(30000);
            return Success("Done RequestAsset " + AssetID);
        }

        private void DecodeFile(string filename, string decodeType, Asset asset)
        {
            try
            {
                asset.Decode();
                WriteLine("Asset decoded as " + asset.AssetType);
            }
            catch (Exception ex)
            {
                WriteLine("Asset not decoded: " + ex);
            }
            MemberInfo[] getTypeGetMember = GetType().GetMember("Decode_" + decodeType);
            MethodInfo mi = null;
            if (getTypeGetMember != null && getTypeGetMember.Length > 0)
            {
                mi = getTypeGetMember[0] as MethodInfo;
            }
            if (mi != null)
            {
                mi.Invoke(this, new object[] { filename, decodeType, asset });
            }
            else
            {
                Decode_none(filename, decodeType, asset);
            }
        }
        public void Decode_none(string filename, string decodeType, Asset asset)
        {
            var data = asset.AssetData;
            File.WriteAllBytes(filename, data);
        }
        public void Decode_jp2k(string filename, string decodeType, Asset asset)
        {
            var img = (AssetTexture)asset;
            OpenMetaverse.Imaging.ManagedImage imgImage = img.Image;
            if (imgImage == null)
            {
                OpenJPEG.DecodeToImage(asset.AssetData, out imgImage);
            }
            if (imgImage != null)
            {
                if (filename.ToLower().EndsWith("tga"))
                {
                    File.WriteAllBytes(filename, imgImage.ExportTGA());
                }
                else
                {
                    File.WriteAllBytes(filename, imgImage.ExportRaw());
                }

            }
        }
    }
}
