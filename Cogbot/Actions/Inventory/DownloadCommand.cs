using System;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.Actions
{
    public class DownloadCommand : Command, GridMasterCommand
    {

        public DownloadCommand(BotClient testClient)
        {
            Name = "download";
            Description = "Downloads the specified asset. Usage: download [uuid] [assetType]";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2) return ShowUsage();
            UUID AssetID;
            int argsUsed;
            if (!UUIDTryParse(args, 0, out AssetID, out argsUsed)) return Failure("Cant determine asset ID");
            object value;                
            if (!TryEnumParse(typeof(AssetType), args, argsUsed, out argsUsed, out value))                      
                return Failure("Cant determine asset type");
            AssetType assetType = (AssetType) value;
            AutoResetEvent DownloadHandle = new AutoResetEvent(false);
            Client.Assets.RequestAsset(AssetID, assetType, true, delegate(AssetDownload transfer, Asset asset)
            {
                WriteLine("Status: " + transfer.Status + "  Asset: " + asset);

                if (transfer.Success) try
                    {
                        File.WriteAllBytes(String.Format("{0}.{1}", AssetID,
                            assetType.ToString().ToLower()), asset.AssetData);
                        try
                        {
                            asset.Decode();
                            WriteLine("Asset decoded as " + asset.AssetType);
                        }
                        catch (Exception ex)
                        {
                            WriteLine("Asset not decoded: " + ex);
                        }
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
    }
}
