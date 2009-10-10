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
    public class DownloadCommand : Command
    {

        public DownloadCommand(BotClient testClient)
        {
            Name = "download";
            Description = "Downloads the specified asset. Usage: download [uuid] [assetType]";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2) return Failure(Usage);
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
            UUID AssetID;
            string botcmd = String.Join(" ", args, 0, args.Length - 1).Trim();
            UUIDTryParse(botcmd, out AssetID);
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
