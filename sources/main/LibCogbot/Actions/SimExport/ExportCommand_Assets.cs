using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using OpenMetaverse.Assets;

using MushDLR223.ScriptEngines;
using PathSystem3D.Mesher;

namespace cogbot.Actions.SimExport
{

    public partial class ExportCommand : Command, RegionMasterCommand
    {

        public readonly HashSet<UUID> ToDownloadAssets = new HashSet<UUID>();
        public readonly HashSet<UUID> CompletedAssets = new HashSet<UUID>();
        public readonly Dictionary<UUID, AssetType> AllRelatedAssets = new Dictionary<UUID, AssetType>();
        public readonly HashSet<UUID> PrimDepsAssets = new HashSet<UUID>();

        private void StartAssetDownload(List<UUID> xferStarted, UUID assetID, AssetType assetType)
        {
            if (xferStarted.Contains(assetID)) return;
            xferStarted.Add(assetID);
            // string filename = assetID + ".asset";
            // ulong xferID = Client.Assets.RequestAssetXfer(filename, false, true, assetID, assetType, false);
            Client.Assets.RequestAsset(assetID, assetType, true, Assets_OnReceived);
            if (assetType == AssetType.Texture)
            {
                // Client.Assets.RequestImage(assetID, ImageType.Normal, Assets_OnImageReceived);
                WorldSystem.TextureBytesForUUID(assetID);
            }
        }

        private void SaveRelatedAssets(string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string exportFile = pathStem + ".deps";
            if (Incremental || showsMissingOnly) lock (fileWriterLock) if (File.Exists(exportFile)) return;
            needFiles++;
            if (showsMissingOnly)
            {
                Failure("NEED DEPS for " + named(exportPrim));
                return;
            }
            if (PrimDepsAssets.Count == 0)
            {
                lock (fileWriterLock) File.WriteAllText(exportFile, "");
                return;
            }
            string content = "";
            foreach (UUID assetID in PrimDepsAssets)
            {
                content += assetTypeOf(assetID) + "," + assetID + "\n";
            }
            lock (fileWriterLock) File.WriteAllText(exportFile, content);
        }



        private AssetType assetTypeOf(UUID uuid)
        {
            AssetType assetType;
            AllRelatedAssets.TryGetValue(uuid, out assetType);
            return assetType;
        }

        public CmdResult ExportRelatedAssets()
        {
            // Create a list of all of the textures to download
            DownloadAssets(ToDownloadAssets);

            return Success("XML exported, downloading " + ToDownloadAssets.Count + " assets for " + successfullyExportedPrims.Count);
        }

        void DownloadAssets(IEnumerable<UUID> downloadList)
        {
            List<ImageRequest> textureRequests = new List<ImageRequest>();
            Dictionary<UUID, AssetType> otherRequests = new Dictionary<UUID, AssetType>();


            // Create a request list from all of the images
            lock (downloadList)
            {
                foreach (var asset in downloadList)
                {
                    if (assetTypeOf(asset) == AssetType.Texture)
                    {
                        textureRequests.Add(new ImageRequest(asset, ImageType.Normal, 1013000.0f, 0));
                    }
                    else
                    {
                        otherRequests.Add(asset, assetTypeOf(asset));
                    }
                }
            }

            // Download all of the textures in the export list
            foreach (ImageRequest request in textureRequests)
            {
                SlowlyDo(() => Client.Assets.RequestImage(request.ImageID, request.Type, Assets_OnImageReceived));
                //SlowlyDo(() => Client.Assets.RequestAsset(request.ImageID, AssetType.Texture, true, Assets_OnReceived));
            }

            foreach (KeyValuePair<UUID, AssetType> asset in otherRequests)
            {
                if (asset.Value != AssetType.Texture)
                {
                    SlowlyDo(() => Client.Assets.RequestAsset(asset.Key, asset.Value, true, Assets_OnReceived));
                }
            }
        }

        void AddRelatedTextures(SimObject simObject)
        {
            var exportPrim = simObject.Prim;
            lock (ToDownloadAssets)
            {
                if (exportPrim.Textures != null)
                {
                    Primitive.TextureEntryFace deftexture = exportPrim.Textures.DefaultTexture;
                    if (deftexture != null)
                    {
                        AddRelated(deftexture.TextureID, AssetType.Texture);
                    }
                    for (int j = 0; j < exportPrim.Textures.FaceTextures.Length; j++)
                    {
                        if (exportPrim.Textures.FaceTextures[j] != null)
                        {
                            AddRelated(exportPrim.Textures.FaceTextures[j].TextureID, AssetType.Texture);
                        }
                    }
                }

                if (exportPrim.Sculpt != null)
                {
                    AddRelated(exportPrim.Sculpt.SculptTexture, AssetType.Texture);
                }
                AddRelatedTexturesFromProps(simObject);
            }
        }

        void AddRelatedTexturesFromProps(SimObject simObject)
        {
            UUID[] textureIDs = simObject.Properties.TextureIDs;
            if (textureIDs == null || textureIDs.Length == 0) return;
            foreach (var c in textureIDs)
            {
                AddRelated(c, AssetType.Texture);
            }
        }

        public void AddRelated(UUID assetID, AssetType assetType)
        {
            if (assetID == null || assetID == UUID.Zero || assetID == Primitive.TextureEntry.WHITE_TEXTURE) return;
            FindOrCreateAsset(assetID, assetType);
            //WorldObjects.GridMaster.EnqueueRequestAsset(assetID, assetType, true);
            lock (PrimDepsAssets)
                if (!PrimDepsAssets.Contains(assetID))
                {
                    PrimDepsAssets.Add(assetID);
                }
            lock (AllRelatedAssets)
                if (!AllRelatedAssets.ContainsKey(assetID))
                {
                    AllRelatedAssets.Add(assetID, assetType);
                }
            lock (CompletedAssets)
                if (CompletedAssets.Contains(assetID))
                {
                    return;
                }
            lock (ToDownloadAssets)
                if (!ToDownloadAssets.Contains(assetID))
                {
                    ToDownloadAssets.Add(assetID);
                }
        }

        private void FindOrCreateAsset(UUID uuid, AssetType type)
        {
            if (!CogbotHelpers.IsNullOrZero(uuid)) return;
            if (type != AssetType.Object) SimAssetStore.FindOrCreateAsset(uuid, type);
        }

        /*

        private bool RequestObjectProperties(IList<SimObject> objects, int msPerRequest, Simulator sim)
        {
            // Create an array of the local IDs of all the prims we are requesting properties for
            uint[] localids = new uint[objects.Count];

            lock (PrimsWaiting)
            {
                PrimsWaiting.Clear();

                for (int i = 0; i < objects.Count; ++i)
                {
                    localids[i] = objects[i].LocalID;
                    PrimsWaiting.Add(objects[i].ID, objects[i].Prim);
                }
            }

            Client.Objects.SelectObjects(sim, localids);

            return AllPropertiesReceived.WaitOne(2000 + msPerRequest * objects.Count, false);
        }
        */
        private void Assets_OnImageReceived(TextureRequestState state, AssetTexture asset)
        {

            lock (ToDownloadAssets) if (state == TextureRequestState.Finished)
                {
                    AssetComplete(asset.AssetID);

                    if (state == TextureRequestState.Finished)
                    {
                        string sfile = Path.GetFileName(SimAsset.CFileName(asset.AssetID, asset.AssetType));
                        try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + sfile, asset.AssetData); }
                        catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                        return;

                        if (asset.Decode())
                        {
                            try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + asset.AssetID + ".tga", asset.Image.ExportTGA()); }
                            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                        }
                        else
                        {
                            try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + sfile, asset.AssetData); }
                            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                            return;
                        }

                        Logger.Log("Finished downloading image " + asset.AssetID, Helpers.LogLevel.Info, Client);
                    }
                    else
                    {
                        Logger.Log("Failed to download image " + asset.AssetID + ":" + state, Helpers.LogLevel.Warning, Client);
                    }
                }
        }

        private void Asset_Xfer(object sender, XferReceivedEventArgs e)
        {
            var assetID = e.Xfer.AssetID;
            var assetType = e.Xfer.AssetType;
            if (assetType==AssetType.Unknown)
            {
                // probly an taskinv so ingnore
                return;
            }
            lock (ToDownloadAssets) if (e.Xfer.Success)
                {
                    AssetComplete(assetID);
                    string sfile = SimAsset.CFileName(assetID, assetType);
                    try
                    {
                        lock (fileWriterLock)
                            File.WriteAllBytes(assetDumpDir + Path.GetFileName(sfile), e.Xfer.AssetData);
                    }
                    catch (Exception ex)
                    {
                        Logger.Log(ex.Message, Helpers.LogLevel.Error);
                    }
                }
        }

        public void Assets_OnReceived(AssetDownload transfer, Asset asset)
        {
            if (transfer.Success)
                {
                    AssetComplete(asset.AssetID);
                    string sfile = SimAsset.CFileName(asset.AssetID, asset.AssetType);
                    try
                    {
                        var data = asset.AssetData;
                        if (data != null && data.Length > 1)
                        {
                            lock (fileWriterLock)
                                File.WriteAllBytes(assetDumpDir + Path.GetFileName(sfile), asset.AssetData);
                            return;
                        }
                    }
                    catch (Exception ex)
                    {
                        Logger.Log(ex.Message, Helpers.LogLevel.Error);
                    }
                }
        }

        public void AssetComplete(UUID assetID)
        {
            if (CogbotHelpers.IsNullOrZero(assetID)) return;
            lock (ToDownloadAssets) ToDownloadAssets.Remove(assetID);
            lock (CompletedAssets) CompletedAssets.Add(assetID);
        }
    }
}
