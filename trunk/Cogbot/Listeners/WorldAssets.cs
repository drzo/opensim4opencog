using System;
using System.Drawing;
using System.IO;
using System.Net.Mime;
using System.Threading;
using System.Collections.Generic;
using System.Windows.Forms;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.Imaging;
using Radegast;
using Object=System.Object;
using Settings=OpenMetaverse.Settings;

namespace cogbot.Listeners
{
    public partial class WorldObjects
    {
        internal readonly AssetManager RegionMasterTexturePipeline;
        private static readonly List<UUID> TexturesSkipped = new List<UUID>();

        /// <summary>
        /// Assets that WorldObjects requested
        /// </summary>
        private static readonly Dictionary<UUID, AssetType> AssetRequestType = new Dictionary<UUID, AssetType>();

        public Asset GetAsset(UUID id)
        {
            lock (uuidTypeObject)
            {
                Object assetObject;
                uuidTypeObject.TryGetValue(id, out assetObject);
                if (assetObject is Asset)
                {
                    return (Asset)assetObject;
                }
            }
            //RequestAsset(id, AssetType.Object, true);
            //IAssetProvider assetProvider = null;// ClientManager.simulator.Assets;
            //if (assetProvider == null)
            //{
            //    Debug("Asset Provider still off line for " + id);
            //    return null;
            //}
            Asset asset;
            //assetProvider.TryGetAsset(id, out asset);
            //if (asset != null)
            //{
            //    RegisterUUID(id, asset);
            //}
            //return asset;
            return null;
        }

        public void DeclareTexture(UUID tid)
        {
            SimAssetStore.FindOrCreateAsset(tid, AssetType.Texture);
        }

        public byte[] TextureBytesForUUID(UUID uUID)
        {
            //imlicitly DeclareTexture(uUID);
            SimAsset assettt = SimAssetStore.FindOrCreateAsset(uUID, AssetType.Texture);
            var AssetData = assettt.AssetData;
            if (AssetData != null)
            {
                assettt.IsTraced = false;
                return AssetData;
            }
            assettt.IsTraced = true;
            ImageDownload ID = null;
            //lock (uuidTypeObject)
            {
                object iObject;
                if (uuidTypeObject.TryGetValue(uUID, out iObject))
                {
                    if (iObject is ImageDownload)
                    {
                        ID = (ImageDownload)iObject;
                    }
                }
            }
            if (ID == null)
            {
                ID = StartTextureDownload(uUID);
                if (ID == null)
                {
                    var giveUpTick = DateTime.Now;//.AddMinutes(1);// +1 * 60000;
                    while (ID == null)
                    {
                        ID = StartTextureDownload(uUID);
                        if (ID == null)
                        {
                            lock (TexturesSkipped) if (TexturesSkipped.Contains(uUID)) return null;
                            if (DateTime.Now > giveUpTick)
                            {
                                lock (TexturesSkipped) TexturesSkipped.Add(uUID);
                                //if (Settings.LOG_LEVEL == Helpers.LogLevel.Warning)
                                Debug("-- ---- ---GIVEUP SculptMesh " + uUID);
                                return null;
                            }
                            //Thread.Sleep(5000);
                            DoEvents();
                            return null;
                        }
                    }
                }
                lock (uuidTypeObject)
                {
                    uuidTypeObject[uUID] = ID;
                }
            }
            if (Settings.LOG_LEVEL == Helpers.LogLevel.Debug)
                Debug("-|-|- SUCCEED SculptMesh " + uUID);

            return ID.AssetData;
        }


        //public override void Assets_OnXferReceived(XferDownload xfer)
        //{
        //    //AssetRequests.
        //    //RegisterUUIDMaybe(xfer.ID, xfer);
        //}

        public SimAsset EnqueueRequestAsset(UUID id, AssetType assetType, bool priority)
        {
            if (id == UUID.Zero) return null;
            SimAsset sa = SimAssetStore.FindOrCreateAsset(id, assetType);
            if (!sa.NeedsRequest) return sa;
            if (assetType == AssetType.Texture)
            {
                StartTextureDownload(id);
                return sa;
            }
            sa.NeedsRequest = false;
            if (assetType == AssetType.SoundWAV) return sa;
            //if (assetType == AssetType.Sound) return sa;
            lock (AssetRequestType)
            {
                if (AssetRequestType.ContainsKey(id)) return sa;
                AssetRequestType[id] = assetType;
                SlowConnectedQueue.Enqueue(
                    () => RegionMasterTexturePipeline.RequestAsset(id, assetType, priority, Assets_OnAssetReceived));
            }

            return sa;
        }

        public override void Assets_OnAssetReceived(AssetDownload transfer, Asset asset)
        {
            bool xfrFailed = !transfer.Success;
            bool decodeFailed = false;

            if (transfer.Success)
            {
                try
                {

                    try
                    {

                        bool tf = asset.Decode();
                        decodeFailed = !tf;
                        if (decodeFailed) Debug("Asset decoded " + tf + " as " + asset.AssetType);
                    }
                    catch (Exception ex)
                    {
                        decodeFailed = true;
                        Debug("Asset not decoded: " + ex);
                    }
                }
                catch (Exception ex)
                {
                    Logger.Log(ex.Message, Helpers.LogLevel.Error, ex);
                }
            }

            if (asset == null)
            {
                lock (AssetRequestType)
                    if (AssetRequestType.ContainsKey(transfer.AssetID))
                    {
                        Debug("Failed transfer for " + AssetRequestType[transfer.AssetID] +
                              " " + transfer.ID + " ");
                    }
                    else
                    {
                        Debug("Unknown transfer Failed for " + transfer.AssetType + " " + transfer.ID + " ");
                    }
                return;
            }
            lock (AssetRequestType)
            {
                EnsureCached(asset, asset.AssetID);
                if (AssetRequestType.ContainsKey(asset.AssetID))
                {
                    AssetType assetRequestType = AssetRequestType[transfer.AssetID];
                    if (assetRequestType == asset.AssetType)
                    {
                       if (false) Debug("Transfer succeeded for " + assetRequestType + " " + transfer.AssetID + " ");
                    }
                    else
                        Debug("Transfer succeeded weirdly as " + asset.AssetType + " for " + assetRequestType +
                              " " + transfer.AssetID + " ");
                }
                else
                {
                    Debug("Unknown transfer succeeded for " + asset.AssetType + " " + transfer.ID + " ");
                }
                OnAssetDownloaded(asset.AssetID, asset);
            }
        }

        public void Init(UUID id)
        {
            if (id == UUID.Zero) return;
            AssetType assetType = AssetType.Texture;
            SimAsset sa = SimAssetStore.FindOrCreateAsset(id, assetType);
            if (!sa.NeedsRequest) return;
 
            {
                StartTextureDownload(id);
            }
            sa.NeedsRequest = false;
            lock (AssetRequestType)
            {
                if (AssetRequestType.ContainsKey(id)) return;
                AssetRequestType[id] = AssetType.Texture;
            }

            // Callbacks
            client.Assets.RequestImage(id, ImageType.Normal, 101300.0f, 0, 0, delegate(TextureRequestState state, AssetTexture assetTexture)
            {
                if (state == TextureRequestState.Finished)
                {
                    Assets_OnImageReceived(assetTexture);
                    sa.SetAsset(assetTexture);
                    return;
                }
                else if (state == TextureRequestState.Progress)
                {
                    // DisplayPartialImage(assetTexture);
                }
                else if (state == TextureRequestState.Timeout)
                {
                    AssetRequestType.Remove(id);
                }
            }, false);
        }

        private void Assets_OnImageReceived(AssetTexture assetTexture)
        {
            try
            {
                byte[] jpegdata;
                ManagedImage imgManaged;
                Image image;
                SimAsset sa = SimAssetStore.FindOrCreateAsset(assetTexture.AssetID, assetTexture.AssetType);
                if (!OpenJPEG.DecodeToImage(assetTexture.AssetData, out imgManaged, out image))
                {
                    throw new Exception("decoding failure");
                }
                jpegdata = assetTexture.AssetData;
                sa._TypeData = jpegdata;
                uuidTypeObject[assetTexture.AssetID] = jpegdata;
                assetTexture.Decode();
            }
            catch (Exception excp)
            {
                System.Console.WriteLine("Error decoding image: " + excp.Message);
            }
        }

        /*
        On-Image-Received
             image: "{OpenMetaverse.ImageDownload,PacketCount=33,Codec=J2C,NotFound=False,Simulator=OpenSim Test (71.197.210.170:9000),PacketsSeen=System.Collections.Generic.SortedList`2[System.UInt16,System.UInt16],ImageType=Normal,DiscardLevel=-1,Priority=1013000,ID=728dd7fa-a688-432d-a4f7-4263b1f97395,Size=33345,AssetData=System.Byte[],Transferred=33345,Success=True,AssetType=Texture}"
             asset: "{OpenMetaverse.AssetTexture,Image=,LayerInfo=,Components=0,AssetData=System.Byte[],Temporary=False}"
         */

        public override void Assets_OnImageReceived(ImageDownload image, AssetTexture asset)
        {
            OnAssetDownloaded(asset.AssetID, asset);
            RegisterUUIDMaybe(image.ID, image);
        }

        static int TextureRequested = 0;
        static int TextureSuccess = 0;

        public ImageDownload StartTextureDownload(UUID id)
        {
            AssetType type = AssetType.Texture;
            SimAsset simAsset = SimAssetStore.FindOrCreateAsset(id, type);
            AssetCache Cache = 
                (RegionMasterTexturePipeline == null ? null : RegionMasterTexturePipeline.Cache);
            if (Cache != null && Cache.HasAsset(id, type))
            {
                ImageDownload dl = Cache.GetCachedImage(id);
                if (dl != null)
                {
                    simAsset.SetAsset(dl);
                }
                return dl;
            }
            lock (TexturesSkipped) if (TexturesSkipped.Contains(id)) return null;
            lock (AssetRequestType)
            {
                if (AssetRequestType.ContainsKey(id)) return null;
                AssetRequestType[id] = AssetType.Texture;
            }
            TextureRequested++;            
            SlowConnectedQueue.Enqueue(()=>client.Assets.RequestImage(id, ImageType.Normal, RegionMasterTexturePipeline_OnDownloadFinished));
            return null;
        }

        private void RegionMasterTexturePipeline_OnDownloadFinished(TextureRequestState state, AssetTexture asset)
        {
            if (state == TextureRequestState.Finished)
            {
                UUID id = asset.AssetID;
                OnAssetDownloaded(id, asset);
                ImageDownload image = RegionMasterTexturePipeline.Cache.GetCachedImage(id);
                if (image == null)
                {
                    Debug("AssetTexture is null?! " + id);
                }
                else
                {
                    TextureSuccess++;
                    RegisterUUIDMaybe(id, image);
                    OnAssetDownloaded(id, asset);
                    //lock (uuidTextures) uuidTextures[id] = image;
                }
            }
            else if (state == TextureRequestState.NotFound || state == TextureRequestState.Timeout)
            {
                lock (TexturesSkipped)
                {
                    Debug(
                        "Texture failed to download: " + asset + " skipped due to " + state + " TexturesSkipped=" +
                        TexturesSkipped.Count + " TextureSuccess=" + TextureSuccess + " TextureRquested=" + TextureRequested, Helpers.LogLevel.Warning);
                    TexturesSkipped.Add(asset.AssetID);
                }
            }
        }


        private bool EnsureCached(Asset asset, UUID id)
        {
            if (!RegionMasterTexturePipeline.Cache.HasAsset(id, asset.AssetType))
            {
                var image = asset.AssetData;
                // force into cache
                if (image == null || image.Length == 0) return false;
                if (!RegionMasterTexturePipeline.Cache.SaveAssetToCache(id, image, asset.AssetType)) return false;
                return RegionMasterTexturePipeline.Cache.HasAsset(id, asset.AssetType);
            }
            return true;
        }

        internal void OnAssetDownloaded(UUID uUID, Asset asset)
        {
            EnsureCached(asset, uUID);
            WorldObjects.RegisterUUIDMaybe(uUID, asset);
            SimAsset A = SimAssetStore.FindOrCreateAsset(uUID, asset.AssetType);
            A.SetAsset(asset);
            //A.TypeData = asset.AssetData;




            if (false)
            {
                BotClient Client = this.client;
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
                                                                WriteLine(
                                                                    String.Format(
                                                                        "RequestCreateItemFromAsset() returned: Success={0}, Status={1}, ItemID={2}, AssetID={3}",
                                                                        success, status, itemID, assetID));
                                                                WriteLine(String.Format("Upload took {0}",
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

    }
}