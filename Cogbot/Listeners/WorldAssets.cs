using System;
using System.Threading;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Assets;
using Object=System.Object;

namespace cogbot.Listeners
{
    public partial class WorldObjects
    {
        private static AssetManager RegionMasterTexturePipeline;
        private static readonly List<UUID> TexturesSkipped = new List<UUID>();

        /// <summary>
        /// Assets that WorldObjects requested
        /// </summary>
        private static readonly Dictionary<UUID, UUID> AssetRequests = new Dictionary<UUID, UUID>();

        /// <summary>
        /// Locked on AssetRequests
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
            //IAssetProvider assetProvider = null;// TextForm.simulator.Assets;
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


        public byte[] TextureBytesFormUUID(UUID uUID)
        {
            ImageDownload ID = null;
            lock (uuidTypeObject)
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
                    int tried = 20;
                    int giveUpTick = Environment.TickCount + 1 * 60000;
                    while (ID == null)
                    {
                        ID = StartTextureDownload(uUID);
                        if (ID == null)
                        {
                            lock (TexturesSkipped) if (TexturesSkipped.Contains(uUID)) return null;
                            if (Environment.TickCount > giveUpTick)
                            {
                                lock (TexturesSkipped) TexturesSkipped.Add(uUID);
                                //if (Settings.LOG_LEVEL == Helpers.LogLevel.Warning)
                                Debug("-- ---- ---GIVEUP SculptMesh " + uUID);
                                return null;
                            }
                            if (tried-- < 0)
                            {
                                //   Debug("-- ---- ---WAITING SculptMesh " + uUID);
                                tried = 20;
                            }
                            Thread.Sleep(5000);
                            DoEvents();
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

        public static void RequestAsset(UUID id, AssetType assetType, bool p)
        {
            lock (AssetRequests)
            {
                if (id == UUID.Zero) return;
                SimAsset sa = SimAssetStore.FindOrCreateAsset(id, assetType);
                if (!sa.NeedsRequest) return;
                sa.NeedsRequest = false;
                if (AssetRequests.ContainsKey(id)) return;
                if (assetType == AssetType.Texture)
                {
                    Master.StartTextureDownload(id);
                    return;
                }
                UUID req = RegionMasterTexturePipeline.RequestAsset(id, assetType, p);
                AssetRequestType[req] = assetType;
                AssetRequests[id] = req;
            }
        }


        public override void Assets_OnXferReceived(XferDownload xfer)
        {
            //AssetRequests.
            //RegisterUUIDMaybe(xfer.ID, xfer);
        }

        public override void Assets_OnAssetReceived(AssetDownload transfer, Asset asset)
        {
            if (asset == null)
            {
                lock (AssetRequests)
                    if (AssetRequests.ContainsValue(transfer.ID))
                    {
                        Debug("Failed transfer for " + AssetRequestType[transfer.ID] +
                              " " + transfer.ID + " ");
                    }
                    else
                    {
                        Debug("Unknown transfer Failed for " + transfer.AssetType + " " + transfer.ID + " ");
                    }
                return;
            }
            lock (AssetRequests)
                if (AssetRequests.ContainsValue(transfer.ID))
                {
                    AssetType assetRequestType = AssetRequestType[transfer.ID];
                    if (assetRequestType == asset.AssetType)
                        Debug("Transfer succeeded for " + assetRequestType +
                              " " + transfer.ID + " ");
                    else
                        Debug("Transfer succeeded weirdly as " + asset.AssetType + " for " + assetRequestType +
                              " " + transfer.ID + " ");
                }
                else
                {
                    Debug("Unknown transfer succeeded for " + asset.AssetType + " " + transfer.ID + " ");
                }
            RegisterAsset(asset.AssetID, asset);
        }
        /*
        On-Image-Received
             image: "{OpenMetaverse.ImageDownload,PacketCount=33,Codec=J2C,NotFound=False,Simulator=OpenSim Test (71.197.210.170:9000),PacketsSeen=System.Collections.Generic.SortedList`2[System.UInt16,System.UInt16],ImageType=Normal,DiscardLevel=-1,Priority=1013000,ID=728dd7fa-a688-432d-a4f7-4263b1f97395,Size=33345,AssetData=System.Byte[],Transferred=33345,Success=True,AssetType=Texture}"
             asset: "{OpenMetaverse.AssetTexture,Image=,LayerInfo=,Components=0,AssetData=System.Byte[],Temporary=False}"
         */

        public override void Assets_OnImageReceived(ImageDownload image, AssetTexture asset)
        {
            RegisterUUIDMaybe(image.ID, image);
            RegisterAsset(asset.AssetID, asset);
        }

        static int TextureRequested = 0;
        static int TextureSuccess = 0;

        public ImageDownload StartTextureDownload(UUID id)
        {
            if (RegionMasterTexturePipeline.Cache.HasImage(id))
            {
                return RegionMasterTexturePipeline.Cache.GetCachedImage(id);
            }
            lock (TexturesSkipped) if (TexturesSkipped.Contains(id)) return null;
            TextureRequested++;
            RegionMasterTexturePipeline.RequestImage(id, ImageType.Normal,
                                                     RegionMasterTexturePipeline_OnDownloadFinished);
            return null;
        }

        private void RegionMasterTexturePipeline_OnDownloadFinished(TextureRequestState state, AssetTexture asset)
        {
            if (state == TextureRequestState.Finished)
            {
                UUID id = asset.AssetID;
                RegisterAsset(id,asset);
                ImageDownload image = RegionMasterTexturePipeline.Cache.GetCachedImage(id);
                if (image == null)
                {
                    Console.WriteLine("AssetTexture is null?! " + id);
                }
                else
                {
                    TextureSuccess++;
                    RegisterUUIDMaybe(id, image);
                    SimAssetSystem.OnAssetDownloaded(id,asset);
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


        private void RegisterAsset(UUID uUID, Asset asset)
        {
            switch (asset.AssetType)
            {
                /// <summary>Unknown asset type</summary>
                case AssetType.Unknown:
                    {
                        break;
                    } //-1,
                /// <summary>Texture asset, stores in JPEG2000 J2C stream format</summary>
                case AssetType.Texture:
                    {
                        SimAssetSystem.OnAssetDownloaded(uUID, asset);
                        break;
                    } //0,
                /// <summary>Sound asset</summary>
                case AssetType.Sound:
                    {
                        SimAssetSystem.OnAssetDownloaded(uUID, asset);
                        break;
                    } //1,
                /// <summary>Calling card for another avatar</summary>
                case AssetType.CallingCard:
                    {
                        break;
                    } //2,
                /// <summary>Link to a location in world</summary>
                case AssetType.Landmark:
                    {
                        break;
                    } //3,
                // <summary>Legacy script asset, you should never see one of these</summary>
                //[Obsolete]
                //Script: {break;}//4,
                /// <summary>Collection of textures and parameters that can be 
                /// worn by an avatar</summary>
                case AssetType.Clothing:
                    {
                        break;
                    } //5,
                /// <summary>Primitive that can contain textures, sounds, 
                /// scripts and more</summary>
                case AssetType.Object:
                    {
                        break;
                    } //6,
                /// <summary>Notecard asset</summary>
                case AssetType.Notecard:
                    {
                        break;
                    } //7,
                /// <summary>Holds a collection of inventory items</summary>
                case AssetType.Folder:
                    {
                        break;
                    } //8,
                /// <summary>Root inventory folder</summary>
                case AssetType.RootFolder:
                    {
                        break;
                    } //9,
                /// <summary>Linden scripting language script</summary>
                case AssetType.LSLText:
                    {
                        break;
                    } //10,
                /// <summary>LSO bytecode for a script</summary>
                case AssetType.LSLBytecode:
                    {
                        break;
                    } //11,
                /// <summary>Uncompressed TGA texture</summary>
                case AssetType.TextureTGA:
                    {
                        SimAssetSystem.OnAssetDownloaded(uUID, asset);
                        break;
                    } //12,
                /// <summary>Collection of textures and shape parameters that can
                /// be worn</summary>
                case AssetType.Bodypart:
                    {
                        break;
                    } //13,
                /// <summary>Trash folder</summary>
                case AssetType.TrashFolder:
                    {
                        break;
                    } //14,
                /// <summary>Snapshot folder</summary>
                case AssetType.SnapshotFolder:
                    {
                        break;
                    } //15,
                /// <summary>Lost and found folder</summary>
                case AssetType.LostAndFoundFolder:
                    {
                        break;
                    } //16,
                /// <summary>Uncompressed sound</summary>
                case AssetType.SoundWAV:
                    {
                        SimAssetSystem.OnAssetDownloaded(uUID, asset);
                        break;
                    } //17,
                /// <summary>Uncompressed TGA non-square image, not to be used as a
                /// texture</summary>
                case AssetType.ImageTGA:
                    {
                        SimAssetSystem.OnAssetDownloaded(uUID, asset);
                        break;
                    } //18,
                /// <summary>Compressed JPEG non-square image, not to be used as a
                /// texture</summary>
                case AssetType.ImageJPEG:
                    {
                        SimAssetSystem.OnAssetDownloaded(uUID, asset);
                        break;
                    } //19,
                /// <summary>Animation</summary>
                case AssetType.Animation:
                    SimAssetSystem.OnAssetDownloaded(uUID, asset);
                    //20,
                    break;
                /// <summary>Sequence of animations, sounds, chat, and pauses</summary>
                case AssetType.Gesture:
                    {
                        break;
                    } //21,
                /// <summary>Simstate file</summary>
                case AssetType.Simstate:
                    {
                        break;
                    } //22,
                default:
                    {
                        break;
                    }
            }
            RegisterUUIDMaybe(uUID, asset);
        }
    }
}
