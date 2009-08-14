/*
 * Copyright (c) 2006-2008, openmetaverse.org
 * All rights reserved.
 *
 * - Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Neither the name of the openmetaverse.org nor the names
 *   of its contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Drawing;
using System.Text;
using System.Collections.Generic;
using System.Threading;
using System.IO;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Imaging;
using OpenMetaverse.Assets;

namespace OpenMetaverse
{
    public class InvalidOutfitException : Exception
    {
        public InvalidOutfitException(string message) : base(message) { }
    }

    #region Enums

    /// <summary>
    /// Index of TextureEntry slots for avatar appearances
    /// </summary>
    public enum AvatarTextureIndex
    {
        Unknown = -1,
        HeadBodypaint = 0,
        UpperShirt,
        LowerPants,
        EyesIris,
        Hair,
        UpperBodypaint,
        LowerBodypaint,
        LowerShoes,
        HeadBaked,
        UpperBaked,
        LowerBaked,
        EyesBaked,
        LowerSocks,
        UpperJacket,
        LowerJacket,
        UpperGloves,
        UpperUndershirt,
        LowerUnderpants,
        Skirt,
        SkirtBaked,
        HairBaked
    }

    /// <summary>
    /// Bake layers for avatar appearance
    /// </summary>
    public enum BakeType
    {
        Unknown = -1,
        Head = 0,
        UpperBody = 1,
        LowerBody = 2,
        Eyes = 3,
        Skirt = 4,
        Hair = 5
    }

    #endregion Enums
    /// <summary>
    /// Manager class to for agents appearance, both body parts and clothing
    /// </summary>
    public class AppearanceManager
    {

        /// <summary>
        /// Contains information about a wearable inventory item
        /// </summary>
        public class WearableData
        {
            private InventoryWearable _item;
            public InventoryWearable Item { 
                get
                {
                    return _item;
                } 
                set
                {
                    _item = value;
                    WearableType = _item.WearableType;
                    AssetID = _item.AssetUUID;
                    AssetType = _item.AssetType;
                    ItemID = _item.UUID;                
                }
            }

            /// <summary>Inventory ItemID of the wearable</summary>
            public UUID ItemID;
            /// <summary>AssetID of the wearable asset</summary>
            public UUID AssetID;
            /// <summary>WearableType of the wearable</summary>
            public WearableType WearableType;
            /// <summary>AssetType of the wearable</summary>
            public AssetType AssetType;
            /// <summary>Asset data for the wearable</summary>
            public AssetWearable Asset;

            public override string ToString()
            {
                return String.Format("ItemID: {0}, AssetID: {1}, WearableType: {2}, AssetType: {3}, Asset: {4}",
                    ItemID, AssetID, WearableType, AssetType, Asset != null ? Asset.Name : "(null)");
            }
        }

        /// <summary>
        /// Data collected from visual params for each wearable
        /// needed for the calculation of the color
        /// </summary>
        private struct ColorParamInfo
        {
            public VisualParam VisualParam;
            public VisualColorParam VisualColorParam;
            public float Value;
            public WearableType WearableType;
        }

        /// <summary>
        /// Holds a texture assetID and the data needed to bake this layer into
        /// an outfit texture. Used to keep track of currently worn textures
        /// and baking data
        /// </summary>
        public struct TextureData
        {
            /// <summary>A texture AssetID</summary>
            public UUID TextureID;
            /// <summary>Asset data for the texture</summary>
            public AssetTexture Texture;
            /// <summary>Collection of alpha masks that needs applying</summary>
            public Dictionary<VisualAlphaParam, float> AlphaMasks;
            /// <summary>Tint that should be applied to the texture</summary>
            public Color Color;

            public override string ToString()
            {
                return String.Format("TextureID: {0}, Texture: {1}",
                    TextureID, Texture != null ? Texture.AssetData.Length + " bytes" : "(null)");
            }
        }

        /// <summary>
        /// 
        /// </summary>
        public delegate void AgentWearablesCallback();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="te"></param>
        public delegate void AppearanceUpdatedCallback(Primitive.TextureEntry te);

        /// <summary></summary>
        public event AgentWearablesCallback OnAgentWearables;
        /// <summary></summary>
        public event AppearanceUpdatedCallback OnAppearanceUpdated;

        /// <summary>
        /// Time that wearable events will wait (140 secs)
        /// </summary>
        public static int WEARABLE_TIMEOUT = 140 * 1000;
        /// <summary>Total number of wearables for each avatar</summary>
        public const int WEARABLE_COUNT = 13;
        /// <summary>Total number of baked textures on each avatar</summary>
        public const int BAKED_TEXTURE_COUNT = 6;
        /// <summary>Total number of wearables per bake layer</summary>
        public const int WEARABLES_PER_LAYER = 7;
        /// <summary>Total number of textures on an avatar, baked or not</summary>
        public const int AVATAR_TEXTURE_COUNT = 21;
        /// <summary>Map of what wearables are included in each bake</summary>
        public static readonly WearableType[][] WEARABLE_BAKE_MAP = new WearableType[][]
        {
            new WearableType[] { WearableType.Shape, WearableType.Skin,    WearableType.Hair,    WearableType.Invalid, WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid    },
            new WearableType[] { WearableType.Shape, WearableType.Skin,    WearableType.Shirt,   WearableType.Jacket,  WearableType.Gloves,  WearableType.Undershirt, WearableType.Invalid    },
            new WearableType[] { WearableType.Shape, WearableType.Skin,    WearableType.Pants,   WearableType.Shoes,   WearableType.Socks,   WearableType.Jacket,     WearableType.Underpants },
            new WearableType[] { WearableType.Eyes,  WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid    },
            new WearableType[] { WearableType.Skirt, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid    },
            new WearableType[] { WearableType.Hair,  WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid    }
        };
        /// <summary>Secret values to finalize the cache check hashes for each
        /// bake</summary>
        public static readonly UUID[] BAKED_TEXTURE_HASH = new UUID[]
        {
            new UUID("18ded8d6-bcfc-e415-8539-944c0f5ea7a6"),
            new UUID("338c29e3-3024-4dbb-998d-7c04cf4fa88f"),
            new UUID("91b4a2c7-1b1a-ba16-9a16-1f8f8dcc1c3f"),
            new UUID("b2cf28af-b840-1071-3c6a-78085d8128b5"),
            new UUID("ea800387-ea1a-14e0-56cb-24f2022f969a"),
            new UUID("0af1ef7c-ad24-11dd-8790-001f5bf833e8")
        };
        /// <summary>Default avatar texture, used to detect when a custom
        /// texture is not set for a face</summary>
        public static readonly UUID DEFAULT_AVATAR_TEXTURE = new UUID("c228d1cf-4b5d-4ba8-84f4-899a0796aa97");


        private GridClient Client;
        private AssetManager Assets;

        /// <summary>
        /// An <seealso cref="InternalDictionary"/> which keeps track of wearables data
        /// </summary>
        public  Dictionary<WearableType, WearableData> Wearables = new  Dictionary<WearableType, WearableData>();
        // As wearable assets are downloaded and decoded, the textures are added to this array
        private UUID[] AgentTextures = new UUID[AVATAR_TEXTURE_COUNT];
        // A cache of the textures worn, needed for rebaking
        private AssetTexture[] AgentAssets = new AssetTexture[AVATAR_TEXTURE_COUNT];

        protected struct PendingAssetDownload
        {
            public UUID Id;
            public AssetType Type;

            public PendingAssetDownload(UUID id, AssetType type)
            {
                Id = id;
                Type = type;
            }
        }
        
        // Wearable assets are downloaded one at a time, a new request is pulled off the queue
        // and started when the previous one completes
        private Queue<PendingAssetDownload> AssetDownloads = new Queue<PendingAssetDownload>();
        // A list of all the images we are currently downloading, prior to baking
        private Dictionary<UUID, AvatarTextureIndex> ImageDownloads = new Dictionary<UUID, AvatarTextureIndex>();
        // A list of all the bakes we need to complete
        private Dictionary<BakeType, Baker> PendingBakes = new Dictionary<BakeType, Baker>(BAKED_TEXTURE_COUNT);
        // A list of all the uploads that are in progress
        private Dictionary<UUID, AvatarTextureIndex> PendingUploads = new Dictionary<UUID, AvatarTextureIndex>(BAKED_TEXTURE_COUNT);
        // Whether the handler for our current wearable list should automatically start downloading the assets
        //private bool DownloadWearables = false;
        private static int CacheCheckSerialNum = 1; //FIXME
        private static uint SetAppearanceSerialNum = 1; //FIXME
        private AutoResetEvent WearablesRequestEvent = new AutoResetEvent(false);
        private AutoResetEvent WearablesDownloadedEvent = new AutoResetEvent(false);
        private AutoResetEvent CachedResponseEvent = new AutoResetEvent(false);
        private AutoResetEvent UpdateEvent = new AutoResetEvent(false);
        // FIXME: Create a class-level appearance thread so multiple threads can't be launched

        /// <summary>
        /// Default constructor
        /// </summary>
        /// <param name="client">This agents <seealso cref="OpenMetaverse.GridClient"/> Object</param>
        /// <param name="assets">Reference to an AssetManager object</param>
        public AppearanceManager(GridClient client, AssetManager assets)
        {
            Client = client;
            Assets = assets;

            // Initialize AgentTextures to zero UUIDs
            for (int i = 0; i < AgentTextures.Length; i++)
                AgentTextures[i] = UUID.Zero;

            Client.Network.RegisterCallback(PacketType.AgentWearablesUpdate, new NetworkManager.PacketCallback(AgentWearablesUpdateHandler));
            Client.Network.RegisterCallback(PacketType.AgentCachedTextureResponse, new NetworkManager.PacketCallback(AgentCachedTextureResponseHandler));
            Client.Network.RegisterCallback(PacketType.RebakeAvatarTextures,new NetworkManager.PacketCallback(RebakeAvatarTexturesHandler));
            Client.Network.OnDisconnected += new NetworkManager.DisconnectedCallback(Network_OnDisconnected);
        }

        private static AssetType WearableTypeToAssetType(WearableType type)
        {
            switch (type)
            {
                case WearableType.Shape:
                case WearableType.Skin:
                case WearableType.Hair:
                case WearableType.Eyes:
                    return AssetType.Bodypart;
                case WearableType.Shirt:
                case WearableType.Pants:
                case WearableType.Shoes:
                case WearableType.Socks:
                case WearableType.Jacket:
                case WearableType.Gloves:
                case WearableType.Undershirt:
                case WearableType.Underpants:
                case WearableType.Skirt:
                    return AssetType.Clothing;
                default:
                    throw new Exception("Unhandled wearable type " + type);
            }
        }

        /// <summary>
        /// Returns the assetID for a given WearableType 
        /// </summary>
        /// <param name="type">the <seealso cref="OpenMetaverse.WearableType"/> of the asset</param>
        /// <returns>The <seealso cref="OpenMetaverse.UUID"/> of the WearableType</returns>
        public UUID GetWearableAsset(WearableType type)
        {
            WearableData wearable;

            if (Wearables.TryGetValue(type, out wearable))
                return wearable.Item.AssetUUID;
            else
                return UUID.Zero;
        }

        /// <summary>
        /// Ask the server what we are wearing and set appearance based on that
        /// </summary>
        public void SetPreviousAppearance()
        {
            SetPreviousAppearance(true);
        }

        public void SetPreviousAppearance(bool bake)
        {
            Thread appearanceThread = new Thread(new ParameterizedThreadStart(StartSetPreviousAppearance));
            appearanceThread.Start(bake);
        }

        private void StartSetPreviousAppearance(object thread_params)
        {
            bool bake = (bool)thread_params;
            SendAgentWearablesRequest();
            WearablesRequestEvent.WaitOne();
            UpdateAppearanceFromWearables(bake);
        }

        private class WearParams
        {
            public object Param;
            public bool Bake;
            public bool RemoveExistingAttachments;

            public WearParams(object param, bool bake, bool removeExistingAttachments)
            {
                Param = param;
                Bake = bake;
                RemoveExistingAttachments = removeExistingAttachments;
            }
        }

        /// <summary>
        /// Replace the current outfit with a list of wearables and set appearance
        /// </summary>
        /// <param name="ibs">List of wearables that define the new outfit</param>
        public void WearOutfit(List<InventoryBase> ibs)
        {
            WearOutfit(ibs, true);
        }
        
        /// <summary>
        /// Replace the current outfit with a list of wearables and set appearance
        /// </summary>
        /// <param name="ibs">List of wearables that define the new outfit</param>
        /// <param name="bake">Whether to bake textures for the avatar or not</param>
        public void WearOutfit(List<InventoryBase> ibs, bool bake)
        {
            WearParams wearParams = new WearParams(ibs, bake,true);
            Thread appearanceThread = new Thread(new ParameterizedThreadStart(StartWearOutfit));
            appearanceThread.Start(wearParams);
        }

        /// <summary>
        /// Add to the current outfit with the list supplied
        /// </summary>
        /// <param name="ibs_new">List of wearables that will be added to the outfit</param>
        /// <param name="bake">Whether to bake textures for the avatar or not</param>
        public void AddToOutfit(List<InventoryBase> ibs_new, bool bake)
        {
            List<InventoryBase> ibs_total = new List<InventoryBase>();

            // Get what we are currently wearing
            TestLock(Wearables); lock (Wearables)
            {
                foreach (KeyValuePair<WearableType, OpenMetaverse.AppearanceManager.WearableData> kvp in Wearables)
                    ibs_total.Add((InventoryBase)kvp.Value.Item);
            
            }
            // Add the new items at the end, ReplaceOutfitWearables() will do the right thing as it places each warable into a slot in order
            // so the end of the list will overwrite earlier parts if they use the same slot.
            foreach (InventoryBase item in ibs_new)
            {
                if (item is InventoryWearable)
                    ibs_total.Add(item);
            }

            WearParams wearParams = new WearParams(ibs_total, bake, false);
            Thread appearanceThread = new Thread(new ParameterizedThreadStart(StartWearOutfit));
            appearanceThread.Start(wearParams);
        }

        private void StartWearOutfit(object thread_params)
        {
            WearParams wearParams = (WearParams)thread_params;

            List<InventoryBase> ibs = (List<InventoryBase>)wearParams.Param;
            List<InventoryWearable> wearables = new List<InventoryWearable>();
            List<InventoryBase> attachments = new List<InventoryBase>();

            foreach (InventoryBase ib in ibs)
            {
                if (ib is InventoryWearable)
                    wearables.Add((InventoryWearable)ib);
                else if (ib is InventoryAttachment || ib is InventoryObject)
                    attachments.Add(ib);
            }


            SendAgentWearablesRequest();
            WearablesRequestEvent.WaitOne();
            ReplaceOutfitWearables(wearables);
            UpdateAppearanceFromWearables(wearParams.Bake);
            AddAttachments(attachments, wearParams.RemoveExistingAttachments);
        }

        /// <summary>
        /// Replace the current outfit with a folder and set appearance
        /// </summary>
        /// <param name="folder">UUID of the inventory folder to wear</param>
        public void WearOutfit(UUID folder)
        {
            WearOutfit(folder, true);
        }

        /// <summary>
        /// Replace the current outfit with a folder and set appearance
        /// </summary>
        /// <param name="path">Inventory path of the folder to wear</param>
        public void WearOutfit(string[] path)
        {
            WearOutfit(path, true);
        }

        /// <summary>
        /// Replace the current outfit with a folder and set appearance
        /// </summary>
        /// <param name="folder">Folder containing the new outfit</param>
        /// <param name="bake">Whether to bake the avatar textures or not</param>
        public void WearOutfit(UUID folder, bool bake)
        {
            WearParams wearOutfitParams = new WearParams(folder, bake,true);
            Thread appearanceThread = new Thread(new ParameterizedThreadStart(StartWearOutfitFolder));
            appearanceThread.Start(wearOutfitParams);
        }

        /// <summary>
        /// Replace the current outfit with a folder and set appearance
        /// </summary>
        /// <param name="path">Path of folder containing the new outfit</param>
        /// <param name="bake">Whether to bake the avatar textures or not</param>
        public void WearOutfit(string[] path, bool bake)
        {
            WearParams wearOutfitParams = new WearParams(path, bake,true);
            Thread appearanceThread = new Thread(new ParameterizedThreadStart(StartWearOutfitFolder));
            appearanceThread.Start(wearOutfitParams);
        }

        public void WearOutfit(InventoryFolder folder, bool bake)
        {
            WearParams wearOutfitParams = new WearParams(folder, bake,true);
            Thread appearanceThread = new Thread(new ParameterizedThreadStart(StartWearOutfitFolder));
            appearanceThread.Start(wearOutfitParams);
        }

        private void StartWearOutfitFolder(object thread_params)
        {
            WearParams wearOutfitParams = (WearParams)thread_params;

            SendAgentWearablesRequest(); // request current wearables async
            List<InventoryWearable> wearables;
            List<InventoryBase> attachments;

            if (!GetFolderWearables(wearOutfitParams.Param, out wearables, out attachments)) // get wearables in outfit folder
            // TODO: this error condition should be passed back to the client somehow}
            {
                Logger.Log(
                    "GetFolderWearables did not find wearables " + 
                    wearOutfitParams.Param + " " + wearables + " " +
                    attachments, Helpers.LogLevel.Error, Client);
                return;
            } 

            if (!WearablesRequestEvent.WaitOne(WEARABLE_TIMEOUT*2)) // wait for current wearables 
            {
                Logger.Log("StartWearOutfitFolder timed out.", Helpers.LogLevel.Error, Client);
            }
            ReplaceOutfitWearables(wearables); // replace current wearables with outfit folder
            UpdateAppearanceFromWearables(wearOutfitParams.Bake);
            AddAttachments(attachments, wearOutfitParams.RemoveExistingAttachments);
        }

        private bool GetFolderWearables(object _folder, out List<InventoryWearable> wearables, out List<InventoryBase> attachments)
        {
            UUID folder;
            wearables = null;
            attachments = null;

            if (_folder.GetType() == typeof(string[]))
            {
                string[] path = (string[])_folder;

                folder = Client.Inventory.FindObjectByPath(
                    Client.Inventory.Store.RootFolder.UUID, Client.Self.AgentID, String.Join("/", path), 1000 * 200);

                if (folder == UUID.Zero)
                {
                    Logger.Log("Outfit path " + path + " not found", Helpers.LogLevel.Error, Client);
                    return false;
                }
            }
            else
                folder = (UUID)_folder;

            wearables = new List<InventoryWearable>();
            attachments = new List<InventoryBase>();
            List<InventoryBase> objects = Client.Inventory.FolderContents(folder, Client.Self.AgentID,
                false, true, InventorySortOrder.ByName, 1000 * 20);

            if (objects != null)
            {
                foreach (InventoryBase ib in objects)
                {
                    if (ib is InventoryWearable)
                    {
                        DebugLog("Adding wearable " + ib.Name, Client);
                        wearables.Add((InventoryWearable)ib);
                    }
                    else if (ib is InventoryAttachment)
                    {
                        DebugLog("Adding attachment (attachment) " + ib.Name, Client);
                        attachments.Add(ib);
                    }
                    else if (ib is InventoryObject)
                    {
                        DebugLog("Adding attachment (object) " + ib.Name, Client);
                        attachments.Add(ib);
                    }
                    else
                    {
                        DebugLog("Ignoring inventory item " + ib.Name, Client);
                    }
                }
            }
            else
            {
                Logger.Log("Failed to download folder contents of + " + folder.ToString(),
                    Helpers.LogLevel.Error, Client);
                return false;
            }

            return true;
        }

        // this method will download the assets for all inventory items in iws
        private void ReplaceOutfitWearables(List<InventoryWearable> iws)
        {
            TestLock(Wearables); lock (Wearables)
            {
                Dictionary<WearableType, WearableData> preserve = new Dictionary<WearableType,WearableData>();
                
                foreach (KeyValuePair<WearableType,WearableData> kvp in Wearables)
                {
                    if (kvp.Value.Item.AssetType == AssetType.Bodypart)
                            preserve.Add(kvp.Key, kvp.Value);
                }

                Wearables = preserve;
            
                foreach (InventoryWearable iw in iws)
                {
                    WearableData wd = new WearableData();
                    wd.Item = iw; 
                    Wearables[wd.Item.WearableType] = wd;
                }
            }
        }

        /// <summary>
        /// Adds a list of attachments to avatar
        /// </summary>
        /// <param name="attachments">A List containing the attachments to add</param>
        /// <param name="removeExistingFirst">If true, tells simulator to remove existing attachment
        /// first</param>
        public void AddAttachments(List<InventoryBase> attachments, bool removeExistingFirst)
        {
            // FIXME: Obey this
            //const int OBJECTS_PER_PACKET = 4;

            // Use RezMultipleAttachmentsFromInv  to clear out current attachments, and attach new ones
            RezMultipleAttachmentsFromInvPacket attachmentsPacket = new RezMultipleAttachmentsFromInvPacket();
            attachmentsPacket.AgentData.AgentID = Client.Self.AgentID;
            attachmentsPacket.AgentData.SessionID = Client.Self.SessionID;

            attachmentsPacket.HeaderData.CompoundMsgID = UUID.Random();
            attachmentsPacket.HeaderData.FirstDetachAll = removeExistingFirst;
            attachmentsPacket.HeaderData.TotalObjects = (byte)attachments.Count;

            attachmentsPacket.ObjectData = new RezMultipleAttachmentsFromInvPacket.ObjectDataBlock[attachments.Count];
            for (int i = 0; i < attachments.Count; i++)
            {
                if (attachments[i] is InventoryAttachment)
                {
                    InventoryAttachment attachment = (InventoryAttachment)attachments[i];
                    attachmentsPacket.ObjectData[i] = new RezMultipleAttachmentsFromInvPacket.ObjectDataBlock();
                    attachmentsPacket.ObjectData[i].AttachmentPt = (byte)attachment.AttachmentPoint;
                    attachmentsPacket.ObjectData[i].EveryoneMask = (uint)attachment.Permissions.EveryoneMask;
                    attachmentsPacket.ObjectData[i].GroupMask = (uint)attachment.Permissions.GroupMask;
                    attachmentsPacket.ObjectData[i].ItemFlags = (uint)attachment.Flags;
                    attachmentsPacket.ObjectData[i].ItemID = attachment.UUID;
                    attachmentsPacket.ObjectData[i].Name = Utils.StringToBytes(attachment.Name);
                    attachmentsPacket.ObjectData[i].Description = Utils.StringToBytes(attachment.Description);
                    attachmentsPacket.ObjectData[i].NextOwnerMask = (uint)attachment.Permissions.NextOwnerMask;
                    attachmentsPacket.ObjectData[i].OwnerID = attachment.OwnerID;
                }
                else if (attachments[i] is InventoryObject)
                {
                    InventoryObject attachment = (InventoryObject)attachments[i];
                    attachmentsPacket.ObjectData[i] = new RezMultipleAttachmentsFromInvPacket.ObjectDataBlock();
                    attachmentsPacket.ObjectData[i].AttachmentPt = 0;
                    attachmentsPacket.ObjectData[i].EveryoneMask = (uint)attachment.Permissions.EveryoneMask;
                    attachmentsPacket.ObjectData[i].GroupMask = (uint)attachment.Permissions.GroupMask;
                    attachmentsPacket.ObjectData[i].ItemFlags = (uint)attachment.Flags;
                    attachmentsPacket.ObjectData[i].ItemID = attachment.UUID;
                    attachmentsPacket.ObjectData[i].Name = Utils.StringToBytes(attachment.Name);
                    attachmentsPacket.ObjectData[i].Description = Utils.StringToBytes(attachment.Description);
                    attachmentsPacket.ObjectData[i].NextOwnerMask = (uint)attachment.Permissions.NextOwnerMask;
                    attachmentsPacket.ObjectData[i].OwnerID = attachment.OwnerID;
                }
                else
                {
                    Logger.Log("Cannot attach inventory item of type " + attachments[i].GetType().ToString(),
                        Helpers.LogLevel.Warning, Client);
                }
            }

            Client.Network.SendPacket(attachmentsPacket);
        }

        /// <summary>
        /// Attach an item to an avatar at a specific attach point
        /// </summary>
        /// <param name="item">A <seealso cref="OpenMetaverse.InventoryItem"/> to attach</param>
        /// <param name="attachPoint">the <seealso cref="OpenMetaverse.AttachmentPoint"/> on the avatar 
        /// to attach the item to</param>
        public void Attach(InventoryItem item, AttachmentPoint attachPoint)
        {
            Attach(item.UUID, item.OwnerID, item.Name, item.Description, item.Permissions, item.Flags, 
                attachPoint);
        }

        /// <summary>
        /// Attach an item to an avatar specifying attachment details
        /// </summary>
        /// <param name="itemID">The <seealso cref="OpenMetaverse.UUID"/> of the item to attach</param>
        /// <param name="ownerID">The <seealso cref="OpenMetaverse.UUID"/> attachments owner</param>
        /// <param name="name">The name of the attachment</param>
        /// <param name="description">The description of the attahment</param>
        /// <param name="perms">The <seealso cref="OpenMetaverse.Permissions"/> to apply when attached</param>
        /// <param name="itemFlags">The <seealso cref="OpenMetaverse.InventoryItemFlags"/> of the attachment</param>
        /// <param name="attachPoint">the <seealso cref="OpenMetaverse.AttachmentPoint"/> on the avatar 
        /// to attach the item to</param>
        public void Attach(UUID itemID, UUID ownerID, string name, string description,
            Permissions perms, uint itemFlags, AttachmentPoint attachPoint)
        {
            // TODO: At some point it might be beneficial to have AppearanceManager track what we
            // are currently wearing for attachments to make enumeration and detachment easier

            RezSingleAttachmentFromInvPacket attach = new RezSingleAttachmentFromInvPacket();

            attach.AgentData.AgentID = Client.Self.AgentID;
            attach.AgentData.SessionID = Client.Self.SessionID;

            attach.ObjectData.AttachmentPt = (byte)attachPoint;
            attach.ObjectData.Description = Utils.StringToBytes(description);
            attach.ObjectData.EveryoneMask = (uint)perms.EveryoneMask;
            attach.ObjectData.GroupMask = (uint)perms.GroupMask;
            attach.ObjectData.ItemFlags = itemFlags;
            attach.ObjectData.ItemID = itemID;
            attach.ObjectData.Name = Utils.StringToBytes(name);
            attach.ObjectData.NextOwnerMask = (uint)perms.NextOwnerMask;
            attach.ObjectData.OwnerID = ownerID;

            Client.Network.SendPacket(attach);
        }

        /// <summary>
        /// Detach an item from avatar using an <seealso cref="OpenMetaverse.InventoryItem"/> object
        /// </summary>
        /// <param name="item">An <seealso cref="OpenMetaverse.InventoryItem"/> object</param>
        public void Detach(InventoryItem item)
        {
            Detach(item.UUID); 
        }

        /// <summary>
        /// Detach an Item from avatar by items <seealso cref="OpenMetaverse.UUID"/>
        /// </summary>
        /// <param name="itemID">The items ID to detach</param>
        public void Detach(UUID itemID)
        {
            DetachAttachmentIntoInvPacket detach = new DetachAttachmentIntoInvPacket();
            detach.ObjectData.AgentID = Client.Self.AgentID;
            detach.ObjectData.ItemID = itemID;

            Client.Network.SendPacket(detach);
        }


        private void UpdateAppearanceFromWearables(bool bake)
        {
            lock (AgentTextures)
            {
                for (int i = 0; i < AgentTextures.Length; i++)
                    AgentTextures[i] = UUID.Zero;
            }

            AssetManager.AssetUploadedCallback uploadCallback = new AssetManager.AssetUploadedCallback(Assets_OnAssetUploaded);
            Assets.OnAssetUploaded += uploadCallback;

            // Download assets for what we are wearing and fill in AgentTextures
            DownloadWearableAssets();
            WearablesDownloadedEvent.WaitOne();

            // Check if anything needs to be rebaked
            if (bake) RequestCachedBakes();

            // Tell the sim what we are wearing
            SendAgentIsNowWearing();

            // Wait for cached layer check to finish
            if (bake) if (!CachedResponseEvent.WaitOne(WEARABLE_TIMEOUT))
            {
                Logger.Log("Giving up on CachedResponseEvent didnt complete (in time)", Helpers.LogLevel.Info, Client);
                Assets.OnAssetUploaded -= uploadCallback;
                return;
            }

            // Unregister the image download and asset upload callbacks
            //Assets.OnImageReceived -= imageCallback;
            Assets.OnAssetUploaded -= uploadCallback;

            DebugLog("CachedResponseEvent completed", Client);

            #region Send Appearance

            Primitive.TextureEntry te = null;

            ObjectManager.NewAvatarCallback updateCallback =
                delegate(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
                {
                    if (avatar.LocalID == Client.Self.LocalID)
                    {
                        if (avatar.Textures.FaceTextures != null)
                        {
                            bool match = true;

                            for (uint i = 0; i < AgentTextures.Length; i++)
                            {
                                Primitive.TextureEntryFace face = avatar.Textures.FaceTextures[i];

                                if (face == null)
                                {
                                    // If the texture is UUID.Zero the face should be null
                                    if (AgentTextures[i] != UUID.Zero)
                                    {
                                        match = false;
                                        break;
                                    }
                                }
                                else if (face.TextureID != AgentTextures[i] && face.TextureID != AppearanceManager.DEFAULT_AVATAR_TEXTURE)
                                {
                                    Logger.DebugLog("*** FACE is " + ((AvatarTextureIndex)i).ToString() + " " + face.TextureID.ToString() + " Agent Texture is " + AgentTextures[i].ToString());
                                    match = false;
                                    //break;
                                }
                            }

                            if (!match)
                                Logger.Log("TextureEntry mismatch after updating our appearance", Helpers.LogLevel.Warning, Client);

                            te = avatar.Textures;
                            UpdateEvent.Set();
                        }
                        else
                        {
                            Logger.Log("Received an update for our avatar with a null FaceTextures array",
                                Helpers.LogLevel.Warning, Client);
                        }
                    }
                };
            Client.Objects.OnNewAvatar += updateCallback;

            // Send all of the visual params and textures for our agent
            SendAgentSetAppearance();

            // Wait for the ObjectUpdate to come in for our avatar after changing appearance
            if (UpdateEvent.WaitOne(WEARABLE_TIMEOUT, false))
            {
                if (OnAppearanceUpdated != null)
                {
                    try { OnAppearanceUpdated(te); }
                    catch (Exception e) { Logger.Log(e.Message, Helpers.LogLevel.Error, Client, e); }
                }
            }
            else
            {
                Logger.Log("Timed out waiting for our appearance to update on the simulator", Helpers.LogLevel.Warning, Client);
            }

            Client.Objects.OnNewAvatar -= updateCallback;
            Logger.Log("Appearance update completed",Helpers.LogLevel.Info);

            #endregion Send Appearance
        }

        /// <summary>
        /// Build hashes out of the texture assetIDs for each baking layer to
        /// ask the simulator whether it has cached copies of each baked texture
        /// </summary>
        public void RequestCachedBakes()
        {
            DebugLog("RequestCachedBakes()", Client);
            
            List<KeyValuePair<int, UUID>> hashes = new List<KeyValuePair<int,UUID>>();

            AgentCachedTexturePacket cache = new AgentCachedTexturePacket();
            cache.AgentData.AgentID = Client.Self.AgentID;
            cache.AgentData.SessionID = Client.Self.SessionID;
            cache.AgentData.SerialNum = CacheCheckSerialNum;

            // Build hashes for each of the bake layers from the individual components
            for (int bakedIndex = 0; bakedIndex < BAKED_TEXTURE_COUNT; bakedIndex++)
            {
                // Don't do a cache request for a skirt bake if we're not wearing a skirt
                TestLock(Wearables); lock (Wearables)
                if (bakedIndex == (int)BakeType.Skirt && 
                    (!Wearables.ContainsKey(WearableType.Skirt) || Wearables[WearableType.Skirt].Asset.AssetID == UUID.Zero))
                    continue;

                UUID hash = new UUID();

                for (int wearableIndex = 0; wearableIndex < WEARABLES_PER_LAYER; wearableIndex++)
                {
                    WearableType type = WEARABLE_BAKE_MAP[bakedIndex][wearableIndex];
                    UUID assetID = GetWearableAsset(type);

                    // Build a hash of all the texture asset IDs in this baking layer
                    if (assetID != UUID.Zero) hash ^= assetID;
                }

                if (hash != UUID.Zero)
                {
                    // Hash with our secret value for this baked layer
                    hash ^= BAKED_TEXTURE_HASH[bakedIndex];

                    // Add this to the list of hashes to send out
                    hashes.Add(new KeyValuePair<int, UUID>(bakedIndex, hash));
                }
            }

            // Only send the packet out if there's something to check
            if (hashes.Count > 0)
            {
                cache.WearableData = new AgentCachedTexturePacket.WearableDataBlock[hashes.Count];

                for (int i = 0; i < hashes.Count; i++)
                {
                    cache.WearableData[i] = new AgentCachedTexturePacket.WearableDataBlock();
                    cache.WearableData[i].TextureIndex = (byte)hashes[i].Key;
                    cache.WearableData[i].ID = hashes[i].Value;

                    DebugLog("Checking cache for index " + cache.WearableData[i].TextureIndex +
                        ", ID: " + cache.WearableData[i].ID, Client);
                }

                // Increment our serial number for this packet
                CacheCheckSerialNum++;

                // Send it out
                Client.Network.SendPacket(cache);
            }
        }
        
        /// <summary>
        /// Force a rebake of the currently worn textures
        /// </summary>
        public void ForceRebakeAvatarTextures()
        {
            Client.Assets.OnAssetUploaded += Assets_OnAssetUploaded;
            for (int i = 0; i < BAKED_TEXTURE_COUNT; i++)
            {
                // Don't bake skirt if not wearing one
                TestLock(Wearables); lock (Wearables)
                if (i == (int)BakeType.Skirt && (!Wearables.ContainsKey(WearableType.Skirt) || Wearables[WearableType.Skirt].Asset.AssetID == UUID.Zero))
                {
                    continue;
                }

                RebakeLayer((BakeType)i);
            }
            if (PendingUploads.Count > 0)
            {
                CachedResponseEvent.WaitOne();
            }
            Client.Assets.OnAssetUploaded -= Assets_OnAssetUploaded;
            SendAgentSetAppearance();
        }

        /// <summary>
        /// Ask the server what textures our avatar is currently wearing
        /// </summary>
        public void SendAgentWearablesRequest()
        {
            AgentWearablesRequestPacket request = new AgentWearablesRequestPacket();
            request.AgentData.AgentID = Client.Self.AgentID;
            request.AgentData.SessionID = Client.Self.SessionID;

            Client.Network.SendPacket(request);
        }


        private void AgentWearablesUpdateHandler(Packet packet, Simulator simulator)
        {
            bool changed = false;
            AgentWearablesUpdatePacket update = (AgentWearablesUpdatePacket)packet;
            // Lock to prevent a race condition with multiple AgentWearables packets
            lock (WearablesRequestEvent)
            TestLock(Wearables); lock (Wearables)
            {
                #region Test if anything changed in this update

                for (int i = 0; i < update.WearableData.Length; i++)
                {
                    AgentWearablesUpdatePacket.WearableDataBlock block = update.WearableData[i];

                    if (block.AssetID != UUID.Zero)
                    {
                        WearableData wearable;
                        if (Wearables.TryGetValue((WearableType)block.WearableType, out wearable))
                        {
                            if (wearable.AssetID != block.AssetID || wearable.ItemID != block.ItemID)
                            {
                                // A different wearable is now set for this index
                                changed = true;
                                break;
                            }
                        }
                        else
                        {
                            // A wearable is now set for this index
                            changed = true;
                            break;
                        }
                    }
                    else if (Wearables.ContainsKey((WearableType)block.WearableType))
                    {
                        // This index is now empty
                        changed = true;
                        break;
                    }
                }

                #endregion Test if anything changed in this update

                if (changed)
                {
                    DebugLog("New wearables received in AgentWearablesUpdate", Client);
                    Wearables.Clear();

                    for (int i = 0; i < update.WearableData.Length; i++)
                    {
                        AgentWearablesUpdatePacket.WearableDataBlock block = update.WearableData[i];

                        if (block.AssetID != UUID.Zero)
                        {
                            WearableType type = (WearableType)block.WearableType;

                            WearableData data = new WearableData();
                            data.Item = new InventoryWearable(update.WearableData[i].ItemID);
                            data.Item.WearableType = type;
                            data.Item.AssetType = WearableTypeToAssetType(type);
                            data.Item.AssetUUID = update.WearableData[i].AssetID;
                            data.Asset = null;
                            data.AssetID = block.AssetID;
                            data.AssetType = WearableTypeToAssetType(type);
                            data.ItemID = block.ItemID;
                            data.WearableType = type;

                            // Add this wearable to our collection
                            Wearables[type] = data;
                        }
                    }
                }
                else
                {
                    DebugLog("Duplicate AgentWearablesUpdate received, discarding",Client);
                }
            }

            if (changed)
            {
                // Fire the callback
                AgentWearablesCallback callback = OnAgentWearables;
                if (callback != null)
                {
                    try { callback(); }
                    catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client, ex); }
                }
            }
            WearablesRequestEvent.Set();
        }

        static void DebugLog(string s, GridClient args)
        {
          Console.WriteLine(s);
        }

        private void SendAgentSetAppearance()
        {
            const bool UseExtraParams = false;
            AgentSetAppearancePacket set = new AgentSetAppearancePacket();
            set.AgentData.AgentID = Client.Self.AgentID;
            set.AgentData.SessionID = Client.Self.SessionID;
            set.AgentData.SerialNum = SetAppearanceSerialNum++;
            set.VisualParam = new AgentSetAppearancePacket.VisualParamBlock[UseExtraParams ? 258 : 218];

            float AgentSizeVPHeight = 0.0f;
            float AgentSizeVPHeelHeight = 0.0f;
            float AgentSizeVPPlatformHeight = 0.0f;
            float AgentSizeVPHeadSize = 0.5f;
            float AgentSizeVPLegLength = 0.0f;
            float AgentSizeVPNeckLength = 0.0f;
            float AgentSizeVPHipLength = 0.0f;

            TestLock(Wearables); lock (Wearables)
            {
                // Only for debugging output
                int count = 0, vpIndex = 0;

                // Build the visual param array
                foreach (KeyValuePair<int, VisualParam> kvp in VisualParams.Params)
                {
                    VisualParam vp = kvp.Value;

                    // Only Group-0 parameters are sent in AgentSetAppearance packets
                    if (vp.Group == 0)
                    {
                        set.VisualParam[vpIndex] = new AgentSetAppearancePacket.VisualParamBlock();
                        set.VisualParam[vpIndex].ParamValue = Utils.FloatToByte(vp.DefaultValue, vp.MinValue, vp.MaxValue);

                        // Try and find this value in our collection of downloaded wearables
                        foreach (WearableData data in Wearables.Values)
                        {
                            if (data.Asset != null && data.Asset.Params.ContainsKey(vp.ParamID))
                            {
                                set.VisualParam[vpIndex].ParamValue = Utils.FloatToByte(data.Asset.Params[vp.ParamID], vp.MinValue, vp.MaxValue);
                                count++;

                                switch (vp.ParamID)
                                {
                                    case 33:
                                        AgentSizeVPHeight = data.Asset.Params[vp.ParamID];
                                        break;
                                    case 198:
                                        AgentSizeVPHeelHeight = data.Asset.Params[vp.ParamID];
                                        break;
                                    case 503:
                                        AgentSizeVPPlatformHeight = data.Asset.Params[vp.ParamID];
                                        break;
                                    case 682:
                                        AgentSizeVPHeadSize = data.Asset.Params[vp.ParamID];
                                        break;
                                    case 692:
                                        AgentSizeVPLegLength = data.Asset.Params[vp.ParamID];
                                        break;
                                    case 756:
                                        AgentSizeVPNeckLength = data.Asset.Params[vp.ParamID];
                                        break;
                                    case 842:
                                        AgentSizeVPHipLength = data.Asset.Params[vp.ParamID];
                                        break;
                                }
                                break;
                            }
                        }

                        ++vpIndex;
                    }
                    else
                    {
                        if (UseExtraParams)
                        {
                            if ((vp.Wearable != null) && vp.Wearable.Equals("shape"))
                            {
                                set.VisualParam[vpIndex] = new AgentSetAppearancePacket.VisualParamBlock();
                                set.VisualParam[vpIndex].ParamValue = Utils.FloatToByte(vp.DefaultValue, vp.MinValue,
                                                                                        vp.MaxValue);
                                ++vpIndex;
                            }
                        }
                    }
                }

                // Build the texture entry for our agent
                Primitive.TextureEntry te = new Primitive.TextureEntry(DEFAULT_AVATAR_TEXTURE);

                // Put our AgentTextures array in to TextureEntry
                lock (AgentTextures)
                {
                    for (uint i = 0; i < AgentTextures.Length; i++)
                    {
                        if (AgentTextures[i] != UUID.Zero)
                        {
                            Primitive.TextureEntryFace face = te.CreateFace(i);
                            face.TextureID = AgentTextures[i];
                        }
                    }
                }

                foreach (WearableData data in Wearables.Values)
                {
                    if (data.Asset != null)
                    {
                        foreach (KeyValuePair<AvatarTextureIndex, UUID> texture in data.Asset.Textures)
                        {
                            Primitive.TextureEntryFace face = te.CreateFace((uint)texture.Key);
                            face.TextureID = texture.Value;

                            DebugLog("Setting agent texture " + ((AvatarTextureIndex)texture.Key).ToString() + " to " +
                                texture.Value.ToString(), Client);
                        }
                    }
                }

                // Set the packet TextureEntry
                set.ObjectData.TextureEntry = te.GetBytes();
            }

            // FIXME: Our hackish algorithm is making squished avatars. See
            // http://www.OpenMetaverse.org/wiki/Agent_Size for discussion of the correct algorithm
            //float height = Utils.ByteToFloat(set.VisualParam[33].ParamValue, VisualParams.Params[33].MinValue,
            //    VisualParams.Params[33].MaxValue);

            // Takes into account the Shoe Heel/Platform offsets but not the Head Size Offset.  But seems to work.
            double AgentSizeBase = 1.706;

            // The calculation for the Head Size scalar may be incorrect.  But seems to work.
            double AgentHeight = AgentSizeBase + (AgentSizeVPLegLength * .1918) + (AgentSizeVPHipLength * .0375) +
                (AgentSizeVPHeight * .12022) + (AgentSizeVPHeadSize * .01117) + (AgentSizeVPNeckLength * .038) +
                (AgentSizeVPHeelHeight * .08) + (AgentSizeVPPlatformHeight * .07);

            set.AgentData.Size = new Vector3(0.45f, 0.6f, (float)AgentHeight);

            // TODO: Account for not having all the textures baked yet
            set.WearableData = new AgentSetAppearancePacket.WearableDataBlock[BAKED_TEXTURE_COUNT];

            // Build hashes for each of the bake layers from the individual components
            for (int bakedIndex = 0; bakedIndex < BAKED_TEXTURE_COUNT; bakedIndex++)
            {
                UUID hash = new UUID();

                for (int wearableIndex = 0; wearableIndex < WEARABLES_PER_LAYER; wearableIndex++)
                {
                    WearableType type = WEARABLE_BAKE_MAP[bakedIndex][wearableIndex];
                    UUID assetID = GetWearableAsset(type);

                    // Build a hash of all the texture asset IDs in this baking layer
                    if (assetID != UUID.Zero) hash ^= assetID;
                }

                if (hash != UUID.Zero)
                {
                    // Hash with our secret value for this baked layer
                    hash ^= BAKED_TEXTURE_HASH[bakedIndex];
                }

                // Tell the server what cached texture assetID to use for each bake layer
                set.WearableData[bakedIndex] = new AgentSetAppearancePacket.WearableDataBlock();
                set.WearableData[bakedIndex].TextureIndex = (byte)bakedIndex;
                set.WearableData[bakedIndex].CacheID = hash;
                DebugLog("Setting baked agent texture hash " + ((BakeType)bakedIndex).ToString() + " to " + hash, Client);

            }

            // Finally, send the packet
            Client.Network.SendPacket(set);
        }


        private void SendAgentIsNowWearing()
        {
            DebugLog("SendAgentIsNowWearing()", Client);

            AgentIsNowWearingPacket wearing = new AgentIsNowWearingPacket();
            wearing.AgentData.AgentID = Client.Self.AgentID;
            wearing.AgentData.SessionID = Client.Self.SessionID;
            wearing.WearableData = new AgentIsNowWearingPacket.WearableDataBlock[WEARABLE_COUNT];

            for (int i = 0; i < WEARABLE_COUNT; i++)
            {
                WearableType type = (WearableType)i;
                wearing.WearableData[i] = new AgentIsNowWearingPacket.WearableDataBlock();
                wearing.WearableData[i].WearableType = (byte)i;

                TestLock(Wearables); lock (Wearables) 
                if (Wearables.ContainsKey(type))
                    wearing.WearableData[i].ItemID = Wearables[type].Item.UUID;
                else
                    wearing.WearableData[i].ItemID = UUID.Zero;
            }

            Client.Network.SendPacket(wearing);
        }

        private AvatarTextureIndex BakeTypeToAgentTextureIndex(BakeType index)
        {
            switch (index)
            {
                case BakeType.Head:
                    return AvatarTextureIndex.HeadBaked;
                case BakeType.UpperBody:
                    return AvatarTextureIndex.UpperBaked;
                case BakeType.LowerBody:
                    return AvatarTextureIndex.LowerBaked;
                case BakeType.Eyes:
                    return AvatarTextureIndex.EyesBaked;
                case BakeType.Skirt:
                    return AvatarTextureIndex.SkirtBaked;
                case BakeType.Hair:
                    return AvatarTextureIndex.HairBaked;
                default:
                    return AvatarTextureIndex.Unknown;
            }
        }

        private void DownloadWearableAssets()
        {
            TestLock(Wearables); lock (Wearables)
            {
                foreach (KeyValuePair<WearableType, WearableData> kvp in Wearables)
                {
                    DebugLog("Requesting asset for wearable item " + kvp.Value.Item.WearableType + " (" + kvp.Value.Item.AssetUUID + ")", Client);
                    AssetDownloads.Enqueue(new PendingAssetDownload(kvp.Value.Item.AssetUUID, kvp.Value.Item.AssetType));
                }
            }

            if (AssetDownloads.Count > 0)
            {
                PendingAssetDownload pad = AssetDownloads.Dequeue();
                Assets.RequestAsset(pad.Id, pad.Type, true, Assets_OnAssetReceived);
            }
        }

        private void RebakeLayer(AvatarTextureIndex index)
        {
            RebakeLayer(Baker.BakeTypeFor(index));
        }

        private void RebakeLayer(BakeType bakeType)
        {
            Dictionary<int, float> paramValues;

            // Build a dictionary of appearance parameter indices and values from the wearables
            paramValues=MakeParamValues();    

            Baker bake = new Baker(Client, bakeType, 0, paramValues);
                
            for (int ii = 0; ii < AVATAR_TEXTURE_COUNT; ii++)
            { 
                if(bakeType==Baker.BakeTypeFor((AvatarTextureIndex)ii) && AgentAssets[ii]!=null)
                {
                    Logger.Log("Adding asset "+AgentAssets[ii].AssetID.ToString()+" to baker",Helpers.LogLevel.Debug);
                    bake.AddTexture((AvatarTextureIndex)ii,(AssetTexture)AgentAssets[ii],true);
                }
            }
        
            UploadBake(bake);
        }

        private void UploadBake(Baker bake)
        {
            lock (PendingUploads)
            {
                if(PendingUploads.ContainsKey(bake.BakedTexture.AssetID))
                {
                    Logger.Log("UploadBake(): Skipping Asset id "+bake.BakedTexture.AssetID.ToString()+" Already in progress",Helpers.LogLevel.Info, Client);
                    return;
                }

                    // Upload the completed layer data and Add it to a pending uploads list
                    UUID id=Assets.RequestUpload(bake.BakedTexture, true);
                    PendingUploads.Add(UUID.Combine(id, Client.Self.SecureSessionID), BakeTypeToAgentTextureIndex(bake.BakeType));
            }

            DebugLog(String.Format("Bake {0} completed. Uploading asset {1}", bake.BakeType,
                bake.BakedTexture.AssetID.ToString()), Client);

        }

        private int AddImageDownload(AvatarTextureIndex index)
        {
            UUID image = AgentTextures[(int)index];

            if (image != UUID.Zero)
            {
                if (!ImageDownloads.ContainsKey(image))
                {
                    DebugLog("Downloading layer " + index.ToString(), Client);
                    ImageDownloads.Add(image, index);
                }

                return 1;
            }

            return 0;
        }
        
        private Dictionary<int, float> MakeParamValues()
        {    
             Dictionary<int, float> paramValues = new Dictionary<int, float>(VisualParams.Params.Count);
            
            TestLock(Wearables); lock (Wearables)
            {
                foreach (KeyValuePair<int,VisualParam> kvp in VisualParams.Params)
                {
                    // Only Group-0 parameters are sent in AgentSetAppearance packets
                    if (kvp.Value.Group == 0)
                    {
                        bool found = false;
                        VisualParam vp = kvp.Value;

                        // Try and find this value in our collection of downloaded wearables
                        foreach (WearableData data in Wearables.Values)
                        {
                            if (data.Asset!=null && data.Asset.Params.ContainsKey(vp.ParamID))
                            {
                                paramValues.Add(vp.ParamID, data.Asset.Params[vp.ParamID]);
                                found = true;
                                break;
                            }
                        }

                        // Use a default value if we don't have one set for it
                        if (!found) paramValues.Add(vp.ParamID, vp.DefaultValue);
                    }
                }
            }
            return paramValues;
        }

        private void TestLock(Object dictionary)
        {
            bool b = Monitor.TryEnter(dictionary, 3000);
            if (b)
            {
                Monitor.Exit(dictionary);
            } else
            {
             Console.WriteLine("Waiting...");   
            }

        }

        private int AddImagesToDownload(BakeType bakeType)
        {
            int imageCount = 0;

            // Download all of the images in this layer
            switch (bakeType)
            {
                case BakeType.Head:
                    lock (ImageDownloads)
                    {
                        imageCount += AddImageDownload(AvatarTextureIndex.HeadBodypaint);
                        //imageCount += AddImageDownload(AvatarTextureIndex.Hair);
                    }
                    break;
                case BakeType.UpperBody:
                    lock (ImageDownloads)
                    {
                        imageCount += AddImageDownload(AvatarTextureIndex.UpperBodypaint);
                        imageCount += AddImageDownload(AvatarTextureIndex.UpperGloves);
                        imageCount += AddImageDownload(AvatarTextureIndex.UpperUndershirt);
                        imageCount += AddImageDownload(AvatarTextureIndex.UpperShirt);
                        imageCount += AddImageDownload(AvatarTextureIndex.UpperJacket);
                    }
                    break;
                case BakeType.LowerBody:
                    lock (ImageDownloads)
                    {
                       imageCount += AddImageDownload(AvatarTextureIndex.LowerBodypaint);
                       imageCount += AddImageDownload(AvatarTextureIndex.LowerUnderpants);
                       imageCount += AddImageDownload(AvatarTextureIndex.LowerSocks);
                       imageCount += AddImageDownload(AvatarTextureIndex.LowerShoes);
                       imageCount += AddImageDownload(AvatarTextureIndex.LowerPants);
                       imageCount += AddImageDownload(AvatarTextureIndex.LowerJacket);
                    }
                    break;
                case BakeType.Eyes:
                    lock (ImageDownloads)
                    {
                        imageCount += AddImageDownload(AvatarTextureIndex.EyesIris);
                    }
                    break;
                case BakeType.Skirt:
                    if (Wearables.ContainsKey(WearableType.Skirt))
                    {
                        lock (ImageDownloads)
                        {
                            imageCount += AddImageDownload(AvatarTextureIndex.Skirt);
                        }
                    }
                    break;
                case BakeType.Hair:
                    lock (ImageDownloads)
                    {
                        imageCount += AddImageDownload(AvatarTextureIndex.Hair);
                    }
                    break;
                default:
                    Logger.Log("Unknown BakeType :" + bakeType.ToString(), Helpers.LogLevel.Warning, Client);
                    break;
            }
            
            return imageCount;
        }

        #region Callbacks
        
        private void RebakeAvatarTexturesHandler(Packet packet, Simulator simulator)
        {
            RebakeAvatarTexturesPacket data=(RebakeAvatarTexturesPacket)packet;
            Logger.Log("Request rebake for :"+data.TextureData.TextureID.ToString(),Helpers.LogLevel.Info);
            
            lock (AgentTextures)
            {
                Client.Assets.OnAssetUploaded += Assets_OnAssetUploaded;
                for (int i = 0; i < AgentTextures.Length; i++)
                {
                    if(AgentTextures[i] == data.TextureData.TextureID)
                    {
                        // Its one of our baked layers, rebake this one
                        RebakeLayer((AvatarTextureIndex)i);
                    }
                }
                if (PendingUploads.Count > 0)
                {
                    CachedResponseEvent.WaitOne();
                }
                Client.Assets.OnAssetUploaded -= Assets_OnAssetUploaded;
                SendAgentSetAppearance();
            }
        }

        private void AgentCachedTextureResponseHandler(Packet packet, Simulator simulator)
        {
            DebugLog("AgentCachedTextureResponseHandler()", Client);
            
            AgentCachedTextureResponsePacket response = (AgentCachedTextureResponsePacket)packet;

            lock (AgentTextures)
            {
                //If we are here then the user has tried to wear stuff or we are at login
                // In either case the existing uploads of this class are very shortly going to be no good
                PendingUploads.Clear();

                foreach (AgentCachedTextureResponsePacket.WearableDataBlock block in response.WearableData)
                {
                    //UUID hash=new UUID();
                    // For each missing element we need to bake our own texture
                    DebugLog("Cache response, index: " + block.TextureIndex + ", ID: " +
                        block.TextureID.ToString(), Client);

                    // FIXME: Use this. Right now we treat baked images on other sims as if they were missing
                    string host = Utils.BytesToString(block.HostName);
                    if (host.Length > 0) DebugLog("Cached bake exists on foreign host " + host, Client);

                    BakeType bakeType = (BakeType)block.TextureIndex;
                    
                    // Note, still should handle block.TextureID != UUID.Zero && host.Length == 0
                    // Not sure what we should do as yet with that.

                    // Convert the baked index to an AgentTexture index
                    if (block.TextureID != UUID.Zero && host.Length != 0)
                    {
                        AvatarTextureIndex index = BakeTypeToAgentTextureIndex(bakeType);
                        AgentTextures[(int)index] = block.TextureID;
                        AddImagesToDownload(bakeType); // We need to do this bit regardless for rebaking purposes later
                    }
                    else
                    {
                        int imageCount=AddImagesToDownload(bakeType);
                       
                        if (!PendingBakes.ContainsKey(bakeType))
                        {
                            DebugLog("Initializing " + bakeType.ToString() + " bake with " + imageCount + " textures", Client);
    
                            Dictionary<int, float> paramValues=MakeParamValues();
                            // Build a dictionary of appearance parameter indices and values from the wearables
                            
                            if (imageCount == 0)
                            {
                                // if there are no textures to download, we can bake right away and start the upload
                                Baker bake = new Baker(Client, bakeType, 0, paramValues);
                                UploadBake(bake);
                            }
                            else
                            {
                                lock (PendingBakes)
                                {
                                    Baker bake=new Baker(Client, bakeType, imageCount,paramValues);
                                    PendingBakes.Add(bakeType,bake);
                                }
                            }
                        }
                        else if (!PendingBakes.ContainsKey(bakeType))
                        {
                            Logger.Log("No cached bake for " + bakeType.ToString() + " and no textures for that " +
                                "layer, this is an unhandled case", Helpers.LogLevel.Error, Client);
                        }
                    }
                }
            }

            if (ImageDownloads.Count > 0)
            {
                lock (ImageDownloads)
                {
                    List<UUID> imgKeys = new List<UUID>(ImageDownloads.Keys);
                    foreach (UUID image in imgKeys)
                    {
                        // Download all the images we need for baking
                        Assets.RequestImage(image, ImageType.Normal, Assets_OnImageReceived);
                    }
                }
            }
            else
            {
                CachedResponseEvent.Set();
            }
        }

        private void Assets_OnAssetReceived(AssetDownload download, Asset asset)
        {
            TestLock(Wearables); lock (Wearables)
            {
                // Check if this is a wearable we were waiting on
                foreach (KeyValuePair<WearableType,WearableData> kvp in Wearables)
                {
                    if (kvp.Value.Item.AssetUUID == download.AssetID)
                    {
                        // Make sure the download succeeded
                        if (download.Success)
                        {
                            kvp.Value.Asset = (AssetWearable)asset;

                            DebugLog("Downloaded wearable asset " + kvp.Value.Asset.Name, Client);

                            if (!kvp.Value.Asset.Decode())
                            {
                                Logger.Log("Failed to decode asset:" + Environment.NewLine +
                                    Utils.BytesToString(asset.AssetData), Helpers.LogLevel.Error, Client);
                            }

                            lock (AgentTextures)
                            {
                                foreach (KeyValuePair<AvatarTextureIndex, UUID> texture in kvp.Value.Asset.Textures)
                                {
                                    if (texture.Value != DEFAULT_AVATAR_TEXTURE) // this texture is not meant to be displayed
                                    {
                                        DebugLog("Setting " + texture.Key + " to " + texture.Value, Client);
                                        AgentTextures[(int)texture.Key] = texture.Value;
                                    }
                                }
                            }
                        }
                        else
                        {
                            Logger.Log("Wearable " + kvp.Key + "(" + download.AssetID.ToString() + ") failed to download, " +
                                download.Status.ToString(), Helpers.LogLevel.Warning, Client);
                        }

                        break;
                    }
                }
            }

            if (AssetDownloads.Count > 0)
            {
                // Dowload the next wearable in line
                PendingAssetDownload pad = AssetDownloads.Dequeue();
                Assets.RequestAsset(pad.Id, pad.Type, true, Assets_OnAssetReceived);
            }
            else
            {
                // Everything is downloaded
                if (OnAgentWearables != null)
                {
                    try { OnAgentWearables(); }
                    catch (Exception e) { Logger.Log(e.Message, Helpers.LogLevel.Error, Client, e); }
                }

                WearablesDownloadedEvent.Set();
            }
        }

        private void Assets_OnImageReceived(TextureRequestState state, AssetTexture assetTexture)
        {
            lock (ImageDownloads)
            {
                if (ImageDownloads.ContainsKey(assetTexture.AssetID))
                {
                    ImageDownloads.Remove(assetTexture.AssetID);

                    // NOTE: This image may occupy more than one AvatarTextureIndex! We must finish this loop
                    for (int at = 0; at < AgentTextures.Length; at++)
                    {
                        if (AgentTextures[at] == assetTexture.AssetID)
                        {
                            AvatarTextureIndex index = (AvatarTextureIndex)at;
                            BakeType type = Baker.BakeTypeFor(index);

                            //BinaryWriter writer = new BinaryWriter(File.Create("wearable_" + index.ToString() + "_" + image.ID.ToString() + ".jp2"));
                            //writer.Write(image.AssetData);
                            //writer.Close();

                            bool baked = false;
                            AgentAssets[at]=assetTexture; //Cache this asset for rebaking, todo this could be better rather than dropping in this list.

                            if (PendingBakes.ContainsKey(type))
                            {
                                if (state == TextureRequestState.Finished)
                                {
                                    DebugLog("Finished downloading texture for " + index.ToString(), Client);
                                    OpenJPEG.DecodeToImage(assetTexture.AssetData, out assetTexture.Image);
                                    baked = PendingBakes[type].AddTexture(index, assetTexture, false);
                                }
                                else
                                {
                                    Logger.Log("Texture for " + index + " failed to download, " +
                                        "bake will be incomplete", Helpers.LogLevel.Warning, Client);
                                    baked = PendingBakes[type].MissingTexture(index);
                                }
                            }

                            if (baked)
                            {
                                UploadBake(PendingBakes[type]);
                                PendingBakes.Remove(type);
                            }

                            if (ImageDownloads.Count == 0 && PendingUploads.Count == 0)
                            {
                                // This is a failsafe catch, as the upload completed callback should normally 
                                // be triggering the event
                                DebugLog("No pending downloads or uploads detected in OnImageReceived", Client);
                                CachedResponseEvent.Set();
                            }
                            else
                            {
                                DebugLog("Pending uploads: " + PendingUploads.Count + ", pending downloads: " +
                                    ImageDownloads.Count, Client);
                            }

                        }
                    }
                }
                else
                {
                    Logger.Log("Received an image download callback for an image we did not request " + assetTexture.AssetID,
                        Helpers.LogLevel.Warning, Client);
                }
            }
        }

        private void Assets_OnAssetUploaded(AssetUpload upload)
        {
            lock (PendingUploads)
            {
                if (PendingUploads.ContainsKey(upload.AssetID))
                {
                    if (upload.Success)
                    {
                        // Setup the TextureEntry with the new baked upload
                        AvatarTextureIndex index = PendingUploads[upload.AssetID];
                        int u = (int) index;
                        if (u < 0)
                        {
                            DebugLog("Invalid Index, AgentTextures " + index.ToString() + " set to " +
                                upload.AssetID.ToString(), Client);
                        }
                        else
                        {
                            AgentTextures[u] = upload.AssetID;
                        }
                        DebugLog("Upload complete, AgentTextures " + index.ToString() + " set to " + 
                            upload.AssetID.ToString(), Client);
                    }
                    else
                    {
                        Logger.Log("Asset upload " + upload.AssetID.ToString() + " failed", 
                            Helpers.LogLevel.Warning, Client);
                    }

                    PendingUploads.Remove(upload.AssetID);

                    DebugLog("Pending uploads: " + PendingUploads.Count + ", pending downloads: " +
                        ImageDownloads.Count, Client);

                    if (PendingUploads.Count == 0 && ImageDownloads.Count == 0)
                    {
                        DebugLog("All pending image downloads and uploads complete", Client);

                        CachedResponseEvent.Set();
                    }
                }
                else
                {
                    // TEMP
                    DebugLog("Upload " + upload.AssetID.ToString() + " was not found in PendingUploads", Client);
                }
            }
        }

        /// <summary>
        /// Terminate any wait handles when the network layer disconnects
        /// </summary>
        private void Network_OnDisconnected(NetworkManager.DisconnectType reason, string message)
        {
            WearablesRequestEvent.Set();
            WearablesDownloadedEvent.Set();
            CachedResponseEvent.Set();
            UpdateEvent.Set();
        }

        #endregion Callbacks

        public void RequestSetAppearance(bool p)
        {
            SetPreviousAppearance(p);
        }

        /// <summary>
        /// Checks if an inventory item is currently being worn
        /// </summary>
        /// <param name="item">The inventory item to check against the agent
        /// wearables</param>
        /// <returns>The WearableType slot that the item is being worn in,
        /// or WearbleType.Invalid if it is not currently being worn</returns>
        public WearableType IsItemWorn(InventoryItem item)
        {
            TestLock(Wearables); lock (Wearables)
            {
                foreach (KeyValuePair<WearableType, WearableData> entry in Wearables)
                {
                    if (entry.Value.ItemID == item.UUID)
                        return entry.Key;
                }
            }

            return WearableType.Invalid;
        }

    }
}
