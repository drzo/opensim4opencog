using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.IO;
using cogbot.Actions.SimExport;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.StructuredData;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public partial class ImportCommand 
    {
        public class MissingItemInfo
        {
            public MemberInfo MemberName;
            public UUID MissingID;
            public string key;
            public AssetType AssetType = OpenMetaverse.AssetType.Unknown;

            public MissingItemInfo(MemberInfo name,UUID id)
            {
                MemberName = name;
                MissingID = id;
                key = MemberName.DeclaringType.Name + "." + MemberName.Name + "=" + MissingID;
                AssetType = GuessType(name);
            }

            private static AssetType GuessType(MemberInfo info)
            {
                string guess = info.DeclaringType.Name + "." + info.Name;
                guess = guess.Replace(".ID", "ID").Replace("GuestureStep", "").ToLower();
                if (guess.Contains("animationid")) return AssetType.Animation;
                if (guess.Contains("soundid")) return AssetType.Sound;
                if (guess.Contains("textureid")) return AssetType.Texture;
                if (guess.Contains("texture")) return AssetType.Texture;
                // synthetic assetTytpes for downloading
                if (guess.Contains("creatorin")) return AssetType.CallingCard;
                if (guess.Contains("creator")) return AssetType.CallingCard;
                if (guess.Contains("ownerid")) return AssetType.CallingCard;
                if (guess.Contains("groupid")) return AssetType.EnsembleStart;
                if (guess.Contains("AssetLandmark")) return AssetType.Landmark;
                return AssetType.Unknown;
            }

            public override int GetHashCode()
            {
                return key.GetHashCode();
            }
            public override bool Equals(object obj)
            {
                var other = obj as MissingItemInfo;
                return other != null && key == other.key;
            }
            public override sealed string ToString()
            {
                return key;
            }
        }

        public static bool UseUploadKnown = true;
        public static ManualResetEvent AssetUploaded = new ManualResetEvent(false);
        public static HashSet<MissingItemInfo> MissingFromExport = new HashSet<MissingItemInfo>();

        public sealed class ItemToCreate : UUIDChange
        {
            public override string ToString()
            {
                return assetType + " " + OldID + " -> " + NewID;
            }
            public override int GetHashCode()
            {
                return OldID.GetHashCode();
            }
            public override bool Equals(object obj)
            {
                var ptc = obj as ItemToCreate;
                return ptc != null && OldID == ptc.OldID;
            }
            public ItemToCreate(Asset item)
            {
                _OldItem = item;
                OldID = item.AssetID;
                assetType = item.AssetType;
                int pn;
                inventoryType = AssetTypeToInventoryType(assetType, out pn);
                PassNumber = pn;
                if (PassNumber == 1) CompletedReplaceAll = true;
                LoadProgressFile();
            }
            public ItemToCreate(UUID oldID, AssetType type)
            {
                OldID = oldID;
                assetType = type;
                int pn;
                inventoryType = AssetTypeToInventoryType(assetType, out pn);
                PassNumber = pn;
                if (PassNumber == 1) CompletedReplaceAll = true;
                LoadProgressFile();

            }
            public Asset OldItem
            {
                get
                {
                    if (_OldItem == null)
                    {
                        _OldItem = AssetManager.CreateAssetWrapper(assetType);
                        _OldItem.AssetData = AssetData;
                    }
                    return _OldItem;
                }
            }

            public string LLSDFilename
            {
                get
                {
                    string sfile = SimAsset.CFileName(OldID, assetType);
                    _afiler = ExportCommand.assetDumpDir + Path.GetFileName(sfile);
                    return _afiler;
                }
                set
                {
                    _afiler = value;
                }
            }

            public AssetType AssetType
            {
                get { return assetType; }
            }

            private SimAsset _rezed;
            private readonly AssetType assetType;
            private bool CompletedReplaceAll = false;
            //public uint NewLocalID;
            public Asset NewItem
            {
                get
                {
                    if (_NewItem != null) return _NewItem;
                    var r = Rezed;
                    if (r == null) return null;
                    return _NewItem = r.ServerAsset;
                }
            }

            public bool RezRequested = false;
            private Asset _OldItem;
            private Asset _NewItem;
            private string _progressFile;
            private string _afiler;
            private byte[] assetData;
            public UUID NewItemID = UUID.Zero;
            public UUID OldItemID = UUID.Zero;
            public UUID NewTaskID = UUID.Zero;
            public UUID OldTaskID = UUID.Zero;
            public InventoryItem InvItem;


            public string ProgressFile
            {
                get
                {
                    if (_progressFile == null)
                    {
                        _progressFile = ExportCommand.assetDumpDir + OldID + ".simasset";
                    }
                    return _progressFile;
                }
            }
            public SimAsset Rezed
            {
                get
                {
                    if (_rezed == null)
                    {
                        LoadProgressFile();
                        _rezed = SimAssetStore.FindAsset(NewID);
                    }
                    return _rezed;
                }
                set
                {
                    if (value == null) return;
                    RezRequested = true;
                    _rezed = value;
                    NewID = value.ID;
                    //NewLocalID = value.LocalID;
                    WriteProgress();
                }
            }

            private void LoadProgressFile()
            {
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    string importProgress = ProgressFile;
                    var item = ExportCommand.Exporting.GetInvItem(Running.Client, "" + OldID, GetAssetUploadsFolder());
                    if (item != null)
                    {
                        NewID = item.AssetUUID;
                        RezRequested = true;
                        NewItemID = item.UUID;
                        NewTaskID = Running.Client.Self.AgentID;
                        lock (ExportCommand.fileWriterLock) if (File.Exists(importProgress)) return;
                        WriteProgress();
                        return;
                    }
                    if (File.Exists(importProgress))
                    {
                        string data = File.ReadAllText(importProgress);
                        var sdata = data.Split(',');
                        NewID = UUIDFactory.GetUUID(sdata[1]);
                        // NewLocalID = uint.Parse(sdata[1]);
                        RezRequested = true;
                    }
                }
            }

            public InventoryManager Inventory
            {
                get
                {
                    return Running.Client.Inventory;
                }
            }

            readonly public InventoryType inventoryType;
            public int PassNumber;
            static public InventoryType AssetTypeToInventoryType(AssetType type, out int PassNumber)
            {
                if (false)
                {
                    var ret = Utils.StringToInventoryType(Utils.AssetTypeToString(type));
                    if (ret != InventoryType.Unknown)
                    {
                        PassNumber = 2;
                        return ret;
                    }                    
                }
                switch (type)
                {
                    case AssetType.Unknown:
                        break;
                    case AssetType.Sound:
                    case AssetType.SoundWAV:
                        PassNumber = 1;
                        return InventoryType.Sound;
                    case AssetType.ImageTGA:
                    case AssetType.ImageJPEG:
                    case AssetType.TextureTGA:
                    case AssetType.Texture:
                        PassNumber = 1;
                        return InventoryType.Texture;
                    case AssetType.Bodypart:
                    case AssetType.Clothing:
                        PassNumber = 2;
                        return InventoryType.Wearable;
                    case AssetType.TrashFolder:
                    case AssetType.SnapshotFolder:
                    case AssetType.LostAndFoundFolder:
                    case AssetType.Folder:
                    case AssetType.RootFolder:
                    case AssetType.FavoriteFolder:
                    case AssetType.CurrentOutfitFolder:
                    case AssetType.OutfitFolder:
                    case AssetType.MyOutfitsFolder:
                        PassNumber = 2;
                        return InventoryType.Folder;
                    case AssetType.Animation:
                        PassNumber = 1;
                        return InventoryType.Animation;
                    case AssetType.CallingCard:
                        PassNumber = 2;
                        return InventoryType.CallingCard;
                    case AssetType.Landmark:
                        PassNumber = 2;
                        return InventoryType.Landmark;
                    case AssetType.Object:
                        PassNumber = 2;
                        return InventoryType.Object;
                    case AssetType.Notecard:
                        PassNumber = 1;
                        return InventoryType.Notecard;
                    case AssetType.LSLText:
                        PassNumber = 1;
                        return InventoryType.LSL;
                    case AssetType.LSLBytecode:
                        PassNumber = 1;
                        return InventoryType.LSL;
                    case AssetType.Gesture:
                        PassNumber = 2;
                        return InventoryType.Gesture;
                    case AssetType.Mesh:
                        PassNumber = 2;
                        return InventoryType.Mesh;
                    case AssetType.Simstate:
                    case AssetType.Link:
                    case AssetType.LinkFolder:
                    case AssetType.EnsembleStart:
                    case AssetType.EnsembleEnd:
                    default:
                        throw new ArgumentOutOfRangeException("type");
                }
                PassNumber = 2;
                return InventoryType.Texture;
            }

            public string Description
            {
                get
                {
                    return DateTime.Now + " " + assetType;
                }
            }

            public bool UploadAssetData(bool storeLocal)
            {
                if (IsLocalScene)
                {
                    ReplaceAll();
                    RezRequested = true;
                    NewID = OldID;
                    _NewItem = OldItem;
                    return true;
                }
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    if (UseUploadKnown)
                    {
                        AssetUploaded.Reset();
                        if (assetType == AssetType.Notecard)
                        {
                            Inventory.RequestCreateItem(GetAssetUploadsFolder(), "" + OldID,
                                                                       Description, assetType, UUID.Zero,
                                                                       OpenMetaverse.InventoryType.Notecard,
                                                                       PermissionMask.All, UpdateInvItem);
                        }
                        else if (assetType == AssetType.Gesture)
                        {
                            Inventory.RequestCreateItem(GetAssetUploadsFolder(), "" + OldID,
                                                                       Description, assetType, UUID.Zero,
                                                                       OpenMetaverse.InventoryType.Gesture,
                                                                       PermissionMask.All, UpdateInvItem);
                           // (gesture.Sequence)
                        }
                        else if (assetType == AssetType.LSLText)
                        {
                            Inventory.RequestCreateItem(GetAssetUploadsFolder(), "" + OldID,
                                                                       Description, assetType, UUID.Zero,
                                                                       OpenMetaverse.InventoryType.LSL,
                                                                       PermissionMask.All, UpdateInvItem);

                        }
                        else if (assetType == AssetType.Landmark)
                        {
                            var decodeME = new AssetLandmark(UUID.Zero, AssetData);
                            decodeME.Decode();
                            byte[] bytes = AssetData;
                            Inventory.RequestCreateItem(GetAssetUploadsFolder(), "" + OldID,
                                                        "RegionID:" + decodeME.RegionID + ",Postion" + decodeME.Position +
                                                        "," + DateTime.Now, assetType, UUID.Zero,
                                                        OpenMetaverse.InventoryType.Landmark,
                                                        PermissionMask.All, UpdateInvItem);

                        }
                        else if (assetType == AssetType.CallingCard)
                        {
                            var decodeME = new AssetCallingCard(UUID.Zero, AssetData);
                            decodeME.Decode();
                            byte[] bytes = AssetData;
                            Inventory.RequestCreateItem(GetAssetUploadsFolder(), "" + OldID,
                                                        "AvatarID:" + decodeME.AvatarID + "," + DateTime.Now,
                                                        assetType, UUID.Zero,
                                                        OpenMetaverse.InventoryType.CallingCard,
                                                        PermissionMask.All, UpdateInvItem);


                        }
                        else
                        {
                            Inventory.RequestCreateItemFromAsset(AssetData, "" + OldID, Description,
                                                                                assetType, inventoryType,
                                                                                GetAssetUploadsFolder(),
                                                                                Permissions.FullPermissions,
                                                                                InvItemCreated);
                        }
                        if (!AssetUploaded.WaitOne(10000))
                        {
                            return false;
                        }
                        return true;
                    }
                    NewID = UUID.Combine(OldID, Running.Client.Self.SecureSessionID);
                    NewUUID2OBJECT[NewID] = this;
                    Running.Client.Assets.RequestUploadKnown(NewID, assetType, AssetData, storeLocal, OldID);
                    RezRequested = true;
                }
                return true;
            }

            private void UpdateInvItem(bool success1, InventoryItem item)
            {
                NewItemID = item.UUID;
                NewID = item.AssetUUID;
                InvItem = item;
                if (assetType == AssetType.LSLText)
                {
                    Inventory.RequestUpdateScriptAgentInventory(AssetData, NewItemID, true, UpdatedLSL);
                }
                else if (assetType == AssetType.Notecard)
                {
                    Inventory.RequestUpdateScriptAgentInventory(AssetData, NewItemID, false, UpdatedLSL);
                }
                else if (assetType == AssetType.Gesture)
                {
                    Inventory.RequestUploadGestureAsset(AssetData, NewItemID, InvItemCreated);
                }
                else if (assetType == AssetType.Landmark)
                {
                    InvItemCreated(true, "", item.UUID, item.AssetUUID);
                }
                else if (assetType == AssetType.CallingCard)
                {
                    InvItemCreated(true, "", item.UUID, item.AssetUUID);
                }
                this.NewTaskID = UUID.Zero;
                NewUUID2OBJECT[NewID] = this;
            }

            private void UpdatedLSL(bool uploadsuccess, string uploadstatus, bool compilesuccess, List<string> compilemessages, UUID itemid, UUID assetid)
            {
                InvItemCreated(uploadsuccess, uploadstatus, itemid, assetid);
            }

            private void InvItemCreated(bool success1, string status, UUID itemid, UUID assetid)
            {
                if (!success1)
                {
                    Running.Failure("!SUCCESS " + status + " ON " + this);
                    return;
                }
                NewID = assetid;
                this.NewItemID = itemid;
                this.NewTaskID = UUID.Zero;
                NewUUID2OBJECT[NewID] = this;
                ConfirmDLable();
            }

            public void WriteProgress()
            {
                // lock (ExportCommand.fileWriterLock) File.WriteAllText(ProgressFile, ProgressString);
            }

            private string ProgressString
            {
                get
                {
                    return "assetid," + uuidString(NewID) + "," + uuidString(OldID) +
                           "," + assetType + "," + uuidString(Running.Client.Self.SecureSessionID) +
                           ",itemid," + uuidString(NewItemID) + "," + uuidString(OldItemID) +
                           ",taskid," + uuidString(NewTaskID) + "," + uuidString(OldTaskID);
                }
            }

            public void WriteComplete()
            {
                ConfirmDLable();
            }

            private void ConfirmDLable()
            {
                Running.Client.Assets.RequestAsset(NewID, assetType, true, OnDownloaded);

            }


            public byte[] AssetData
            {
                get
                {
                    if (assetData == null)
                    {
                        assetData = File.ReadAllBytes(LLSDFilename);
                        ReplaceAll();
                    }
                    return assetData;
                }
            }

            public void OnDownloaded(AssetDownload transfer, Asset asset)
            {
                if (transfer.AssetID != NewID) return;
                if (assetType == AssetType.Texture && !asset.Decode())
                {
                    Difference(asset.AssetData, AssetData);
                    asset.AssetData = AssetData;
                    if (false)
                    {
                        Inventory.RemoveItem(NewItemID);
                        NewItemID = UUID.Zero;
                        NewID = UUID.Zero;
                        UploadAssetData(false);
                        /*Inventory.RequestCreateItemFromAsset(AssetData, "" + OldID, ProgressString,
                                                             assetType, inventoryType,
                                                             GetAssetUploadsFolder(),
                                                             Permissions.FullPermissions,
                                                             InvItemCreated);*/
                        //UpdateAsset(AssetData);
                        return;
                    }
                }
                if (!transfer.Success)
                {
                    Running.Error(ExportCommand.Exporting.LocalFailure, "bad transfer on " + this);
                }
                else
                {
                    WriteProgress();
                }
                _NewItem = asset;
                NewUUID2OBJECT[NewID] = this;
                UUID2OBJECT[OldID] = this;
                AssetUploaded.Set();
            }

            private void Difference(byte[] b1, byte[] b2)
            {
                if (b1.Length != b2.Length)
                {

                }
                for (int i = 0; i < b1.Length - 1; i++)
                {
                    byte b11 = b1[i];
                    byte b12 = b1[i + 1];
                    byte b21 = b2[i];
                    byte b22 = b2[i + 1];
                    if (b11 != b21)
                    {

                    }

                }
            }

            public void UpdateAsset(byte[] data)
            {
                Running.Client.Assets.RequestUploadKnown(NewID, assetType, data, false, OldID);
            }

            public override void ReplaceAll()
            {
                if (CompletedReplaceAll || PassNumber == 1) return;
                _OldItem = null;
                assetData = File.ReadAllBytes(LLSDFilename);
                var item = OldItem;
                try
                {
                    item.Decode0();
                }
                catch (Exception)
                {
                }
                ReplaceAllMembers(item, typeof(UUID), UUIDReplacer, MissingFromExport);
                try
                {
                    if (AssetType != AssetType.Object) item.Encode0();
                }
                catch (Exception)
                {
                }
                CompletedReplaceAll = true;
                assetData = item.AssetData;
            }
        }

        private void GleanUUIDsFrom(UUID uuid)
        {
            if (CogbotHelpers.IsNullOrZero(uuid)) return;
            foreach (var item0 in Client.Inventory.Store.GetContents(uuid))
            {
                UUID oldID;
                if (UUID.TryParse(item0.Name, out oldID))
                {
                    var item = item0 as InventoryItem;
                    if (item == null) continue;
                    ItemToCreate itc = FindItemToCreate(oldID, item.AssetType, true);
                    if (itc.RezRequested)
                    {

                    }
                }
            }
            lock (UUID2OBJECT) lock (MissingFromExport)
            {
                foreach (MissingItemInfo ur in LockInfo.CopyOf(MissingFromExport))
                {
                    if (UUID2OBJECT.ContainsKey(ur.MissingID))
                    {
                        Success("Not Missing anymore: " + ur);
                        MissingFromExport.Remove(ur);
                    }
                }
            }
        }

        private void Assets_AssetUploaded(object sender, AssetUploadEventArgs e)
        {
            if (UUID2OBJECT != null)
            {
                var u = e.Upload;
                if (u.Success)
                {
                    var itc = GetNew(u.AssetID) as ItemToCreate;
                    if (itc != null)
                    {
                        itc.WriteComplete();
                    }
                }
            }
        }

        private void UploadAllAssets(ImportSettings arglist)
        {
            bool sameIds = arglist.Contains("sameid");
            int uploaded = 0;
            int reuploaded = 0;
            int seenAsset = 0;
            HashSet<ItemToCreate> ItemsToCreate = LocalScene.Assets;
            bool alwayReupload = arglist.Contains("reup");
            Success("Uploading assets... sameIds=" + sameIds);
            foreach (var file in Directory.GetFiles(ExportCommand.assetDumpDir, "*.*"))
            {
                if (file.EndsWith(".object") || file.EndsWith(".simasset") || file.EndsWith(".rzi"))
                {
                    continue;
                }
                string sid = file;
                AssetType assetType = AssetType.Unknown;
                foreach (var ate in ArchiveConstants.ASSET_TYPE_TO_EXTENSION)
                {
                    string ateValue = ate.Value;
                    if (file.EndsWith(ateValue))
                    {
                        assetType = ate.Key;
                        sid = Path.GetFileName(file.Substring(0, file.Length - ateValue.Length));
                        break;
                    }
                }
                if (assetType == AssetType.Unknown)
                {
                    if (file.EndsWith(".jp2"))
                    {
                        assetType = AssetType.Texture;
                        sid = Path.GetFileName(file.Substring(0, file.Length - ".jp2".Length));
                    }
                    if (file.EndsWith(".ogg"))
                    {
                        assetType = AssetType.Sound;
                        sid = Path.GetFileName(file.Substring(0, file.Length - ".ogg".Length));
                    }
                }
                if (assetType == AssetType.Unknown)
                {
                    Failure("Cant guess assetyype for " + file);
                    continue;
                }
                seenAsset++;
                UUID oldID = UUID.Parse(sid);
                ItemToCreate itc = FindItemToCreate(oldID, assetType, sameIds);
                itc.LLSDFilename = file;
                ItemsToCreate.Add(itc);
            }
            foreach (ItemToCreate itc in ItemsToCreate)
            {
                if (itc.PassNumber == 1)
                {
                    EnsureUploaded(itc, alwayReupload, ref uploaded, ref reuploaded);
                }
            }
            foreach (ItemToCreate itc in ItemsToCreate)
            {
                if (itc.PassNumber == 2)
                {
                    itc.ReplaceAll();
                    EnsureUploaded(itc, alwayReupload, ref uploaded, ref reuploaded);
                }
            }
            Success("Uploaded assets=" + uploaded + " seenAssets=" + seenAsset + " reuploaded=" + reuploaded);
        }

        private void EnsureUploaded(ItemToCreate itc, bool alwayReupload, ref int uploaded, ref int reuploaded)
        {
            if (!itc.RezRequested)
            {
                itc.UploadAssetData(false);
                if (!CogbotHelpers.IsNullOrZero(itc.NewID))
                {
                    NewUUID2OBJECT[itc.NewID] = itc;
                }
                uploaded++;
            }
            else if (alwayReupload)
            {
                itc.UpdateAsset(itc.AssetData);
                reuploaded++;
            }
            if (!CogbotHelpers.IsNullOrZero(itc.NewID))
            {
                NewUUID2OBJECT[itc.NewID] = itc;
            }
        }

        private ItemToCreate FindItemToCreate(UUID uuid, AssetType assetType, bool sameIds)
        {
            var itc = GetOld(uuid) as ItemToCreate;
            if (itc == null)
            {
                itc = new ItemToCreate(uuid, assetType);
                UUID2OBJECT[uuid] = itc;
                if (!CogbotHelpers.IsNullOrZero(itc.NewID))
                {
                    NewUUID2OBJECT[itc.NewID] = itc;
                }
            }
            return itc;
        }

        static public UUID GetMissingFiller(AssetType type)
        {
            string id = "40400000-0404-0404-0404-0000000000" + String.Format("{0:X2}", (sbyte)type);
            return UUIDFactory.GetUUID(id);
        }
    }
}
