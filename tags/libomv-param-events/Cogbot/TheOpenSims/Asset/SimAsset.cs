using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    abstract public class SimAsset : BotMentalAspect
    {

        static public void WriteLine(string s, params object[] args)
        {
            SimAssetStore.WriteLine(s,args);
        }
        //   public abstract bool NeedsRequest{ get; set;}
        //     public abstract bool SameAsset(SimAsset animation);
        //    public abstract bool HasData();
        //     public abstract float Length { get; }
        //   public abstract bool IsContinuousEffect { get; }
        protected Asset _ServerAsset;

        public SimAssetStore Store
        {
            get
            {
                if (WorldObjects.GridMaster != null && WorldObjects.GridMaster.SimAssetSystem != null)
                    return WorldObjects.GridMaster.SimAssetSystem;
                return SimAssetStore.TheStore;
            }
        }

        public bool PullServerAsset = true;
        public virtual Asset ServerAsset
        {
            get
            {
                if (_ServerAsset == null)
                {
                    if (PullServerAsset && AssetID!=UUID.Zero)
                    {
                        GridClient c = Store.Client;
                        if (c != null && c.Network.Connected)
                        {
                            PullServerAsset = false;
                            c.Assets.RequestAsset(AssetID, AssetType, true, On_AssetDownloaded);
                        }
                        else
                        {              
                           // PullServerAsset = false;
                            Store.taskQueue.Enqueue(() =>
                                                        {
                                                            Thread.Sleep(10);                                                       
                                                            var v = ServerAsset;
                                                        });
                        }
                    }
                }
                return _ServerAsset;
            }
            set
            {
                _ServerAsset = value;
                if (value != null)
                {
                    AssetType = value.AssetType;
                    AssetID = value.AssetID;
                }
            }
        }

        private void On_AssetDownloaded(AssetDownload transfer, Asset asset)
        {
            PullServerAsset = false;
            //if (!transfer.Success) PullServerAsset = true;
            _ServerAsset = asset;
            GuessAssetName();
        }

        readonly public List<string> Meanings = new List<string>();
        public string Comment;
        public InventoryItem Item;
        public AssetType AssetType = OpenMetaverse.AssetType.Unknown;
        //protected abstract void SaveFile(string tmpname);
        //public abstract byte[] AssetData { get; set; }
        protected abstract string GuessAssetName();

        public SimAsset(UUID anim, String name)
        {
            AssetID = anim;
            Name = name;
            //lock (SimAssetStore.SimAssets)
                SimAssetStore.SimAssets.Add(this);
        }
        public SimAsset(UUID anim, String name, AssetType type)
        {
            AssetID = anim;
            Name = name;
            AssetType = type;
            //lock (SimAssetStore.SimAssets) 
                SimAssetStore.SimAssets.Add(this);
        }

        public bool Matches(String s)
        {
            return ToString().ToLower().Contains(s.ToLower());
        }

        public virtual string DebugInfo()
        {
            return ToString();
        }

        public override string ToString()
        {
            string s = String.Empty;
            lock (_Name)
                foreach (string n in _Name)
                {
                    s += " " + n;
                }
            lock (AssetIDs)
                foreach (UUID n in AssetIDs)
                {
                    s += " " + n;
                }
            lock (Meanings)
                if (Meanings.Count > 0)
                {
                    s += "[";
                    foreach (string n in Meanings)
                    {
                        s += " " + n;
                    }
                    s += "]";
                }
            if (!HasData()) s += " NODATA";
            return s.TrimStart();
        }

        public List<UUID> AssetIDs = new List<UUID>();

        public List<string> _Name = new List<string>();
        public string Name
        {
            get
            {
                if (_Name.Count == 0)
                {
                    // InventoryFolder AF = (InventoryFolder) Client.Inventory.Store[Client.AnimationFolder];

                    //InventoryItem item = InventoryManager.CreateInventoryItem(InventoryType.Animation, uUID);
                    //item.InventoryType = InventoryType.Animation;
                    //item.AssetUUID = uUID;
                    //item.AssetType = AssetType.Animation;
                    //item.Name = "Animation" + uUID.ToString();
                    //item.ParentUUID = Client.AnimationFolder; // FindFolderForType(item.AssetType);
                    //item.CreationDate = new DateTime();
                    String ExpressionName = GuessAssetName();
                    if (!string.IsNullOrEmpty(ExpressionName))
                    {
                        _Name.Add(ExpressionName);
                        return ExpressionName;
                    }
                    string tmpname = "" + AssetID;
                    if (AssetIDs.Count == 1)
                    {
                        SaveFile(tmpname);
                    }
                    return tmpname;
                }
                return _Name[0];
            }
            set
            {
                if (value == null) return;
                if (!_Name.Contains(value))
                    _Name.Add(value);
                if (!SimAssetStore.nameAsset.ContainsKey(value))
                    SimAssetStore.nameAsset[value] = this;
            }
        }

        public void AddName(string n)
        {
            if (!string.IsNullOrEmpty(n))
            {
                if (!_Name.Contains(n)) _Name.Add(n);
            }
        }


        public UUID AssetID
        {
            get
            {
                lock (AssetIDs)
                {
                    if (AssetIDs.Count == 0) return UUID.Zero;
                    return AssetIDs[0];
                }
            }
            set
            {
                if (value == UUID.Zero) return;
                //lock (SimAssetStore.uuidAsset)
                    SimAssetStore.uuidAsset[value] = this;
                lock (AssetIDs)
                {
                    if (AssetIDs.Contains(value)) return;
                    AssetIDs.Add(value);
                }
            }
        }

        protected string UnknownName
        {
            get
            {
                if (Item!=null)
                {
                    return Item.Name;
                }
                return null;
            }
        }


        public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public ICollection<NamedParam> GetInfoMap()
        {
            return WorldObjects.GetMemberValues("", this);
        }

        internal bool IsIncomplete()
        {
            return !HasData() || AssetIDs.Count == 0 || _Name.Count == 0;
        }

        internal void AddType(string anims)
        {
            lock (Meanings)
            {
                if (!Meanings.Contains(anims))
                {
                    Meanings.Add(anims);
                    List<UUID> AMeanings = SimAssetStore.MeaningUUIDs(anims);
                    if (!AMeanings.Contains(AssetID))
                    {
                        AMeanings.Add(AssetID);
                    }

                }
            }
        }

        public string GetMeaning()
        {
            lock (Meanings) if (Meanings.Count > 0) return Meanings[Meanings.Count - 1];
            return null;
        }

        protected virtual void SaveFile(string tmpname)
        {
            //  if (!HasData()) return;
            //WriteLine("Not implemented save {0} file {1}", this, tmpname);
        }

        private byte[] _TypeData;
        public virtual byte[] AssetData
        {
            get
            {
                if (_TypeData != null) return _TypeData;
                if (ServerAsset == null) return null;
                return ServerAsset.AssetData;
            }
            set
            {
                if (_TypeData != null)
                {

                }
                _TypeData = value;
                if (ServerAsset == null)
                {
                    if (AssetID != UUID.Zero)
                    {
                        ServerAsset = CreateAssetWrapper(AssetType,AssetID,value);
                    }
                    return;
                }
                else
                {
                    ServerAsset.AssetData = value;
                }
            }
        }

        private Asset CreateAssetWrapper(AssetType type, UUID uuid, byte[] data)
        {
            Asset asset;

            switch (type)
            {
                case AssetType.Animation:
                    asset = new AssetAnimation(uuid, data);
                    break;
                case AssetType.Gesture:
                    asset = new AssetGesture(uuid, data);
                    break;
                case AssetType.Landmark:
                    asset = new AssetLandmark(uuid, data);
                    break;
                case AssetType.Bodypart:
                    asset = new AssetBodypart(uuid, data);
                    break;
                case AssetType.Clothing:
                    asset = new AssetClothing(uuid, data);
                    break;
                case AssetType.LSLBytecode:
                    asset = new AssetScriptBinary(uuid, data);
                    break;
                case AssetType.LSLText:
                    asset = new AssetScriptText(uuid, data);
                    break;
                case AssetType.Notecard:
                    asset = new AssetNotecard(uuid, data);
                    break;
                case AssetType.Sound:
                    asset = new AssetSound(uuid, data);
                    break;
                case AssetType.Texture:
                    asset = new AssetTexture(uuid, data);
                    break;
                case AssetType.CallingCard:
                    asset = new AssetCallingCard(uuid, data);
                    break;
                default:
                    throw new NotImplementedException( "Unimplemented asset type: " + type);
            }
            return asset;
        }

        public virtual float Length
        {
            get { return 3;  }
        }

        public virtual bool IsContinuousEffect
        {
            get { return true; }
        }

        public virtual bool HasData()
        {
            return ServerAsset != null || _TypeData != null;
        }

        private bool _NeedsRequest = true;
        public virtual bool NeedsRequest
        {
            get
            {
                if (!_NeedsRequest) return false;
                if (HasData())
                {
                    _NeedsRequest = false;
                    return false;
                }
                return _NeedsRequest;
            }
            set { _NeedsRequest = value; }
        }

        public virtual bool SameAsset(SimAsset asset)
        {
            if (asset == null) return false;
            if (asset.AssetType != AssetType) return false;
            if (HasData())
            {

            }
            return false;
        }
    }
}