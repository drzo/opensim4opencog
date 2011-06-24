using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using cogbot.Listeners;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    abstract public class SimAssetV : BotMentalAspect
    {
        #region Implementation of BotMentalAspect
        public abstract FirstOrderTerm GetTerm();
        public abstract ICollection<NamedParam> GetInfoMap();
        #endregion

        //   public abstract bool NeedsRequest{ get; set;}
        //     public abstract bool SameAsset(SimAsset animation);
        //    public abstract bool HasData();
        //     public abstract float Length { get; }
        //   public abstract bool IsContinuousEffect { get; }
        public abstract bool HasData();
        public abstract Asset ServerAsset { get; set; }
        public abstract byte[] AssetData { get; set; }
        public abstract bool NeedsRequest { get; set; }

        public UUID ID
        {
            get
            {
                return AssetID;
            }
        }

        abstract public UUID AssetID { get; set; }
    }

    abstract public class SimAsset : SimAssetV, ITraceable
    {

        virtual public void WriteLine(string s, params object[] args)
        {
            SimAssetStore.WriteLine(DebugInfo() + ": " + s, args);
        }
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
        public bool PushAssetToServer = false;
        public bool Priority = true;
        private int Requests = 0;
        private DateTime when;
        public bool NeedsSave = true;
        protected static DateTime waitUntil = DateTime.Now;
        protected static object timerLock = new object();

        public override sealed Asset ServerAsset
        {
            get
            {
                lock (this)
                {
                    if (_ServerAsset != null)
                    {
                        return _ServerAsset;
                    }
                    if (this.AssetID == UUID.Zero)
                    {
                        return _ServerAsset;
                    }
                    if (!PullServerAsset)
                    {
                        if (Requests > 10)
                        {
                            WriteLine("Not PullServerAsset: Giving up on {0} after {1} tries", DebugInfo(), Requests);
                            return _ServerAsset;
                        }
                        return _ServerAsset;
                    }
                    else
                    {
                        PullServerAsset = false;
                    }
                    GridClient c = Store.Client;
                    string s = null;
                    if (Store.Client == null)
                    {
                        s = "needs client";
                    }
                    else
                    {
                        if (!c.Network.Connected)
                        {
                            s = "needs connect";
                            c.Network.SimConnected += foo;
                        }
                        else
                        {
                            s = "needs MakeRequest";
                            MakeRequest();
                        }
                    }
                    WriteLine("ASSET: " + s);
                }
                return _ServerAsset;
            }
            set
            {
                SetAsset(value);
            }
        }

        public void foo(object sender, SimConnectedEventArgs e)
        {
            BotClient c = Store.Client;
            c.Network.SimConnected -= foo;
            lock (this)
                if (PullServerAsset)
                {
                    PullServerAsset = false;
                }
            if (e.Simulator == c.Network.CurrentSim)
            {
                MakeRequest();
            }
        }


        private bool MakeRequest()
        {
            if (AssetID == UUID.Zero) return false;
            if (AssetType <= 0) return false;
            if (HasData())
            {
                return false;
            }
            if (!NeedsRequest) return false;

            GridClient c = Store.Client;
            if (c != null && c.Network.Connected && NeedsRequest)
            {
                if (Requests == 0 || DateTime.Now.Subtract(when).Seconds > 120)
                {
                    int ms = 0;
                    lock (timerLock)
                    {
                        ms = waitUntil.Subtract(DateTime.Now).Milliseconds;
                        waitUntil = DateTime.Now.AddSeconds(3);
                    }
                    if (ms > 0)
                    {
                        Thread.Sleep(ms);
                    }
                    Requests++;                                
                    WorldObjects.GridMaster.EnqueueRequestAsset(AssetID, AssetType, true);
                    //c.Assets.RequestAsset(AssetID, AssetType, Priority, On_AssetDownloaded);
                    when = DateTime.Now;
                    return true;
                }
            }
            RetryAsset(100);
            return false;
        }

        private void RetryAsset(int ms)
        {
            Store.Enqueue(() =>
                              {
                                  Thread.Sleep(ms);
                                  var v = ServerAsset;
                              });
        }

        public void SetAsset(Asset value)
        {
            if (value != null)
            {
                lock (value)
                {
                    if (value.AssetType > 0)
                    {
                        AssetType = value.AssetType;
                    }
                    if (value.AssetID != UUID.Zero)
                    {
                        AssetID = value.AssetID;
                    }
                    byte[] ad = value.AssetData;
                    if (ad != null && ad.Length > 0)
                    {
                        if (_TypeData == null || _TypeData.Length == 0) _TypeData = ad;
                    }

                    if (AssetComplete) return;
                    if ((_ServerAsset == value))
                    {
                        WriteLine("RE-Got server asset!");
                    }
                    AssetComplete = true;
                    PullServerAsset = false;
                    NeedsRequest = false;
                    GuessAssetName();
                    GetParts();
                    SimAssetStore.InternAsset(this);
                }
            }
        }

        public void SetAsset(AssetDownload download, Asset asset)
        {
            SetAsset(download);
            SetAsset(asset);
        }

        public void SetAsset(AssetDownload download)
        {
            bool xferFailed = !download.Success;
            PullServerAsset = xferFailed;
            NeedsRequest = xferFailed;
            if (download.Success)
            {
                AssetType = download.AssetType;
                AssetData = download.AssetData;
                AssetID = download.AssetID;
            }
        }

        public void SetAsset(ImageDownload download)
        {
            bool xferFailed = !download.Success;
            PullServerAsset = xferFailed;
            NeedsRequest = xferFailed;
            if (download.Success)
            {
                AssetType = download.AssetType;
                AssetData = download.AssetData;
            }
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
            if (name != null) Name = string.Intern(name);
            //lock (SimAssetStore.SimAssets)
            SimAssetStore.SimAssets.Add(this);
        }
        public SimAsset(UUID anim, String name, AssetType type)
        {
            AssetID = anim;
            if (name != null) Name = string.Intern(name);
            AssetType = type;
            //lock (SimAssetStore.SimAssets) 
            SimAssetStore.SimAssets.Add(this);
            if (!SimAssetStore.EnableDownloadAssetDefault)
            {
                AssetComplete = true;
                PullServerAsset = false;
                NeedsRequest = false;
            }
        }
    

        public bool Matches(String s)
        {
            return ToString().ToLower().Contains(s.ToLower());
        }

        public virtual string DebugInfo()
        {
            return ToString();
        }

        sealed public override string ToString()
        {
            string s = String.Empty;
            lock (_NamesList)
                foreach (string n in _NamesList)
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

        public List<string> _NamesList = new List<string>();
        public List<string> Names
        {
            get
            {
                return _NamesList;
            }
        }
        public string Name
        {
            get
            {
                if (_NamesList.Count == 0)
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
                        _NamesList.Add(ExpressionName);
                        return ExpressionName;
                    }
                    string tmpname = "" + AssetID;
                    if (AssetIDs.Count == 1 && NeedsSave)
                    {
                        SaveFile(tmpname);
                    }
                    return tmpname;
                }
                return _NamesList[0];
            }
            set
            {
                if (string.IsNullOrEmpty(value)) return;
                string intern = string.Intern(value);
                if (!_NamesList.Contains(value))
                {
                    _NamesList.Add(intern);
                    if (SimAssetStore.downloadedAssetFoldersComplete || _NamesList.Count > 3)
                    {
                        WriteLine("SetAssetName: {0} {1}", AssetType, intern);
                    }
                }
                if (!SimAssetStore.nameAsset.ContainsKey(intern))
                    SimAssetStore.nameAsset[intern] = this;
            }
        }

        public void AddName(string n)
        {
            if (!string.IsNullOrEmpty(n))
            {
                n = string.Intern(n);
                if (!_NamesList.Contains(n)) _NamesList.Add(n);
            }
        }


        override public UUID AssetID
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
                SimAssetStore.uuidAssetSetValue(value, this);
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
                if (Item != null)
                {
                    return Item.Name;
                }
                return null;
            }
        }


        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override ICollection<NamedParam> GetInfoMap()
        {
            return WorldObjects.GetMemberValues("", this);
        }

        internal bool IsIncomplete()
        {
            return !HasData() || AssetIDs.Count == 0 || _NamesList.Count == 0;
        }

        internal void AddType(string anims)
        {
            lock (Meanings)
            {
                if (!Meanings.Contains(anims))
                {
                    anims = string.Intern(anims);
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

        public byte[] _TypeData;
        public sealed override byte[] AssetData
        {
            get
            {
                ProbeCache();
                if (_TypeData != null) return _TypeData;
                if (_ServerAsset == null) return null;
                return _ServerAsset.AssetData;
            }
            set
            {
                if (_TypeData != null)
                {
                    int b = _TypeData == null ? -1 : _TypeData.Length;
                    int a = value == null ? -1 : value.Length;
                }
                _TypeData = value;
                if (_ServerAsset == null)
                {
                    if (AssetID != UUID.Zero)
                    {
                        _ServerAsset = CreateAssetWrapper(AssetType, AssetID, value);
                    }
                    return;
                }
                else
                {
                    _ServerAsset.AssetData = value;
                }
            }
        }

        public static Asset CreateAssetWrapper(AssetType type, UUID uuid, byte[] data)
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
                    asset = new AssetMutable(type, uuid, data);
                    Logger.Log("[OarFile] Not Implemented asset type " + type, Helpers.LogLevel.Error);
                    //throw new NotImplementedException("Unimplemented asset type: " + type);
                    break;
            }
            return asset;
        }

        public virtual float Length
        {
            get { return 3; }
        }

        public virtual bool IsContinuousEffect
        {
            get { return true; }
        }

        public override bool HasData()
        {
            byte[] d = AssetData;
            return d != null && d.Length > 0;
        }

        private bool _NeedsRequest = true;
        private bool AssetComplete;


        protected virtual void SaveFile(string tmpname)
        {
            //throw new NotImplementedException();
        }

        private bool needPush = true;
        private string _fileName;

        public void InternOnRegion(WorldObjects world)
        {
            if (!needPush) return;
            needPush = false;
            world.SendNewRegionEvent(SimEventType.DATA_UPDATE, "OnAssetInfo", this);
        }

        public sealed override bool NeedsRequest
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

        protected virtual List<SimAsset> GetParts()
        {
            return new List<SimAsset>() {this};
        }

        public static bool Decode(Asset sa)
        {
            if (sa == null) return false;
            try
            {
                return sa.Decode();
            }
            catch (Exception exception)
            {
                DLRConsole.DebugWriteLine("decoded " + sa.GetType() + " " + exception);
                return false;
            }
        }

        #region ITraceable Members
        public bool IsTraced { get; set; }

        public string FileName
        {
            get
            {
                if (!string.IsNullOrEmpty(_fileName))
                    return _fileName;
                UUID uuid = AssetID;
                if (uuid == UUID.Zero) return null;
                var cache = Store.Client.WorldSystem.RegionMasterTexturePipeline.Cache;
                string named = cache.FileName(uuid, AssetType);
                if (!string.IsNullOrEmpty(named) && File.Exists(named))
                {
                    return named;
                }
                /*
                named = uuid + "." + cache.AssetTypeExtension(AssetType);
                string gname = Name;
                if (!string.IsNullOrEmpty(gname))
                {
                    named = gname + "_" + named;
                }*/
                return named;
            }
            set {
                _fileName = value;
            }
        }

        #endregion

        public void ProbeCache()
        {
            if (_TypeData != null)
            {
                return;
            }
            var fileName = _fileName;
            if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName))
            {
                AssetData = File.ReadAllBytes(fileName);
                if (_TypeData != null && _TypeData.Length > 0) return;
            }
            if (false && _NamesList.Count > 0)
            {
                fileName = FileName;
                if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName))
                {
                    AssetData = File.ReadAllBytes(fileName);
                    if (_TypeData != null && _TypeData.Length > 0)
                    {
                        _fileName = fileName;
                        return;
                    }
                }
            }
            var cache = Store.Client.WorldSystem.RegionMasterTexturePipeline.Cache;
            AssetData = cache.GetCachedAssetBytes(AssetID, AssetType);
        }
    }
}