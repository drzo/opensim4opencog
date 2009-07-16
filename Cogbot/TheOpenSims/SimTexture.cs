using System;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    internal class SimTexture : SimAsset
    {
        private bool _NeedsRequest = true;
        public override bool NeedsRequest
        {
            get
            {
                if (HasData()) return false;
                return _NeedsRequest;
            }
            set { _NeedsRequest = value; }
        }

        public SimTexture(UUID uuid, string name, AssetType type)
            : base(uuid, name, type)
        {
        }

        public override bool HasData()
        {
            return ServerAsset != null || _TypeData != null;
        }

        protected override void SaveFile(string tmpname)
        {
            Console.WriteLine("Not implemented save texture file " + tmpname);
        }
        private byte[] _TypeData;
        public override byte[] AssetData
        {
            get
            {
                if (_TypeData != null) return _TypeData;
                if (ServerAsset == null) return null;
                return ServerAsset.AssetData;
            }
            set
            {
                _TypeData = value;
                if (ServerAsset == null)
                {
                    if (AssetID != UUID.Zero)
                    {
                        ServerAsset = new AssetTexture(AssetID, value);
                    }
                    return;
                }
                ServerAsset.AssetData = value;
            }
        }
        protected override string GuessAssetName()
        {
            return UnknownName;
        }

        public override float Length
        {
            get { return 2; }
        }

        public override bool IsLoop
        {
            get { return true; }
        }
                
        public override bool SameAsset(SimAsset asset)
        {
            if (asset==null) return false;
            if (asset.AssetType!=AssetType) return false;
            if (HasData())
            {
                
            }
            if (asset is SimAnimation)
            {
//                r = animation.Reader;
                
            }
            return false;
        }
    }
}