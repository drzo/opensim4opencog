using System;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    internal class SimSound : SimAsset
    {

        public SimSound(UUID uuid, string name, AssetType type)
            : base(uuid, name, type)
        {
        }

        protected override void SaveFile(string tmpname)
        {
            //WriteLine("Not implemented save sound file " + tmpname);
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
                if (_TypeData!=null)
                {
                    
                }
                _TypeData = value;
                if (ServerAsset == null)
                {
                    if (AssetID != UUID.Zero)
                    {
                        ServerAsset = new AssetSound(AssetID, value);
                    }
                    return;
                }
                else
                {
                    ServerAsset.AssetData = value;
                }
            }
        }

        protected override string GuessAssetName()
        {
            if (ServerAsset == null) return null;
            ServerAsset.Decode();
            AssetSound S = (AssetSound)ServerAsset;
            AssetData = S.AssetData;
            return UnknownName;
        }

        public override float Length
        {
            get { throw new NotImplementedException(); }
        }

        public override bool IsLoop
        {
            get
            {
                AssetSound S = (AssetSound)ServerAsset;         
                throw new NotImplementedException();
            }
        }

        public override bool HasData()
        {
            return ServerAsset != null || _TypeData != null;
        }

        private bool _NeedsRequest = true;
        public override bool NeedsRequest
        {
            get
            {
                if (HasData()) return false;
                return _NeedsRequest;
            }
            set { _NeedsRequest=value; }
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