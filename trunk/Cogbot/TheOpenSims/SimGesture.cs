using System;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    internal class SimGesture : SimAsset
    {

        public SimGesture(UUID uuid, string name)
            : base(uuid, name)
        {
        }

        protected override void SaveFile(string tmpname)
        {
            Console.WriteLine("Not implemented save gesture file " + tmpname);
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
                        ServerAsset = new AssetGesture(AssetID, value);
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
            AssetGesture S = (AssetGesture)ServerAsset;
            AssetData = S.AssetData;
            return UnknownName;
        }

        public override float Length
        {
            get { throw new NotImplementedException(); }
        }

        public override bool IsLoop
        {
            get { throw new NotImplementedException(); }
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

        public override bool SameAsset(SimAsset animation)
        {
            if (animation==null) return false;
            if (animation.AssetType!=AssetType) return false;
            if (HasData())
            {
                
            }
            if (animation is SimGesture)
            {
//                r = animation.Reader;
                
            }
            return false;
        }
    }
}