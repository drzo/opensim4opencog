using System;
using System.Collections.Generic;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    internal class SimTexture : SimAsset
    {
        protected override List<SimAsset> GetParts()
        {
            try
            {
                GuessAssetName();
                Decode(ServerAsset);
            }
            catch (System.Exception ex)
            {
                WriteLine("" + ex);
                //_TypeData = null;
            }
            return new List<SimAsset>() { this };
        }

        public SimTexture(UUID uuid, string name, AssetType type)
            : base(uuid, name, type)
        {
        }

        protected override string GuessAssetName()
        {
            return UnknownName;
        }

        public override float Length
        {
            get { return 2; }
        }

        public override bool IsContinuousEffect
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