using System;
using OpenMetaverse;
using OpenMetaverse.Assets;

namespace cogbot.TheOpenSims
{
    public class SimScript : SimAsset
    {
        public SimScript(UUID uuid, string name, AssetType type)
            : base(uuid, name, type)
        {
        }
        protected override string GuessAssetName()
        {
            if (ServerAsset == null) return UnknownName;
            Decode(ServerAsset);
            //  Ass S = (AssetCallingCard)ServerAsset;
            //AssetData = S.AssetData;
            return UnknownName;
        }
    }

   
}