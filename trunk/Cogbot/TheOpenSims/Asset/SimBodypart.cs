using System;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    public class SimBodypart : SimAsset
    {
        public SimBodypart(UUID uuid, string name)
            : base(uuid, name, AssetType.Bodypart)
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