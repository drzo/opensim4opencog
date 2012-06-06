using System;
using System.Collections.Generic;
using OpenMetaverse;

namespace Cogbot.World
{
    public class SimClothing : SimAsset
    {
        public SimClothing(UUID uuid, string name)
            : base(uuid, name, OpenMetaverse.AssetType.Clothing)
        {

        }

        protected override string GuessAssetName()
        {
            if (_ServerAsset == null) return UnknownName;
            Decode(ServerAsset);
            //  Ass S = (AssetCallingCard)ServerAsset;
            //AssetData = S.AssetData;
            return UnknownName;
        }
    }
}
