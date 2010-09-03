using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    public class SimCallingCard : SimAsset
    {
        public SimCallingCard(UUID uuid, string name)
            : base(uuid, name, OpenMetaverse.AssetType.CallingCard)
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
