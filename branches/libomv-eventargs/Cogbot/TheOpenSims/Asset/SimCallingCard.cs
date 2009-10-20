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
            if (ServerAsset == null) return UnknownName;
            ServerAsset.Decode();
          //  Ass S = (AssetCallingCard)ServerAsset;
            //AssetData = S.AssetData;
            return UnknownName;
        }
    }
}