using System;
using System.Collections.Generic;
using System.Reflection;
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

        protected override string GuessAssetName()
        {
            if (_ServerAsset == null) return null;
            Decode(ServerAsset);
            AssetSound S = (AssetSound)ServerAsset;
            AssetData = S.AssetData;
            return UnknownName;
        }

        public override float Length
        {
            get
            {
                AssetSound S = (AssetSound)ServerAsset;
                WriteLine("Notimplemented " + MethodInfo.GetCurrentMethod());
                return float.MaxValue;
            }
        }

        public override bool IsContinuousEffect
        {
            get
            {
                AssetSound S = (AssetSound)ServerAsset;                
                WriteLine("Notimplemented " + MethodInfo.GetCurrentMethod());
                return false;
            }
        }

        public override bool SameAsset(SimAsset asset)
        {
            if (asset == null) return false;
            if (asset.AssetType != AssetType) return false;
            if (SameAssetBytes(asset)) return true;
            var anim2 = asset as SimSound;
            if (anim2 == null) return false;
            return anim2.Sound.Equals(Sound);
        }

        protected object Sound
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }
    }
}