using System;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    internal class SimSound : SimAsset
    {
        public SimSound(UUID uuid, string name)
            : base(uuid, name)
        {
        }

        protected override void SaveFile(string tmpname)
        {
            throw new NotImplementedException();
        }

        public override byte[] TypeData
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        protected override string GuessAssetName()
        {
            throw new NotImplementedException();
        }

        public override float Length
        {
            get { throw new NotImplementedException(); }
        }

        public override bool IsLoop
        {
            get { throw new NotImplementedException(); }
        }
    }
}