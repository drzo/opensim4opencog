using System;
using System.Threading;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    public class AssetThread
    {
        private readonly SimAsset asset;
        private readonly AgentManager ClientSelf;
        private Thread assetLoop;
        private bool repeat = true;

        public AssetThread(AgentManager c, SimAsset amin0)
        {
            ClientSelf = c; //.Self;
            repeat = !amin0.IsLoop;
            asset = amin0;
        }

        public override string ToString()
        {
            return String.Format("AssetLoop {0} of {1}", asset, ClientSelf);
        }

        public void Start()
        {
            assetLoop = new Thread(LoopAnim) { Name = string.Format("Thread for {0}", ToString()) };
            assetLoop.Start();
        }

        private void LoopAnim()
        {
            try
            {
                ClientSelf.AnimationStart(AssetID, true);
                while (NeedsLooping)
                {
                    // some anims will only last a short time so we have to 
                    // remind the server we still want to be using it 
                    // like Laugh .. lasts for about .9 seconds
                    //12000 is a estimate average
                    Thread.Sleep((int)(asset.Length * 1000));
                    ClientSelf.AnimationStop(AssetID, true);
                    ClientSelf.AnimationStart(AssetID, true);
                }
            }
            catch (Exception)
            {
            } // for the Abort 
        }

        protected bool NeedsLooping
        {
            get
            {
                if (repeat) return true;
                if (!asset.IsLoop) return true;
                return false;
            }
        }

        protected UUID AssetID
        {
            get { return asset.AssetID; }
        }

        public void Stop()
        {
            repeat = false;
            if (assetLoop != null)
            {
                try
                {
                    if (assetLoop.IsAlive) assetLoop.Abort();
                }
                catch (Exception)
                {
                }
                assetLoop = null;
            }
            ClientSelf.AnimationStop(AssetID, true);
        }
    }
}