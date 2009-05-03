using System;
using System.Collections.Generic;
using OpenMetaverse;
using System.Threading;
using System.Reflection;
using cogbot.Listeners;

namespace cogbot.TheOpenSims
{


    public class AnimThread
    {
        AgentManager ClientSelf;
        UUID anim;
        bool repeat = true;
        Thread animLoop;
        public AnimThread(AgentManager c, UUID amin0)
        {
            ClientSelf = c;//.Self;
            //c.Client
            if (WorldObjects.Master.GetAnimationName(amin0).StartsWith("S"))
            {
                repeat = false;
            }
            anim = amin0;
        }

        public override string ToString()
        {
            return "AnimLoop " + anim + " of " + ClientSelf;
        }

        public void Start()
        {
            animLoop = new Thread(new ThreadStart(LoopAnim));
            animLoop.Name = "Thread for " + this.ToString();
            animLoop.Start();
        }
        void LoopAnim()
        {
            try
            {
                ClientSelf.AnimationStart(anim, true);
                while (repeat)
                {
                    // some anims will only last a short time so we have to 
                    // remind the server we still want to be ussing it 
                    // like Laugh .. lasts for about .9 seconds
                    //12000 is a estimate avage
                    Thread.Sleep(3200);
                    ClientSelf.AnimationStop(anim, true);
                    ClientSelf.AnimationStart(anim, true);
                }
            }
            catch (Exception) { } // for the Abort 
        }
        public void Stop()
        {
            repeat = false;
            if (animLoop != null)
            {
                try
                {
                    if (animLoop.IsAlive) animLoop.Abort();
                }
                catch (Exception) { }
                animLoop = null;
            }
            ClientSelf.AnimationStop(anim, true);
        }
    }

    public class SimAnimation
    {
        static Dictionary<UUID, string> animationName = new Dictionary<UUID, string>();
        static Dictionary<string, UUID> nameAnimation = new Dictionary<string, UUID>();

        public UUID AnimationID;
        public string Name;

        public SimAnimation(UUID anim, String name)
        {
            AnimationID = anim;
            Name = name;
        }
        static void FillAnimationNames()
        {
            lock (animationName)
            {
                if (animationName.Count > 0) return;


                foreach (FieldInfo fi in typeof(Animations).GetFields())
                {
                    UUID uid = (UUID)fi.GetValue(null);
                    string uids = uid.ToString();
                    animationName[uid] = fi.Name;
                    WorldObjects.RegisterUUID(uid, fi.Name);
                    nameAnimation[fi.Name] = uid;
                }
            }
        }
        public static ICollection<string> GetAnimationList()
        {
            FillAnimationNames();
            return nameAnimation.Keys;
        }
        public static String GetAnimationName(UUID uuid)
        {
            FillAnimationNames();
            String name;
            if (animationName.TryGetValue(uuid, out name))
            {
                return name;
            }
            return null;
        }


        public static UUID GetAnimationUUID(string a)
        {
            a = a.ToLower();
            FillAnimationNames();
            UUID partial = default(UUID);
            foreach (String name in nameAnimation.Keys)
            {
                String sname = name.ToLower();
                if (sname.Equals(a))
                {
                    return nameAnimation[name];
                }
                if (sname.Contains(a))
                {
                    partial = nameAnimation[name];
                }
            }
            return partial;

        }

        internal static void SetAnimationName(UUID uUID, string s)
        {
            if (!nameAnimation.ContainsKey(s))
                nameAnimation[s] = uUID;
            if (!animationName.ContainsKey(uUID))
                animationName[uUID] = s;
        }
    }

    /// <summary>
    /// Asset request download handler, allows a configurable number of download slots
    /// </summary>
    public class AssetPipeline
    {
        class TaskInfo
        {
            public UUID RequestID;
            public int RequestNbr;
            public AssetType Type;

            public TaskInfo(UUID reqID, int reqNbr, AssetType type)
            {
                RequestID = reqID;
                RequestNbr = reqNbr;
                Type = type;
            }
        }

        public delegate void DownloadFinishedCallback(UUID id, bool success);
        public delegate void DownloadProgressCallback(UUID image, int recieved, int total);

        /// <summary>Fired when a texture download completes</summary>
        public event DownloadFinishedCallback OnDownloadFinished;
        /// <summary>Fired when some texture data is received</summary>
        public event DownloadProgressCallback OnDownloadProgress;

        public int CurrentCount { get { return currentRequests.Count; } }
        public int QueuedCount { get { return requestQueue.Count; } }

        GridClient client;
        /// <summary>Maximum concurrent texture requests</summary>
        int maxAssetRequests;
        /// <summary>Queue for image requests that have not been sent out yet</summary>
        List<TaskInfo> requestQueue;
        /// <summary>Current texture downloads</summary>
        Dictionary<UUID, int> currentRequests;
        /// <summary>Storage for completed texture downloads</summary>
        Dictionary<UUID, ImageDownload> completedDownloads;
        AutoResetEvent[] resetEvents;
        int[] threadpoolSlots;
        Thread downloadMaster;
        bool running;
        object syncObject = new object();

        /// <summary>
        /// Default constructor
        /// </summary>
        /// <param name="client">Reference to <code>SecondLife</code> client</param>
        /// <param name="maxRequests">Maximum number of concurrent texture requests</param>
        public AssetPipeline(GridClient client, int maxRequests)
        {
            running = true;
            this.client = client;
            maxAssetRequests = maxRequests;

            requestQueue = new List<TaskInfo>();
            currentRequests = new Dictionary<UUID, int>(maxAssetRequests);
            completedDownloads = new Dictionary<UUID, ImageDownload>();
            resetEvents = new AutoResetEvent[maxAssetRequests];
            threadpoolSlots = new int[maxAssetRequests];

            // Pre-configure autoreset events/download slots
            for (int i = 0; i < maxAssetRequests; i++)
            {
                resetEvents[i] = new AutoResetEvent(false);
                threadpoolSlots[i] = -1;
            }

            client.Assets.OnImageReceived += Assets_OnImageReceived;
            client.Assets.OnImageReceiveProgress += Assets_OnImageReceiveProgress;

            // Fire up the texture download thread
            downloadMaster = new Thread(new ThreadStart(DownloadThread));
            downloadMaster.Start();
        }

        public void Shutdown()
        {
            client.Assets.OnImageReceived -= Assets_OnImageReceived;
            client.Assets.OnImageReceiveProgress -= Assets_OnImageReceiveProgress;

            requestQueue.Clear();

            for (int i = 0; i < resetEvents.Length; i++)
                if (resetEvents[i] != null)
                    resetEvents[i].Set();

            running = false;
        }

        /// <summary>
        /// Request a texture be downloaded, once downloaded OnImageRenderReady event will be fired
        /// containing texture key which can be used to retrieve texture with GetAssetToRender method
        /// </summary>
        /// <param name="textureID">Asset to request</param>
        /// <param name="type">Type of the requested texture</param>
        public void RequestAsset(UUID textureID, AssetType type0)
        {
            lock (syncObject)
            {
                if (client.Assets.Cache.HasImage(textureID))
                {
                    // Add to rendering dictionary
                    if (!completedDownloads.ContainsKey(textureID))
                    {
                        completedDownloads.Add(textureID, client.Assets.Cache.GetCachedImage(textureID));

                        // Let any subscribers know about it
                        if (OnDownloadFinished != null)
                            OnDownloadFinished(textureID, true);
                    }
                    else
                    {
                        // This image has already been served up, ignore this request
                    }
                }
                else
                {
                    // Make sure the request isn't already queued up
                    foreach (TaskInfo task in requestQueue)
                    {
                        if (task.RequestID == textureID)
                            return;
                    }

                    // Make sure we aren't already downloading the texture
                    if (!currentRequests.ContainsKey(textureID))
                        requestQueue.Add(new TaskInfo(textureID, 0, type0));
                }
            }
        }

        /// <summary>
        /// retrieve texture information from dictionary
        /// </summary>
        /// <param name="textureID">Asset ID</param>
        /// <returns>ImageDownload object</returns>
        public ImageDownload GetAssetToRender(UUID textureID)
        {
            lock (syncObject)
            {
                if (completedDownloads.ContainsKey(textureID))
                {
                    return completedDownloads[textureID];
                }
                else
                {
                    Logger.Log("Requested texture data for texture that does not exist in dictionary", Helpers.LogLevel.Warning);
                    return null;
                }
            }
        }

        /// <summary>
        /// Remove no longer necessary texture from dictionary
        /// </summary>
        /// <param name="textureID"></param>
        public bool RemoveFromPipeline(UUID textureID)
        {
            lock (syncObject)
                return completedDownloads.Remove(textureID);
        }

        public void AbortDownload(UUID textureID)
        {
            lock (syncObject)
            {
                for (int i = 0; i < requestQueue.Count; i++)
                {
                    TaskInfo task = requestQueue[i];

                    if (task.RequestID == textureID)
                    {
                        requestQueue.RemoveAt(i);
                        --i;
                    }
                }

                int current;
                if (currentRequests.TryGetValue(textureID, out current))
                {
                    currentRequests.Remove(textureID);
                    resetEvents[current].Set();

                    // FIXME: Send an abort packet
                }
            }
        }

        /// <summary>
        /// Master Download Thread, Queues up downloads in the threadpool
        /// </summary>
        private void DownloadThread()
        {
            int reqNbr;

            while (running)
            {
                if (requestQueue.Count > 0)
                {
                    reqNbr = -1;
                    // find available slot for reset event
                    for (int i = 0; i < threadpoolSlots.Length; i++)
                    {
                        if (threadpoolSlots[i] == -1)
                        {
                            threadpoolSlots[i] = 1;
                            reqNbr = i;
                            break;
                        }
                    }

                    if (reqNbr != -1)
                    {
                        TaskInfo task = null;
                        lock (syncObject)
                        {
                            if (requestQueue.Count > 0)
                            {
                                task = requestQueue[0];
                                requestQueue.RemoveAt(0);
                            }
                        }

                        if (task != null)
                        {
                            task.RequestNbr = reqNbr;

                            Logger.DebugLog(String.Format("Sending Worker thread new download request {0}", reqNbr));
                            ThreadPool.QueueUserWorkItem(AssetRequestDoWork, task);
                            continue;
                        }
                    }
                }

                // Queue was empty, let's give up some CPU time
                Thread.Sleep(500);
            }

            Logger.Log("Asset pipeline shutting down", Helpers.LogLevel.Info);
        }

        private void AssetRequestDoWork(Object threadContext)
        {
            TaskInfo ti = (TaskInfo)threadContext;

            lock (syncObject)
            {
                if (currentRequests.ContainsKey(ti.RequestID))
                {
                    threadpoolSlots[ti.RequestNbr] = -1;
                    return;
                }
                else
                {
                    currentRequests.Add(ti.RequestID, ti.RequestNbr);
                }
            }

            Logger.DebugLog(String.Format("Worker {0} Requesting {1}", ti.RequestNbr, ti.RequestID));

            resetEvents[ti.RequestNbr].Reset();
            client.Assets.RequestAsset(ti.RequestID, ti.Type,true);

            // don't release this worker slot until texture is downloaded or timeout occurs
            if (!resetEvents[ti.RequestNbr].WaitOne(45 * 1000, false))
            {
                // Timed out
                Logger.Log("Worker " + ti.RequestNbr + " Timeout waiting for Asset " + ti.RequestID + " to Download", Helpers.LogLevel.Warning);

                lock (syncObject)
                    currentRequests.Remove(ti.RequestID);

                if (OnDownloadFinished != null)
                    OnDownloadFinished(ti.RequestID, false);
            }

            // free up this download slot
            threadpoolSlots[ti.RequestNbr] = -1;
        }

        private void Assets_OnImageReceived(ImageDownload image, Asset asset)
        {
            int requestNbr;
            bool found;

            lock (syncObject)
                found = currentRequests.TryGetValue(image.ID, out requestNbr);

            if (asset != null && found)
            {
                Logger.DebugLog(String.Format("Worker {0} Downloaded texture {1}", requestNbr, image.ID));

                // Free up this slot in the ThreadPool
                lock (syncObject)
                    currentRequests.Remove(image.ID);

                resetEvents[requestNbr].Set();

                if (image.Success)
                {
                    // Add to the completed texture dictionary
                    lock (syncObject)
                        completedDownloads[image.ID] = image;
                }
                else
                {
                    Logger.Log(String.Format("Download of texture {0} failed. NotFound={1}", image.ID, image.NotFound),
                        Helpers.LogLevel.Warning);
                }

                // Let any subscribers know about it
                if (OnDownloadFinished != null)
                    OnDownloadFinished(image.ID, image.Success);
            }
        }

        private void Assets_OnImageReceiveProgress(UUID image, int lastPacket, int recieved, int total)
        {
            if (OnDownloadProgress != null && currentRequests.ContainsKey(image))
                OnDownloadProgress(image, recieved, total);
        }
    }
}
