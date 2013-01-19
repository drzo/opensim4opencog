using System;
using System.Collections.Generic;
using Cogbot.World;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;
#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
using System.Threading;



namespace Cogbot
{
    public class WorldPathSystem: IDisposable
    {
       public SimGlobalRoutes GlobalRoutes = SimGlobalRoutes.Instance;
       static public readonly TaskQueueHandler MeshingQueue = new TaskQueueHandler(null, "world MeshingQueue", TimeSpan.FromSeconds(10), true);
       //   static object GlobalRoutes = new object();
        static Thread TrackPathsThread;
        static bool IsDisposing = false;

        public static bool SculptCollisions = false;
        public static bool MaintainCollisionsForeground = true;
        public static int MaxMeshes = 18000;
        static public int RealMeshes = 0;
        public static float MinEdgeSizeOfSimplify = 0.5f;
        public static float MinMassOfSimplify = 0.5f;

        [ConfigSetting(Description = "If you make true bot will mesh entire simulator for pathfinding. keep false so the bot only meshes what it needs")]
        public static bool MaintainCollisions = true; // keep false so the bot only meshes what it needs
        [ConfigSetting(Description = "if false, system wide don't mesh anything. e.g. a bot that never uses collisions.")]
        public static bool MaintainMeshes = true;
        [ConfigSetting(Description = "Distance within which the av meshes objects automatically")]
        public static int WorthMeshingDistance = 160;
        [ConfigSetting(Description = "if true, go ahead and mesh phantom objects. So you might want to set this true in a sim with many objects toggling between phantom and nonphantom.")]
        public static bool SkipPassableMeshes = true;

        public static readonly List<ulong> MaintainSimCollisionsList = new List<ulong>();
        //public static GridClient GridClientMaster;
        public static bool DebugPathSystem = false;

        public static bool MaintainSimCollisions(ulong handle)
        {
            lock (MaintainSimCollisionsList) return MaintainSimCollisionsList.Contains(handle);
        }

        public static bool IsWorthMeshing(SimObjectImpl impl)
        {
            Vector3d GlobalPosition;
            //GridClient client = GridClientMaster;
            //if (client == null) return true;
            if (impl.TryGetGlobalPosition(out GlobalPosition))
            {
                bool atLeastOnBC = false;
                foreach (var client in ClientManager.KnownBotClients)
                {
                    if (!client.IsLoggedInAndReady) continue;                    ;
                    double d = Vector3d.Distance(GlobalPosition, client.Self.GlobalPosition);
                    if (d < WorldPathSystem.WorthMeshingDistance)
                    {
                        return true;
                    }
                    atLeastOnBC = true;
                }
                if (!atLeastOnBC) return true;
            }
            return false;
        }

        public WorldPathSystem(GridClient gc)
        {
            MeshingQueue.StackerThread.Priority = System.Threading.ThreadPriority.AboveNormal;
            lock (GlobalRoutes)
            {
                if ((!gc.Settings.AVATAR_TRACKING)) Error("client.Settings.AVATAR_TRACKING != true");
                if ((!gc.Settings.ALWAYS_DECODE_OBJECTS)) Error("client.Settings.ALWAYS_DECODE_OBJECTS != true");
                if ((!gc.Settings.OBJECT_TRACKING)) Error("client.Settings.OBJECT_TRACKING != true");
                if (TrackPathsThread == null)
                {
                    TrackPathsThread = new Thread(TrackPaths);
                    TrackPathsThread.Name = "Track paths";
                    //TrackPathsThread.Priority = ThreadPriority.AboveNormal;
                    TrackPathsThread.Start();
                }
            }
        }

        static ListAsSet<SimObject> SimObjects
        {
            get
            {
                return WorldObjects.SimObjects;
            }
        }

        static void TrackPaths()
        {
            Thread.Sleep(30000);
            int lastCount = 0;
            while (!(IsDisposing))
            {
                Thread.Sleep(10000);
                if (!MaintainCollisions && !MaintainCollisionsForeground) continue;
                int thisCount = SimObjects.Count;

                if (thisCount == lastCount)
                {
                    Thread.Sleep(20000);
                    lastCount--;
                    
                    continue;
                }

                Debug("\nTrackPaths Started: " + lastCount + "->" + thisCount);
                
                lastCount = thisCount;
                int occUpdate = 0;
                int realUpdates = 0;
                foreach (SimObject O in SimObjects.CopyOf())
                {
                    if (!MaintainSimCollisions(O.RegionHandle)) continue;
                    if (O.IsRegionAttached)
                    {
                        SimObjectPathFinding o = O.PathFinding;
                        if (o.IsWorthMeshing)
                        {
                            bool didIt = false;
                            if (MaintainCollisionsForeground)
                            {
                                didIt = o.AddCollisionsNow();
                            }
                            else
                            {
                                didIt = o.AddCollisions();
                            }
                            if (didIt)
                            {
                                RealMeshes++;
                                realUpdates++;
                            }
                        }
                        else
                        {
                            if (MaintainCollisionsForeground)
                            {
                                if (O.IsPhantom || O.IsPassable) continue;
                                continue;
                                var p = O.PathFinding;
                                WorldPathSystem.MeshingQueue.Enqueue(() => p.AddCollisionsNow());
                            }
                        }
                    }
                    occUpdate++;
                    if (occUpdate % 100 == 0 && DebugPathSystem)
                    {
                        DLRConsole.DebugWrite("." + occUpdate);
                        DLRConsole.SystemFlush();
                    }
                    if (RealMeshes >= MaxMeshes || !MaintainCollisions) break;
                }

                Debug("\nTrackPaths Completed: " + thisCount + " realUpdates=" + realUpdates);
                
                //SimRegion.BakeRegions();
                
            }
        }

        private static void Debug(string p)
        {
            if (DebugPathSystem) DLRConsole.DebugWriteLine(p);
        }

        private void Error(string p)
        {
            DLRConsole.DebugWriteLine(p);
        }

        internal void UpdateFromImage(System.Drawing.Image I)
        {
            throw new NotImplementedException("UpdateFromImage we use 3d not 2d " + 
            I);
        }

        public void Dispose()
        {
            MaintainCollisions = false;
            IsDisposing = true;
            TrackPathsThread.Abort();
        }
    }
}
