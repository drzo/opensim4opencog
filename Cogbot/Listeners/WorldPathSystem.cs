using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using System.Threading;
using cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
using PathSystem3D.Mesher;

namespace cogbot.Listeners
{
    public class WorldPathSystem: IDisposable
    {
       public SimGlobalRoutes GlobalRoutes = SimGlobalRoutes.Instance;
     //   static object GlobalRoutes = new object();
        static Thread TrackPathsThread;
        static bool IsDisposing = false;

        public static int MaxMeshes = 18000;
        static public int RealMeshes = 0;
        public static float MinEdgeSizeOfSimplify = 0.5f;
        public static float MinMassOfSimplify = 0.5f;

        public WorldPathSystem(GridClient gc)
        {
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
                if (!WorldObjects.MaintainCollisions) continue;
                int thisCount = SimObjects.Count;

                if (thisCount == lastCount)
                {
                    Thread.Sleep(20000);
                    
                    continue;
                }

                Debug("\nTrackPaths Started: " + lastCount + "->" + thisCount);
                
                lastCount = thisCount;
                int occUpdate = 0;
                int realUpdates = 0;
                foreach (SimObject O in SimObjects.CopyOf())
                {
                    if (!WorldObjects.MaintainSimCollisions(O.RegionHandle)) continue;
                    if (O.IsRegionAttached)
                    {
                        if (O.UpdateOccupied())
                        {
                            RealMeshes++;
                            realUpdates++;
                        }
                    }
                    occUpdate++;
                    if (occUpdate % 100 == 0)
                    {
                        DLRConsole.SystemWrite("." + occUpdate);
                        DLRConsole.SystemFlush();
                    }
                    if (RealMeshes >= MaxMeshes || !WorldObjects.MaintainCollisions) break;
                }

                Debug("\nTrackPaths Completed: " + thisCount + " realUpdates=" + realUpdates);
                
                //SimRegion.BakeRegions();
                
            }
        }

        private static void Debug(string p)
        {
            DLRConsole.SystemWriteLine(p);
        }

        private void Error(string p)
        {
            DLRConsole.SystemWriteLine(p);
            throw new NotImplementedException();
        }

        internal void UpdateFromImage(System.Drawing.Image I)
        {
            throw new NotImplementedException();
        }

        public void Dispose()
        {
            WorldObjects.MaintainCollisions = false;
            IsDisposing = true;
            TrackPathsThread.Abort();
        }
    }
}
