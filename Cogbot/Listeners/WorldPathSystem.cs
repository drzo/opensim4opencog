using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using System.Threading;
using OpenMetaverse;
using PathSystem3D.Navigation;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
using PathSystem3D.Mesher;

namespace cogbot.Listeners
{
    public class WorldPathSystem
    {
       public SimGlobalRoutes GlobalRoutes = SimGlobalRoutes.Instance;
     //   static object GlobalRoutes = new object();
        static Thread TrackPathsThread;


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
            while (true)
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
                    if (O.IsRegionAttached())
                    {
                        if (O.UpdateOccupied())
                        {
                            RealMeshes++;
                            realUpdates++;
                        }
                    }
                    occUpdate++;
                    if (RealMeshes > MaxMeshes) break;
                    if (occUpdate % 100 == 0)
                    {
                        Console.Write("." + occUpdate);                             
                        Console.Out.Flush();
                    }
                }

                Debug("\nTrackPaths Completed: " + thisCount + " realUpdates=" + realUpdates);
                
                //SimRegion.BakeRegions();
                
            }
        }

        private static void Debug(string p)
        {
            Console.WriteLine(p);
        }

        private void Error(string p)
        {
            Console.WriteLine(p);
            throw new NotImplementedException();
        }

        internal void UpdateFromImage(System.Drawing.Image I)
        {
            throw new NotImplementedException();
        }
    }
}
