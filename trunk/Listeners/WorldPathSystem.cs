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



        public WorldPathSystem(GridClient gc)
        {
            lock (GlobalRoutes)
            {
                if ((!gc.Settings.AVATAR_TRACKING)) Error("client.Settings.AVATAR_TRACKING != true");
                if ((!gc.Settings.ALWAYS_DECODE_OBJECTS)) Error("client.Settings.ALWAYS_DECODE_OBJECTS != true");
                if ((!gc.Settings.OBJECT_TRACKING)) Error("client.Settings.OBJECT_TRACKING != true");
                if (TrackPathsThread == null && WorldObjects.MaintainCollisions)
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
                int thisCount = SimObjects.Count;

                if (thisCount == lastCount)
                {
                    Thread.Sleep(20000);
                    
                    continue;
                }

                Debug("TrackPaths Started: " + lastCount + "->" + thisCount);
                
                lastCount = thisCount;
                int occUpdate = 0;
                foreach (SimObject O in SimObjects.CopyOf())
                {
                    if (O.IsRegionAttached())
                    {
                        O.UpdateOccupied();
                    }
                    occUpdate++;
                    if (occUpdate % 100 == 0)
                    {
                        Console.Write("." + occUpdate);
                        Console.Out.Flush();
                    }
                }

                Debug("TrackPaths Completed: " + thisCount);
                
                SimRegion.BakeRegions();
                
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
