using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims.Navigation;
using cogbot.Listeners;
using cogbot.TheOpenSims.Navigation.Debug;

namespace cogbot.TheOpenSims
{
    /// <summary>
    /// Denotes a Simulator region and can help with bot navigation
    /// </summary>
    public class SimRegion : SimPosition
    {
        readonly public SimPathStore PathStore;
        PathFinderDemo PathFinder;
        public GridRegion theGridRegion;
        string RegionName;
        WorldObjects WorldSystem;
        static int NumRegions = 0;
        public SimRegion(string gridRegionName, WorldObjects worldSystem)
        {
            NumRegions++;
            RegionName = gridRegionName;
            WorldSystem = worldSystem;
            Console.WriteLine("Created region: " + gridRegionName);
            PathStore = new SimPathStore(gridRegionName+".serz");
            if (NumRegions > 1)
            {
                throw new ArgumentException("too many pathstores");
            }
        }

        public void ShowDebugger()
        {
            if (PathFinder == null)
            {
                PathFinder = new PathFinderDemo(PathStore);
            }
            PathFinder.Activate();
        }
        /// <summary>
        ///  The closet usable space to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        internal Vector3 GetUsePositionOf(Vector3 v3,float useDist)
        {
            byte b = PathStore.GetNodeQuality(v3);
           // float useDist = GetSizeDistance();
            if (b > 0) return v3;
            SimWaypoint swp = PathStore.CreateClosestWaypoint(v3);
            for (float distance = PathStore.StepSize; distance < useDist *1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = SimObject.GetLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3);
                    if (b > 0) return v3;
                }
            }
            Console.WriteLine("Clearing area " + swp);
            PathStore.SetNodeQuality(v3,200);
            for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = SimObject.GetLeftPos(swp,dir, distance);
                    b = PathStore.GetNodeQuality(v3);
                    if (b == 0)
                    {
                        PathStore.SetNodeQuality(v3, 200);
                    }
                }
            }
            return GetWaypointOf(v3).GetUsePosition();
        }

        /// <summary>
        ///  The closet usable waypoint to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        internal SimWaypoint GetWaypointOf(Vector3 v3)
        {
            SimWaypoint swp = PathStore.CreateClosestWaypoint(v3);
            float dist = Vector3.Distance(v3, swp.GetSimPosition());
            if (!swp.Passable)
            {
                WorldSystem.output("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
            }
            return swp;
        }

        #region SimPosition Members

        /// <summary>
        /// The middle of the Region
        /// </summary>
        /// <returns></returns>
        public Vector3 GetSimPosition()
        {
            float size = GetSizeDistance();
            return new Vector3(size, size, theGridRegion.WaterHeight);
        }

        /// <summary>
        /// The closet usable space to the middle of the Region
        /// </summary>
        /// <returns></returns>
        public Vector3 GetUsePosition()
        {
            return GetUsePositionOf(GetSimPosition(),GetSizeDistance());
        }

        public float GetSizeDistance()
        {
            return 128f;
        }

        public SimWaypoint GetWaypoint()
        {
            return GetWaypointOf(GetUsePosition());
        }

        public bool IsRegionAttached()
        {
            return theGridRegion.RegionHandle != 0;
        }

        #endregion

        internal List<Vector3> GetV3Route(Vector3 start, Vector3 end)
        {
            return (List<Vector3>)PathStore.GetV3Route0(start, end,PathFinder );
        }

        internal void SetPassable(float x, float y)
        {
            PathStore.SetPassable(x, y);
        }

        internal void SetObjectAt(float x, float y, SimObject simObject)
        {
            PathStore.SetObjectAt(x, y, simObject);
        }

        internal void SetBlocked(float x, float y, SimObject simObject)
        {
            PathStore.SetBlocked(x, y, simObject);
        }

        internal SimWaypoint CreateClosestWaypoint(Vector3 v3)
        {
            return PathStore.CreateClosestWaypoint(v3);
        }

        #region SimPosition Members


        public Quaternion GetSimRotation()
        {
            return Quaternion.Identity;
        }

        #endregion
    }
}
