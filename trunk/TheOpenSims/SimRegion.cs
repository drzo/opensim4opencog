using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims.Navigation;
using cogbot.Listeners;

namespace cogbot.TheOpenSims
{
    /// <summary>
    /// Denotes a Simulator region and can help with bot navigation
    /// </summary>
    public class SimRegion : SimPosition
    {
        readonly public SimPathStore PathStore;
//        public static PathFinderDemo PathFinder = new PathFinderDemo(PathStore);
        public GridRegion theGridRegion;
        string RegionName;
        WorldObjects WorldSystem;
        public SimRegion(string gridRegionName, WorldObjects worldSystem)
        {
            RegionName = gridRegionName;
            WorldSystem = worldSystem;
            Console.WriteLine("Created region: " + gridRegionName);
            PathStore = new SimPathStore(gridRegionName+".serz");
        }

        /// <summary>
        ///  The closet usable space to the vector3 TODO
        /// </summary>
        /// <param name="vector3"></param>
        /// <returns></returns>
        internal Vector3 GetUsePositionOf(Vector3 vector3)
        {
            return GetWaypointOf(vector3).GetUsePosition();
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
            return GetUsePositionOf(GetSimPosition());
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
            return (List<Vector3>)PathStore.GetV3Route(start, end);
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
    }
}
