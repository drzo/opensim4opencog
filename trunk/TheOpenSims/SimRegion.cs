using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using PathSystem3D.Navigation;
using cogbot.Listeners;
using PathSystem3D.Navigation.Debug;
using PathSystem3D.Mesher;
using System.Threading;
using cogbot.ScriptEngines;
using System.Drawing;
using PathSystem3D;

namespace cogbot.TheOpenSims
{
    public enum NESW : int
    {
        C = 0,
        N = 1,
        NE, E, SE, S, SW, W, NW
    }



    /// <summary>
    /// Denotes a Simulator region and can help with bot navigation
    /// </summary>
    public class SimRegion  //: IPathStore
    {

        public static List<SimRegion> CurrentRegions
        {
            get
            {
                List<SimRegion> sims = new List<SimRegion>();
                lock (_CurrentRegions)
                {
                    foreach (SimRegion R in _CurrentRegions.Values)
                    {
                        sims.Add(R);
                    }
                }
                return sims;
            }
        }


        public SimPathStore GetPathStore3D(Vector3 v3)
        {
            return PathStore;
        }

        public static void BakeRegions()
        {
            foreach (SimRegion R in CurrentRegions)
            {
                R.UpdateMatrixes();
            }
        }

        private void UpdateMatrixes()
        {
            SimRegion R = GetSimRegion();
            R.BakeTerrain();

                //PathStore.UpdateMatrix(PS.CurrentPlane);
        }

        //public static implicit operator SimRegion(SimPathStore m)
        //{
        //    return m.GetPathStore();
        //}

        readonly float MAXY = 256f;

        static Vector2 vC = new Vector2(0, 0), // C
               vN = new Vector2(0, 1), // N
               vNE = new Vector2(1, 1), // NE
               vE = new Vector2(1, 0),  // E
               vSE = new Vector2(1, -1), // SE
               vS = new Vector2(0, -1), // S
               vSW = new Vector2(-1, -1), // SW
               vW = new Vector2(-1, 0), // W
               vNW = new Vector2(-1, 1); // NW

        public static Vector2[] XYOf = { vC, vN, vNE, vE, vSE, vS, vSW, vW, vNW };

        public static SimRegion GetRegion(Vector3d pos)
        {
            if (pos.X < 0 || pos.Y < 0)
            {
                throw new ArgumentException("GlobalToWaypoint? " + pos);
            }
            if (pos.X < 256 || pos.Y < 256)
            {
                Console.WriteLine("GlobalToWaypoint? " + pos);
            }
            return GetRegion(Utils.UIntsToLong(Round256(pos.X), Round256(pos.Y)));
        }

        /// <summary>
        /// Given an X/Y location in absolute (grid-relative) terms, a region
        /// handle is returned along with the local X/Y location in that region
        /// </summary>
        /// <param name="globalX">The absolute X location, a number such as 
        /// 255360.35</param>
        /// <param name="globalY">The absolute Y location, a number such as
        /// 255360.35</param>
        /// <param name="localX">The sim-local X position of the global X
        /// position, a value from 0.0 to 256.0</param>
        /// <param name="localY">The sim-local Y position of the global Y
        /// position, a value from 0.0 to 256.0</param>
        /// <returns>A 64-bit region handle that can be used to teleport to</returns>
        public static ulong GlobalPosToRegionHandle(float globalX, float globalY, out float localX, out float localY)
        {
            uint x = ((uint)globalX / 256) * 256;
            uint y = ((uint)globalY / 256) * 256;
            localX = globalX - (float)x;
            localY = globalY - (float)y;
            return Utils.UIntsToLong(x, y);
        }

        public static SimWaypoint GetWaypoint(Vector3d gloabl)
        {
            if (gloabl.X == float.NaN) return null;
            uint x = Round256(gloabl.X);
            uint y = Round256(gloabl.Y);
            Vector3 local;
            local.X = (float)gloabl.X - x;
            local.Y = (float)gloabl.Y - y;
            if (local.X >= 256)
            {
                local.X -= 256f;
                x++;
            }
            if (local.Y >= 256)
            {
                local.Y -= 256f;
                y++;
            }
            SimRegion R = GetRegion(Utils.UIntsToLong(x, y));
            local.Z = (float)gloabl.Z;
            try
            {
                return R.GetWaypointOf(local);
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
                Vector3 l2 = local;
                l2 *= 0.99f;
                return R.GetWaypointOf(l2);
            }
        }

        public static Vector3 GlobalToLocal(Vector3d pos)
        {
            if (pos.X < 0 || pos.Y < 0)
            {
                throw new ArgumentException("GlobalToWaypoint? " + pos);
            }
            if (pos.X < 256 || pos.Y < 256)
            {
                Console.WriteLine("GlobalToWaypoint? " + pos);
            }
            return new Vector3((float)pos.X - Round256(pos.X), (float)pos.Y - Round256(pos.Y), (float)pos.Z);
        }

        public static uint Round256(double global)
        {
            return ((uint)global / 256) * 256;
        }


        static Dictionary<ulong, SimRegion> _CurrentRegions = new Dictionary<ulong, SimRegion>();

        static public SimRegion GetRegion(ulong id)
        {
            if (id == 0)
            {
                throw new ArgumentException("GetRegion: region handle cant be zero");
            }
            lock (_CurrentRegions)
            {
                SimRegion R;
                if (_CurrentRegions.TryGetValue(id, out R))
                    return R;
                R = new SimRegion(id);
                _CurrentRegions[id] = R;
                return R;
            }

        }

        Vector2 _GridLoc = Vector2.Zero;
        public Vector2 GetGridLocation()
        {
            if (_GridLoc == Vector2.Zero)
            {
                uint regionX = 0, regionY = 0;
                Utils.LongToUInts(RegionHandle, out regionX, out regionY);
                _GridLoc = new Vector2((float)(regionX / 256), (float)(regionY / 256));
            }
            return _GridLoc;
        }

        static public SimRegion GetRegion(string simname)
        {
            foreach (SimRegion R in CurrentRegions)
                if (R.RegionName.Contains(simname)) return R;
            return null;
        }

        static public SimRegion GetRegion(Simulator sim)
        {
            if (sim == null) return null;
            ulong Handle = sim.Handle;
            SimRegion R = GetRegion(Handle);
            R.TheSimulator = sim;
            return R;
        }


        ///readonly public List<SimPathStore> PathStores = new List<SimPathStore>();

        public SimRegion N
        {
            get { return GetOffsetRegion(vN); }
            set { SetRegionOffset(vN, value); }
        }
        public SimRegion E
        {
            get { return GetOffsetRegion(vE); }
            set { SetRegionOffset(vE, value); }
        }
        public SimRegion S
        {
            get { return GetOffsetRegion(vS); }
            set { SetRegionOffset(vS, value); }
        }
        public SimRegion W
        {
            get { return GetOffsetRegion(vW); }
            set { SetRegionOffset(vW, value); }
        }

        public void SetRegionOffset(Vector2 v2, SimRegion value)
        {
            SetRegion(HandleOf(GetGridLocation() + v2), value);
        }

        public SimRegion GetOffsetRegion(Vector2 v2)
        {
            return GetRegion(GetGridLocation() + v2);
        }

        static void SetRegion(ulong h, SimRegion value)
        {
            if (_CurrentRegions.ContainsKey(h))
            {
                SimRegion OLD = _CurrentRegions[h];
                if (OLD == null || OLD == value) return;
                throw new ArgumentException("Bad region change " + OLD + " -> " + value);
            }
            _CurrentRegions[h] = value;
        }

        public static SimRegion GetRegion(Vector2 v2)
        {
            ulong h = HandleOf(v2);
            return GetRegion(h);
        }

        public static ulong HandleOf(Vector2 v2)
        {
            return Utils.UIntsToLong((uint)v2.X * 256, (uint)v2.Y * 256);
        }

        public Vector3d LocalToGlobal(Vector3 objectLoc)
        {
            return PathStore.LocalToGlobal(objectLoc);
        }

        AutoResetEvent regionEvent = new AutoResetEvent(false);
        bool GridInfoKnown = false;
        GridRegion _GridInfo;
        GridClient Client;
        public GridRegion GridInfo
        {
            get
            {
                if (GridInfoKnown) return _GridInfo;
                if (!String.IsNullOrEmpty(PathStore.RegionName))
                {
                    regionEvent.Reset();
                    OpenMetaverse.GridManager.GridRegionCallback callback =
                        delegate(GridRegion gridRegion)
                        {
                            if (gridRegion.RegionHandle == RegionHandle)
                                regionEvent.Set();
                        };
                    Client.Grid.OnGridRegion += callback;
                    Client.Grid.RequestMapRegion(PathStore.RegionName, GridLayerType.Objects);
                    regionEvent.WaitOne(Client.Settings.MAP_REQUEST_TIMEOUT, false);
                    Client.Grid.OnGridRegion -= callback;

                }
                return _GridInfo;
            }
            set
            {           
                GridInfoKnown = true;
                _GridInfo = value;
                if (value.WaterHeight == 0) _GridInfo.WaterHeight = 20;
                PathStore.WaterHeight = value.WaterHeight;
                PathStore.RegionName = _GridInfo.Name;
                regionEvent.Set();
                //Client.Grid.RequestMapRegion(PathStore.RegionName, GridLayerType.Terrain);
            }
        }

        public float[] ResizeTerrain512Interpolation(float[] heightMap,int m_regionWidth,int m_regionHeight)
        {
            float[] returnarr = new float[262144];
            float[,] resultarr = new float[m_regionWidth, m_regionHeight];

            // Filling out the array into it's multi-dimentional components
            for (int y = 0; y < m_regionHeight; y++)
            {
                for (int x = 0; x < m_regionWidth; x++)
                {
                    resultarr[y, x] = heightMap[y * m_regionWidth + x];
                }
            }

            // Resize using interpolation

            // This particular way is quick but it only works on a multiple of the original

            // The idea behind this method can be described with the following diagrams
            // second pass and third pass happen in the same loop really..  just separated
            // them to show what this does.

            // First Pass
            // ResultArr:
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1

            // Second Pass
            // ResultArr2:
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,

            // Third pass fills in the blanks
            // ResultArr2:
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1

            // X,Y = .
            // X+1,y = ^
            // X,Y+1 = *
            // X+1,Y+1 = #

            // Filling in like this;
            // .*
            // ^#
            // 1st .
            // 2nd *
            // 3rd ^
            // 4th #
            // on single loop.

            float[,] resultarr2 = new float[512, 512];
            for (int y = 0; y < m_regionHeight; y++)
            {
                for (int x = 0; x < m_regionWidth; x++)
                {
                    resultarr2[y * 2, x * 2] = resultarr[y, x];

                    if (y < m_regionHeight)
                    {
                        if (y + 1 < m_regionHeight)
                        {
                            if (x + 1 < m_regionWidth)
                            {
                                resultarr2[(y * 2) + 1, x * 2] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                               resultarr[y, x + 1] + resultarr[y + 1, x + 1]) / 4);
                            }
                            else
                            {
                                resultarr2[(y * 2) + 1, x * 2] = ((resultarr[y, x] + resultarr[y + 1, x]) / 2);
                            }
                        }
                        else
                        {
                            resultarr2[(y * 2) + 1, x * 2] = resultarr[y, x];
                        }
                    }
                    if (x < m_regionWidth)
                    {
                        if (x + 1 < m_regionWidth)
                        {
                            if (y + 1 < m_regionHeight)
                            {
                                resultarr2[y * 2, (x * 2) + 1] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                               resultarr[y, x + 1] + resultarr[y + 1, x + 1]) / 4);
                            }
                            else
                            {
                                resultarr2[y * 2, (x * 2) + 1] = ((resultarr[y, x] + resultarr[y, x + 1]) / 2);
                            }
                        }
                        else
                        {
                            resultarr2[y * 2, (x * 2) + 1] = resultarr[y, x];
                        }
                    }
                    if (x < m_regionWidth && y < m_regionHeight)
                    {
                        if ((x + 1 < m_regionWidth) && (y + 1 < m_regionHeight))
                        {
                            resultarr2[(y * 2) + 1, (x * 2) + 1] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                                 resultarr[y, x + 1] + resultarr[y + 1, x + 1]) / 4);
                        }
                        else
                        {
                            resultarr2[(y * 2) + 1, (x * 2) + 1] = resultarr[y, x];
                        }
                    }
                }
            }
            //Flatten out the array
            int i = 0;
            for (int y = 0; y < 512; y++)
            {
                for (int x = 0; x < 512; x++)
                {
                    if (Single.IsNaN(resultarr2[y, x]) || Single.IsInfinity(resultarr2[y, x]))
                    {
                        Logger.Log("[PHYSICS]: Non finite heightfield element detected.  Setting it to 0",Helpers.LogLevel.Warning);
                        resultarr2[y, x] = 0;
                    }

                    if (resultarr2[y, x] <= 0)
                    {
                        returnarr[i] = 0.0000001f;

                    }
                    else
                        returnarr[i] = resultarr2[y, x];

                    i++;
                }
            }

            return returnarr;
        }

        List<Simulator> _Simulators = new List<Simulator>();

        /// <summary>
        ///Getter gets the best simulator and the setter adds the simulator to the known collection
        /// </summary>
        public Simulator TheSimulator
        {
            get
            {
                Simulator best = null;
                lock (_Simulators) foreach (Simulator S in _Simulators)
                    {
                        if (!S.Connected)
                        {
                            if (best != null) continue;
                        }
                        if (!S.IsRunning)
                        {
                            if (best != null) continue;
                        }
                        if (!S.Client.Settings.OBJECT_TRACKING)
                        {
                            if (best != null) continue;
                        }
                        best = S;
                    }
                return best;
            }
            set
            {
                if (value == null) return;
                lock (_Simulators)
                    if (!_Simulators.Contains(value))
                        _Simulators.Add(value);
                Simulator simulator = TheSimulator;
                if (simulator == value) return;
                SetMaster((GridClient)value.Client);
            }
        }

        public string RegionName
        {
            get
            {
                if (PathStore.RegionName == null)
                {
                    Simulator sim = TheSimulator;
                    if (sim != null && !String.IsNullOrEmpty(sim.Name))
                        PathStore.RegionName = sim.Name;
                }
                if (!String.IsNullOrEmpty(_GridInfo.Name))
                {
                    PathStore.RegionName = _GridInfo.Name;
                }
                if (PathStore.RegionName != null) return PathStore.RegionName;
                return "region" + RegionHandle;
            }
            set { _GridInfo.Name = value; PathStore.RegionName = value; }
        }

        readonly public ulong RegionHandle;
        public SimRegion(ulong Handle)
        {
            RegionHandle = Handle;
            // RegionName = gridRegionName;
            //WorldSystem = worldSystem;
            //Console.WriteLine("++++++++++++++++++++++++++Created region: ");
            PathStore = SimPathStore.GetPathStore(GetGridLocation());
            PathStore.SetGroundLevel(this.GetGroundLevel);
           // if (PathStore.RegionName
            //new SimPathStore("region" + Handle + ".serz", GetGridLocation(), GetWorldPosition(), new Vector3(256, 256, float.MaxValue));
        }

        public void SetNodeQualityTimer(Vector3 vector3, int value, int seconds)
        {
            SimPathStore PathStore = GetPathStore(vector3);
            Point P = PathStore.ToPoint(vector3);
            CollisionIndex WP = PathStore.GetCollisionIndex(P.X, P.Y);
            WP.SetNodeQualityTimer(PathStore.GetCollisionPlane(vector3.Z), value, seconds);
        }

        public float WaterHeight()
        {
            return GridInfo.WaterHeight;
        }

        public void ShowDebugger()
        {
                PathStore.ShowDebugger();
        }
        /// <summary>
        ///  The closet usable space to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public Vector3 GetUsableLocalPositionOf(Vector3 v3, float useDist)
        {
            return PathStore.GetUsableLocalPositionOf(PathStore.GetCollisionPlane(v3.Z), v3, useDist);
        }

        /// <summary>
        ///  The closet usable waypoint to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public SimWaypoint GetWaypointOf(Vector3 v3)
        {
            return PathStore.GetWaypointOf(v3);
        }

        #region SimPosition Members

        /// <summary>
        /// The middle of the Region
        /// </summary>
        /// <returns></returns>
        public Vector3 GetSimPosition()
        {
            float size = GetSizeDistance();
            return new Vector3(size, size, GridInfo.WaterHeight);
        }

        /// <summary>
        /// The closet usable space to the middle of the Region
        /// </summary>
        /// <returns></returns>
        //public Vector3 GetUsePosition()
        //{
        //    return GetUsableLocalPositionOf(GetSimPosition(), GetSizeDistance());
        //}

        public float GetSizeDistance()
        {
            return 128f;
        }

        //public CollisionIndex GetWaypoint()
        //{
        //    return GetWaypointOf(GetUsePosition());
        //}

        //public bool IsRegionAttached()
        //{
        //    return GridInfo.RegionHandle != 0;
        //}

        //#endregion

        //public Vector3 LocalOuterEdge(Vector3 startLocal, SimPosition endPosOther, out SimRegion nextRegion)
        //{
        //    return PathStore.LocalOuterEdge(startLocal, endPosOther, o nextathStroe);
        //}



        //public static List<Vector3d> GetPath(Vector3d globalStart, Vector3d globalEnd, double endFudge, out bool OnlyStart)
        //{
        //    SimPosition posStart = SimWaypointImpl.CreateGlobal(globalStart);
        //    SimPosition posEnd = SimWaypointImpl.CreateGlobal(globalEnd);
        //    SimRegion regStart = posStart.GetSimRegion();
        //    SimRegion regEnd = posEnd.GetSimRegion();
        //    Vector3 localStart = posStart.GetSimPosition();
        //    Vector3 localEnd = posEnd.GetSimPosition();
        //    List<Vector3d> route;
        //    // Same region?
        //    if (regStart == regEnd)
        //    {
        //        return regStart.GetAtLeastPartial(localStart, localEnd,(float) endFudge, out OnlyStart);
        //    }
        //    OnlyStart = true; // will be only a partial path
        //    SimRegion nextRegion;
        //    Vector3 localLast = regStart.LocalOuterEdge(localStart, posEnd, out nextRegion);
        //    // needs to go to edge
        //    route = regStart.GetLocalPath(localStart, localLast);
        //    // at egde so make a crossing
        //    Vector3 enterEdge = EnterEdge(localLast, nextRegion.GetGridLocation() - regStart.GetGridLocation());
        //    route.Add(nextRegion.LocalToGlobal(enterEdge));
        //    return route;
        //}

        //internal List<Vector3d> GetAtLeastPartial(Vector3 localStart, Vector3 localEnd, float endFudge, out bool OnlyStart)
        //{
        //    List<Vector3d> route;
        //    Vector3 newEnd = localEnd;
        //    route = GetLocalPath(localStart, newEnd);
        //    if (route.Count > 1)
        //    {
        //        OnlyStart = false;
        //        return route;
        //    }
        //    OnlyStart = true;
        //    Vector3 diff = localEnd - localStart;
        //    while (diff.Length() > 10)
        //    {
        //        diff = diff * 0.8f;
        //        newEnd = localStart + diff;
        //        route = GetLocalPath(localStart, newEnd);
        //        if (route.Count > 1) return route;
        //    }
        //    OnlyStart = false; // Since this will be the best
        //    // try to move to nearby
        //    float step = 45 * SimPathStore.RAD2DEG;
        //    for (double angle = 0; angle < SimPathStore.PI2; angle += step)
        //    {
        //        newEnd = localEnd + ZAngleVector(angle) * endFudge;
        //        route = GetLocalPath(localStart, newEnd);
        //        if (route.Count > 1) return route;
        //    }
        //    route = new List<Vector3d>();
        //    route.Add(LocalToGlobal(localStart));
        //    SimPathStore PathStore = GetPathStore(localStart);
        //    CollisionPlane CP = PathStore.GetCollisionPlane(localStart.Z);

        //    Console.WriteLine("very bad fake route for " + CP);
        //    return route;
        //}

        //public Vector3 ZAngleVector(double ZAngle)
        //{
        //    while (ZAngle < 0)
        //    {
        //        ZAngle += SimPathStore.PI2;
        //    }
        //    while (ZAngle > SimPathStore.PI2)
        //    {
        //        ZAngle -= SimPathStore.PI2;
        //    }
        //    return new Vector3((float)Math.Sin(ZAngle), (float)Math.Cos(ZAngle), 0);
        //}

        //public static Vector3 EnterEdge(Vector3 localLast, Vector2 dir)
        //{
        //    if (Math.Abs(dir.X) > Math.Abs(dir.Y))
        //    {
        //        dir.Y = 0;
        //    }
        //    else  // avoid diagonals
        //    {
        //        dir.X = 0;
        //    }

        //    Vector3 exitEdge = new Vector3(localLast);
        //    if (dir.X != 0)
        //    {
        //        exitEdge.X = 256f - exitEdge.X;
        //    } 
        //    if (dir.Y != 0)
        //    {
        //        exitEdge.Y = 256f - exitEdge.Y;
        //    }
        //    return exitEdge;
        //}

        ////public static Vector3 FindExitEdge(SimPosition current, SimPosition other)
        ////{
        ////    SimRegion R = current.GetSimRegion();
        ////    return R.LocalOuterEdge(current.GetSimPosition(), other);
        ////}

        //public List<Vector3d> GetLocalPath(Vector3 start, Vector3 end)
        //{
        //    if (!TerrainBaked) BakeTerrain();
        //    SimPathStore PathStore = GetPathStore(start);
        //    return (List<Vector3d>)PathStore.GetLocalPath(GetUsableLocalPositionOf(start, 4), GetUsableLocalPositionOf(end, 4));
        //}

        //public void SetPassable(float x, float y, float z)
        //{
        //    SimPathStore PathStore = GetPathStore(new Vector3(x,y,z));
        //    PathStore.SetPassable(x, y,z);
        //}

        //public CollisionIndex SetObjectAt(float x, float y, float z, SimMesh simObject)
        //{
        //    SimPathStore PathStore = GetPathStore(new Vector3(x, y, z));
        //    return PathStore.SetObjectAt(x, y, simObject,z,z);
        //}

        ////public void SetBlocked(float x, float y, ISimObject simObject)
        ////{
        ////    PathStore.SetBlocked(x, y, simObject);
        ////}

        //public SimWaypoint CreateClosestRegionWaypoint(Vector3 v3, float dist)
        //{
        //    SimPathStore PathStore = GetPathStore(v3);
        //    return PathStore.CreateClosestRegionWaypoint(v3, dist);
        //}

        //#region SimPosition Members


        //public Quaternion GetSimRotation()
        //{
        //    return Quaternion.Identity;
        //}

        ////public List<ISimObject> ObjectsBottemToTop(float ix, float iy)
        ////{
        ////    return SortObjectByStacked(ObjectsAt1x1(ix, iy),GetGroundLevel((int)ix,(int)iy));
        ////}

        //internal List<SimObject> SortObjectByStacked(List<SimObject> list, float groundLevel)
        //{
        //    if (list.Count > 1)
        //    {
        //        list.Sort(SimObjectImpl.CompareLowestZ);
        //    }
        //    return list;
        //}

        //public List<ISimObject> ObjectsAt1x1(float ix, float iy)
        //{
        //    List<ISimObject> objects = new List<ISimObject>();
        //    float fx = (float)Math.Floor(ix);
        //    float fy = (float)Math.Floor(iy);
        //    float fex = fx + 1f;
        //    float fey = fy + 1f;
        //    float StepSize = PathStore.StepSize;
        //    for (float x = fx; x < fex; x += StepSize)
        //        for (float y = fy; y < fey; y += StepSize)
        //        {
        //            foreach (ISimObject A in PathStore.ObjectsAt(x, y))
        //            {
        //                if (!objects.Contains(A))
        //                    objects.Add(A);
        //            }
        //        }
        //    return objects;
        //}

        //public void SimZLevelBlocks(CallbackXY cb)
        //{
        //    int N = (int)NESW.N;
        //    int NW = (int)NESW.NW;
        //    for (int x = 1; x < 254; x++)
        //    {
        //        for (int y = 1; y < 254; y++)
        //        {
        //            float average = 0.0f;
        //            for (int i = N; i <= NW; i++)
        //            {

        //                float d = SimZLevel(x + XYOf[i].X, y + XYOf[i].Y);
        //                average += d;
        //            }
        //            average /= 8f;
        //            float diff = Math.Abs(average - SimZLevel(x, y));
        //            if (diff > 1f)
        //            {
        //                // bump
        //                cb(x, y);
        //            }
        //        }
        //    }
        //}

        #endregion

        public float AverageHieght = 21.5f;

        //float SimZLevel(float vx, float vy)
        //{
        //    return PathStore.SimLevel(vx,vy);          
        //}

        int GetGroundLevelTried = 0;
        public float GetGroundLevel(float x, float y)
        {
            if (Client != null && x >= 0 && x < 256 && y >= 0 && y < 256)
            {
                float height;
                while(!Client.Terrain.TerrainHeightAtPoint(RegionHandle, (int)x, (int)y, out height))
                {
                    if (GetGroundLevelTried > 20)
                    {
                        return AverageHieght;
                    }
                    GetGroundLevelTried++;
                    if (GetGroundLevelTried == 1)
                    {
                        Client.Grid.RequestMapRegion(RegionName, GridLayerType.Terrain);
                    }
                    if (GetGroundLevelTried > 20)
                    {
                        return AverageHieght;
                    }
                    Thread.Sleep(4000);
                    if (GetGroundLevelTried > 10)
                    {
                        Console.WriteLine("BADDDDD Height " + x + " " + y + " waiting " + AverageHieght + " sim " + RegionName);
                        return AverageHieght;
                    }
                }
                AverageHieght = height;
                return height;
                //Client.Grid.RequestMapRegion(
            }
            return AverageHieght;
        }

        public static bool IsMaster(Simulator simulator, GridClient client)
        {
            SimRegion R = GetRegion(simulator);
            if (R.Client == null)
            {
                R.SetMaster(client);
                return true;
            }
            if (R.Client == client)
            {
                EnsureClientEvents(client);
                return true;
            }
            return false;
        }

        public void SetMaster(GridClient gridClient)
        {
            if (Client == gridClient) return;
            Client = gridClient;
            Console.WriteLine("SetMaster " + GridInfo.Name);
            EnsureClientEvents(Client);
        }

        public static void EnsureClientEvents(GridClient client)
        {
            client.Settings.ALWAYS_DECODE_OBJECTS = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_ACL = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_DWELL = true;
            client.Settings.STORE_LAND_PATCHES = true;
            client.Settings.OBJECT_TRACKING = true;
            client.Settings.PARCEL_TRACKING = true;
            // client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
            //WorldObjects.BotClientFor(client).WorldSystem.WorldMaster(true);           
        }

        public byte[] TextureBytesToUUID(UUID uUID)
        {
            return WorldObjects.Master.TextureBytesFormUUID(uUID);
        }

        bool TerrainBaked = false;
        object TerrainBakedLock = new object();
        public void BakeTerrain()
        {
            lock (TerrainBakedLock)
            {
               // if (TerrainBaked) return;
                TerrainBaked = true;
                PathStore.BakeTerrain();
            }
        }

        internal void BlockRange(float x, float y, float sx, float sy, float z)
        {
            SimPathStore PathStore = GetPathStore(new Vector3(x, y, z));
            PathStore.BlockRange(x, y, sx, sy, z);
        }

        #region SimPosition Members


        public Vector3d GetWorldPosition()
        {
            return LocalToGlobal(GetSimPosition());
        }

        #endregion

        #region SimPosition Members


        public SimRegion GetSimRegion()
        {
            return this;
        }

        #endregion

        public override string ToString()
        {
            Simulator sim = TheSimulator;
            if (sim != null) return sim.ToString();
            return "sim " + RegionName;

        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="zAngleFromFace"></param>
        /// <param name="distance"></param>
        /// <returns></returns>
        static public Vector3 GetLocalLeftPos(SimPosition pos, int zAngleFromFace, double distance)
        {
            double RAD_TO_DEG = 57.29577951f;
            double Pi2 = (double)(Math.PI * 2.0);

            while (zAngleFromFace > 360)
            {
                zAngleFromFace -= 360;
            }
            while (zAngleFromFace < 0)
            {
                zAngleFromFace += 360;
            }

            double radAngle = zAngleFromFace / RAD_TO_DEG;


            Quaternion rot = pos.GetSimRotation();
            rot.Normalize();
            float rx, ry, rz;
            rot.GetEulerAngles(out rx, out rz, out ry);
            //if (rx != 0f || ry != 0f)
            //{
            //    Debug("180 Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            //else
            //{
            //    Debug("Current Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            double az = rz + radAngle;


            while (az < 0)
            {
                az += Pi2;
            }
            while (az > Pi2)
            {
                az -= Pi2;
            }

            float xmul = (float)Math.Cos(az);
            float ymul = (float)Math.Sin(az);
            Vector3 diff = new Vector3(xmul, ymul, 0) * (float)distance;

            Vector3 result = pos.GetSimPosition() + diff;

            if (result.X > 254f)
            {
                result.X = 254;
            }
            else if (result.X < 1f)
            {
                result.X = 1;
            }
            if (result.Y > 254f)
            {
                result.Y = 254;
            }
            else if (result.Y < 1f)
            {
                result.Y = 1;
            }
            return result;
            /*
             * Client.Self.Movement.SendManualUpdate(AgentManager.ControlFlags.AGENT_CONTROL_AT_POS, Client.Self.Movement.Camera.Position,
                    Client.Self.Movement.Camera.AtAxis, Client.Self.Movement.Camera.LeftAxis, Client.Self.Movement.Camera.UpAxis,
                    Client.Self.Movement.BodyRotation, Client.Self.Movement.HeadRotation, Client.Self.Movement.Camera.Far, AgentFlags.None,
                    AgentState.None, true);*/
        }


        /// <summary>
        /// 
        /// </summary>
        /// <param name="zAngleFromFace"></param>
        /// <param name="distance"></param>
        /// <returns></returns>
        static public Vector3d GetGlobalLeftPos(SimPosition pos, int zAngleFromFace, double distance)
        {
            double RAD_TO_DEG = 57.29577951f;
            double Pi2 = (double)(Math.PI * 2.0);

            while (zAngleFromFace > 360)
            {
                zAngleFromFace -= 360;
            }
            while (zAngleFromFace < 0)
            {
                zAngleFromFace += 360;
            }

            double radAngle = zAngleFromFace / RAD_TO_DEG;


            Quaternion rot = pos.GetSimRotation();
            rot.Normalize();
            float rx, ry, rz;
            rot.GetEulerAngles(out rx, out rz, out ry);
            //if (rx != 0f || ry != 0f)
            //{
            //    Debug("180 Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            //else
            //{
            //    Debug("Current Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            double az = rz + radAngle;


            while (az < 0)
            {
                az += Pi2;
            }
            while (az > Pi2)
            {
                az -= Pi2;
            }

            double xmul = (double)Math.Cos(az);
            double ymul = (double)Math.Sin(az);
            Vector3d diff = new Vector3d(xmul, ymul, 0) * distance;

            Vector3d result = pos.GetWorldPosition() + diff;

            return result;
            /*
             * Client.Self.Movement.SendManualUpdate(AgentManager.ControlFlags.AGENT_CONTROL_AT_POS, Client.Self.Movement.Camera.Position,
                    Client.Self.Movement.Camera.AtAxis, Client.Self.Movement.Camera.LeftAxis, Client.Self.Movement.Camera.UpAxis,
                    Client.Self.Movement.BodyRotation, Client.Self.Movement.HeadRotation, Client.Self.Movement.Camera.Far, AgentFlags.None,
                    AgentState.None, true);*/
        }

        //ISceneProvider SceneProvider = new SceneManager();

        //internal static ISceneProvider SceneProviderFromSimulator(Simulator sim)
        //{
        //    return GetRegion(sim).SceneProvider;
        //}

        internal static void TaintArea(Vector3d targetPos)
        {
            // if <0,0,0> return;
            if (!InWorld(targetPos)) return;
            SimRegion R = GetRegion(targetPos);
            Vector3 V = GlobalToLocal(targetPos);
            CollisionIndex W = R.GetCollisionIndex(V);
            W.RegionTaintedThis();
        }

        private CollisionIndex GetCollisionIndex(Vector3 V)
        {
            throw new NotImplementedException();
        }

        internal static bool InWorld(Vector3d targetPos)
        {
            if (targetPos.X < 255) return false;
            if (targetPos.Y < 255) return false;
            return true;
        }

        internal string NetworkInfo()
        {
            Simulator master = TheSimulator;
            string sdebug = "ConnectionInfo: " + RegionName + "\n " + ScriptEventListener.argString(GridInfo);
            lock (_Simulators) foreach (Simulator S in _Simulators)
                {
                    sdebug += "\n  " + ((S == master) ? "*" : " ");
                    sdebug += "" + S;
                    if (!S.Connected) sdebug += " DISCONNECTED ";
                    else sdebug += " CONNECTED ";
                    sdebug += S.Client;
                }
            return sdebug;
        }

        SimPathStore PathStore;
        internal SimPathStore GetPathStore(Vector3 vector3)
        {
            return PathStore;

            //lock (PathStores)
            //{
            //    foreach (SimPathStore PS in PathStores)
            //    {
            //        if (PS.OuterBounds.IsInside(vector3.X, vector3.Y, vector3.Z))
            //        {
            //            return PS;
            //        }

            //    }
            //    if (OutOfRegion(vector3)) return null;
            //    SimPathStore PPS = null;
            //            //new SimPathStore("region" + RegionHandle + ".serz", this, new Vector3(0, 0, 10), new Vector3(256, 256, 30));
            //    //PathStores.Add(PPS);
            //    return PPS;
            //}
        }

        public static bool OutOfRegion(Vector3 v3)
        {
            if (v3.X < 0 || v3.X >= 256f)
                return true;
            if (v3.Y < 0 || v3.Y >= 256f)
                return true;
            if (v3 == Vector3.Zero) return true;
            return false;
        }

        public bool IsPassable(Vector3 next)
        {
            if (OutOfRegion(next)) return false;
            SimPathStore PathStore = GetPathStore(next);
            CollisionPlane CP = PathStore.GetCollisionPlane(next.Z);
            return PathStore.IsPassable(next,CP);
        }

        public void UpdateTraveled(UUID uUID, Vector3 vector3, Quaternion quaternion)
        {
            GetPathStore(vector3).UpdateTraveled(uUID,vector3,quaternion);
        }

        internal void Refresh(Box3Fill changed)
        {
           // throw new NotImplementedException();
        }
      
        internal void AddCollisions(SimMesh C)
        {
            C.UpdateOccupied();
        }

    }
}
