using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims.Navigation;
using cogbot.Listeners;
using cogbot.TheOpenSims.Navigation.Debug;
using cogbot.TheOpenSims.Mesher;
using Simian;

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
    public class SimRegion
    {

        public static void BakeRegions()
        {
            lock (_CurrentRegions)
            {
                foreach (SimRegion R in _CurrentRegions.Values)
                {
                    R.PathStore.UpdateMatrix();
                }
            }
        }

        public static implicit operator SimPathStore(SimRegion m)
        {
            return m.PathStore;
        }
        public static implicit operator SimRegion(SimPathStore m)
        {
            return m.GetSimRegion();
        }


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
                if (_CurrentRegions.ContainsKey(id))
                    return _CurrentRegions[id];

                SimRegion R = new SimRegion(id);
                _CurrentRegions[id] = R;
                return R;
            }
        }

        public uint regionX = 0, regionY = 0;
        public Vector2 GetGridLocation()
        {
            if (regionX == 0)
                Utils.LongToUInts(RegionHandle, out regionX, out regionY);
            return new Vector2((float)(regionX / 256), (float)(regionY / 256));
        }

        static public SimRegion GetRegion(string simname)
        {
            foreach (SimRegion R in _CurrentRegions.Values)
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


        readonly public SimPathStore PathStore;

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

        static SimRegion GetRegion(Vector2 v2)
        {
            ulong h = HandleOf(v2);
            if (_CurrentRegions.ContainsKey(h))
            {
                return _CurrentRegions[h];
            }
            return null;
        }

        public static ulong HandleOf(Vector2 v2)
        {
            return Utils.UIntsToLong((uint)v2.X * 256, (uint)v2.Y * 256);
        }

        public Vector3d LocalToGlobal(Vector3 objectLoc)
        {
            Vector2 V2 = GetGridLocation();
            return new Vector3d(V2.X * 256 + objectLoc.X, V2.Y * 256 + objectLoc.Y, objectLoc.Z);
        }

        PathFinderDemo PathFinder;
        bool GridInfoKnown = false;
        GridRegion _GridInfo;
        BotClient Client;
        public GridRegion GridInfo
        {
            get
            {
                if (GridInfoKnown) return _GridInfo;
                if (!String.IsNullOrEmpty(_RegionName))
                {
                    GridRegion r;
                    if (Client.Grid.GetGridRegion(_RegionName, GridLayerType.Objects, out r))
                    {
                        _GridInfo = r;
                        GridInfoKnown = true;
                    }
                    else
                        if (Client.Grid.GetGridRegion(_RegionName, GridLayerType.Terrain, out r))
                        {
                            _GridInfo = r;
                            GridInfoKnown = true;
                        }
                        else
                            if (Client.Grid.GetGridRegion(_RegionName, GridLayerType.LandForSale, out r))
                            {
                                _GridInfo = r;
                                GridInfoKnown = true;
                            }

                }
                return _GridInfo;
            }
            set
            {
                _GridInfo = value;
                GridInfoKnown = true;
                _RegionName = _GridInfo.Name;
            }
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
                if (simulator != value) return;
                SetMaster((BotClient)value.Client);
            }
        }

        string _RegionName;
        public string RegionName
        {
            get
            {
                if (_RegionName == null)
                {
                    if (TheSimulator != null && !String.IsNullOrEmpty(TheSimulator.Name))
                        _RegionName = TheSimulator.Name;
                }
                if (!String.IsNullOrEmpty(GridInfo.Name))
                {
                    _RegionName = _GridInfo.Name;
                }
                if (_RegionName != null) return _RegionName;
                return "region" + RegionHandle;
            }
            set { _GridInfo.Name = value; }
        }

        readonly public ulong RegionHandle;
        public SimRegion(ulong Handle)
        {
            RegionHandle = Handle;
            // RegionName = gridRegionName;
            //WorldSystem = worldSystem;
            //Console.WriteLine("++++++++++++++++++++++++++Created region: ");
            PathStore = new SimPathStore("region" + Handle + ".serz", Handle);
        }

        public float WaterHeight()
        {
            return GridInfo.WaterHeight;
        }

        public void ShowDebugger()
        {
            if (PathFinder == null)
            {
                PathFinder = new PathFinderDemo(PathStore);
            }
            PathFinder.Show();
        }
        /// <summary>
        ///  The closet usable space to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public Vector3 GetUsableLocalPositionOf(Vector3 v3, float useDist)
        {
            byte b = PathStore.GetNodeQuality(v3);
            // float useDist = GetSizeDistance();
            if (b > 0) return v3;
            SimWaypoint swp = GetWaypointOf(v3);
            for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = SimRegion.GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3);
                    if (b > 0) return v3;
                }
            }
            Console.WriteLine("Clearing area " + swp);
            PathStore.SetNodeQualityTimer(v3, 200,30);
            for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = SimRegion.GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3);
                    if (b == 0)
                    {
                        PathStore.SetNodeQualityTimer(v3, 200,30);
                    }
                }
            }
            return GetWaypointOf(v3).GetSimPosition();
        }

        /// <summary>
        ///  The closet usable waypoint to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public SimWaypoint GetWaypointOf(Vector3 v3)
        {
            return SimWaypoint.CreateLocal(v3, this);
            SimWaypoint swp = PathStore.CreateClosestRegionWaypoint(v3, 2);
            float dist = Vector3.Distance(v3, swp.GetSimPosition());
            if (!swp.Passable)
            {
                Console.WriteLine("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
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

        //public SimWaypoint GetWaypoint()
        //{
        //    return GetWaypointOf(GetUsePosition());
        //}

        public bool IsRegionAttached()
        {
            return GridInfo.RegionHandle != 0;
        }

        #endregion

        public Vector3 LocalOuterEdge(Vector3 startLocal, SimPosition endPosOther, out SimRegion nextRegion)
        {
            SimRegion rother = endPosOther.GetSimRegion();
            Vector3 vother = endPosOther.GetSimPosition();
            if (rother == this)
            {
                nextRegion = this;
                return vother;
            }

            Vector2 VD = rother.GetGridLocation() - GetGridLocation();

            Vector2 SD = new Vector2(Math.Sign(VD.X), Math.Sign(VD.Y));
            float x = 128 + SD.X * 127;
            if (x == 128)
            {
                x = (startLocal.X + vother.X) / 2;
            }
            float y = 128 + SD.Y * 127;
            if (y == 128)
            {
                y = (startLocal.Y + vother.Y) / 2;
            }
            nextRegion = GetRegion(GetGridLocation() + SD);
            return new Vector3(x, y, vother.Z);
        }



        public static List<Vector3d> GetPath(Vector3d globalStart, Vector3d globalEnd, double endFudge, out bool OnlyStart)
        {
            SimPosition posStart = SimWaypoint.CreateGlobal(globalStart);
            SimPosition posEnd = SimWaypoint.CreateGlobal(globalEnd);
            SimRegion regStart = posStart.GetSimRegion();
            SimRegion regEnd = posEnd.GetSimRegion();
            Vector3 localStart = posStart.GetSimPosition();
            Vector3 localEnd = posEnd.GetSimPosition();
            List<Vector3d> route;
            // Same region?
            if (regStart == regEnd)
            {
                return regStart.GetAtLeastPartial(localStart, localEnd,(float) endFudge, out OnlyStart);
            }
            OnlyStart = true; // will be only a partial path
            SimRegion nextRegion;
            Vector3 localLast = regStart.LocalOuterEdge(localStart, posEnd, out nextRegion);
            // needs to go to edge
            route = regStart.GetLocalPath(localStart, localLast);
            // at egde so make a crossing
            Vector3 enterEdge = EnterEdge(localLast, nextRegion.GetGridLocation() - regStart.GetGridLocation());
            route.Add(nextRegion.LocalToGlobal(enterEdge));
            return route;
        }

        internal List<Vector3d> GetAtLeastPartial(Vector3 localStart, Vector3 localEnd, float endFudge, out bool OnlyStart)
        {
            List<Vector3d> route;
            Vector3 newEnd = localEnd;
            route = GetLocalPath(localStart, newEnd);
            if (route.Count > 1)
            {
                OnlyStart = false;
                return route;
            }
            OnlyStart = true;
            Vector3 diff = localEnd - localStart;
            while (diff.Length() > 10)
            {
                diff = diff * 0.8f;
                newEnd = localStart + diff;
                route = GetLocalPath(localStart, newEnd);
                if (route.Count > 1) return route;
            }
            OnlyStart = false; // Since this will be the best
            // try to move to nearby
            float step = 45 * SimPathStore.RAD2DEG;
            for (double angle = 0; angle < SimPathStore.PI2; angle += step)
            {
                newEnd = localEnd + ZAngleVector(angle) * endFudge;
                route = GetLocalPath(localStart, newEnd);
                if (route.Count > 1) return route;
            }
            route = new List<Vector3d>();
            route.Add(LocalToGlobal(localStart));
            Console.WriteLine("very bad fake route ");
            return route;
        }

        public Vector3 ZAngleVector(double ZAngle)
        {
            while (ZAngle < 0)
            {
                ZAngle += SimPathStore.PI2;
            }
            while (ZAngle > SimPathStore.PI2)
            {
                ZAngle -= SimPathStore.PI2;
            }
            return new Vector3((float)Math.Sin(ZAngle), (float)Math.Cos(ZAngle), 0);
        }

        public static Vector3 EnterEdge(Vector3 localLast, Vector2 dir)
        {
            if (Math.Abs(dir.X) > Math.Abs(dir.Y))
            {
                dir.Y = 0;
            }
            else  // avoid diagonals
            {
                dir.X = 0;
            }

            Vector3 exitEdge = new Vector3(localLast);
            if (dir.X != 0)
            {
                exitEdge.X = 256f - exitEdge.X;
            } 
            if (dir.Y != 0)
            {
                exitEdge.Y = 256f - exitEdge.Y;
            }
            return exitEdge;
        }

        //public static Vector3 FindExitEdge(SimPosition current, SimPosition other)
        //{
        //    SimRegion R = current.GetSimRegion();
        //    return R.LocalOuterEdge(current.GetSimPosition(), other);
        //}

        public List<Vector3d> GetLocalPath(Vector3 start, Vector3 end)
        {
            if (!TerrainBaked) BakeTerrain();
            return (List<Vector3d>)PathStore.GetLocalPath(GetUsableLocalPositionOf(start,4), GetUsableLocalPositionOf(end,4), PathFinder);
        }

        public void SetPassable(float x, float y)
        {
            PathStore.SetPassable(x, y);
        }

        public void SetObjectAt(float x, float y, SimObject simObject, float minZ, float maxZ)
        {
            PathStore.SetObjectAt(x, y, simObject,minZ,maxZ);
        }

        //public void SetBlocked(float x, float y, SimObject simObject)
        //{
        //    PathStore.SetBlocked(x, y, simObject);
        //}

        public SimWaypoint CreateClosestRegionWaypoint(Vector3 v3, float dist)
        {
            return PathStore.CreateClosestRegionWaypoint(v3, dist);
        }

        #region SimPosition Members


        public Quaternion GetSimRotation()
        {
            return Quaternion.Identity;
        }

        //public List<SimObject> ObjectsBottemToTop(float ix, float iy)
        //{
        //    return SortObjectByStacked(ObjectsAt1x1(ix, iy),GetGroundLevel((int)ix,(int)iy));
        //}

        internal List<SimObject> SortObjectByStacked(List<SimObject> list, float groundLevel)
        {
            if (list.Count > 1)
            {
                list.Sort(SimObject.CompareLowestZ);
            }
            return list;
        }

        //public List<SimObject> ObjectsAt1x1(float ix, float iy)
        //{
        //    List<SimObject> objects = new List<SimObject>();
        //    float fx = (float)Math.Floor(ix);
        //    float fy = (float)Math.Floor(iy);
        //    float fex = fx + 1f;
        //    float fey = fy + 1f;
        //    float StepSize = PathStore.StepSize;
        //    for (float x = fx; x < fex; x += StepSize)
        //        for (float y = fy; y < fey; y += StepSize)
        //        {
        //            foreach (SimObject A in PathStore.ObjectsAt(x, y))
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

        float SimZLevel(float vx, float vy)
        {
            return PathStore.SimLevel(vx,vy);          
        }

        public float GetGroundLevel(float x, float y)
        {
            float height;
            if (Client!=null && Client.Terrain.TerrainHeightAtPoint(RegionHandle, (int)x, (int)y, out height))
            {
                AverageHieght = height;
                return height;
            }
            // Console.WriteLine("BADDDDD Height " + x + " " + y + " returning " + AverageHieght + " sim " + RegionName);
            return AverageHieght;
        }

        public static bool IsMaster(Simulator simulator, BotClient client)
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

        public void SetMaster(BotClient gridClient)
        {
            if (Client == gridClient) return;
            Client = gridClient;
            Console.WriteLine("SetMaster " + GridInfo.Name);
            EnsureClientEvents(Client);
        }

        public static void EnsureClientEvents(BotClient client)
        {
            client.Settings.ALWAYS_DECODE_OBJECTS = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_ACL = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_DWELL = true;
            client.Settings.STORE_LAND_PATCHES = true;
            client.Settings.OBJECT_TRACKING = true;
            client.Settings.PARCEL_TRACKING = true;
            // client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
            client.WorldSystem.WorldMaster(true);           
        }

        public byte[] TextureBytesToUUID(UUID uUID)
        {
            return Client.WorldSystem.TextureBytesToUUID(uUID);
        }

        bool TerrainBaked = false;
        object TerrainBakedLock = new object();
        public void BakeTerrain()
        {
            lock (TerrainBakedLock)
            {
               // if (TerrainBaked) return;
                float LastHieght = GetGroundLevel(0, 0);

                Console.WriteLine("ScanTerrainBlockages: " + RegionName);
                float WH = GridInfo.WaterHeight;
                for (int y = 0; y < 256; y++)
                    for (int x = 0; x < 256; x++)
                    {
                        float thisH = GetGroundLevel(x, y);
                        float thisH2 = GetGroundLevel(x, y + 1);
                        float thisH3 = GetGroundLevel(x+1, y);
                        if (Math.Abs(thisH - LastHieght) > 1 || Math.Abs(thisH - thisH2) > 1 || Math.Abs(thisH - thisH3) > 1)
                        {
                            BlockRange(x, y, 1.001f, 1.001f);
                        }
                        LastHieght = thisH;
                    }
                TerrainBaked = true;
            }
        }

        internal void BlockRange(float x, float y, float sx, float sy)
        {
            float StepSize = PathStore.StepSize;
            sx += x;
            sy += y;
            float loopY = sy;
            while (sx >= x)
            {
                while (sy >= y)
                {
                    PathStore.SetBlocked(sx, sy, null);
                    sy -= StepSize;
                }
                sy = loopY;
                sx -= StepSize;
            }
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

        ISceneProvider SceneProvider = new SceneManager();

        internal static ISceneProvider SceneProviderFromSimulator(Simulator sim)
        {
            return GetRegion(sim).SceneProvider;
        }
    }
}
