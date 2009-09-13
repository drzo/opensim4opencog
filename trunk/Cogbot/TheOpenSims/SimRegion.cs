using System;
using System.Collections.Generic;
using System.Drawing;
using System.Text;
using System.Threading;
using cogbot.Listeners;
using cogbot.ScriptEngines;
using OpenMetaverse;
using PathSystem3D.Mesher;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public enum NESW : int
    {
        C = 0,
        N = 1,
        NE,
        E,
        SE,
        S,
        SW,
        W,
        NW
    }


    /// <summary>
    /// Denotes a Simulator region and can help with bot navigation
    /// </summary>
    public class SimRegion
    {
        private static Dictionary<ulong, SimRegion> _CurrentRegions = new Dictionary<ulong, SimRegion>();
        readonly private static Vector2 vC = new Vector2(0, 0); // NW
        readonly private static Vector2 vE = new Vector2(1, 0); // NW

        readonly private static Vector2 // C
                               vN = new Vector2(0, 1),
                               // N
                               vNE = new Vector2(1, 1); // NW

        readonly private static Vector2 // SE
            vNW = new Vector2(-1, 1); // NW

        readonly private static Vector2 // SE
                               vS = new Vector2(0, -1); // NW

        readonly private static Vector2 vSE = new Vector2(1, -1); // NW

        readonly private static Vector2 // S
                               vSW = new Vector2(-1, -1),
                               // SW
                               vW = new Vector2(-1, 0); // NW

        public static Vector2[] XYOf = {vC, vN, vNE, vE, vSE, vS, vSW, vW, vNW};
        private readonly float MAXY = 256f;
        public readonly ulong RegionHandle;
        private GridRegion _GridInfo;
        private Vector2 _GridLoc = Vector2.Zero;
        readonly private List<Simulator> _Simulators = new List<Simulator>();
        public float AverageHieght = 21.5f;
        private GridClient Client;
        private int GetGroundLevelTried = 0;
        private bool GridInfoKnown = false;
        private SimPathStore PathStore
        {
            get
            {
                if (_PathStore==null)
                {
                    _PathStore = SimPathStore.GetPathStore(GetGridLocation());
                    _PathStore.SetGroundLevel(GetGroundLevel);
                }
                return _PathStore;
            }
        }
        readonly private AutoResetEvent regionEvent = new AutoResetEvent(false);

        public static SimRegion GetRegion(UUID uuid, GridClient client)
        {
            if (client == null) client = AnyClient;
            else AnyClient = client;
            if (uuid == UUID.Zero) return null;
            foreach (var v in CurrentRegions)
            {
                if (v.Client == null) v.Client = client;
                if (uuid == v.RegionID) return v;
            }
            SimRegion r = null;
            {
                AutoResetEvent are = new AutoResetEvent(false);
                GridManager.RegionHandleReplyCallback rrc =
                    new GridManager.RegionHandleReplyCallback(delegate(UUID regionID, ulong regionHandle)
                    {
                        if (regionID == uuid)
                        {
                            are.Set();
                            r = GetRegion(regionHandle, client);
                        }

                    });
                client.Grid.OnRegionHandleReply += rrc;
                client.Grid.RequestRegionHandle(uuid);
                if (!are.WaitOne(10000, false))
                {
                    foreach (var v in CurrentRegions)
                    {
                        if (uuid == v.RegionID) return v;
                    }
                }
                client.Grid.OnRegionHandleReply -= rrc;
                if (r == null)
                {
                    return SimRegion.UNKNOWN;
                }
            }
            return r;

        }

        private UUID _RegionID;
        public static SimRegion UNKNOWN = new SimRegion(0, null);

        protected UUID RegionID
        {
            get
            {
                if (_RegionID == UUID.Zero)
                {
                    Simulator sim = TheSimulator;
                    if (sim != null) _RegionID = sim.ID;
                    if (_RegionID == UUID.Zero)
                    {
                        Client.Grid.RequestMapRegion(RegionName, GridLayerType.Terrain);
                    }
                }
                return _RegionID;
            }
        }

        private SimRegion(ulong Handle, GridClient bc)
            //: base(null, default(Vector2), default(Vector3d), default(Vector3))
        {           
            RegionHandle = Handle;
            Client = bc;
            if (bc == null) bc = AnyClient;
            else AnyClient = bc;
            // RegionName = gridRegionName;
            //WorldSystem = worldSystem;
            //Debug("++++++++++++++++++++++++++Created region: ");
            String rname = RegionName;


            // if (PathStore.RegionName
            //new SimPathStore("region" + Handle + ".serz", GetGridLocation(), GetWorldPosition(), new Vector3(256, 256, float.MaxValue));
        }

        public static List<SimRegion> CurrentRegions
        {
            get
            {
                var sims = new List<SimRegion>();
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

        public GridRegion GridInfo
        {
            get
            {
                if (GridInfoKnown) return _GridInfo;
                if (!String.IsNullOrEmpty(PathStore.RegionName))
                {
                    regionEvent.Reset();
                    GridManager.GridRegionCallback callback =
                        delegate(GridRegion gridRegion)
                            {
                                if (gridRegion.RegionHandle == RegionHandle)
                                    regionEvent.Set();
                            };
                    if (Client==null)
                    {
                        Client = AnyClient;
                    }
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
                if (value.WaterHeight != 0)
                {
                    PathStore.WaterHeight = value.WaterHeight;
                    Console.WriteLine("{0} WaterHeight = {1}", value.Name, PathStore.WaterHeight);
                }
                //PathStore.RegionName = _GridInfo.Name;
                regionEvent.Set();
                //  Client.Grid.RequestMapRegion(PathStore.RegionName, GridLayerType.Terrain);
            }
        }

        private Simulator bestSimulator;
        /// <summary>
        ///Getter gets the best simulator and the setter adds the simulator to the known collection
        /// </summary>
        public Simulator TheSimulator
        {
            get
            {
                if (bestSimulator != null) return bestSimulator;
                lock (_Simulators)
                {
                    int sc = _Simulators.Count;
                    if (sc == 1) return _Simulators[0];
                    if (sc == 0) return null;
                    foreach (Simulator sim0 in _Simulators)
                    {
                        if (!sim0.Connected)
                        {
                            if (bestSimulator != null) continue;
                        }
                        if (!sim0.IsRunning)
                        {
                            if (bestSimulator != null) continue;
                        }
                        if (!S.Client.Settings.OBJECT_TRACKING)
                        {
                            if (bestSimulator != null) continue;
                        }
                        bestSimulator = sim0;
                    }
                    return bestSimulator;
                }
            }
            set
            {
                if (value == null || bestSimulator == value) return;
                lock (_Simulators)
                {
                    bool found = false;
                    foreach (Simulator simulator in _Simulators)
                    {
                        if (simulator.Client == value.Client)
                        {
                            found = true;
                            break;
                        }
                    }

                    if (!found)
                    {
                        _Simulators.Add(value);
                        PathStore.WaterHeight = value.WaterHeight;
                        _GridInfo.WaterHeight = (byte) value.WaterHeight;
                        if (value.ID != UUID.Zero) this._RegionID = value.ID;
                        if (!string.IsNullOrEmpty(value.Name)) this.RegionName = value.Name;
                        Console.WriteLine("{0} SimWaterHeight = {1}", value.Name, PathStore.WaterHeight);
                    }
                }
                bestSimulator = TheSimulator;
                if (bestSimulator == value && Client==null) return;
                SetMaster((GridClient) value.Client);
            }
        }


        public void RemoveSim(Simulator simulator)
        {
            lock(_Simulators)
            {
                _Simulators.Remove(simulator);
            }
        }


        public string RegionName
        {
            get
            {
                if (_PathStore != null && PathStore.RegionName == null)
                {
                    Simulator sim = TheSimulator;
                    if (sim != null && !String.IsNullOrEmpty(sim.Name))
                        PathStore.RegionName = sim.Name;
                }
                if (!String.IsNullOrEmpty(_GridInfo.Name))
                {
                    if (_PathStore != null) PathStore.RegionName = _GridInfo.Name;
                    return _GridInfo.Name;
                }
                if (_PathStore != null && PathStore.RegionName != null)
                {
                    return PathStore.RegionName;
                }
                return "region" + RegionHandle;
            }
            set
            {
                _GridInfo.Name = value;
                if (_PathStore != null) PathStore.RegionName = value;
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
                R.RecomputeMatrix();
            }
        }

        private void RecomputeMatrix()
        {
            PathStore.RecomputeMatrix();
        }

        public static SimRegion GetRegion(Vector3d pos)
        {
            if (pos.X < 0 || pos.Y < 0)
            {
                throw new ArgumentException("GlobalToWaypoint? " + pos);
            }
            if (pos.X < 256 || pos.Y < 256)
            {
                Debug("GlobalToWaypoint? " + pos);
            }
            return GetRegion(Utils.UIntsToLong(Round256(pos.X), Round256(pos.Y)), null);
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
            uint x = ((uint) globalX/256)*256;
            uint y = ((uint) globalY/256)*256;
            localX = globalX - (float) x;
            localY = globalY - (float) y;
            return Utils.UIntsToLong(x, y);
        }

        public static SimWaypoint GetWaypoint(Vector3d gloabl)
        {
            if (gloabl.X == float.NaN) return null;
            uint x = Round256(gloabl.X);
            uint y = Round256(gloabl.Y);
            Vector3 local;
            local.X = (float) gloabl.X - x;
            local.Y = (float) gloabl.Y - y;
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
            SimRegion R = GetRegion(Utils.UIntsToLong(x, y), null);
            local.Z = (float) gloabl.Z;
            try
            {
                return R.GetWaypointOf(local);
            }
            catch (Exception e)
            {
                Debug("" + e);
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
                Debug("GlobalToWaypoint? " + pos);
            }
            return new Vector3((float) pos.X - Round256(pos.X), (float) pos.Y - Round256(pos.Y), (float) pos.Z);
        }

        public static uint Round256(double global)
        {
            return ((uint) global/256)*256;
        }


        internal static SimRegion GetRegion(ulong handle)
        {
            return GetRegion(handle, null);
        }

        public static SimRegion GetRegion(ulong id, GridClient bc)
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
                if (_CurrentRegions.Count > 10)
                {
                 //   return null;  
                }
                R = new SimRegion(id, bc);
                _CurrentRegions[id] = R;
                return R;
            }
        }

        public Vector2 GetGridLocation()
        {
            if (_GridLoc == Vector2.Zero)
            {
                uint regionX = 0, regionY = 0;
                Utils.LongToUInts(RegionHandle, out regionX, out regionY);
                _GridLoc = new Vector2((float) (regionX/256), (float) (regionY/256));
            }
            return _GridLoc;
        }

        public static SimRegion GetRegion(string simname)
        {
            foreach (SimRegion R in CurrentRegions)
                if (R.RegionName.Contains(simname)) return R;
            return null;
        }

        public static SimRegion GetRegion(Simulator sim)
        {
            if (sim == null) return null;
            ulong Handle = sim.Handle;
            SimRegion R = GetRegion(Handle,sim.Client);
            R.TheSimulator = sim;
            //R.RegionHandle = Handle;
            return R;
        }


        public void SetRegionOffset(Vector2 v2, SimRegion value)
        {
            SetRegion(HandleOf(GetGridLocation() + v2), value);
        }

        public SimRegion GetOffsetRegion(Vector2 v2)
        {
            return GetRegion(GetGridLocation() + v2, this.Client);
        }

        private static void SetRegion(ulong h, SimRegion value)
        {
            if (_CurrentRegions.ContainsKey(h))
            {
                SimRegion OLD = _CurrentRegions[h];
                if (OLD == null || OLD == value) return;
                throw new ArgumentException("Bad region change " + OLD + " -> " + value);
            }
            _CurrentRegions[h] = value;
        }

        public static SimRegion GetRegion(Vector2 v2, GridClient bc)
        {
            ulong h = HandleOf(v2);
            return GetRegion(h, bc);
        }

        public static ulong HandleOf(Vector2 v2)
        {
            return Utils.UIntsToLong((uint) v2.X*256, (uint) v2.Y*256);
        }

        public Vector3d LocalToGlobal(Vector3 objectLoc)
        {
            return PathStore.LocalToGlobal(objectLoc);
        }

        public float[] ResizeTerrain512Interpolation(float[] heightMap, int m_regionWidth, int m_regionHeight)
        {
            var returnarr = new float[262144];
            var resultarr = new float[m_regionWidth,m_regionHeight];

            // Filling out the array into it's multi-dimentional components
            for (int y = 0; y < m_regionHeight; y++)
            {
                for (int x = 0; x < m_regionWidth; x++)
                {
                    resultarr[y, x] = heightMap[y*m_regionWidth + x];
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

            var resultarr2 = new float[512,512];
            for (int y = 0; y < m_regionHeight; y++)
            {
                for (int x = 0; x < m_regionWidth; x++)
                {
                    resultarr2[y*2, x*2] = resultarr[y, x];

                    if (y < m_regionHeight)
                    {
                        if (y + 1 < m_regionHeight)
                        {
                            if (x + 1 < m_regionWidth)
                            {
                                resultarr2[(y*2) + 1, x*2] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                               resultarr[y, x + 1] + resultarr[y + 1, x + 1])/4);
                            }
                            else
                            {
                                resultarr2[(y*2) + 1, x*2] = ((resultarr[y, x] + resultarr[y + 1, x])/2);
                            }
                        }
                        else
                        {
                            resultarr2[(y*2) + 1, x*2] = resultarr[y, x];
                        }
                    }
                    if (x < m_regionWidth)
                    {
                        if (x + 1 < m_regionWidth)
                        {
                            if (y + 1 < m_regionHeight)
                            {
                                resultarr2[y*2, (x*2) + 1] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                               resultarr[y, x + 1] + resultarr[y + 1, x + 1])/4);
                            }
                            else
                            {
                                resultarr2[y*2, (x*2) + 1] = ((resultarr[y, x] + resultarr[y, x + 1])/2);
                            }
                        }
                        else
                        {
                            resultarr2[y*2, (x*2) + 1] = resultarr[y, x];
                        }
                    }
                    if (x < m_regionWidth && y < m_regionHeight)
                    {
                        if ((x + 1 < m_regionWidth) && (y + 1 < m_regionHeight))
                        {
                            resultarr2[(y*2) + 1, (x*2) + 1] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                                 resultarr[y, x + 1] + resultarr[y + 1, x + 1])/4);
                        }
                        else
                        {
                            resultarr2[(y*2) + 1, (x*2) + 1] = resultarr[y, x];
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
                        Logger.Log("[PHYSICS]: Non finite heightfield element detected.  Setting it to 0",
                                   Helpers.LogLevel.Warning);
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

        public void SetNodeQualityTimer(Vector3 vector3, int value, int seconds)
        {
            SimPathStore PathStore = GetPathStore(vector3);
            Point P = PathStore.ToPoint(vector3);
            CollisionIndex WP = PathStore.GetCollisionIndex(P.X, P.Y);

            var undo = new List<ThreadStart>();
            WP.SetNodeQualityTimer(PathStore.GetCollisionPlane(vector3.Z), value, undo);
            if (undo.Count > 0)
                new Thread(() =>
                               {
                                   Thread.Sleep(seconds*1000);
                                   foreach (ThreadStart u in undo)
                                   {
                                       u();
                                   }
                               }).Start();
        }

        public void Parcels_OnParcelInfo(ParcelInfo parcel)
        {
            lock (parcelsI)
            {
                if (parcelsI.ContainsKey(parcel.ID)) return;
                parcelsI[parcel.ID] = parcel;
            }
           // Client.Parcels.GetParcelLocalID()
           // base.Parcels_OnParcelInfo(parcel);
        }

        Dictionary<int,Parcel> parcels = new Dictionary<int, Parcel>();
        Dictionary<UUID, ParcelInfo> parcelsI = new Dictionary<UUID, ParcelInfo>();
        static private GridClient AnyClient;
        private SimPathStore _PathStore;

        public void Parcels_OnParcelProperties(Simulator simulator, Parcel parcel, ParcelResult result, int selectedPrims, int sequenceID, bool snapSelection)
        {
            lock (parcels)
            {
                if (parcels.ContainsKey(parcel.LocalID)) return;
                parcels[parcel.LocalID] = parcel;
            }
            simulator.Client.Parcels.SelectObjects(parcel.LocalID, (ObjectReturnType)31, parcel.OwnerID);
            //ParcelSelectObjects(simulator, parcel.LocalID, parcel.OwnerID);
            //SimRegion r = SimRegion.GetRegion(simulator);
            //r.Parcels_OnParcelProperties(simulator, parcel, result, selectedPrims, sequenceID, snapSelection);
        }
        public void Parcels_OnParcelSelectedObjects(Simulator simulator, List<uint> objectIDs, bool resetList)
        {
            foreach (uint u in objectIDs)
            {
                WorldObjects.EnsureSelected(u, simulator);
            }
           // base.Parcels_OnParcelSelectedObjects(simulator, objectIDs, resetList);
        }

        static void ParcelSelectObjects(Simulator sim, int parcelID, UUID ownerUUID)
        {
            GridClient client = sim.Client;
            int counter = 0;
            StringBuilder result = new StringBuilder();
            // test argument that is is a valid integer, then verify we have that parcel data stored in the dictionary
            {
                AutoResetEvent wait = new AutoResetEvent(false);
                ParcelManager.ForceSelectObjects callback =
                    delegate(Simulator simulator, List<uint> objectIDs, bool resetList)
                    {
                        //result.AppendLine("New List: " + resetList.ToString());
                        for (int i = 0; i < objectIDs.Count; i++)
                        {
                            result.Append(objectIDs[i].ToString() + " ");
                            counter++;
                        }
                        //result.AppendLine("Got " + objectIDs.Count.ToString() + " Objects in packet");
                        if (objectIDs.Count < 251)
                            wait.Set();
                    };

                client.Parcels.OnParcelSelectedObjects += callback;
                client.Parcels.SelectObjects(parcelID, (ObjectReturnType)31, ownerUUID);


                client.Parcels.ObjectOwnersRequest(sim, parcelID);
                if (!wait.WaitOne(30000, false))
                {
                    result.AppendLine("Timed out waiting for packet.");
                }

                client.Parcels.OnParcelSelectedObjects -= callback;
                result.AppendLine("Found a total of " + counter + " Objects");

            }
        }

        public float WaterHeight()
        {
            return TheSimulator.WaterHeight;
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

        //    Debug("very bad fake route for " + CP);
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

        


        public float GetGroundLevel(float x, float y)
        {
            Parcel P = GetParcel(x, y);
            if (!ParcelAllowsEntry(P, Client.Self.AgentID))
            {
                return P.AABBMax.Z;
            }

            if (Client != null && x >= 0 && x < 256 && y >= 0 && y < 256)
            {
                float height;
                while (!Client.Terrain.TerrainHeightAtPoint(RegionHandle, (int) x, (int) y, out height))
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
                    Thread.Sleep(1000);
                    if (GetGroundLevelTried > 10)
                    {
                        if (Settings.LOG_LEVEL != Helpers.LogLevel.None)
                            Console.WriteLine("BADDDDD Height " + x + " " + y + " waiting " + AverageHieght + " sim " +
                                              RegionName);
                        return AverageHieght;
                    }
                }
                AverageHieght = height;
                return height;
                //Client.Grid.RequestMapRegion(
            }
            return AverageHieght;
        }

        static bool ParcelAllowsEntry(Parcel P, UUID uUID)
        {

            if ((P.Flags & ParcelFlags.UseAccessList) != 0)
            {
                foreach (var af in P.AccessWhiteList)
                {
                    if (af.AgentID == uUID)
                    {
                        return true;
                    }
                }
                return false;
            }
            if ((P.Flags & ParcelFlags.UseAccessGroup) != 0)
            {
                if (P.GroupID == uUID) return true;
                return false;
            }
            if ((P.Flags & ParcelFlags.UseBanList) != 0)
            {
                foreach (var af in P.AccessBlackList)
                {
                    if (af.AgentID == uUID)
                    {
                        return false;
                    }
                }                
                return true;
            }
            return true;
        }

        private static void Debug(string p, params object[] args)
        {
            if (Settings.LOG_LEVEL == Helpers.LogLevel.Debug)
                Console.WriteLine(p, args);
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
            Debug("SetMaster {0}", GridInfo.Name);
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
            return WorldObjects.GridMaster.TextureBytesFormUUID(uUID);
        }


        internal void BlockRange(float x, float y, float sx, float sy, float z)
        {
            SimPathStore PathStore = GetPathStore(new Vector3(x, y, z));
            PathStore.BlockRange(x, y, sx, sy, z);
        }

        public Vector3d GetWorldPosition()
        {
            return LocalToGlobal(GetSimPosition());
        }

        public SimRegion GetSimRegion()
        {
            return this;
        }

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
        public static Vector3 GetLocalLeftPos(SimPosition pos, int zAngleFromFace, double distance)
        {
            double RAD_TO_DEG = 57.29577951f;
            var Pi2 = (double) (Math.PI*2.0);

            while (zAngleFromFace > 360)
            {
                zAngleFromFace -= 360;
            }
            while (zAngleFromFace < 0)
            {
                zAngleFromFace += 360;
            }

            double radAngle = zAngleFromFace/RAD_TO_DEG;
            Quaternion rot = pos.GetSimRotation();
            Vector3 v3 = Vector3.Transform(Vector3.UnitX, Matrix4.CreateFromQuaternion(rot));
            double rz = Math.Atan2(v3.Y, v3.X);
            double az = rz + radAngle;


            while (az < 0)
            {
                az += Pi2;
            }
            while (az > Pi2)
            {
                az -= Pi2;
            }

            var xmul = (float) Math.Cos(az);
            var ymul = (float) Math.Sin(az);
            Vector3 diff = new Vector3(xmul, ymul, 0)*(float) distance;

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
        public static Vector3d GetGlobalLeftPos(SimPosition pos, int zAngleFromFace, double distance)
        {
            double RAD_TO_DEG = 57.29577951f;
            var Pi2 = (double) (Math.PI*2.0);

            while (zAngleFromFace > 360)
            {
                zAngleFromFace -= 360;
            }
            while (zAngleFromFace < 0)
            {
                zAngleFromFace += 360;
            }

            double radAngle = zAngleFromFace/RAD_TO_DEG;
            Quaternion rot = pos.GetSimRotation();
            Vector3 v3 = Vector3.Transform(Vector3.UnitX, Matrix4.CreateFromQuaternion(rot));
            double rz = Math.Atan2(v3.Y, v3.X);

            double az = rz + radAngle;

            while (az < 0)
            {
                az += Pi2;
            }
            while (az > Pi2)
            {
                az -= Pi2;
            }

            var xmul = (double) Math.Cos(az);
            var ymul = (double) Math.Sin(az);
            Vector3d diff = new Vector3d(xmul, ymul, 0)*distance;

            Vector3d result = pos.GetWorldPosition() + diff;

            return result;
            /*
             * Client.Self.Movement.SendManualUpdate(AgentManager.ControlFlags.AGENT_CONTROL_AT_POS, Client.Self.Movement.Camera.Position,
                    Client.Self.Movement.Camera.AtAxis, Client.Self.Movement.Camera.LeftAxis, Client.Self.Movement.Camera.UpAxis,
                    Client.Self.Movement.BodyRotation, Client.Self.Movement.HeadRotation, Client.Self.Movement.Camera.Far, AgentFlags.None,
                    AgentState.None, true);*/
        }

        public bool GetParcelDeny(Vector3 v3)
        {
            Parcel P = GetParcel((int) v3.X, (int) v3.Y);
            return !ParcelAllowsEntry(P, Client.Self.AgentID);
        }

        public Parcel GetParcel(float x, float y)
        {
            Simulator sim = TheSimulator;
            int local =  sim.ParcelMap[(byte)x / 4, (byte)y / 4];
            Parcel parcel;
            InternalDictionary<int, Parcel> PD = sim.Parcels;
            //if (local == 0) return parcel;
            if (PD.TryGetValue(local, out parcel))
                return parcel;
            return parcel;
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
            lock (_Simulators)
                foreach (Simulator S in _Simulators)
                {
                    sdebug += "\n  " + ((S == master) ? "*" : " ");
                    sdebug += "" + S;
                    if (!S.Connected) sdebug += " DISCONNECTED ";
                    else sdebug += " CONNECTED ";
                    sdebug += S.Client;
                }
            return sdebug;
        }

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
            if (OutOfRegion(next)) return true;
            SimPathStore PathStore = GetPathStore(next);
            CollisionPlane CP = PathStore.GetCollisionPlane(next.Z);
            return PathStore.IsPassable(next, CP);
        }

        public void UpdateTraveled(UUID uUID, Vector3 vector3, Quaternion quaternion)
        {
            GetPathStore(vector3).UpdateTraveled(uUID, vector3, quaternion);
        }

        internal void Refresh(Box3Fill changed)
        {
            // throw new NotImplementedException();
        }

        internal bool AddCollisions(SimMesh C)
        {
            bool b = C.UpdateOccupied(PathStore);
            return b;
        }

        public static Vector3d GetGlobalFromLocal(ulong handle, Vector3 objectLoc)
        {
            uint regionX = 0, regionY = 0;
            Utils.LongToUInts(handle, out regionX, out regionY);
            return new Vector3d(regionX + objectLoc.X, regionY + objectLoc.Y, objectLoc.Z);
        }

    }
}