using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using PathSystem3D.Mesher;
using OpenMetaverse;

namespace PathSystem3D.Navigation
{
    /// <summary>
    /// An x/y postion in a region that indexes the objects that can collide at this x/y
    ///  Also indexes waypoints for fast lookup
    /// </summary>
    [Serializable]
    public class CollisionIndex
    {
        //public Point Point;

        /// <summary>
        /// Gets Point.X coordinate on the PathStore.
        /// </summary>
        readonly public int PX;// { get { return (int)Math.Round(_GlobalPos.X * PathStore.POINTS_PER_METER); } }

        /// <summary>
        /// Gets Point.Y coordinate on the PathStore.
        /// </summary>
        readonly public int PY;// { get { return (int)Math.Round(_GlobalPos.Y * PathStore.POINTS_PER_METER); } }

        public Vector3d GetWorldPosition()
        {
            return _GlobalPos;
        }

        public byte GetMatrix(CollisionPlane CP)
        {
            return CP.ByteMatrix[PX, PY];
        }

        public void SetMatrix(CollisionPlane CP, int v)
        {
            if (GetMatrix(CP) == SimPathStore.STICKY_PASSABLE) return;
            //if (PathStore.mMatrix[PX, PY] == SimRegion.MAYBE_BLOCKED) return;
            SetMatrixForced(CP, v);
        }

        public void SetMatrixForced(CollisionPlane CP, int v)
        {
            CP[PX, PY] = (byte)v;
        }

        private byte _OccupiedCount;

        public int OccupiedCount
        {
            get { return _OccupiedCount; }
            set { _OccupiedCount = (byte)value; }
        }

        public bool SolidAt(float z)
        {
            return SomethingBetween(z, z, GetOccupied(z,z));
        }

        private CollisionPlane CollisionPlaneAt(float z)
        {
            return PathStore.GetCollisionPlane(z);
        }

        byte IsSolid = 0;


        //public bool IsGroundLevel()
        //{
        //    return GetZLevel(LastPlane) == GetGroundLevel();
        //}

        public bool IsUnderWater(CollisionPlane CP)
        {
            if (CP == null) return GetGroundLevel() < GetSimRegion().WaterHeight;
            return CP.MinZ < GetSimRegion().WaterHeight;
        }

        public bool IsFlyZone(CollisionPlane CP)
        {
            return IsUnderWater(CP);
        }

        public byte GetOccupiedValue(CollisionPlane CP)
        {
            int b = SimPathStore.INITIALLY + OccupiedCount * 3 + IsSolid * 3;
            if (b > 240) return 240;
            return (byte)b;
        }
        public void UpdateMatrix(CollisionPlane CP)
        {
            SetMatrix(CP, GetOccupiedValue(CP));
            float zlevel = GetZLevel(CP);
            if (NeighborBump(CP, zlevel, 0.55f))
                SetMatrix(CP, SimPathStore.BLOCKED);
            else if (IsSolid != 0)
            {
                List<IMeshedObject> OccupiedCP = GetOccupied(CP);
                if (SomethingBetween(zlevel + 0.35f, zlevel + 1.7f, OccupiedCP))
                    SetMatrix(CP, SimPathStore.BLOCKED);
                else if (SomethingBetween(zlevel + 0.1f, zlevel + 0.3f, OccupiedCP))
                    SetMatrix(CP, SimPathStore.MAYBE_BLOCKED);
                else if (NeighborBump(CP, zlevel, 0.2f))
                    SetMatrix(CP, SimPathStore.MAYBE_BLOCKED);
            }
            else if (IsUnderWater(CP))
                SetMatrix(CP, SimPathStore.MAYBE_BLOCKED);
        }

        internal bool SomethingBetween(float low, float high, bool onlySolids)
        {
            return SomethingBetween(low, high, GetOccupied(low,high));
        }

        private List<IMeshedObject> GetOccupied(float low, float high)
        {
            return ShadowList;
        }

        internal bool SomethingBetween(float low, float high, IEnumerable OccupiedListObject)
        {
            if (IsSolid == 0) return false;
            lock (OccupiedListObject) foreach (IMeshedObject O in OccupiedListObject)
                {
                    if (!O.IsPassable)
                    {
                        if (O.SomethingBetween(_LocalPos, low, high)) return true;
                    }
                }
            return false;
        }

        public bool NeighborBump(CollisionPlane CP, float original, float mostDiff)
        {
            if (PX < 1 || PY < 1 || _LocalPos.X > 254 || _LocalPos.Y > 254)
                return false;
            float O;

            O = NeighborLevel(CP, PX, PY + 1);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            O = NeighborLevel(CP, PX + 1, PY + 1);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            O = NeighborLevel(CP, PX + 1, PY);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            O = NeighborLevel(CP, PX + 1, PY - 1);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            O = NeighborLevel(CP, PX, PY - 1);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            O = NeighborLevel(CP, PX - 1, PY - 1);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            O = NeighborLevel(CP, PX - 1, PY);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            O = NeighborLevel(CP, PX - 1, PY + 1);
            if (!DiffLessThan(O, original, mostDiff)) return true;

            return false;
        }

        internal float NeighborLevel(CollisionPlane CP, int PX, int PY)
        {
            CollisionIndex WP = PathStore.MeshIndex[PX, PY];
            if (WP != null) return WP.GetZLevel(CP);
            float x = PX / PathStore.POINTS_PER_METER;
            float y = PY / PathStore.POINTS_PER_METER;
            float GL = GetSimRegion().GetGroundLevel(x, y);
            float CPL = CP.MinZ;
            return (GL > CPL) ? GL : CPL;
        }


        float _GroundLevelCache = float.MinValue;
        public float GetGroundLevel()
        {
            if (_GroundLevelCache > 0) return _GroundLevelCache;
            _GroundLevelCache = GetSimRegion().GetGroundLevel(_LocalPos.X, _LocalPos.Y);
            return _GroundLevelCache;
        }

        public SimPathStore GetSimRegion()
        {
            return PathStore.GetPathStore();
        }


        public void TaintMatrix()
        {
            LastPlane = null;
            _GroundLevelCache = float.MinValue;
            _ZLevelCache_ = float.MinValue;
        }

        float _ZLevelCache_ = float.MinValue;
        CollisionPlane LastPlane;

        public float GetZLevel(CollisionPlane CP)
        {
            float _ZLevelCache = _ZLevelCache_;
            if (LastPlane != CP)
            {
                LastPlane = CP;
                List<IMeshedObject> OccupiedCP = GetOccupied(CP);
                float above = CP.MinZ;
                float GL = GetGroundLevel(CP);
                if (above < GL) above = GL;
                // when the Two Zs are differnt that means global Pos has been computed
                if (_ZLevelCache > above) return _ZLevelCache;
                _ZLevelCache = above;
                ///  OccupiedListObject.Sort(ZOrder);
                /// OccupiedListObject.Reverse();
                if (IsSolid != 0)
                {
                    bool ChangeD = false;
                    IMeshedObject Flooring = null;
                    for (int d = IsSolid; d > 0; d--)
                    {
                        lock (OccupiedCP)
                            foreach (IMeshedObject O in OccupiedCP)
                            {
                                if (O.IsPassable) continue;
                                float MaxZ;
                                if (O.SomethingMaxZ(_LocalPos, _ZLevelCache, _ZLevelCache + 1.5f, out MaxZ))
                                {
                                    //bool wpfound = O.GetZLevel(Point, out MinZ, out MaxZ);

                                    // The object is higher
                                    if (_ZLevelCache < MaxZ)
                                    // And the object is below or the bottem of object is less than a meter above or the top of object is less than 1.5 meters
                                    //if (MinZ <= _ZLevelCache || DiffLessThan(MinZ, _ZLevelCache, 1.5f) || DiffLessThan(MaxZ, _ZLevelCache, 2f))
                                    {
                                        Flooring = O;
                                        ChangeD = true;
                                        _ZLevelCache = MaxZ;
                                    }

                                }

                            }
                        if (!ChangeD) break;
                    }
                    //if (Flooring != null)
                    //{
                    //    if (ChangeD) lock (OccupiedListObject)
                    //    {
                    //        OccupiedListObject.Remove(Flooring);
                    //        OccupiedListObject.Insert(0, Flooring);
                    //    }
                    //}
                }
                _LocalPos.Z = _ZLevelCache;
                _GlobalPos.Z = _ZLevelCache + 1;
                _ZLevelCache_ = _ZLevelCache;
            }
            return _ZLevelCache;
        }

        public float GetGroundLevel(CollisionPlane CP)
        {
            float GL = GetGroundLevel();
            if (CP == null) return GL;
            float CPL = CP.MinZ;
            return (CPL > GL) ? CPL : GL;
        }

        static bool DiffLessThan(float A, float B, float D)
        {
            return Math.Abs(A - B) <= D;
        }


        public bool AddOccupied(IMeshedObject simObject, float minZ, float maxZ)
        {
            List<IMeshedObject> meshes = GetOccupied(minZ, maxZ);
            lock (meshes)
                if (!meshes.Contains(simObject))
                {
                    meshes.Add(simObject);
                    OccupiedCount++;
                    if (!simObject.IsPassable)
                        IsSolid++;
                    TaintMatrix();
                    return true;
                }
            return false;
        }

        public List<IMeshedObject> ShadowList = new List<IMeshedObject>();
        public List<IMeshedObject> GetOccupied(CollisionPlane CP)
        {
            return ShadowList;
        }
        //string OcString = null;
        //public IList<Vector2> OccupiedListMinMaxZ = new List<Vector2>();

        public string OccupiedString()
        {
            string S = "";

            if (OccupiedCount > 0)
            {
                lock (GetOccupied(LastPlane))
                {
                    foreach (IMeshedObject O in GetOccupied(LastPlane))
                    {
                        S += O.ToString();
                        S += "\r\n";
                    }
                }
            }
            return S + this.ToString() + " " + ExtraInfoString(LastPlane);
        }

        public string ExtraInfoString(CollisionPlane LastPlane)
        {
            string S = "GLevel=" + GetGroundLevel(LastPlane);
            if (IsUnderWater(LastPlane)) S += " UnderWater=" + GetSimRegion().WaterHeight;
            S += " LastGL=" + LastPlane;
            if (LastPlane != null)
                S += " ZLevel=" + GetZLevel(LastPlane)
                 + " Maxtrix=" + GetMatrix(LastPlane);
            return S;
        }



        /// <summary>
        /// object.ToString() override.
        /// Returns the textual description of the node.
        /// </summary>
        /// <returns>String describing this node.</returns>
        public override string ToString()
        {
            SimPathStore R = GetSimRegion();
            Vector3 loc = GetSimPosition();
            return String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, GetGroundLevel(LastPlane));
        }

        ///// <summary>
        ///// Object.Equals override.
        ///// Tells if two nodes are equal by comparing positions.
        ///// </summary>
        ///// <exception cref="ArgumentException">A Node cannot be compared with another type.</exception>
        ///// <param name="O">The node to compare with.</param>
        ///// <returns>'true' if both nodes are equal.</returns>
        //public override bool Equals(object O)
        //{
        //    if (O is MeshableObject)
        //    {
        //        return _GlobalPos == ((MeshableObject)O).GetWorldPosition();
        //    }
        //    //if (O is Vector3d)
        //    //{
        //    //    return Create((Vector3d)O).Equals(this);
        //    //}

        //    throw new ArgumentException("Type " + O.GetType() + " cannot be compared with type " + GetType() + " !");
        //}
        /// <summary>
        /// Object.GetHashCode override.
        /// </summary>
        /// <returns>HashCode value.</returns>
        public override int GetHashCode() { return _GlobalPos.GetHashCode(); }

        public SimPathStore PathStore;
        Vector3 _LocalPos;
        Vector3d _GlobalPos;
        private CollisionIndex(Vector3 local, Vector3d global, int PX0, int PY0, SimPathStore pathStore)
        {
            PathStore = pathStore;
            _GlobalPos = global;
            _LocalPos = local;
            PX = PX0;
            PY = PY0;
            PathStore.MeshIndex[PX, PY] = this;
            TaintMatrix();
            //pathStore.NeedsUpdate = true;
            //  UpdateMatrix(pathStore.CurrentPlane);
        }

        public static Vector3 RoundPoint(Vector3 vect3, SimPathStore PathStore)
        {
            double POINTS_PER_METER = PathStore.POINTS_PER_METER;
            vect3.X = (float)(Math.Round(vect3.X * POINTS_PER_METER, 0) / POINTS_PER_METER);
            vect3.Y = (float)(Math.Round(vect3.Y * POINTS_PER_METER, 0) / POINTS_PER_METER);
            vect3.Z = (float)(Math.Round(vect3.Z * POINTS_PER_METER, 0) / POINTS_PER_METER);
            return vect3;
        }

        public static CollisionIndex CreateCollisionIndex(Vector3 from, SimPathStore PathStore)
        {
            float POINTS_PER_METER = PathStore.POINTS_PER_METER;
            int PX = PathStore.ARRAY_X(from.X);
            int PY = PathStore.ARRAY_Y(from.Y);
            CollisionIndex WP;
            lock (PathStore.MeshIndex)
            {
                WP = PathStore.MeshIndex[PX, PY];
                if (WP != null) return WP;
                from.X = PX / POINTS_PER_METER;
                from.Y = PY / POINTS_PER_METER;
                Vector3d GlobalPos = PathStore.GetPathStore().LocalToGlobal(from);
                WP = new CollisionIndex(from, GlobalPos, PX, PY, PathStore);
            }
            return WP;
        }

        public Vector3 GetSimPosition()
        {
            return _LocalPos;
        }

        public void RemoveObject(IMeshedObject simObject)
        {
            foreach (List<IMeshedObject> MOL in MeshedObjectIndexes())
            {
                lock (MOL) if (MOL.Contains(simObject))
                    {
                        OccupiedCount--;
                        if (!simObject.IsPassable) IsSolid--;
                        TaintMatrix();
                        MOL.Remove(simObject);
                    }
            }
        }

        public void RemeshObjects()
        {
            Box3Fill changed = new Box3Fill(true);
            foreach (List<IMeshedObject> MOL in MeshedObjectIndexes())
            {
                lock (MOL) foreach (IMeshedObject O in new List<IMeshedObject>(MOL))
                    {
                        O.RemeshObject(changed);
                    }
            }
            PathStore.Refresh(changed);

        }

        public void RegionTaintedThis()
        {
            foreach (List<IMeshedObject> MOL in MeshedObjectIndexes())
            {
                lock (MOL) foreach (IMeshedObject O in new List<IMeshedObject>(MOL))
                    {
                        O.RegionTaintedThis();
                    }
            }
            TaintMatrix();
            //RemeshObjects();
            //UpdateMatrix(CP);
        }
        List<List<IMeshedObject>> _MeshedObjectIndexs = null;
        private IEnumerable<List<IMeshedObject>> MeshedObjectIndexes()
        {

            if (_MeshedObjectIndexs == null)
            {
                _MeshedObjectIndexs = new List<List<IMeshedObject>>();
                _MeshedObjectIndexs.Add(ShadowList);
            }
            return _MeshedObjectIndexs;
        }

        bool IsTimerTicking = false;
        public void SetNodeQualityTimer(CollisionPlane CP, int value, int seconds)
        {
            byte oldValue = GetMatrix(CP);
            if (oldValue == value) // already set
                return;
            Debug("SetNodeQualityTimer of {0} form {1} to {2}", this, oldValue, value);
            SetMatrixForced(CP, value);
            if (IsTimerTicking) return;
            IsTimerTicking = true;

            float StepSize = PathStore.StepSize;
            new Thread(new ThreadStart(delegate()
            {
                Thread.Sleep(seconds * 1000);
                byte newValue = GetMatrix(CP);

                if (newValue != value)
                {
                    // its been changed by something else since we set to Zero
                    Debug("SetNodeQualityTimer Thread out of date {0} value changed to {1}", this, newValue);
                }
                else
                {
                    SetMatrixForced(CP, oldValue);
                    Debug("ResetNodeQualityTimer {0} value reset to {1}", this, oldValue);
                }
                IsTimerTicking = false;
            })).Start();
        }

        private void Debug(string format, params object[] objs)
        {
            SimPathStore.Debug(format, objs);
        }

        internal static CollisionIndex CreateCollisionIndex(float x, float y, SimPathStore simPathStore)
        {
            return CreateCollisionIndex(new Vector3(x, y, 0), simPathStore);
        }

        Dictionary<CollisionPlane, SimWaypoint> WaypointsHash = new Dictionary<CollisionPlane, SimWaypoint>();
        public SimWaypoint FindWayPoint(float z)
        {
            CollisionPlane CP = CollisionPlaneAt(z);
            SimWaypoint v;
            if (WaypointsHash.TryGetValue(CP, out v))
            {
                return v;
            }
            return null;// SimWaypointImpl.CreateLocal(from, PathStore);
        }

        public SimWaypoint GetWayPoint(float z)
        {
            CollisionPlane CP = CollisionPlaneAt(z);
            SimWaypoint v;
            if (!WaypointsHash.TryGetValue(CP, out v))
            {
                v = SimWaypointImpl.CreateLocal(_LocalPos.X, _LocalPos.Y, z, PathStore);
                v.Plane = CP;
            }
            return v;
        }

        public void SetWayPoint(float z, SimWaypoint v)
        {
            CollisionPlane CP = CollisionPlaneAt(z);
            WaypointsHash[CP] = v;
        }
    }

}
