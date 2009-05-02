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

        public const float CapsuleZ = 1.99f;
        public const float MaxBump = 0.40f;

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

        //public void SetMatrix(CollisionPlane CP, int v)
        //{
        //    if (GetMatrix(CP) == SimPathStore.STICKY_PASSABLE) return;
        //    //if (PathStore.mMatrix[PX, PY] == SimRegion.MAYBE_BLOCKED) return;
        //    SetMatrixForced(CP, v);
        //}

        public void SetMatrixForced(CollisionPlane CP, int v)
        {
            CP.ByteMatrix[PX, PY] = (byte)v;
        }

        public int OccupiedCount;


        public bool SolidAt(float z)
        {
            float maxZ;
            if (SomethingBetween(z, z, GetOccupied(z, z), out maxZ))
            {
                return true;
            }
            return false;
        }

        private CollisionPlane CollisionPlaneAt(float z)
        {
            return PathStore.GetCollisionPlane(z);
        }

        int IsSolid = 0;


        //public bool IsGroundLevel()
        //{
        //    return GetZLevelFree(LastPlane) == GetGroundLevel();
        //}

        public bool IsUnderWater(float low,float high)
        {
            return GetZLevel(low,high) < PathStore.WaterHeight;
        }

        public bool IsFlyZone(float low,float high)
        {
            return IsUnderWater(low,high) || IsMidAir(low,high);
        }

        public byte GetOccupiedValue(float low,float high)
        {
            int b = SimPathStore.INITIALLY + OccupiedCount * 1 + IsSolid * 2;
            if (b > 190) return 190;
            return (byte)b;
        }

        public byte UpdateMatrix(CollisionPlane CP, float low, float high, float[,] heightMap)
        {
            //  float zlevel = low;
            float zlevel = GetZLevel(low, high);
            if (zlevel < low) zlevel = low;
            heightMap[PX, PY] = zlevel;
            if (NeighborBump(zlevel, zlevel + CapsuleZ, zlevel, MaxBump, heightMap) > 1)
                return SimPathStore.BLOCKED;
            //else
            //if (false && IsSolid != 0)
            {
                float maxZ;
                IEnumerable<IMeshedObject> OccupiedCP = GetOccupied(low, high);
                if (SomethingBetween(zlevel + 0.35f, zlevel + CapsuleZ, OccupiedCP, out maxZ))
                {
                    if (maxZ > heightMap[PX, PY])
                        heightMap[PX, PY] = maxZ;
                    return SimPathStore.BLOCK_ORANGE;
                }
                else if (SomethingBetween(zlevel + 0.1f, zlevel + 0.3f, OccupiedCP, out maxZ))
                {
                    if (maxZ > heightMap[PX, PY])
                        heightMap[PX, PY] = maxZ;
                    return SimPathStore.BLOCK_PURPLE;
                }
                else if (NeighborBump(low, high, zlevel, 0.2f, heightMap) > 1)
                {
                    return SimPathStore.BLOCKED_YELLOW;
                }
                else if (IsMidAir(low, high))
                {
                    return SimPathStore.BLOCKED_AIR;
                }
            }
            if (IsUnderWater(low, high))
                return SimPathStore.MAYBE_BLOCKED;
            else if (IsMidAir(low, high))
                return SimPathStore.MAYBE_BLOCKED;

            return GetOccupiedValue(low, high);
            //else low,high.DefaultCollisionValue(_GroundLevelCache, OV);
        }

        private bool IsMidAir(float low,float high)
        {
            float zlevel = GetZLevel(low,high);
            return low > zlevel;
        }

        internal bool SomethingBetween(float low, float high, IEnumerable OccupiedListObject, out float maxZ)
        {
            maxZ = low;// GetGroundLevel();
            if (IsSolid == 0)
            {
                return (maxZ > high);
            }
            lock (OccupiedListObject) foreach (IMeshedObject O in OccupiedListObject)
                {
                    if (!O.IsPassable)
                    {
                        if (O.SomethingMaxZ(_LocalPos.X, _LocalPos.Y, low, high, out maxZ)) return true;
                    }
                }
            return false;
        }

        public byte NeighborBump(float low, float high, float original, float mostDiff, float[,] hightMap)
        {
            if (PX < 1 || PY < 1 || _LocalPos.X > 254 || _LocalPos.Y > 254)
                return 0;
            return CollisionPlane.NeighborBump(PX, PY, low, high, original, mostDiff, hightMap);
        }

        float _GroundLevelCache = float.MinValue;
        public float GetGroundLevel()
        {
            //if (_GroundLevelCache > 0) return _GroundLevelCache;
            _GroundLevelCache = PathStore.GetGroundLevel(_LocalPos.X, _LocalPos.Y);
            return _GroundLevelCache;
        }

        public void TaintMatrix()
        {            
            _GroundLevelCache = float.MinValue;
        }


        public float GetZLevel(float low, float high)
        {
            // IsDebugged();
            float above = low;
            {
                if (//(PX == 566 && PY == 759) || (PX == 373 && PY == 1097) || 
                    (PX == 615 && PY == 594))
                {
                }
                float newMaxZ;
                // float above = low;
                float GL = GetGroundLevel();
                bool found = false;
                if (above < GL) above = GL;
                IEnumerable<IMeshedObject> objs = GetOccupied(above, high);
                while (above < high)
                {
                    if (SomethingBetween(above, above + CapsuleZ, objs, out newMaxZ))
                    {
                        if (newMaxZ > above)
                        {
                            found = true;
                            above = newMaxZ;
                        }
                        else
                        {
                            above += 0.1f;
                        }
                    }
                    else break;
                }
                if (!found) return high;
                _LocalPos.Z = low;
                _GlobalPos.Z = above;
            }
            return above;
        }

        public bool OpenCapsuleAt(float low, float high,float capsuleSize, out float above)
        {    
            above = low;
            IEnumerable<IMeshedObject> objs = GetOccupied(low, high);
            while (above < high)
            {
                float newMaxZ;
                if (!SomethingBetween(above, above + capsuleSize, objs, out newMaxZ))
                {
                    //above = 
                    IsDebugged();
                    return true;
                }
                IsDebugged();
                if (newMaxZ > above)
                {
                    above = newMaxZ;
                }
                else
                {
                    above += 0.1f;
                }
             
            }
            return false;
        }

        public bool AddOccupied(IMeshedObject simObject, float minZ, float maxZ)
        {
            //float GroundLevel = GetGroundLevel();
           // if (simObject.OuterBox.MaxZ < GetGroundLevel())
           // {
            //    return false;
            //}
            List<IMeshedObject> meshes = ShadowList;
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
        public IEnumerable<IMeshedObject> GetOccupied(float low, float high)
        {
            List<IMeshedObject> objs = new List<IMeshedObject>();
            lock (ShadowList)
            {
                foreach (IMeshedObject O in ShadowList)
                {
                    if (O.OuterBox.MaxZ < low) continue;
                    if (O.OuterBox.MinZ > high) continue;
                    objs.Add(O);
                }
            }
            return objs;
        }

        private void IsDebugged()
        {
            if (PX == 402 && PY == 403)
            {
            }
        }
        //string OcString = null;
        //public IList<Vector2> OccupiedListMinMaxZ = new List<Vector2>();

        public string OccupiedString(CollisionPlane cp)
        {
            string S = "";

            float low = cp.MinZ-10f;
            float high = cp.MaxZ+10f;

            if (OccupiedCount > 0)
            {
                IEnumerable<IMeshedObject> objs = GetOccupied(low,high);
                lock (objs)
                {
                    foreach (IMeshedObject O in objs)
                    {
                        S += ""+O;//.ToBoxString(_LocalPos.X, _LocalPos.Y, low, high);
                        S += Environment.NewLine;
                    }
                }
            }
            return String.Format("{0}{1} {2}", S, this.ToString(), ExtraInfoString(cp));
        }

        public string ExtraInfoString(CollisionPlane cp)
        {
            float low = cp.MinZ;
            float high = cp.MaxZ;
            string S = String.Format("{0}/{1} GLevel={2}", PX, PY, GetGroundLevel());
            S += String.Format(" HLevel={0}", cp.HeightMap[PX,PY]);
            if (IsUnderWater(low, high)) S += String.Format(" UnderWater={0}", PathStore.WaterHeight);
            if (IsFlyZone(low, high)) S += String.Format(" FlyZone={0}", low);
            S += String.Format(" LastGL={0}-{1}", low, high);
            S += String.Format(" ZLevel={0}", GetZLevel(low, high));
            return S;
        }



        /// <summary>
        /// object.ToString() override.
        /// Returns the textual description of the node.
        /// </summary>
        /// <returns>String describing this node.</returns>
        public override string ToString()
        {
            Vector3 loc = GetSimPosition();
            return String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", PathStore.RegionName, loc.X, loc.Y, GetGroundLevel());
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
            //UpdateMatrix(low,high);
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

        public bool IsTimerTicking = false;
        public void SetNodeQualityTimer(CollisionPlane CP, int value, int seconds)
        {
            return;
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

        private Dictionary<CollisionPlane, SimWaypoint> WaypointsHash = new Dictionary<CollisionPlane, SimWaypoint>();
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
       
        internal bool IsPortal(CollisionPlane collisionPlane)
        {
            IEnumerable<IMeshedObject> mis = GetOccupied(collisionPlane.MinZ, collisionPlane.MaxZ);
            foreach (var o in mis)
            {
                string s = o.ToString().ToLower();
                {
                    if (s.Contains("stair")) return true;
                    if (s.Contains("ramp")) return true;
                    if (s.Contains("brigde")) return true;
                    if (s.Contains(" path ")) return true;
                }
            }
            return false;
        }
    }

}
