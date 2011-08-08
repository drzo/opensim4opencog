#define USE_MINIAABB
using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Threading;
using PathSystem3D.Mesher;
using OpenMetaverse;

namespace PathSystem3D.Navigation
{
    /// <summary>
    /// An x/y position in a region that indexes the objects that can collide at this x/y
    ///  Also indexes way points for fast lookup
    /// right now i divide a 256fx256f to a 1280x1280  (0.2f x 0.2f) CollisionIndex[,]  where any box that touches in x/ys thhen its indexed no mater what the Z is.. then i can drill down any Z looking for openspaces for avatar capsules of 2f
    /// </summary>
    [Serializable]
    public class CollisionIndex
    {

        public const float CapsuleZ = 1.99f;
        public const float MaxBump = 0.30f;

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

        public bool IsUnderWater(float low, float high)
        {
            return GetZLevel(low, high) < PathStore.WaterHeight;
        }

        public bool IsFlyZone(float low, float high)
        {
            return IsUnderWater(low, high) || IsMidAir(low, high);
        }

        public byte GetOccupiedValue(float low, float high)
        {
            int b = SimPathStore.INITIALLY + OccupiedCount * 1 + IsSolid * 2;
            if (b > 190) return 190;
            return (byte)b;
        }

        private bool IsMidAir(float low, float high)
        {
            float zlevel = GetZLevel(low, high);
            return low > zlevel;
        }


#if USE_MINIAABB
        private List<Box3Fill> InnerBoxes = new List<Box3Fill>();
        private bool InnerBoxesSimplified = false;
        public bool SomethingMaxZ(float low, float high, out float maxZ)
        {
            bool found = false;
            maxZ = low;
            List<CollisionObject> meshes = ShadowList;
            // this outer lock is to prevent AddOccupied from getting lost
            lock (meshes)
            {
                lock (InnerBoxes)
                {
                    if (!InnerBoxesSimplified)
                    {
                        int b = InnerBoxes.Count;
                        if (b < 100)
                        {
                            InnerBoxes = Box3Fill.Simplify(InnerBoxes);
                        }
                        else
                        {
                            InnerBoxes = Box3Fill.Simplify(InnerBoxes);                            
                        }
                        if (b > 100 || InnerBoxes.Count * 4 < b)
                            Console.Error.WriteLine("Simplfy CI {0} -> {1} ", b,
                                                    InnerBoxes.Count + " " + this);
                        InnerBoxesSimplified = true;
                    }
                    // this second lock is because we may have replaced the reference during simplification
                    lock (InnerBoxes)
                        foreach (CollisionObject B in InnerBoxes)
                        {
                            if (B.IsZInside(low, high))
                            {
                                found = true;
                                if (B.MaxZ > maxZ) maxZ = B.MaxZ;
                            }
                        }
                }
                return found;
            }
        }
#endif
        internal bool SomethingBetween(float low, float high, IEnumerable OccupiedListObject, out float maxZ)
        {
            if (IsSolid == 0)
            {
                maxZ = low;// GetGroundLevel();
                return (maxZ > high);
            }

#if USE_MINIAABB
            return SomethingMaxZ(low, high, out maxZ);
#endif
            lock (OccupiedListObject) foreach (CollisionObject O in OccupiedListObject)
                {
                    if (O.IsSolid)
                    {
                        if (O is IMeshedObject) if (((IMeshedObject)O).SomethingMaxZ(_LocalPos.X, _LocalPos.Y, low, high, out maxZ)) return true;
                    }
                }
            maxZ = low;// GetGroundLevel();
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
            return GetZLevel(low, high, CollisionIndex.CapsuleZ);
        }

        public float GetZLevel(float low, float high, float capsuleSize)
        {
            float groundLevel = GetGroundLevel();
            if (high < groundLevel) return groundLevel;
            float above = low;
            IEnumerable<CollisionObject> objs = GetOccupied(low, high);
            while (above < high)
            {
                float newMaxZ;
                if (!SomethingBetween(above, above + capsuleSize, objs, out newMaxZ))
                {
                    return above;
                }
                if (newMaxZ > above)
                {
                    above = newMaxZ;
                }
                else
                {
                    above += 0.1f;
                }
            }
            float below;
            if (OpenCapsuleBelow(low, groundLevel, capsuleSize, out below))
            {
                if (below < groundLevel)
                {
                    below = groundLevel;
                }
                return below;
            }
            return above;
            // IsDebugged();
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
                // IEnumerable<CollisionObject> objs = GetOccupied(above, high);
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
            }
            return above;
        }

        public bool OpenCapsuleAbove(float low, float high, float capsuleSize, out float above)
        {
            above = low;
            IEnumerable<CollisionObject> objs = GetOccupied(low, high);
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


        public bool OpenCapsuleBelow(float high, float low, float capsuleSize, out float below)
        {
            below = high;
            IEnumerable<CollisionObject> objs = GetOccupied(low, high);
            while (below > low)
            {
                float newMaxZ;
                if (SomethingBetween(below, below + capsuleSize, objs, out newMaxZ))
                {
                    below -= 0.1f;
                }
                else
                {
                    return true;
                }

            }
            return false;
        }

        public bool AddOccupied(IMeshedObject simObject, float minZ, float maxZ)
        {
            float x = _LocalPos.X,
                  y = _LocalPos.Y;
            //float GroundLevel = GetGroundLevel();
            // if (simObject.OuterBox.MaxZ < GetGroundLevel())
            // {
            //    return false;
            //}
            List<CollisionObject> meshes = ShadowList;
            lock (meshes)
                if (!meshes.Contains(simObject))
                {
                    meshes.Add(simObject);
                    OccupiedCount++;
                    if (simObject.IsSolid)
                    {
                        IsSolid++;
#if USE_MINIAABB
                        IEnumerable<Box3Fill> mini = simObject.InnerBoxes;
                        lock (mini) foreach (var o in mini)
                        {
                            if (o.IsInsideXY(x, y))
                                lock (InnerBoxes)
                                {
                                    InnerBoxesSimplified = false;
                                    InnerBoxes.Add(o);
                                }
                        }
#endif
#if COLLIDER_TRIANGLE
                        IEnumerable<CollisionObject> mini = simObject.triangles;
                        foreach (var o in mini)
                        {
                            if (o.IsInsideXY(_LocalPos.X, _LocalPos.Y))
                                InnerBoxes.Add(o);
                        }
#endif
                    }
                    TaintMatrix();
                    return true;
                }
            return false;
        }

        public List<CollisionObject> ShadowList = new List<CollisionObject>();
        public IEnumerable<CollisionObject> GetOccupied(float low, float high)
        {
            if (true) return ShadowList;
            List<CollisionObject> objs = new List<CollisionObject>();
            lock (ShadowList)
            {
                foreach (CollisionObject O in ShadowList)
                {
                    if (O.MaxZ < low) continue;
                    if (O.MinZ > high) continue;
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

            float low = cp.MinZ - 10f;
            float high = cp.MaxZ + 10f;

            if (OccupiedCount > 0)
            {
                IEnumerable<CollisionObject> objs = GetOccupied(low, high);
                lock (objs)
                {
                    foreach (CollisionObject O in objs)
                    {
                        S += "" + O;//.ToBoxString(_LocalPos.X, _LocalPos.Y, low, high);
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
            S += String.Format(" HLevel={0}", cp.HeightMap[PX, PY]);
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
        //        return _GlobalPos == ((MeshableObject)O).GlobalPosition();
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

        public void RemoveObject(CollisionObject simObject)
        {
            foreach (List<CollisionObject> MOL in MeshedObjectIndexes())
            {
                lock (MOL) if (MOL.Contains(simObject))
                    {
                        OccupiedCount--;
                        if (simObject.IsSolid) IsSolid--;
                        TaintMatrix();
                        MOL.Remove(simObject);
                    }
            }
        }

        public void RemeshObjects()
        {
            Box3Fill changed = new Box3Fill(true);
            foreach (List<CollisionObject> MOL in MeshedObjectIndexes())
            {
                lock (MOL) foreach (CollisionObject O in new List<CollisionObject>(MOL))
                    {
                        if (O is IMeshedObject) ((IMeshedObject)O).RemeshObject(changed);
                    }
            }
            PathStore.Refresh(changed);

        }

        public void RegionTaintedThis()
        {
            foreach (List<CollisionObject> MOL in MeshedObjectIndexes())
            {
                lock (MOL) foreach (CollisionObject O in new List<CollisionObject>(MOL))
                    {
                        if (O is IMeshedObject) ((IMeshedObject)O).RegionTaintedThis();
                    }
            }
            TaintMatrix();
            //RemeshObjects();
            //UpdateMatrix(low,high);
        }
        List<List<CollisionObject>> _MeshedObjectIndexs = null;
        private IEnumerable<List<CollisionObject>> MeshedObjectIndexes()
        {

            if (_MeshedObjectIndexs == null)
            {
                _MeshedObjectIndexs = new List<List<CollisionObject>>();
                _MeshedObjectIndexs.Add(ShadowList);
            }
            return _MeshedObjectIndexs;
        }

        // public bool IsTimerTicking = false;
        public void SetNodeQualityTimer(CollisionPlane CP, int value, List<ThreadStart> undo)
        {
            byte oldValue = GetMatrix(CP);
            if (oldValue == value) // already set
                return;
            Debug("SetNodeQualityTimer of {0} form {1} to {2}", this, oldValue, value);
            SetMatrixForced(CP, value);
            //if (IsTimerTicking) return;
            //IsTimerTicking = true;

            float StepSize = PathStore.StepSize;
            undo.Add(() =>
                     {
                         byte newValue = GetMatrix(CP);
                         if (newValue != value)
                             // its been changed by something else since we set to Zero
                             Debug("SetNodeQualityTimer Thread out of date {0} value changed to {1}", this, newValue);
                         else
                         {
                             SetMatrixForced(CP, oldValue);
                             Debug("ResetNodeQualityTimer {0} value reset to {1}", this, oldValue);
                         }
                         //IsTimerTicking = false;
                     });
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

        internal bool IsPortal(CollisionPlane collisionPlane)
        {
            IEnumerable<CollisionObject> mis = GetOccupied(collisionPlane.MinZ, collisionPlane.MaxZ);
            lock (mis)
                foreach (object o in mis)
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

        internal System.Drawing.Color DebugColor(CollisionPlane collisionPlane)
        {
            IEnumerable<CollisionObject> mis = GetOccupied(collisionPlane.MinZ, collisionPlane.MaxZ);
            lock (mis)
                foreach (object O in mis)
                {
                    if (O is IMeshedObject)
                    {
                        Color c = ((IMeshedObject)O).DebugColor();
                        if (c != Color.Empty) return c;
                    }
                }
            return Color.Empty;
        }
    }

}
