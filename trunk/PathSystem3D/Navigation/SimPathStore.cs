// Copyleft 2009 Douglas R. Miles (Daxtron Labs) - <dmiles@daxtron.com> 55%
// Copyright 2003 Eric Marchesin - <eric.marchesin@laposte.net> 45%
//
// This source file(s) may be redistributed by any means PROVIDING they
// are not sold for profit without the authors expressed written consent,
// and providing that this notice and the authors name and all copyright
// notices remain intact.
// THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED. USE IT AT YOUR OWN RISK. THE AUTHOR ACCEPTS NO
// LIABILITY FOR ANY DATA DAMAGE/LOSS THAT THIS PRODUCT MAY CAUSE.
//-----------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Drawing;
using PathSystem3D.Mesher;
using PathSystem3D.Navigation.Debug;
using OpenMetaverse;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;

namespace PathSystem3D.Navigation
{

    public delegate void CallbackXY(float x, float y, float minZ, float maxZ);
    public delegate void CallbackXYBox(float x, float y, Box3Fill box);

    public delegate float SimZLevel(float x, float y);
    public delegate void SimZMinMaxLevel(float x, float y, out float minLevel, out float maxLevel);
    /// <summary>
    /// Graph structure. It is defined with :
    /// It is defined by a 2D matrix of CollisionIndex 
    /// It is used to perform operations on CollisionPlanes
    /// </summary>
    [Serializable]
    public class SimPathStore //: PathSystem3D.Navigation.IPathStore
    {
        public string RegionName { get; set; }
        // util
        static public void TrianglesToBoxes(IList<Triangle> tl, Box3Fill OuterBox, float PADXY, IList<Box3Fill> InnerBoxes)
        {


            int tc = tl.Count;
            if (tc < 16)
            {
                AddTrianglesV1(tl, tc, OuterBox, PADXY, InnerBoxes);
            }
            else
            {
                AddTrianglesV2(tl, tc, OuterBox, PADXY, InnerBoxes);
            }
            // Console.WriteLine(InnerBoxes.Count);
        }

        private static void AddTrianglesV1(IList<Triangle> ts, int len, Box3Fill OuterBox, float PADXY, IList<Box3Fill> InnerBoxes)
        {
            int len1 = len - 1;
            for (int i = 0; i < len1; i++)
            {
                Triangle t1 = ts[i];
                bool used = false;
                OuterBox.AddTriangle(t1, PADXY);
                for (int ii = i + 1; ii < len; ii++)
                {
                    Triangle t2 = ts[ii];
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        Box3Fill B = new Box3Fill(t1, t2, PADXY);
                        InnerBoxes.Add(B);
                        used = true;
                    }
                }
                if (!used)
                {
                    Box3Fill B = new Box3Fill(true);
                    B.AddTriangle(t1, PADXY);
                    InnerBoxes.Add(B);
                }
            }
        }

        private static int SharedVertexs(Triangle t1, Triangle t2)
        {
            int sharedV = 0;
            if (t1.v1 == t2.v1) sharedV++;
            else
                if (t1.v1 == t2.v2) sharedV++;
                else
                    if (t1.v1 == t2.v3) sharedV++;
            if (t1.v2 == t2.v1) sharedV++;
            else
                if (t1.v2 == t2.v2) sharedV++;
                else
                    if (t1.v2 == t2.v3) sharedV++;
            if (t1.v3 == t2.v1 || t1.v3 == t2.v2 || t1.v3 == t2.v3) return sharedV + 1;
            return sharedV;
        }

        private static void AddTrianglesV2(IList<Triangle> ts, int len, Box3Fill OuterBox, float PADXY, IList<Box3Fill> InnerBoxes)
        {
            int len1 = len - 2;
            for (int i = 0; i < len1; i += 2)
            {
                Triangle t1 = ts[i];
                Triangle t2 = ts[i + 1];
                OuterBox.AddTriangle(t1, PADXY);
                OuterBox.AddTriangle(t2, PADXY);
                Box3Fill B = new Box3Fill(t1, t2, PADXY);
                InnerBoxes.Add(B);
                bool used = false;
                for (int ii = i + 2; ii < len; ii++)
                {
                    t2 = ts[ii];
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        B = new Box3Fill(t1, t2, PADXY);
                        InnerBoxes.Add(B);
                        used = true;
                    }
                }
                if (!used)
                {
                    B = new Box3Fill(true);
                    B.AddTriangle(t1, PADXY);
                    InnerBoxes.Add(B);
                }
            }
        }


        Dictionary<IComparable, IMeshedObject> meshedObjects = new Dictionary<IComparable, IMeshedObject>();
        /// <summary>
        /// By default no boxes are passable
        /// </summary>
        public Predicate<IComparable> IsPassablePredicate = delegate(IComparable id) { return false; };
        /// <summary>
        /// The Pathstore can implment this
        /// </summary>
        public SimZLevel GroundLevel = delegate(float x, float y) { return 10; };

        // setup
        void AddBoxes(IComparable id, IList<Box3Fill> boxes)
        {
            Box3Fill Outer = new Box3Fill(true);

            foreach (Box3Fill B in boxes)
            {
                Outer.Expand(B);

            }
            IMeshedObject MO = new RuntimeMesh(id, Outer, boxes, this);
            //MeshedObject simMesh = new MeshedObject(boxes);
            meshedObjects.Add(id, MO);
            AddCollisions(MO);
        }

        public void AddCollisions(IMeshedObject MO)
        {           
            throw new NotImplementedException();
        }
        public void RemoveBoxes(IComparable id)
        {
            IMeshedObject MO = meshedObjects[id];//.Remove(id);

            Box3Fill changed = new Box3Fill(true);
            MO.RemoveFromWaypoints(changed);
            UpdateMatrixes();
            meshedObjects.Remove(id);

        }

        private void UpdateMatrixes()
        {
            throw new NotImplementedException();
        }


        public void SetPhysicalPredicate(Predicate<IComparable> callback)
        {
            IsPassablePredicate = callback;
        }
        public void SetGroundLevel(SimZLevel callback)
        {
            GroundLevel = callback;
        }

        // Updates
        public void SetTraveled(Vector3 LastPosition, Vector3 nextPosition)
        {
            Vector3 dif = LastPosition - nextPosition;
            if (!(dif.X == 0 && dif.Y == 0))
            {
                float dist = Vector3.Distance(LastPosition, nextPosition);
                int stepsNeeded = (int)(dist * POINTS_PER_METER) + 1;
              // if (OpenMetaverse.Settings.LOG_LEVEL == OpenMetaverse.Helpers.LogLevel.Debug) 
                Console.WriteLine("MakeMovement " + LastPosition + " -> " + stepsNeeded + " -> " + nextPosition + " " + this);
                Vector3 vstep = dif / stepsNeeded;
                Vector3 traveled = nextPosition;
                SetTraveled(nextPosition.X, nextPosition.Y, nextPosition.Z);
                for (int i = 0; i < stepsNeeded; i++)
                {
                    traveled = traveled + vstep;
                    if (stepsNeeded > 10) SetTraveled(traveled.X, traveled.Y, traveled.Z);
                    else SetPassable(traveled.X, traveled.Y, traveled.Z);
                }
            }
        }

        public void SetBlockedTemp(Vector3 cp, Vector3 v3)
        {
            Point P1 = ToPoint(cp);
            Vector3 offset = v3 - cp;
            double ZAngle = (double)Math.Atan2(offset.X, offset.Y);
            Point Last = ToPoint(v3);
            float Dist = 0.3f;
            Vector3 b1 = v3;
            while (offset.Length() > StepSize)
            {
                offset *= 0.75f;
                Vector3 blocked = cp + offset;
                Point P2 = ToPoint(blocked);
                if (P2 != P1)
                {
                    Dist = offset.Length();
                    Last = P2;
                    b1 = blocked;
                }
            }
            float x = UNARRAY_X(Last.X);
            float y = UNARRAY_Y(Last.Y);
            Vector3 v3o = new Vector3(x, y, v3.Z);
            BlockPointTemp(v3o);
            double A45 = 45f / SimPathStore.RAD2DEG;
            Debug("BlockTowardsVector {0}", Vector3.Distance(cp,v3o));
            Vector3 middle = ZAngleVector(ZAngle) * Dist;
            middle += cp;
            double mdist = Vector3.Distance(middle, b1);
            if (mdist > 0.1)
            {
                Debug("Wierd mdist = " + mdist);
            }
            Dist = 0.4f;
            //ZAngle -= (90 / SimPathStore.RAD2DEG);

            BlockPointTemp(ZAngleVector(ZAngle - A45 * 1.5) * Dist + cp);
            BlockPointTemp(ZAngleVector(ZAngle - A45) * Dist + cp);
            BlockPointTemp(ZAngleVector(ZAngle - A45 * 0.5) * Dist + cp);

            BlockPointTemp(ZAngleVector(ZAngle) * Dist + cp);

            BlockPointTemp(ZAngleVector(ZAngle + A45 * 0.5) * Dist + cp);
            BlockPointTemp(ZAngleVector(ZAngle + A45) * Dist + cp);
            BlockPointTemp(ZAngleVector(ZAngle + A45 * 1.5) * Dist + cp);
            //Dont Run back
            //MoveTo(cp + ZAngleVector(ZAngle - Math.PI) * 2, 1f, 2);
        }

        /// <summary>
        /// Blocks a point temporarilly (one minute)
        /// </summary>
        /// <param name="vector3"></param>
        internal void BlockPointTemp(Vector3 vector3)
        {
            SetNodeQualityTimer(vector3, SimPathStore.BLOCKED, 60);
        }


        internal static byte[] TextureBytesToUUID(UUID uUID)
        {
            throw new NotImplementedException();
        }


        private float _WaterHeight=float.MaxValue;

        public float WaterHeight
        {
            get { return _WaterHeight; }
            set { _WaterHeight = value; }
        }


        public static IList<Vector3d> GetPath(Vector3d globalStart, Vector3d globalEnd, double endFudge, out bool OnlyStart)
        {
            SimPosition posStart = SimWaypointImpl.CreateGlobal(globalStart);
            SimPosition posEnd = SimWaypointImpl.CreateGlobal(globalEnd);
            SimPathStore regStart = posStart.GetPathStore();
            SimPathStore regEnd = posEnd.GetPathStore();
            Vector3 localStart = posStart.GetSimPosition();
            Vector3 localEnd = posEnd.GetSimPosition();
            IList<Vector3d> route;
            // Same region?
            if (regStart == regEnd)
            {
                return regStart.GetAtLeastPartial(localStart, localEnd, (float)endFudge, out OnlyStart);
            }
            OnlyStart = true; // will be only a partial path
            SimPathStore nextRegion;
            Vector3 localLast = regStart.LocalOuterEdge(localStart, posEnd, out nextRegion);
            // needs to go to edge
            route = regStart.GetLocalPath(localStart, localLast);
            // at egde so make a crossing
            Vector3 enterEdge = EnterEdge(localLast, nextRegion.GetGridLocation() - regStart.GetGridLocation());
            route.Add(nextRegion.LocalToGlobal(enterEdge));
            return route;
        }


        internal IList<Vector3d> GetAtLeastPartial(Vector3 localStart, Vector3 localEnd, float endFudge, out bool OnlyStart)
        {
            IList<Vector3d> route;
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
            SimPathStore PathStore = GetPathStore3D(localStart);
            CollisionPlane CP = PathStore.GetCollisionPlane(localStart.Z);

            Console.WriteLine("very bad fake route for " + CP);
            return route;
        }

        public Vector3d LocalToGlobal(Vector3 objectLoc)
        {
            Vector2 V2 = GetGridLocation();
            return new Vector3d(V2.X * 256 + objectLoc.X, V2.Y * 256 + objectLoc.Y, objectLoc.Z);
        }

        /// <summary>
        ///  The closet usable space to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public Vector3 GetUsableLocalPositionOf(Vector3 v3, float useDist)
        {
            SimPathStore PathStore = GetPathStore3D(v3);
            CollisionPlane CP = PathStore.GetCollisionPlane(v3.Z);

            byte b = PathStore.GetNodeQuality(v3, CP);
            // float useDist = GetSizeDistance();
            if (b != SimPathStore.BLOCKED) return v3;
            SimWaypoint swp = GetWaypointOf(v3);
            for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = SimPathStore.GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3, CP);
                    if (b < SimPathStore.BLOCKED) return v3;
                }
            }
            Console.WriteLine("Clearing area " + swp);
            SetNodeQualityTimer(v3, SimPathStore.MAYBE_BLOCKED, 30);
            for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = SimPathStore.GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3, CP);
                    if (b == SimPathStore.BLOCKED)
                    {
                        SetNodeQualityTimer(v3, SimPathStore.MAYBE_BLOCKED, 30);
                    }
                }
            }
            return GetWaypointOf(v3).GetSimPosition();
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

        public void SetNodeQualityTimer(Vector3 vector3, int value, int seconds)
        {
            SimPathStore PathStore = GetPathStore3D(vector3);
            Point P = PathStore.ToPoint(vector3);
            CollisionIndex WP = PathStore.GetCollisionIndex(P.X, P.Y);
            WP.SetNodeQualityTimer(PathStore.GetCollisionPlane(vector3.Z), value, seconds);
        }



        static Dictionary<Vector2, SimPathStore> _PathStores = new Dictionary<Vector2, SimPathStore>();

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

        public void SetPathStoreAtOffset(Vector2 v2, SimPathStore value)
        {
            RegisterPathStore((GetGridLocation() + v2), value);
        }

        public SimPathStore GetOffsetPathStore(Vector2 v2)
        {
            return GetPathStore(GetGridLocation() + v2);
        }

        private Vector2 GetGridLocation()
        {
            return RegionLocation;
        }

        static void RegisterPathStore(Vector2 h, SimPathStore value)
        {
            lock (_PathStores) if (_PathStores.ContainsKey(h))
            {
                SimPathStore OLD = _PathStores[h];
                if (OLD == null || OLD == value) return;
                throw new ArgumentException("Bad region change " + OLD + " -> " + value);
            }
            _PathStores[h] = value;
        }

        public SimPathStore N
        {
            get { return GetOffsetPathStore(vN); }
            set { SetPathStoreAtOffset(vN, value); }
        }
        public SimPathStore E
        {
            get { return GetOffsetPathStore(vE); }
            set { SetPathStoreAtOffset(vE, value); }
        }
        public SimPathStore S
        {
            get { return GetOffsetPathStore(vS); }
            set { SetPathStoreAtOffset(vS, value); }
        }
        public SimPathStore W
        {
            get { return GetOffsetPathStore(vW); }
            set { SetPathStoreAtOffset(vW, value); }
        }


        /// <summary>
        ///  The closet usable waypoint to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public SimWaypoint GetWaypointOf(Vector3 v3)
        {
            if (v3.X < 0)
            {
                Vector3 V = v3;
                V.X += 256f;
                return W.GetWaypointOf(V);
            }
            else
                if (v3.X >= MAXY)
                {
                    Vector3 V = v3;
                    V.X -= 256f;
                    return E.GetWaypointOf(V);
                }
                else
                    if (v3.Y < 0)
                    {
                        Vector3 V = v3;
                        V.Y += 256f;
                        return S.GetWaypointOf(V);
                    }
                    else
                        if (v3.Y >= MAXY)
                        {
                            Vector3 V = v3;
                            V.Y -= 256f;
                            return N.GetWaypointOf(V);
                        }
                        else
                        {
                            SimPathStore PathStore0 = GetPathStore3D(v3);
                            return SimWaypointImpl.CreateLocal(v3, PathStore0);
                        }

            SimPathStore PathStore = GetPathStore3D(v3);
            SimWaypoint swp = PathStore.CreateClosestRegionWaypoint(v3, 2);
            float dist = Vector3.Distance(v3, swp.GetSimPosition());
            if (!swp.IsPassable)
            {
                Console.WriteLine("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
            }
            return swp;
        }



        public IList<Vector3d> GetLocalPath(Vector3 start, Vector3 end)
        {
            if (!TerrainBaked) BakeTerrain();
            float Z = start.Z;
            CollisionPlane CP = GetCollisionPlane(Z);
            if (!IsPassable(start, CP))
            {
                Debug("start is not passable: " + start);
            }
            if (!IsPassable(end, CP))
            {
                Debug("end is not passable: " + end);
            }
            return (IList<Vector3d>)GetLocalPath0(GetUsableLocalPositionOf(start, 4), GetUsableLocalPositionOf(end, 4),CP,Z);
        }

        bool TerrainBaked = false;
        object TerrainBakedLock = new object();
        public void BakeTerrain()
        {
            lock (TerrainBakedLock)
            {
                // if (TerrainBaked) return;
                TerrainBaked = true;

                Console.WriteLine("ScanTerrainBlockages: {0}", RegionName);
                float WH = WaterHeight;
                float LastHieght = GetGroundLevel(0, 0);
                return;
                for (int y = 0; y < 256; y++)
                    for (int x = 0; x < 256; x++)
                    {
                        float thisH = GetGroundLevel(x, y);
                        float thisH2 = GetGroundLevel(x, y + 1);
                        float thisH3 = GetGroundLevel(x + 1, y);
                        if (Math.Abs(thisH - LastHieght) > 1 || Math.Abs(thisH - thisH2) > 1 || Math.Abs(thisH - thisH3) > 1)
                        {
                            BlockRange(x, y, 1.001f, 1.001f, thisH);
                        }
                        LastHieght = thisH;
                    }
            }
        }

        public float GetGroundLevel(float x, float y)
        {
            return GroundLevel(x,y);
        }

        public void BlockRange(float x, float y, float sx, float sy, float z)
        {
            SimPathStore PathStore = GetPathStore3D(new Vector3(x, y, z));
            if (PathStore == null) return;
            float StepSize = PathStore.StepSize;
            sx += x;
            sy += y;
            float loopY = sy;
            while (sx >= x)
            {
                while (sy >= y)
                {
                    PathStore.SetBlocked(sx, sy, z, null);
                    sy -= StepSize;
                }
                sy = loopY;
                sx -= StepSize;
            }
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


        private /*i*/ float _XY256;

        public float XY256
        {
            get { return _XY256; }
        }
        private /*i*/ float _Max256;

        public float Max256
        {
            get { return _Max256; }
        }
        private /*i*/ Box3Fill _OuterBounds;

        public Box3Fill OuterBounds
        {
            get { return _OuterBounds; }
        }

        public override string ToString()
        {
            return String.Format("{0}: {1}", GetType().Name,RegionName);// +" Level=" + SimZAverage;
        }

        public const byte BLOCKED = 255;
        public const byte MAYBE_BLOCKED = 254;
        public const byte INITIALLY = 20;
        public const byte PASSABLE = 1;
        public const byte STICKY_PASSABLE = 0;
        readonly public float POINTS_PER_METER = 5f;
        readonly private float LargeScale = 1f;//StepSize;//0.2f;

        public static float PI2 = (float)(Math.PI * 2f);
        public static float RAD2DEG = 360f / PI2;

        readonly public float StepSize;// = 1f / POINTS_PER_METER;
        readonly public int MAPSPACE;// = XY256 * ((int)POINTS_PER_METER);

        /// <summary>
        ///  Todo needs a smarter way to grab the right layers.. 
        ///    The CollisionPlane[] is pretty much a hack
        /// </summary>
        /// <param name="Z"></param>
        /// <returns></returns>
        public CollisionPlane GetCollisionPlane(float Z)
        {
            lock (Matrixes) return GetCollisionPlane0(Z);
        }
        internal CollisionPlane GetCollisionPlane0(float Z)
        {
            if (Matrixes.Count > 0)
            {                
                foreach (CollisionPlane P in Matrixes)
                {
                    if (P.Accepts(Z))
                    {
                        return P;
                    }
                }
            }
            CollisionPlane found = new CollisionPlane(MAPSPACE, MAPSPACE, Z - 0.5f, Z + 1.7f, this);
            Debug("Created matrix[{0}] {1} for {2}", Z, found, this);
            Matrixes.Add(found);
            if (PathFinder != null) PathFinder.OnNewCollisionPlane(found);
            return found;
        }

        float CollisionPlaneHeights = 3.0f;
        internal IList<CollisionPlane> Matrixes = new List<CollisionPlane>();
        public byte[,] GetByteMatrix(float Z)
        {
            return GetCollisionPlane(Z).ByteMatrix;
        }

        public CollisionIndex[,] MeshIndex;

        //readonly string RegionName;
        //readonly byte[,] paths = PathFinding.PathFinderDemo.Instance


        public float UNARRAY_X(int p)
        {
            return p / POINTS_PER_METER + Start.X;
        }
        public float UNARRAY_Y(int p)
        {
            return p / POINTS_PER_METER + Start.Y;
        }

        public int ARRAY_X(float x)
        {
            x -= Start.X;
            if (x < StepSize) return 0;
            double i = Math.Round(x * POINTS_PER_METER, 1);
            int ii = (int)i;
            if ((double)i != ii)
            {
                //throw new ArgumentException("ARRAY_IDX " + i + "!=" + ii);
            }
            if (ii < 0) return 0;
            if (ii >= MAPSPACE) ii = MAPSPACE - 1;
            return ii;
        }

        public int ARRAY_Y(float x)
        {
            x -= Start.Y;
            if (x < StepSize) return 0;
            double i = Math.Round(x * POINTS_PER_METER, 1);
            int ii = (int)i;
            if ((double)i != ii)
            {
                //throw new ArgumentException("ARRAY_IDX " + i + "!=" + ii);
            }
            if (ii < 0) return 0;
            if (ii >= MAPSPACE) ii = MAPSPACE - 1;
            return ii;
        }

        readonly Color[] lastColour = new Color[256];//(Color.Black);

        public Color GetColor(int x, int y , byte[,] matrix)
        {
            byte p = matrix[x, y];
            switch (p)
            {
                case BLOCKED:
                    return OccupiedColor(Color.Olive, MeshIndex[x, y]);
                case MAYBE_BLOCKED:
                    return OccupiedColor(Color.Pink, MeshIndex[x, y]);
                case PASSABLE:
                    return OccupiedColor(Color.Blue, MeshIndex[x, y]);
                case STICKY_PASSABLE:
                    return OccupiedColor(Color.Green, MeshIndex[x, y]);
            }
            Color sb = lastColour[p];
            if (sb == Color.Empty)
            {
                int colorIndex = 240 - ((int)(Math.Log10(p) * 127));
                colorIndex = colorIndex < byte.MinValue ? byte.MinValue : colorIndex > byte.MaxValue ? byte.MaxValue : colorIndex;
                sb = Color.FromArgb(byte.MaxValue, colorIndex, colorIndex, colorIndex);
                lastColour[p] = sb;
            }
            return sb;
        }

        private static Color OccupiedColor(Color c, CollisionIndex cIndex)
        {
            if (cIndex != null)
            {
                int dense = cIndex.OccupiedCount;
                int A = 240 - 10 * dense;
                if (A < 0) A = 20;

                return Color.FromArgb(A, c.R, c.G, c.B);
            }
            return c;
        }


        public byte GetNodeQuality(Vector3 v3, CollisionPlane CP)
        {
            return CP[ARRAY_X((v3.X)), ARRAY_Y((v3.Y))];
        }

        public void SetNodeQuality(Vector3 v3, byte v, CollisionPlane CP)
        {
            CP[ARRAY_X((v3.X)), ARRAY_Y((v3.Y))] = v;
        }

        /// <summary>
        /// Will not changed blocked points - if needed use SetPassable
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        public void SetTraveled(float x, float y, float z)
        {
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            byte[,] mMatrix = GetByteMatrix(z);
            byte b = mMatrix[ix, iy];
            switch (b)
            {
                case BLOCKED:
                    {
                        mMatrix[ix, iy] = MAYBE_BLOCKED;
                        return;
                    }
                case MAYBE_BLOCKED:
                    return;
                case STICKY_PASSABLE:
                    return;
                case PASSABLE:
                    return;
            }
            if (b < 3)
            {
                return;
            }
            mMatrix[ix, iy] = (byte)(b / 2);
        }

        public CollisionIndex SetObjectAt(float x, float y, IMeshedObject simObject, float minZ, float maxZ)
        {
            int ix = ARRAY_X((x));
            int iy = ARRAY_Y((y));

            CollisionIndex W = GetCollisionIndex(ix, iy);
            if (W.AddOccupied(simObject,minZ, maxZ))
            {
               // NeedsUpdate = true;
            }
            return W;
        }

        public void SetPassable(float x, float y, float z)
        {
            ///Debug("SetBlocked: {0} {1}", x, y);
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            if (GetByteMatrix(z)[ix, iy] > 100)
            {
                return;
            }
            GetByteMatrix(z)[ix, iy] = STICKY_PASSABLE;
        }

        //static IList<ISimObject> NOOBJECTS = new List<ISimObject>();
        //private IEnumerable<ISimObject> ObjectsAt(float x, float y)
        //{
        //    x = RangeCheck(x); y = RangeCheck(y);
        //    ///Debug("SetBlocked: {0} {1}", x, y);
        //    int ix = ARRAY_IDX(x);
        //    int iy = ARRAY_IDX(y);
        //    CollisionIndex P = mWaypoints[ix, iy];
        //    if (P == null)
        //    {
        //        return NOOBJECTS;
        //    }
        //    return P.OccupiedListObject;
        //}



        public void SetBlocked(float x, float y, float z, IMeshedObject blocker)
        {
            if (blocker != null) SetObjectAt(x, y, blocker,z,z);
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            ///Debug("SetBlocked: {0} {1}", x, y);
            // if was set Passable dont re-block
            if (GetByteMatrix(z)[ix, iy] == STICKY_PASSABLE)
            {
                //  return;
            }
            GetByteMatrix(z)[ix, iy] = BLOCKED;
            //   SetBubbleBlock(ix, iy, blocker);
        }

        //private void SetBubbleBlock(int ix, int iy, ISimObject blocker)
        //{
        //    int TWO = 2;// (int)Math.Round(POINTS_PER_METER / 2);
        //    int ONE = 1;// (int)Math.Round(POINTS_PER_METER / 2);
        //    if (ix > TWO && iy > TWO && ix < MAPSPACE - TWO && iy < MAPSPACE - TWO)
        //    {
        //        for (int x = ix - ONE; x < ix + TWO; x++)
        //        {
        //            for (int y = iy - ONE; y < iy + TWO; y++)
        //            {
        //                if (x != ix || y != iy)
        //                {
        //                    if (ZMatrix(Z)[x, y] != 0)
        //                    {
        //                        if (blocker != null)
        //                        {
        //                            CollisionIndex W = Waypoint(x, y);
        //                            if (W.AddShadow(blocker) > 1)
        //                            {
        //                                ZMatrix(Z)[x, y] = 0;
        //                            }
        //                            else
        //                            {
        //                                int newV = ZMatrix(Z)[x, y] * 2;
        //                                if (newV > 200) newV = 200;
        //                                ZMatrix(Z)[x, y] = (byte)newV;
        //                            }
        //                        }
        //                    }
        //                }
        //            }
        //        }
        //    }
        //}

        public SimPathStore GetPathStore()
        {
            return this;
        }

        public readonly Vector2 RegionLocation;
        readonly Vector3d GlobalStart;
        readonly Vector3d GlobalEnd;
        readonly Vector3 Start;
        readonly Vector3 Size;
        /// <summary>
        /// Constructor.
        /// </summary>
        public SimPathStore(String simName, Vector2 pos, Vector3d globalPos, Vector3 endTop)
        {
            RegisterPathStore(pos, this);
            RegionName = simName;
            RegionLocation = pos;
            Start = Vector3.Zero;
            GlobalStart = globalPos;
            GlobalEnd = globalPos;
            Size = endTop;
            _XY256 = Size.X;
            _OuterBounds = new Box3Fill(true);
            OuterBounds.AddPoint(Start.X, Start.Y, Start.Z, 0);
            OuterBounds.AddPoint(endTop.X, endTop.Y, endTop.Z, 0);
            //TheSimZMinMaxLevel = new SimZMinMaxLevel(MinMaxLevel);
            StepSize = 1f / POINTS_PER_METER;
            _Max256 = XY256 - StepSize;
            MAPSPACE = (int)XY256 * ((int)POINTS_PER_METER);
            SetMapSpace(MAPSPACE);
            if (Size.X != Size.Y) throw new Exception("X and Y must be the same for " + this);
            //CreateDefaultRoutes();
          //  CurrentPlane = new CollisionPlane(MAPSPACE,MAPSPACE,0);
        }


        internal void SetMapSpace(int MAPSPACE)
        {
            if (MeshIndex == null)
                MeshIndex = new CollisionIndex[MAPSPACE, MAPSPACE];
        }

        //private float RangeCheckX(float PtY)
        //{
        //    PtY -= Start.X;
        //    if (PtY > Max256)
        //    {
        //        return Max256;
        //    }
        //    else if (PtY < 0)
        //    {
        //        return 0f;
        //    }
        //    else
        //    {
        //        return (float)(Math.Round(PtY * POINTS_PER_METER, 0) / POINTS_PER_METER);
        //    }
        //}
        //private float RangeCheckY(float PtY)
        //{
        //    PtY -= Start.Y;
        //    if (PtY > Max256)
        //    {
        //        return Max256;
        //    }
        //    else if (PtY < 0)
        //    {
        //        return 0f;
        //    }
        //    else
        //    {
        //        return (float)(Math.Round(PtY * POINTS_PER_METER, 0) / POINTS_PER_METER);
        //    }
        //}

        public CollisionIndex FindNode(float x, float y)
        {
            return MeshIndex[ARRAY_X((x)), ARRAY_Y((y))];
        }

        public CollisionIndex CreateFirstNode(float x, float y)
        {
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            return GetCollisionIndex(ix, iy);
        }

        public CollisionIndex GetCollisionIndex(int ix, int iy)
        {
            lock (MeshIndex)
            {
                CollisionIndex wp = MeshIndex[ix, iy];
                if (wp == null)
                {
                    float x = UNARRAY_X(ix);
                    float y = UNARRAY_Y(iy);
                    wp = CollisionIndex.CreateCollisionIndex(x, y, this);
                    //if (mWaypoints[ix, iy] != wp)
                    //{
                    //    Debug("diff wapoints in same space {0} != {1}", mWaypoints[ix, iy], wp);
                    //}
                }
                return wp;
            }
        }
         

        public IList<SimRoute> GetSimplifedRoute(IList<SimRoute> v3s)
        {
            List<SimRoute> vectors = new List<SimRoute>();
            List<SimRoute> skipped = new List<SimRoute>();
            float ZAngle = float.NaN;
            bool ZAngleValid = false;
            Vector3d currentV3 = v3s[0].StartNode.Position;
            vectors.Add(v3s[0]);
            for (int Current = 0; Current < v3s.Count; Current++)
            {
                Vector3d compare = v3s[Current].EndNode.Position;
                Vector3d dif = compare - currentV3;
                float NewZAngle = ComparableAngle(Math.Atan2(dif.X, dif.Y));
                if (!ZAngleValid)
                {
                    ZAngleValid = true;
                }
                else
                {
                    float adif = Math.Abs(NewZAngle - ZAngle);
                    if (adif * RAD2DEG > 10)
                    {
                        if (skipped.Count == 0)
                        {
                            vectors.Add(v3s[Current]);
                        }
                        else
                        {
                            SimRoute R = new SimRouteMulti(skipped.ToArray());
                            R.ReWeight(0.9f);
                            AddArc(R);
                            skipped.Clear();
                            vectors.Add(R);
                            skipped.Add(v3s[Current]);
                        }
                    }
                    else
                    {
                        skipped.Add(v3s[Current]);
                    }
                }
                ZAngle = NewZAngle;
                currentV3 = compare;
            }

            return vectors;
        }


        static public IList<Vector3d> GetSimplifedRoute(Vector3d currentV3In, IList<Vector3d> v3s, int MaxTurnDegrees, float MaxDist)
        {
            if (v3s.Count < 3) return v3s;
            Vector3d currentV3 = currentV3In;
            IList<Vector3d> vectors = new List<Vector3d>();
            float MaxTurnRadians = MaxTurnDegrees / RAD2DEG;
            float ZAngle = float.NaN;
            bool ZAngleValid = false;
            int Max = v3s.Count - 1;
            for (int Current = 0; Current < Max; Current++)
            {
                bool UsePoint = false;
                Vector3d compare = v3s[Current];
                Vector3d dif = compare - currentV3;
                float NewZAngle = ComparableAngle(Math.Atan2(dif.Y, dif.X));
                if (!ZAngleValid)
                {
                    ZAngleValid = true;
                    ZAngle = NewZAngle;
                    // UsePoint = true;
                }
                else
                {
                    float adif = Math.Abs(NewZAngle - ZAngle);
                    if (adif > MaxTurnRadians)
                        UsePoint = true;
                    else
                        if (Vector3d.Distance(currentV3, compare) > MaxDist)
                            UsePoint = true;
                }
                if (UsePoint)
                {
                    vectors.Add(compare);
                    currentV3 = compare;
                    ZAngle = NewZAngle;
                }
            }
            // add last
            vectors.Add(v3s[v3s.Count - 1]);
            return vectors;
        }


        private static float ComparableAngle(double p)
        {
            while (p < 0)
            {
                p += PI2;
            }
            while (p > PI2)
            {
                p -= PI2;
            }
            return (float)p + PI2;
        }


        static public IList<SimWaypoint> RouteListToPoints(IList<SimRoute> routeToSimplify)
        {
            IList<SimWaypoint> points = new List<SimWaypoint>();
            SimWaypoint Last = null;
            foreach (SimRoute move in routeToSimplify)
            {
                if (move.StartNode != Last)
                {
                    Last = move.StartNode;
                    points.Add(Last);
                }
                if (move.EndNode != Last)
                {
                    Last = move.EndNode;
                    points.Add(Last);
                }
            }
            return points;
        }

        bool PunishChangeDirection;
        public IList<Vector3d> GetLocalPath0(Vector3 start, Vector3 end, CollisionPlane CP, float Z)
        {
            PathFinderDemo panel = PathFinder;
            PunishChangeDirection = !PunishChangeDirection;    //toggle each time
            if (!PunishChangeDirection)
            {

            }
            Point S = ToPoint(start);
            Point E = ToPoint(end);
            IList<PathFinderNode> pfn = null;
            
            CP.EnsureUpToDate();
            try
            {
                PathFinderFasting pff = new PathFinderFasting(CP.ByteMatrix);
                if (panel != null) panel.SetStartEnd(S, E);
                // pff.Diagonals = false;
                //pff.ReopenCloseNodes = true;
                pff.SearchLimit = 100000000;
                pff.PunishChangeDirection = PunishChangeDirection;
                pfn = pff.FindPath(S, E);
            }
            catch (Exception e)
            {
                Debug("Cant do route! " + e + " on " + CP);
            }
            if (pfn == null || pfn.Count == 0)
            {
                Debug("Cant do pfn on " + CP);
                IList<Vector3d> temp = new List<Vector3d>();
                temp.Add(GetPathStore().LocalToGlobal(end));
                return temp;
            }
            if (panel != null) panel.ShowPath(pfn);
            List<Vector3d> r = (List<Vector3d>)PathfinderNodesToV3s(pfn, Z);
            r.Reverse();
            return r;
        }

        public bool IsPassable(Vector3 end, CollisionPlane CP)
        {
            double Dist;
            if (GetNodeQuality(end,CP) == BLOCKED) return false;
            // if (true) return true;
            SimWaypoint W = ClosestRegionNode(end.X, end.Y, end.Z, out Dist, true);
            return W.IsPassable;
        }



        private IList<Vector3d> PathfinderNodesToV3s(IList<PathFinderNode> pfn, float Z)
        {
            Vector3d V = GetGlobalCorner();
            IList<Vector3d> v3s = new List<Vector3d>();
            foreach (PathFinderNode P in pfn)
            {
                float x = UNARRAY_X(P.X);
                float y = UNARRAY_Y(P.Y);
                Vector3d v3 = new Vector3d(x + V.X, y + V.Y, Z);
                v3s.Add(v3);
            }
            return GetSimplifedRoute(v3s[0], v3s, 5, 4f);
        }

        private Vector3d GetGlobalCorner()
        {
            return new Vector3d(GetPathStore().LocalToGlobal(new Vector3(0, 0, 0)));
        }

        public Point ToPoint(Vector3 start)
        {
            return new Point(ARRAY_X((start.X)), ARRAY_Y((start.Y)));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="point"></param>
        /// <param name="rotation"></param>
        private void Update(UUID agentID, Vector3 point, Quaternion rotation)
        {
            SetTraveled(point.X, point.Y, point.Z);
        }


        internal void UpdateFromImage(Image image)
        {
            if (image == null) return;
            Bitmap edges = new Bitmap((Image)image.Clone());
            new DisplayImage("Edges", edges).Activate();
            Debug("START Edge detection " + image);
            Bitmap e = EdgeDetection(edges, 34f, delegate(int x, int y)
            {
                edges.SetPixel(x, y, Color.Yellow);
                SetBlocked((float)x, (float)y, 0f, null);
            });
            Debug("END Edge detection");
            new DisplayImage("Clone", e).Activate();
        }


        private delegate void EdgeAction(int x, int y);

        private static Bitmap EdgeDetection(Bitmap Image, double Threshold, EdgeAction EdgeColor)
        {
            System.Drawing.Bitmap TempBitmap = Image;
            System.Drawing.Bitmap NewBitmap = new System.Drawing.Bitmap(TempBitmap.Width, TempBitmap.Height);
            System.Drawing.Graphics NewGraphics = System.Drawing.Graphics.FromImage(NewBitmap);
            NewGraphics.DrawImage(TempBitmap, new System.Drawing.Rectangle(0, 0, TempBitmap.Width, TempBitmap.Height), new System.Drawing.Rectangle(0, 0, TempBitmap.Width, TempBitmap.Height), System.Drawing.GraphicsUnit.Pixel);
            NewGraphics.Dispose();
            for (int x = 0; x < NewBitmap.Width; ++x)
            {
                for (int y = 0; y < NewBitmap.Height; ++y)
                {
                    bool EdgeSet = false;
                    Color CurrentColor = NewBitmap.GetPixel(x, y);
                    if (y < NewBitmap.Height - 1 && x < NewBitmap.Width - 1)
                    {
                        Color TempColor = NewBitmap.GetPixel(x + 1, y + 1);
                        if (Math.Sqrt(((CurrentColor.R - TempColor.R) * (CurrentColor.R - TempColor.R)) +
                            ((CurrentColor.G - TempColor.G) * (CurrentColor.G - TempColor.G)) +
                            ((CurrentColor.B - TempColor.B) * (CurrentColor.B - TempColor.B))) > Threshold)
                        {
                            EdgeColor(x, y);
                        }
                        EdgeSet = true;
                    }
                    if (y < NewBitmap.Height - 1 && !EdgeSet)
                    {
                        Color TempColor = NewBitmap.GetPixel(x, y + 1);
                        if (Math.Sqrt(((CurrentColor.R - TempColor.R) * (CurrentColor.R - TempColor.R)) +
                            ((CurrentColor.G - TempColor.G) * (CurrentColor.G - TempColor.G)) +
                            ((CurrentColor.B - TempColor.B) * (CurrentColor.B - TempColor.B))) > Threshold)
                        {
                            EdgeColor(x, y);
                            // NewBitmap.SetPixel(x, y, EdgeColor);
                        }
                        EdgeSet = true;
                    }
                    if (x < NewBitmap.Width - 1 && !EdgeSet)
                    {
                        Color TempColor = NewBitmap.GetPixel(x + 1, y);
                        if (Math.Sqrt(((CurrentColor.R - TempColor.R) * (CurrentColor.R - TempColor.R)) +
                            ((CurrentColor.G - TempColor.G) * (CurrentColor.G - TempColor.G)) +
                            ((CurrentColor.B - TempColor.B) * (CurrentColor.B - TempColor.B))) > Threshold)
                        {
                            EdgeColor(x, y);
                            //NewBitmap.SetPixel(x, y, EdgeColor);
                        }
                        EdgeSet = true;
                    }
                }
            }
            return NewBitmap;
        }
        public static void Debug(string format, params object[] arg)
        {
            Console.WriteLine(String.Format("[SimPathStore] {0}", format), arg);
        }


        public void TaintMatrix()
        {
            //lock (mWaypoints)
            for (int x = 0; x < MAPSPACE; x++)
            {
                for (int y = 0; y < MAPSPACE; y++)
                {
                    CollisionIndex W = MeshIndex[x, y];
                    if (W != null)
                        W.TaintMatrix();
                }
            }
        }

        public SimRoute InternArc(SimWaypoint StartNode, SimWaypoint EndNode, double Weight)
        {
            return SimGlobalRoutes.Instance.InternArc(StartNode, EndNode, Weight);
        }

        public SimWaypoint CreateClosestRegionWaypoint(Vector3 v3, double maxDist)
        {
            Vector3d v3d = GetPathStore().LocalToGlobal(v3);
            double Dist;
            SimWaypoint W = SimGlobalRoutes.Instance.ClosestNode(v3d.X, v3d.Y, v3d.Z, out Dist, true);
            if (Dist > maxDist)
            {
                SimWaypoint V3 = SimGlobalRoutes.Instance.CreateClosestWaypoint(v3d);
                return V3;
            }
            return W;
        }

        public SimRoute Intern2Arc(SimWaypoint StartNode, SimWaypoint EndNode, double Weight)
        {
            return SimGlobalRoutes.Instance.Intern2Arc(StartNode, EndNode, Weight);
        }

        private SimWaypoint ClosestRegionNode(float x, float y, float z, out double Dist, bool IgnorePassable)
        {
            Vector3d v3d = GetPathStore().LocalToGlobal(new Vector3(x, y, z));
            return SimGlobalRoutes.Instance.ClosestNode(v3d.X, v3d.Y, v3d.Z, out Dist, IgnorePassable);
        }

        public void Clear()
        {
            SimGlobalRoutes.Instance.Clear();
        }

        private void AddArc(SimRoute R)
        {
            SimGlobalRoutes.Instance.AddArc(R);
        }

        Dictionary<UUID, MoverTracking> LaskKnownPos = new Dictionary<UUID, MoverTracking>();

        public void UpdateTraveled(UUID uUID, Vector3 after, Quaternion rot)
        {
            //return;
            lock (LaskKnownPos) if (!LaskKnownPos.ContainsKey(uUID))
                {
                    LaskKnownPos[uUID] = new MoverTracking(after, rot, this);
                    return;
                }
            LaskKnownPos[uUID].Update(after, rot);
        }

        private void Refresh(PathSystem3D.Mesher.Box3Fill changed, CollisionPlane CurrentPlane)
        {
            int xs = (int)changed.MinX;
            int ys = (int)changed.MaxX;
            int xe = (int)changed.MinY;
            int ye = (int)changed.MaxY;
            float ConsiderOnlyAboveZ = changed.MinZ;
            if (CurrentPlane!=null)
                ConsiderOnlyAboveZ = CurrentPlane.MinZ - 1f;
            if (ye < 0 || xe < 0) return;
            if (xs > 0) xs--;
            if (ys > 0) ys--;
            int MAX = MAPSPACE - 1;
            if (xe < MAX) xe++;
            if (ys < MAX) ye++;


            float[,] GP = CurrentPlane.GroundPlane;
            //  lock (mWaypoints)
            {
                for (int x = xs; x <= xe; x++)
                    for (int y = ys; y <= ye; y++)
                    {
                        CollisionIndex WP = MeshIndex[x, y];
                        if (WP != null) WP.TaintMatrix();
                    }
                if (CurrentPlane != null)
                    for (int x = xs; x <= xe; x++)
                        for (int y = ys; y <= ye; y++)
                        {
                            CollisionIndex WP = MeshIndex[x, y];
                            if (WP != null)
                            {
                                WP.UpdateMatrix(CurrentPlane,GP[x,y],CurrentPlane.MinZ, CurrentPlane.MaxZ,GP);
                            }
                        }
            }
        }

        public void Refresh(Box3Fill changed)
        {
            Refresh(changed, null);
        }

        internal PathFinderDemo PathFinder;

        public void ShowDebugger()
        {
            if (PathFinder == null)
            {
                PathFinder = new PathFinderDemo(this);
            }
            PathFinder.CollisionPlaneListUpdate();
            PathFinder.Show();
        }


        public Vector3 LocalOuterEdge(Vector3 startLocal, SimPosition endPosOther, out SimPathStore nextRegion)
        {
            SimPathStore rother = endPosOther.GetPathStore();
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
            nextRegion = GetPathStore(GetGridLocation() + SD);
            return new Vector3(x, y, vother.Z);
        }


        public static SimPathStore GetPathStore(Vector3d pos)
        {
            if (pos.X < 0 || pos.Y < 0)
            {
                throw new ArgumentException("GlobalToWaypoint? " + pos);
            }
            if (pos.X < 256 || pos.Y < 256)
            {
                Console.WriteLine("GlobalToWaypoint? " + pos);
            }
            return GetPathStore(new Vector2(Round256(pos.X)/256, Round256(pos.Y)/256));
        }

        public static SimPathStore GetPathStore(Vector2 loc)
        {
            lock (_PathStores)
            {
                SimPathStore PS;
                if (_PathStores.TryGetValue(loc,out PS)) {
                    return PS;
                }
                return new SimPathStore(null, loc, new Vector3d(loc.X * 256, loc.Y * 256, 0), new Vector3(256, 256, float.MaxValue));
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

        public SimPathStore GetPathStore3D(Vector3 vv3)
        {
            return this;
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
            SimPathStore PathStore = GetPathStore3D(next);
            CollisionPlane CP = PathStore.GetCollisionPlane(next.Z);
            return PathStore.IsPassable(next, CP);
        }


        public CollisionIndex GetCollisionIndexAt(Vector3 V)
        {
            throw new NotImplementedException();
        }

        internal CollisionPlane CreateMoverPlane(float Z)
        {
            CollisionPlane found = new CollisionPlane(MAPSPACE, MAPSPACE, Z - 0.5f, Z + 1.7f, this);
            Debug("Created matrix[{0}] {1} for {2}", Z, found, this);
            Matrixes.Add(found);
            if (PathFinder != null) PathFinder.OnNewCollisionPlane(found);
            return found;
        }
    }

    public class MoverTracking
    {
        protected double MovedAllot = 3.0f;
        Vector3 LastPosition;
        Quaternion LastRotation;
        SimPathStore Store;
        public MoverTracking(Vector3 firstP, Quaternion firtsR, SimPathStore store)
        {
            LastPosition = firstP;
            Store = store;
            LastRotation = firtsR;
        }

        public void Update(Vector3 nextPosition, Quaternion rotation)
        {
            double dist = Vector3.Distance(LastPosition, nextPosition);
            if (dist > MovedAllot)
            {
                MakeMovement(nextPosition);
            }
            else
                if (RotationDiffernt(rotation, LastRotation))
                {
                    MakeMovement(nextPosition);
                    LastRotation = rotation;
                }
        }

        private void MakeMovement(Vector3 nextPosition)
        {
            float dist = Vector3.Distance(LastPosition, nextPosition);
            if (dist > Store.StepSize)
            {
                Store.SetTraveled(LastPosition, nextPosition);
                LastPosition = nextPosition;
            }
        }

        static bool RotationDiffernt(Quaternion rotation, Quaternion LastRotation)
        {
            Quaternion diff = rotation - LastRotation;
            return (diff.Length() > 0.2);
        }
    }
}
