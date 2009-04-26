using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using PathSystem3D.Mesher;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;

namespace PathSystem3D.Navigation
{

    //public class SimPathStore
    //{
    //    SimPathStore PathStore;
    //    public string RegionName { get; private set; }
    //    // util
    //    static public List<Box3Fill> TrianglesToBoxes(List<Triangle> tl, Box3Fill OuterBox, float padXYZ)
    //    {


    //        int tc = tl.Count;
    //        if (tc < 16)
    //        {
    //            return AddTrianglesV1(tl, tc, OuterBox, padXYZ);
    //        }
    //        else
    //        {
    //            return AddTrianglesV2(tl, tc, OuterBox, padXYZ);
    //        }
    //        // Console.WriteLine(InnerBoxes.Count);
    //    }

    //    private static List<Box3Fill> AddTrianglesV1(List<Triangle> triangles, int len, Box3Fill OuterBox, float padXYZ)
    //    {
    //        List<Box3Fill> InnerBoxes = new List<Box3Fill>();
    //        Triangle[] ts = triangles.ToArray();
    //        int len1 = len - 1;
    //        for (int i = 0; i < len1; i++)
    //        {
    //            Triangle t1 = ts[i];
    //            bool used = false;
    //            OuterBox.AddTriangle(t1, padXYZ);
    //            for (int ii = i + 1; ii < len; ii++)
    //            {
    //                Triangle t2 = ts[ii];
    //                int shared = SharedVertexs(t1, t2);
    //                if (shared == 3) continue;
    //                if (shared == 2)
    //                {
    //                    Box3Fill B = new Box3Fill(t1, t2, padXYZ);
    //                    InnerBoxes.Add(B);
    //                    used = true;
    //                }
    //            }
    //            if (!used)
    //            {
    //                Box3Fill B = new Box3Fill(true);
    //                B.AddTriangle(t1, padXYZ);
    //                InnerBoxes.Add(B);
    //            }
    //        }
    //        return InnerBoxes;
    //    }
    //    private static int SharedVertexs(Triangle t1, Triangle t2)
    //    {
    //        int sharedV = 0;
    //        if (t1.v1 == t2.v1) sharedV++;
    //        else
    //            if (t1.v1 == t2.v2) sharedV++;
    //            else
    //                if (t1.v1 == t2.v3) sharedV++;
    //        if (t1.v2 == t2.v1) sharedV++;
    //        else
    //            if (t1.v2 == t2.v2) sharedV++;
    //            else
    //                if (t1.v2 == t2.v3) sharedV++;
    //        if (t1.v3 == t2.v1 || t1.v3 == t2.v2 || t1.v3 == t2.v3) return sharedV + 1;
    //        return sharedV;
    //    }

    //    private static List<Box3Fill> AddTrianglesV2(List<Triangle> ts, int len, Box3Fill OuterBox, float padXYZ)
    //    {
    //        List<Box3Fill> InnerBoxes = new List<Box3Fill>();
    //        int len1 = len - 2;
    //        for (int i = 0; i < len1; i += 2)
    //        {
    //            Triangle t1 = ts[i];
    //            Triangle t2 = ts[i + 1];
    //            OuterBox.AddTriangle(t1, padXYZ);
    //            OuterBox.AddTriangle(t2, padXYZ);
    //            Box3Fill B = new Box3Fill(t1, t2, padXYZ);
    //            InnerBoxes.Add(B);
    //            bool used = false;
    //            for (int ii = i + 2; ii < len; ii++)
    //            {
    //                t2 = ts[ii];
    //                int shared = SharedVertexs(t1, t2);
    //                if (shared == 3) continue;
    //                if (shared == 2)
    //                {
    //                    B = new Box3Fill(t1, t2, padXYZ);
    //                    InnerBoxes.Add(B);
    //                    used = true;
    //                }
    //            }
    //            if (!used)
    //            {
    //                B = new Box3Fill(true);
    //                B.AddTriangle(t1, padXYZ);
    //                InnerBoxes.Add(B);
    //            }
    //        }
    //        return InnerBoxes;
    //    }


    //    Dictionary<IComparable, SimMesh> meshedObjects = new Dictionary<IComparable, SimMesh>();
    //    /// <summary>
    //    /// By default no boxes are passable
    //    /// </summary>
    //    public Predicate<IComparable> IsPassablePredicate = delegate(IComparable id) { return false; };
    //    /// <summary>
    //    /// The Pathstore can implment this
    //    /// </summary>
    //    public SimZLevel GroundLevel = delegate(float x, float y) { return 0; };
    //    public class RuntimeMesh : SimMesh
    //    {
    //        IComparable ID;
    //        static SimPathStore system;

    //        public RuntimeMesh(IComparable id, Box3Fill outer, List<Box3Fill> inners, SimPathStore paths)
    //            : base(outer, inners, paths)
    //        {
    //        }

    //        public override bool Update(MeshableObject simObject)
    //        {
    //            return true;
    //        }

    //        public override void RemeshObject(Box3Fill changed)
    //        {
    //            return;
    //        }

    //        public override bool IsRegionAttached()
    //        {
    //            return true;
    //        }

    //        public override bool IsPassable
    //        {
    //            get { return system.IsPassablePredicate(ID); }
    //        }
    //    }
    //    // setup
    //    void AddBoxes(IComparable id, List<Box3Fill> boxes)
    //    {
    //        Box3Fill Outer = new Box3Fill(true);

    //        foreach (Box3Fill B in boxes)
    //        {
    //            Outer.Expand(B);

    //        }
    //        SimMesh MO = new RuntimeMesh(id, Outer, boxes, PathStore);
    //        //MeshedObject simMesh = new MeshedObject(boxes);
    //        meshedObjects.Add(id, MO);
    //        PathStore.AddCollisions(MO);
    //    }
    //    public void RemoveBoxes(IComparable id)
    //    {
    //        SimMesh MO = meshedObjects[id];//.Remove(id);

    //        Box3Fill changed = new Box3Fill(true);
    //        MO.RemoveFromWaypoints(changed);
    //        PathStore.UpdateMatrixes();
    //        meshedObjects.Remove(id);

    //    }
    //    public void SetPhysicalPredicate(Predicate<IComparable> callback)
    //    {
    //        IsPassablePredicate = callback;
    //    }
    //    public void SetGroundLevel(SimZLevel callback)
    //    {
    //        GroundLevel = callback;
    //    }

    //    public void SetPathStore(SimPathStore PS)
    //    {
    //        PathStore = PS;
    //    }

    //    // work
    //    public IList<Vector3d> GetPath(Vector3 capsuleSize, Vector3 from, Vector3 to)
    //    {
    //        return PathStore.GetLocalPath(from, to);
    //    }

    //    // Updates
    //    public void SetTraveled(Vector3 from, Vector3 to)
    //    {
    //        PathStore.SetTraveled(from.X, from.Y, from.Z);
    //    }

    //    public void SetBlocked(Vector3 from, Vector3 to)
    //    {
    //        PathStore.SetBlocked(from.X, from.Y, from.Z, null);
    //    }

    //    internal static byte[] TextureBytesToUUID(UUID uUID)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal static Vector3 GlobalToLocal(Vector3d vstart)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal SimPathStore GetPathStore(Vector3 vv3)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal bool IsPassable(Vector3 next)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal Vector3d LocalToGlobal(Vector3 start)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal float GetGroundLevel(float p, float p_2)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal static SimPathStore GetRegion(Vector3d v3d)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal void SetMapSpace(int MAPSPACE)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    internal void SetNodeQualityTimer(Vector3 vector3, byte p, int p_3)
    //    {
    //        throw new NotImplementedException();
    //    }


    //    internal static SimPathStore GetRegion(Vector2 RegionHandle)
    //    {
    //        throw new NotImplementedException();
    //    }

    //    public float WaterHeight { get; private set; }


    //    public static IList<Vector3d> GetPath(Vector3d globalStart, Vector3d globalEnd, double endFudge, out bool OnlyStart)
    //    {
    //        MeshableObject posStart = SimWaypointImpl.CreateGlobal(globalStart);
    //        MeshableObject posEnd = SimWaypointImpl.CreateGlobal(globalEnd);
    //        SimPathStore regStart = posStart.GetSimRegion();
    //        SimPathStore regEnd = posEnd.GetSimRegion();
    //        Vector3 localStart = posStart.GetSimPosition();
    //        Vector3 localEnd = posEnd.GetSimPosition();
    //        IList<Vector3d> route;
    //        // Same region?
    //        if (regStart == regEnd)
    //        {
    //            return regStart.GetAtLeastPartial(localStart, localEnd, (float)endFudge, out OnlyStart);
    //        }
    //        OnlyStart = true; // will be only a partial path
    //        SimPathStore nextRegion;
    //        Vector3 localLast = regStart.LocalOuterEdge(localStart, posEnd, out nextRegion);
    //        // needs to go to edge
    //        route = regStart.GetLocalPath(localStart, localLast);
    //        // at egde so make a crossing
    //        Vector3 enterEdge = EnterEdge(localLast, nextRegion.GetGridLocation() - regStart.GetGridLocation());
    //        route.Add(nextRegion.LocalToGlobal(enterEdge));
    //        return route;
    //    }


    //    internal IList<Vector3d> GetAtLeastPartial(Vector3 localStart, Vector3 localEnd, float endFudge, out bool OnlyStart)
    //    {
    //        List<Vector3d> route;
    //        Vector3 newEnd = localEnd;
    //        route = GetLocalPath(localStart, newEnd);
    //        if (route.Count > 1)
    //        {
    //            OnlyStart = false;
    //            return route;
    //        }
    //        OnlyStart = true;
    //        Vector3 diff = localEnd - localStart;
    //        while (diff.Length() > 10)
    //        {
    //            diff = diff * 0.8f;
    //            newEnd = localStart + diff;
    //            route = GetLocalPath(localStart, newEnd);
    //            if (route.Count > 1) return route;
    //        }
    //        OnlyStart = false; // Since this will be the best
    //        // try to move to nearby
    //        float step = 45 * SimPathStore.RAD2DEG;
    //        for (double angle = 0; angle < SimPathStore.PI2; angle += step)
    //        {
    //            newEnd = localEnd + ZAngleVector(angle) * endFudge;
    //            route = GetLocalPath(localStart, newEnd);
    //            if (route.Count > 1) return route;
    //        }
    //        route = new List<Vector3d>();
    //        route.Add(LocalToGlobal(localStart));
    //        SimPathStore PathStore = GetPathStore(localStart);
    //        CollisionPlane CP = PathStore.GetCollisionPlane(localStart.Z);

    //        Console.WriteLine("very bad fake route for " + CP);
    //        return route;
    //    }


    //    /// <summary>
    //    ///  The closet usable space to the v3 TODO
    //    /// </summary>
    //    /// <param name="v3"></param>
    //    /// <returns></returns>
    //    public Vector3 GetUsableLocalPositionOf(Vector3 v3, float useDist)
    //    {
    //        SimPathStore PathStore = GetPathStore(v3);
    //        CollisionPlane CP = PathStore.GetCollisionPlane(v3.Z);

    //        byte b = PathStore.GetNodeQuality(v3, CP);
    //        // float useDist = GetSizeDistance();
    //        if (b != SimPathStore.BLOCKED) return v3;
    //        SimWaypoint swp = GetWaypointOf(v3);
    //        for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
    //        {
    //            for (int dir = 0; dir < 360; dir += 15)
    //            {
    //                v3 = SimPathStore.GetLocalLeftPos(swp, dir, distance);
    //                b = PathStore.GetNodeQuality(v3, CP);
    //                if (b < SimPathStore.BLOCKED) return v3;
    //            }
    //        }
    //        Console.WriteLine("Clearing area " + swp);
    //        SetNodeQualityTimer(v3, SimPathStore.MAYBE_BLOCKED, 30);
    //        for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
    //        {
    //            for (int dir = 0; dir < 360; dir += 15)
    //            {
    //                v3 = SimPathStore.GetLocalLeftPos(swp, dir, distance);
    //                b = PathStore.GetNodeQuality(v3, CP);
    //                if (b == SimPathStore.BLOCKED)
    //                {
    //                    SetNodeQualityTimer(v3, SimPathStore.MAYBE_BLOCKED, 30);
    //                }
    //            }
    //        }
    //        return GetWaypointOf(v3).GetSimPosition();
    //    }



    //    static Dictionary<Vector2, SimPathStore> _CurrentRegions = new Dictionary<Vector2, SimPathStore>();


    //    readonly float MAXY = 256f;

    //    static Vector2 vC = new Vector2(0, 0), // C
    //           vN = new Vector2(0, 1), // N
    //           vNE = new Vector2(1, 1), // NE
    //           vE = new Vector2(1, 0),  // E
    //           vSE = new Vector2(1, -1), // SE
    //           vS = new Vector2(0, -1), // S
    //           vSW = new Vector2(-1, -1), // SW
    //           vW = new Vector2(-1, 0), // W
    //           vNW = new Vector2(-1, 1); // NW

    //    public static Vector2[] XYOf = { vC, vN, vNE, vE, vSE, vS, vSW, vW, vNW };

    //    public void SetRegionOffset(Vector2 v2, SimPathStore value)
    //    {
    //        SetRegion(HandleOf(GetGridLocation() + v2), value);
    //    }

    //    public static Vector2 HandleOf(Vector2 v2)
    //    {
    //        return v2;
    //    }

    //    public SimPathStore GetOffsetRegion(Vector2 v2)
    //    {
    //        return GetRegion(GetGridLocation() + v2);
    //    }

    //    private Vector2 GetGridLocation()
    //    {
    //        throw new NotImplementedException();
    //    }

    //    static void SetRegion(Vector2 h, SimPathStore value)
    //    {
    //        if (_CurrentRegions.ContainsKey(h))
    //        {
    //            SimPathStore OLD = _CurrentRegions[h];
    //            if (OLD == null || OLD == value) return;
    //            throw new ArgumentException("Bad region change " + OLD + " -> " + value);
    //        }
    //        _CurrentRegions[h] = value;
    //    }

    //    public SimPathStore N
    //    {
    //        get { return GetOffsetRegion(vN); }
    //        set { SetRegionOffset(vN, value); }
    //    }
    //    public SimPathStore E
    //    {
    //        get { return GetOffsetRegion(vE); }
    //        set { SetRegionOffset(vE, value); }
    //    }
    //    public SimPathStore S
    //    {
    //        get { return GetOffsetRegion(vS); }
    //        set { SetRegionOffset(vS, value); }
    //    }
    //    public SimPathStore W
    //    {
    //        get { return GetOffsetRegion(vW); }
    //        set { SetRegionOffset(vW, value); }
    //    }


    //    /// <summary>
    //    ///  The closet usable waypoint to the v3 TODO
    //    /// </summary>
    //    /// <param name="v3"></param>
    //    /// <returns></returns>
    //    public SimWaypoint GetWaypointOf(Vector3 v3)
    //    {
    //        if (v3.X < 0)
    //        {
    //            Vector3 V = v3;
    //            V.X += 256f;
    //            return W.GetWaypointOf(V);
    //        }
    //        else
    //            if (v3.X >= MAXY)
    //            {
    //                Vector3 V = v3;
    //                V.X -= 256f;
    //                return E.GetWaypointOf(V);
    //            }
    //            else
    //                if (v3.Y < 0)
    //                {
    //                    Vector3 V = v3;
    //                    V.Y += 256f;
    //                    return S.GetWaypointOf(V);
    //                }
    //                else
    //                    if (v3.Y >= MAXY)
    //                    {
    //                        Vector3 V = v3;
    //                        V.Y -= 256f;
    //                        return N.GetWaypointOf(V);
    //                    }
    //                    else
    //                    {
    //                        SimPathStore PathStore0 = GetPathStore(v3);
    //                        return SimWaypointImpl.CreateLocal(v3, PathStore0);
    //                    }

    //        SimPathStore PathStore = GetPathStore(v3);
    //        SimWaypoint swp = PathStore.CreateClosestRegionWaypoint(v3, 2);
    //        float dist = Vector3.Distance(v3, swp.GetSimPosition());
    //        if (!swp.Passable)
    //        {
    //            Console.WriteLine("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
    //        }
    //        return swp;
    //    }



    //    public List<Vector3d> GetLocalPath(Vector3 start, Vector3 end)
    //    {
    //        if (!TerrainBaked) BakeTerrain();
    //        SimPathStore PathStore = GetPathStore(start);
    //        return (List<Vector3d>)PathStore.GetLocalPath(GetUsableLocalPositionOf(start, 4), GetUsableLocalPositionOf(end, 4));
    //    }

    //    bool TerrainBaked = false;
    //    object TerrainBakedLock = new object();
    //    public void BakeTerrain()
    //    {
    //        lock (TerrainBakedLock)
    //        {
    //            // if (TerrainBaked) return;
    //            TerrainBaked = true;

    //            Console.WriteLine("ScanTerrainBlockages: {0}", RegionName);
    //            float WH = WaterHeight;
    //            float LastHieght = GetGroundLevel(0, 0);
    //            return;
    //            for (int y = 0; y < 256; y++)
    //                for (int x = 0; x < 256; x++)
    //                {
    //                    float thisH = GetGroundLevel(x, y);
    //                    float thisH2 = GetGroundLevel(x, y + 1);
    //                    float thisH3 = GetGroundLevel(x + 1, y);
    //                    if (Math.Abs(thisH - LastHieght) > 1 || Math.Abs(thisH - thisH2) > 1 || Math.Abs(thisH - thisH3) > 1)
    //                    {
    //                        BlockRange(x, y, 1.001f, 1.001f, thisH);
    //                    }
    //                    LastHieght = thisH;
    //                }
    //        }
    //    }

    //    internal void BlockRange(float x, float y, float sx, float sy, float z)
    //    {
    //        SimPathStore PathStore = GetPathStore(new Vector3(x, y, z));
    //        if (PathStore == null) return;
    //        float StepSize = PathStore.StepSize;
    //        sx += x;
    //        sy += y;
    //        float loopY = sy;
    //        while (sx >= x)
    //        {
    //            while (sy >= y)
    //            {
    //                PathStore.SetBlocked(sx, sy, z, null);
    //                sy -= StepSize;
    //            }
    //            sy = loopY;
    //            sx -= StepSize;
    //        }
    //    }


    //    public Vector3 ZAngleVector(double ZAngle)
    //    {
    //        while (ZAngle < 0)
    //        {
    //            ZAngle += SimPathStore.PI2;
    //        }
    //        while (ZAngle > SimPathStore.PI2)
    //        {
    //            ZAngle -= SimPathStore.PI2;
    //        }
    //        return new Vector3((float)Math.Sin(ZAngle), (float)Math.Cos(ZAngle), 0);
    //    }

    //    public static Vector3 EnterEdge(Vector3 localLast, Vector2 dir)
    //    {
    //        if (Math.Abs(dir.X) > Math.Abs(dir.Y))
    //        {
    //            dir.Y = 0;
    //        }
    //        else  // avoid diagonals
    //        {
    //            dir.X = 0;
    //        }

    //        Vector3 exitEdge = new Vector3(localLast);
    //        if (dir.X != 0)
    //        {
    //            exitEdge.X = 256f - exitEdge.X;
    //        }
    //        if (dir.Y != 0)
    //        {
    //            exitEdge.Y = 256f - exitEdge.Y;
    //        }
    //        return exitEdge;
    //    }

 //   }
}
