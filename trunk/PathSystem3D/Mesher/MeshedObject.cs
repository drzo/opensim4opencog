using System;
using PathSystem3D.Navigation;
using OpenMetaverse;
using System.Threading;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
using System.Collections.Generic;
namespace PathSystem3D.Mesher
{
    //public interface MeshedObject
    //{
    //    string DebugString();
    //    void ForceUpdateOccupied(PathSystem3D.Navigation.SimPathStore PathStore);
    //    bool IsInside(OpenMetaverse.Vector3 L);
    //    bool IsInside(float x, float y, float z);
    //    bool IsPassable { get; }        
    //    Box3Fill OuterBox { get; set; }
    //    void RegionTaintedThis();
    //    void RemeshObject(PathSystem3D.Mesher.Box3Fill changed);
    //    void RemeshObject();
    //    void RemoveFromWaypoints(PathSystem3D.Mesher.Box3Fill changed);
    //    void SetLocated(float x, float y, float minZ, float maxZ);
    //    bool SomethingBetween(OpenMetaverse.Vector3 vector3, float low, float high);
    //    bool SomethingMaxZ(OpenMetaverse.Vector3 vector3, float low, float high, out float maxZ);
    //    string ToString();
    //    bool Update(PathSystem3D.Mesher.MeshableObject simObject);
    //    void UpdateOccupied(PathSystem3D.Navigation.SimPathStore PathStore);
    //    void UpdateOccupied();
    //}


    public interface IMeshedObject
    {
        IList<Box3Fill> InnerBoxes {get;}
        Box3Fill OuterBox {get;}
        bool IsPassable { get; }
        void RegionTaintedThis();
        void RemeshObject(Box3Fill changed);
        bool SomethingBetween(float x, float y, float low, float high);
        bool SomethingMaxZ(float x, float y, float low, float high, out float maxZ);
        void RemoveFromWaypoints(Box3Fill changed);
        void RemeshObject();
        string DebugString();
    }
    abstract public class MeshedObject: IMeshedObject
    {

        public string DebugString()
        {
            string MI = ToString() + " ";

            MI += "\n Box Info:";
            foreach (Box3Fill B in InnerBoxes)
            {
                MI += "\n    Box: " + B.ToString();
            }
            return MI;
        }

        abstract public bool Update(SimPosition simObject);
        /// <summary>
        /// Build the Boxes
        /// </summary>

        abstract public void RemeshObject(Box3Fill changed);


        abstract public bool IsRegionAttached();


        abstract public bool IsPassable
        {
            get;
            set;
        }


        public void RegionTaintedThis()
        {
            //((SimObjectImpl)RootObject).WorldSystem.ReSelectObject(RootObject.Prim);
        }

        //public bool SomethingBetween(Vector3 vector3, float low, float high)
        //{
        //    return this.SomethingBetween(vector3.X, vector3.Y, low, high);
        //}
        //public bool SomethingMaxZ(Vector3 vector3, float low, float high, out float maxZ)
        //{
        //    return SomethingMaxZ(vector3.X, vector3.Y, low, high, out maxZ);
        //}

        List<SimPathStore> SimPathStoresOccupied = new List<SimPathStore>();
        public static bool DoNotMeshPassable = true;

        public virtual void UpdateOccupied(SimPathStore pathStore)
        {
            if (pathStore == null)
            {
                Console.WriteLine(String.Format("Cant UpdateOccupied for {0}", this));// + " pos " + RootObject.DistanceVectorString(RootObject));
                return;
            }
            if (DoNotMeshPassable && IsPassable) return;
            if (!IsRegionAttached()) return;
            lock (SimPathStoresOccupied)
            {
                if (SimPathStoresOccupied.Contains(pathStore)) return;
                SimPathStoresOccupied.Add(pathStore);
            }
            try
            {
                ForceUpdateOccupied(pathStore);
            }
            catch (Exception e)
            {
                lock (SimPathStoresOccupied)
                {
                    SimPathStoresOccupied.Remove(pathStore);
                }
            };
        }

        public SimPathStore PathStore { get; set; }

        List<CollisionIndex> OccupiedWPs = new List<CollisionIndex>();

        public bool IsInside(Vector3 L)
        {
            return (this.IsInside(L.X, L.Y, L.Z));
        }

        public void RemeshObject()
        {
            Box3Fill changed = new Box3Fill(true);
            RemeshObject(changed);
            PathStore.Refresh(changed);
        }


        protected void RemoveCollisions(SimPathStore simPathStore)
        {
            Box3Fill changed = new Box3Fill(true);
            RemoveFromWaypoints(changed);
            simPathStore.Refresh(changed);            
        }

        public void RemoveFromWaypoints(Box3Fill changed)
        {
            lock (OccupiedWPs)
            {
//                SimPathStore S = GetPathStore();
                foreach (CollisionIndex P in OccupiedWPs)
                {
                    Vector3 Pos = P.GetSimPosition();
                    changed.AddPoint(Pos.X, Pos.Y, Pos.Z, 0);
                    P.RemoveObject(this);
                }
                OccupiedWPs.Clear();
            }
        }

        public void ForceUpdateOccupied(SimPathStore PathStore)
        {
            if (!IsRegionAttached()) return;
            // if (!IsSculpted)
            {
                UpdatePathOccupied(PathStore);
                return;
            }
            new Thread(new ThreadStart(delegate()
            {
                try
                {
                    UpdatePathOccupied(PathStore);
                }
                catch (Exception)
                {
                    lock (SimPathStoresOccupied)
                    {
                        SimPathStoresOccupied.Remove(PathStore);
                    }
                }
            })).Start();
        }

        public static bool UpdateMeshPaths = true;

        public static bool tryFastVersion = false;

        public void UpdatePathOccupied(SimPathStore pathStore)
        {
            if (InnerBoxes.Count == 0) return;
            if (!UpdateMeshPaths) return;
            if (!IsRegionAttached()) return;
            int t1;
            if (tryFastVersion)
            {
                //                UpdateOccupiedFast(PathStore);
                int tc = Environment.TickCount;
                UpdateOccupiedFast(pathStore);
                t1 = Environment.TickCount - tc;
                //  Console.WriteLine("t1 vs t2 = " + t1 );
                return;
            }
            int t2;
            {
                //  SetOccupied(SetLocated, float.MinValue, float.MaxValue, PathStore.StepSize);
                int tc = Environment.TickCount;
                // 10 60
                SetOccupied(SetLocatedOld, float.MinValue, float.MaxValue, pathStore.StepSize);
                t2 = Environment.TickCount - tc;
            }
            this.PathStore = pathStore;
            // Console.WriteLine("t1 vs t2 = " + t1 + " vs " + t2);
            // Mesh = null;
        }

        private void UpdateOccupiedFast(SimPathStore PathStore)
        {
            float detail = PathStore.StepSize;
            float MinX = OuterBox.MinX;
            float MaxX = OuterBox.MaxX;
            float MinY = OuterBox.MinY;
            float MaxY = OuterBox.MaxY;

            float MinZ = OuterBox.MinZ;
            float MaxZ = OuterBox.MaxZ;

            for (float x = MinX; x <= MaxX; x += detail)
            {
                for (float y = MinY; y <= MaxY; y += detail)
                {
                    if (xyInside(x, y))
                        SetLocatedOld(x, y, MinZ, MaxZ);
                }
            }
            SetLocatedOld(MaxX, MaxY, MinZ, MaxZ);
        }

        private bool xyInside(float x, float y)
        {
            foreach (Box3Fill B in InnerBoxes)
            {
                if (B.IsInsideXY(x, y)) return true;
            }
            return false;
        }


        public void SetLocatedOld(float x, float y, float minZ, float maxZ)
        {
            //SimPathStore PathStore = GetPathStore();
            CollisionIndex P = PathStore.SetObjectAt(x, y, this, minZ, maxZ);
            return;
            lock (OccupiedWPs)
            {
                if (OccupiedWPs.Contains(P)) return;
                OccupiedWPs.Add(P);
            }
        }


        static void AllTerrainMinMaxLevel(float x, float y, out double minLevel, out double maxLevel)
        {
            minLevel = double.MinValue;
            maxLevel = double.MaxValue;
        }

        public virtual void UpdateOccupied()
        {
            if (IsRegionAttached())
            {
                UpdateOccupied(PathStore);
            }
        }

        //   public static readonly ILog m_log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

        public static float PADXY = 0.2f;

        public Box3Fill OuterBox { get; set; }

        public IList<Box3Fill> InnerBoxes { get; set; }//  = new List<Box3Fill>();


        public MeshedObject(Box3Fill o, IList<Box3Fill> i, SimPathStore R)
        {
            OuterBox = o;
            InnerBoxes = i;
            PathStore = R;
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

        public bool IsInside(float x, float y, float z)
        {
            // Offset position

            if (OuterBox.IsInside(x, y, z)) // Is possible?
            {
                if (InnerBoxes.Count == 0)
                {
                    Console.WriteLine("using outerbox for " + this);
                    return true;
                }
                foreach (Box3Fill box in InnerBoxes)
                {
                    if (box.IsInside(x, y, z)) return true;
                }
            }
            return false;
        }

        internal void SetOccupied(CallbackXY p, float SimZLevel, float SimZMaxLevel, float detail)
        {
            if (InnerBoxes.Count == 0)
            {
                Console.WriteLine("using outerbox for " + this);
                OuterBox.SetOccupied(p, SimZLevel, SimZMaxLevel, detail);
                return;
            }

            {
                foreach (Box3Fill box in InnerBoxes)
                {
                    box.SetOccupied(p, SimZLevel, SimZMaxLevel, detail);
                }
            }
        }

        internal void SetOccupied(CallbackXY p, SimZMinMaxLevel MinMaxZ, float detail)
        {
            if (InnerBoxes.Count == 0)
            {
                Console.WriteLine("using outerbox for " + this);
                OuterBox.SetOccupied(p, MinMaxZ, detail);
                return;
            }
            foreach (Box3Fill box in InnerBoxes)
            {
                box.SetOccupied(p, MinMaxZ, detail);
            }
        }

        internal bool MinMaxZ(float xf, float yf, ref Vector2 V2)
        {
            bool found = false;
            foreach (Box3Fill B in InnerBoxes)
            {
                if (B.MinX > xf
                    || B.MaxX < xf
                    || B.MinY > yf
                    || B.MaxY < yf) continue;
                if (B.MinZ < V2.X)
                {
                    V2.X = B.MinZ;
                    found = true;
                }
                if (B.MaxZ > V2.Y)
                {
                    V2.Y = B.MaxZ;
                    found = true;
                }
            }
            return found;
        }

        public bool SomethingBetween(float xf, float yf, float low, float high)
        {
            if (OuterBox.MaxZ < low) return false;
            if (OuterBox.MinZ > high) return false;
            foreach (Box3Fill B in InnerBoxes)
            {
                if (B.MinX > xf
                    || B.MaxX < xf
                    || B.MinY > yf
                    || B.MaxY < yf) continue;
                if (B.IsZInside(low, high)) return true;
            }
            return false;
        }

        public bool SomethingMaxZ(float xf, float yf, float low, float high, out float maxZ)
        {
            bool found = false;
            maxZ = OuterBox.MinZ;
            foreach (Box3Fill B in InnerBoxes)
            {
                if (B.MinX > xf
                    || B.MaxX < xf
                    || B.MinY > yf
                    || B.MaxY < yf) continue;
                if (B.IsZInside(low, high))
                {
                    found = true;
                    if (B.MaxZ > maxZ) maxZ = B.MaxZ;
                }
            }
            return found;
        }
    }

}
