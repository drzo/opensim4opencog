//#define COLLIDER_TRIANGLE
using System;
using System.Drawing;
using PathSystem3D.Navigation;
using OpenMetaverse;
using System.Threading;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
using System.Collections.Generic;
using THIRDPARTY.OpenSim.Region.Physics.Manager;
#if COLLIDER_ODE
using THIRDPARTY.OpenSim.Region.Physics.OdePlugin;
#endif

namespace PathSystem3D.Mesher
{
    //public interface MeshedObject
    //{
    //    string DebugString();
    //    void ForceUpdatePathOccupied(PathSystem3D.Navigation.SimPathStore PathStore);
    //    bool IsInside(OpenMetaverse.Vector3 L);
    //    bool IsInside(float x, float y, float z);
    //    bool IsPhantom { get; }        
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
    //    void UpdatePathOccupied(PathSystem3D.Navigation.SimPathStore PathStore);
    //    void UpdatePathOccupied();
    //}

    public interface MeshableObject : SimPosition
    {
        bool IsSolid { get; set; }
    }

    public interface IMeshedObject : CollisionObject
    {
        IList<Box3Fill> InnerBoxes { get; }
        Box3Fill OuterBox {get;}
      //  bool IsSolid { get; }
        void RegionTaintedThis();
        void RemeshObject(Box3Fill changed);
        bool SomethingBetween(float x, float y, float low, float high);
        bool SomethingMaxZ(float x, float y, float low, float high, out float maxZ);
        void RemoveFromWaypoints(Box3Fill changed);
        bool xyMaxZ(float x, float y, float z, out float zout);
        void RemeshObject();
        string DebugString();
        Color DebugColor();
        //Mesh GetTriMesh();

        bool UpdateOccupied(SimPathStore simPathStore);
    }
    public interface CollisionObject
    {
        bool IsInsideXY(float xf, float yf);

        bool IsZInside(float low, float high);
        float MaxZ { get; }
        float MinZ { get; }
        bool IsSolid { get; }

        bool IsInside(float x, float y, float z);

        void SetOccupied(CallbackXY p, float SimZLevel, float SimZMaxLevel, float detail);

        void SetOccupied(CallbackXY p, SimZMinMaxLevel MinMaxZ, float detail);

        void AddPos(Vector3 offset);

        //bool SomethingMaxZ(float x, float y, float low, float high, out float maxZ);
    }

    abstract public class MeshedObject : IMeshedObject, CollisionObject
    {

        public virtual Color DebugColor()
        {
            return Color.Empty;
        }

        public string DebugString()
        {
            string MI = ToString() + " ";

            MI += "\n Box Info:";
            if (InnerBoxes!=null) foreach (CollisionObject B in InnerBoxes)
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


        abstract public bool IsSolid
        {
            get; set;
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

        protected SimPathStore PathStore { get; set; }// = new List<SimPathStore>();
        public static bool MeshOnlySolids = true;

        public virtual bool UpdateOccupied(SimPathStore pathStore)
        {
            if (pathStore == null)
            {
                Console.WriteLine(String.Format("Cant UpdatePathOccupied for {0}", this));// + " pos " + RootObject.DistanceVectorString(RootObject));
                return false;
            }
            if (!UpdateMeshPaths) return false;
            if (MeshOnlySolids && !IsSolid) return false;
            if (InnerBoxes == null) return false;
            if (InnerBoxes.Count == 0) return false;
            if (!IsRegionAttached()) return false;
          //  int t1;
            try
            {
                //lock (PathStore)
                {
                    if (PathStore == pathStore) return false;
                    PathStore = pathStore;//.Add(pathStore);
                    if (tryFastVersion)
                    {
                        //                UpdatePathOccupiedFast(PathStore);
                       // int tc = Environment.TickCount;
                        UpdatePathOccupiedVeryFast(pathStore);
                       // t1 = Environment.TickCount - tc;
                        InnerBoxes = null;
                        PathStore.AddedCount++;
                        //  Console.WriteLine("t1 vs t2 = " + t1 );
                        return true;
                    }
                }
                ForceUpdatePathOccupied(pathStore);
                return true;
            }
            catch (Exception e)
            {
              //  lock (PathStore)
                {
                    PathStore = null;//.Remove(pathStore);
                }
                return false;
            };
        }

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
            if (simPathStore != PathStore) return;
            if (PathStore == null) IsSolid = false;
            else
            {
                Box3Fill changed = new Box3Fill(true);
                RemoveFromWaypoints(changed);
                simPathStore.Refresh(changed);            
            }
        }


        public void RemoveFromWaypoints(Box3Fill changed)
        {
            float detail = PathStore.StepSize;// -0.001f;
            float MinX = OuterBox.MinX;
            float MaxX = OuterBox.MaxX;
            float MinY = OuterBox.MinY;
            float MaxY = OuterBox.MaxY;

            float MinZ = OuterBox.MinZ;
            float MaxZ = OuterBox.MaxZ;

            changed.AddPoint(MinX, MinY, MinZ, Vector3.Zero);
            changed.AddPoint(MaxX, MaxY, MaxZ, Vector3.Zero);

            for (float x = MinX; x <= MaxX; x += detail)
            {
                for (float y = MinY; y <= MaxY; y += detail)
                {
                    RemoveFromWaypoint(x, y, MinZ, MaxZ);
                }
            }
            RemoveFromWaypoint(MaxX, MaxY, MinZ, MaxZ);
        }

        private void RemoveFromWaypoint(float MaxX, float MaxY, float MinZ, float MaxZ)
        {
            CollisionIndex I = PathStore.CreateFirstNode(MaxX, MaxY);
            I.RemoveObject(this);
        }

        public void ForceUpdatePathOccupied(SimPathStore pathStore)
        {
            if (!IsRegionAttached()) return;
            // if (!IsSculpted)
            {
                UpdateOccupied(pathStore);
                PathStore.AddedCount++;
                return;
            }
            new Thread(new ThreadStart(delegate()
            {
                try
                {
                    UpdateOccupied(pathStore);
                }
                catch (Exception)
                {
                    //lock (PathStore)
                    {
                        PathStore=null;//.Remove(PathStore);
                    }
                }
            })).Start();
        }

        public static bool UpdateMeshPaths = true;

        public static bool tryFastVersion = true;



        private void UpdatePathOccupiedVeryFast(SimPathStore pathStore)
        {
            float detail = pathStore.StepSize; // -0.001f;
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
                    PathStore.SetObjectAt(x, y, this, MinZ, MaxZ); //return true;
                }
            }
            PathStore.SetObjectAt(MaxX, MaxY, this, MinZ, MaxZ);
        }
        private void UpdatePathOccupiedFast(SimPathStore pathStore)
        {
            float detail = pathStore.StepSize; // -0.001f;
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
                    foreach (CollisionObject B in InnerBoxes)
                    {
                        if (B.IsInsideXY(x, y))
                        {
                            PathStore.SetObjectAt(x, y, this, MinZ, MaxZ); //return true;
                            break;

                        }
                    }
                }
            }
            PathStore.SetObjectAt(MaxX, MaxY, this, MinZ, MaxZ);
        }


        static void AllTerrainMinMaxLevel(float x, float y, out double minLevel, out double maxLevel)
        {
            minLevel = double.MinValue;
            maxLevel = double.MaxValue;
        }


        //   public static readonly ILog m_log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);

        readonly public static Vector3 PadXYZ = new Vector3(0.2f,0.2f,0.2f);

        public Box3Fill OuterBox { get; set; }

        public IList<Box3Fill> InnerBoxes { get; set; }//  = new List<Box3Fill>();


        public MeshedObject(Box3Fill o, IList<Box3Fill> i, SimPathStore R)
        {
            OuterBox = o;
            InnerBoxes = i;
            PathStore = null;// R;
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
                foreach (CollisionObject box in InnerBoxes)
                {
                    if (box.IsInside(x, y, z)) return true;
                }
            }
            return false;
        }

        public void SetOccupied(CallbackXY p, float SimZLevel, float SimZMaxLevel, float detail)
        {
            if (InnerBoxes.Count == 0)
            {
                Console.WriteLine("using outerbox for " + this);
                OuterBox.SetOccupied(p, SimZLevel, SimZMaxLevel, detail);
                return;
            }

            {
                foreach (CollisionObject box in InnerBoxes)
                {
                    box.SetOccupied(p, SimZLevel, SimZMaxLevel, detail);
                }
            }
        }

        public void SetOccupied(CallbackXY p, SimZMinMaxLevel MinMaxZ, float detail)
        {
            if (InnerBoxes.Count == 0)
            {
                Console.WriteLine("using outerbox for " + this);
                OuterBox.SetOccupied(p, MinMaxZ, detail);
                return;
            }
            foreach (CollisionObject box in InnerBoxes)
            {
                box.SetOccupied(p, MinMaxZ, detail);
            }
        }

        public bool SomethingBetween(float xf, float yf, float low, float high)
        {
            if (OuterBox.MaxZ < low) return false;
            if (OuterBox.MinZ > high) return false;
            foreach (CollisionObject B in InnerBoxes)
            {
                if (!B.IsInsideXY(xf,yf)) continue;
                if (B.IsZInside(low, high)) return true;
            }
            return false;
        }

        public bool SomethingMaxZ(float xf, float yf, float low, float high, out float maxZ)
        {
#if COLLIDER_TRIANGLE
                        return xyMaxZ(xf, yf, high, out maxZ);
#endif
            bool found = false;
            maxZ = OuterBox.MinZ;
            foreach (CollisionObject B in InnerBoxes)
            {
                if (!B.IsInsideXY(xf, yf)) continue;
                if (B.IsZInside(low, high))
                {
                    found = true;
                    if (B.MaxZ > maxZ) maxZ = B.MaxZ;
                }
            }
            return found;

        }

        public virtual bool xyMaxZ(float x, float y, float z, out float zout)
        {            
            //start ray at our feet
            Vector3 rayStart = new Vector3(
                x,
                y,
                z
                );

            //end ray at 0.1m above our feet
            Vector3 rayEnd = new Vector3(
                x,
                y,
                z - 2f
                );

            zout = z;
            Vector3 collision = ObjectCollisionTest(rayStart, rayEnd);

            if (collision != rayEnd) //we collided!
            {
                //check if we are any higher than before
                zout = collision.Z;
                return true;
                //if (height > lowerLimit) lowerLimit = height;
            }
            return false;
        }
        public Vector3 ObjectCollisionTest(Vector3 rayStart, Vector3 rayEnd)
        {
            Vector3 closestPoint = rayEnd;

            if (rayStart == rayEnd)
            {
                //Logger.Debug("RayStart is equal to RayEnd, returning given location");
                return closestPoint;
            }

            Vector3 direction = Vector3.Normalize(rayEnd - rayStart);

            // Get the mesh that has been transformed into world-space
            {
                // Iterate through all of the triangles in the mesh, doing a ray-triangle intersection
                IEnumerable<Triangle> triangles = null;
      
#if COLLIDER_TRIANGLE
  triangles = this.InnerBoxes;
#endif
                float closestDistance = Single.MaxValue;
                foreach (Triangle tri in triangles)
                {
                    Vector3 point0 = tri.v1.ToVector3(); //mesh.Vertices[mesh.Indices[i + 0]].Position;
                    Vector3 point1 = tri.v2.ToVector3();// mesh.Vertices[mesh.Indices[i + 1]].Position;
                    Vector3 point2 = tri.v3.ToVector3();//mesh.Vertices[mesh.Indices[i + 2]].Position;

                    Vector3 collisionPoint;
                    if (RayTriangleIntersection(rayStart, direction, point0, point1, point2, out collisionPoint))
                    {
                        if ((collisionPoint - rayStart).Length() < closestDistance)
                            closestPoint = collisionPoint;
                    }
                }
            }

            return closestPoint;
        }


    
        /// <summary>
        /// Adapted from http://www.cs.virginia.edu/~gfx/Courses/2003/ImageSynthesis/papers/Acceleration/Fast%20MinimumStorage%20RayTriangle%20Intersection.pdf
        /// </summary>
        /// <param name="origin">Origin point of the ray</param>
        /// <param name="direction">Unit vector representing the direction of the ray</param>
        /// <param name="vert0">Position of the first triangle corner</param>
        /// <param name="vert1">Position of the second triangle corner</param>
        /// <param name="vert2">Position of the third triangle corner</param>
        /// <param name="collisionPoint">The collision point in the triangle</param>
        /// <returns>True if the ray passes through the triangle, otherwise false</returns>
        static bool RayTriangleIntersection(Vector3 origin, Vector3 direction, Vector3 vert0, Vector3 vert1, Vector3 vert2, out Vector3 collisionPoint)
        {
            const float EPSILON = 0.00001f;

            Vector3 edge1, edge2, pvec;
            float determinant, invDeterminant;

            // Find vectors for two edges sharing vert0
            edge1 = vert1 - vert0;
            edge2 = vert2 - vert0;

            // Begin calculating the determinant
            pvec = Vector3.Cross(direction, edge2);

            // If the determinant is near zero, ray lies in plane of triangle
            determinant = Vector3.Dot(edge1, pvec);

            if (determinant > -EPSILON && determinant < EPSILON)
            {
                collisionPoint = Vector3.Zero;
                return false;
            }

            invDeterminant = 1f / determinant;

            // Calculate distance from vert0 to ray origin
            Vector3 tvec = origin - vert0;

            // Calculate U parameter and test bounds
            float u = Vector3.Dot(tvec, pvec) * invDeterminant;
            if (u < 0.0f || u > 1.0f)
            {
                collisionPoint = Vector3.Zero;
                return false;
            }

            // Prepare to test V parameter
            Vector3 qvec = Vector3.Cross(tvec, edge1);

            // Calculate V parameter and test bounds
            float v = Vector3.Dot(direction, qvec) * invDeterminant;
            if (v < 0.0f || u + v > 1.0f)
            {
                collisionPoint = Vector3.Zero;
                return false;
            }

            //t = Vector3.Dot(edge2, qvec) * invDeterminant;

            collisionPoint = new Vector3(
                vert0.X + u * (vert1.X - vert0.X) + v * (vert2.X - vert0.X),
                vert0.Y + u * (vert1.Y - vert0.Y) + v * (vert2.Y - vert0.Y),
                vert0.Z + u * (vert1.Z - vert0.Z) + v * (vert2.Z - vert0.Z));

            return true;
        }


        /// <summary>
        /// Adapted from http://www.cs.virginia.edu/~gfx/Courses/2003/ImageSynthesis/papers/Acceleration/Fast%20MinimumStorage%20RayTriangle%20Intersection.pdf
        /// </summary>
        /// <param name="origin">Origin point of the ray</param>
        /// <param name="direction">Unit vector representing the direction of the ray</param>
        /// <param name="vert0">Position of the first triangle corner</param>
        /// <param name="vert1">Position of the second triangle corner</param>
        /// <param name="vert2">Position of the third triangle corner</param>
        /// <param name="collisionPoint">The collision point in the triangle</param>
        /// <returns>True if the ray passes through the triangle, otherwise false</returns>
        static bool RayTriangleIntersection(Vertex origin, Vertex direction, Vertex vert0, Vertex vert1, Vertex vert2, out Vertex collisionPoint)
        {
            const float EPSILON = 0.00001f;

            Vertex edge1, edge2, pvec;
            float determinant, invDeterminant;

            // Find vectors for two edges sharing vert0
            edge1 = vert1 - vert0;
            edge2 = vert2 - vert0;

            // Begin calculating the determinant
            pvec = direction.cross(edge2);

            // If the determinant is near zero, ray lies in plane of triangle
            determinant = edge1.dot(pvec);

            if (determinant > -EPSILON && determinant < EPSILON)
            {
                collisionPoint = (Vertex)Vertex.Zero;
                return false;
            }

            invDeterminant = 1f / determinant;

            // Calculate distance from vert0 to ray origin
            Vertex tvec = origin - vert0;

            // Calculate U parameter and test bounds
            float u = tvec.dot(pvec) * invDeterminant;
            if (u < 0.0f || u > 1.0f)
            {
                collisionPoint = (Vertex)Vertex.Zero;
                return false;
            }

            // Prepare to test V parameter
            Vertex qvec = tvec.cross(edge1);

            // Calculate V parameter and test bounds
            float v = direction.dot(qvec) * invDeterminant;
            if (v < 0.0f || u + v > 1.0f)
            {
                collisionPoint = (Vertex)Vertex.Zero;
                return false;
            }

            //t = Vertex.Dot(edge2, qvec) * invDeterminant;

            collisionPoint = new Vertex(
                vert0.X + u * (vert1.X - vert0.X) + v * (vert2.X - vert0.X),
                vert0.Y + u * (vert1.Y - vert0.Y) + v * (vert2.Y - vert0.Y),
                vert0.Z + u * (vert1.Z - vert0.Z) + v * (vert2.Z - vert0.Z));

            return true;
        }

        #region CollisionObject Members

        public bool IsInsideXY(float xf, float yf)
        {
            foreach (var o in InnerBoxes)
            {
                if (o.IsInsideXY(xf, yf)) return true;
            }
            return false;
        }

        public bool IsZInside(float low, float high)
        {
            foreach (var o in InnerBoxes)
            {
                if (o.IsZInside(low, high)) return true;
            }
            return false;
        }

        public float MaxZ
        {
            get { return OuterBox.MaxZ; }
        }

        public float MinZ
        {
            get { return OuterBox.MinZ; }
        }


        public abstract void AddPos(Vector3 offset);

        #endregion
    }

}
