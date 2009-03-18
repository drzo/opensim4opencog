using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using OpenMetaverse;
using THIRDPARTY.PrimMesher;
using log4net;
using log4net.Appender;
using log4net.Core;
using log4net.Repository;
using cogbot.TheOpenSims;
using cogbot.TheOpenSims.Navigation;
//using THIRDPARTY.OpenSim.Framework;
//using THIRDPARTY.OpenSim.Region.Physics.Manager;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;

namespace cogbot.TheOpenSims.Mesher
{

    // from IdealistViewer
    public enum LevelOfDetail
    {
        Low,
        Medium,
        High
    }

    public delegate bool CollisionTest(float x, float y, float z);


    public class SimMesh
    {
        /// <summary>
        /// UseExtremeDetailSize is compared to Scale X/Y/Z added together and if greater will try to
        ///   generate more faces
        /// </summary>
        static float UseExtremeDetailSize = 3f;
        static bool UseViewerMode = false;
        public static readonly ILog m_log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);
        readonly SimObject RootObject;

        float PADXY = 0.2f;

        Box3Fill OuterBox
        {
            get { return RootObject.OuterBox; }
        }

        List<Box3Fill> InnerBoxes = new List<Box3Fill>();

        public bool IsSculpted
        {
            get { return RootObject.thePrim.Sculpt != null; }
        }
        public SimMesh(SimObject simObject)
        {
            RootObject = simObject;
            Update(simObject);
        }

        public override string ToString()
        {
            string s = "Mesh " + OuterBox.ToString();
            if (IsSculpted) s = " Sculpty" + s;
            return s + " for " + RootObject;
        }

        public bool Update(SimObject simObject)
        {
            Quaternion Rotation = simObject.GetSimRotation();
            Vector3 Scale = simObject.GetSimScale();
            List<Mesh> MeshList = new List<Mesh>();

            //PrimMesh primMesh;

            //if (false)
            //{

            //    // Add Low PrimMesh (IdealistViewer code)
            //    primMesh = PrimitiveToPrimMesh(simObject.thePrim, LevelOfDetail.Low, Scale, Rotation);
            //    AddMesh(primMesh);

            //}

            // Add High PrimMesh (IdealistViewer code)
            Mesh mesh = PrimitiveToMesh(simObject.thePrim, LevelOfDetail.High, Scale, Rotation);
            MeshList.Add(mesh);


            //if (false)
            //{
            //    // Add based on PrimitiveBaseShape (OpenSim Meshmerizer code)
            //    PrimitiveBaseShape primShape = PrimToBaseShape(simObject.thePrim);
            //    Mesh mesh = CreateMesh(simObject.ToString(), primShape,
            //        new PhysicsVector(Scale.X, Scale.Y, Scale.Z), 32f, Object.IsPhysical, QuaternionToQuat(Rotation)); // why 32?
            //    AddMesh(mesh);

            //    // i am going to see if i can make simple bounding mox meshes for unhollow cubes
            //    if (primShape.ProfileShape == ProfileShape.Square && primShape.HollowShape == HollowShape.Same)
            //    {
            //        Mesh extramesh = Meshmerizer.CreateBoundingBoxMesh(mesh);
            //        AddMesh(extramesh);
            //    }
            //}

            InnerBoxes.Clear();
            OuterBox.Reset();
            CalcBoxesFromMeshes(MeshList);
            MeshList.Clear();
            MeshList = null;
            AddPos(simObject.GetSimPosition());
            return true;
        }

        /// <summary>
        /// Build the Boxes
        /// </summary>
        void CalcBoxesFromMeshes(List<Mesh> MeshList)
        {
            foreach (Mesh M in MeshList)
            {
                foreach (Vertex v in M.vertices)
                {
                    if (v != null)
                    {
                        OuterBox.AddVertex(v, PADXY);
                    }
                }
                Triangle[] ts = M.triangles.ToArray();
                int len = ts.Length - 1;
                for (int i = 0; i < ts.Length; i++)
                {
                    Triangle t1 = ts[i];
                    bool used = false;
                    OuterBox.AddTriange(t1, PADXY);
                    for (int ii = i + 1; ii < ts.Length; ii++)
                    {
                        Triangle t2 = ts[ii];
                        int shared = SharedVertexs(t1, t2);
                        if (shared == 3) continue;
                        if (shared == 2)
                        {
                            Box3Fill B = new Box3Fill(true);
                            B.AddTriange(t1, PADXY);
                            B.AddTriange(t2, PADXY);
                            InnerBoxes.Add(B);
                            used = true;
                        }
                    }
                    if (!used)
                    {
                        Box3Fill B = new Box3Fill(true);
                        B.AddTriange(t1, PADXY);
                        InnerBoxes.Add(B);
                    }
                }
            }
            InnerBoxes = Box3Fill.Simplify(InnerBoxes);
        }

        internal string DebugString()
        {
            string MI = ToString() + " ";

            MI += "\n Box Info:";
            foreach (Box3Fill B in InnerBoxes)
            {
                MI += "\n    Box: " + B.ToString();
            }
            return MI;
        }


        public void AddPos(Vector3 offset)
        {
            OuterBox.AddPos(offset);
            foreach (Box3Fill B in InnerBoxes)
            {
                B.AddPos(offset);
            }
        }

        private int SharedVertexs(Triangle t1, Triangle t2)
        {
            int sharedV = 0;
            if (t1.v1 == t2.v1) sharedV++;
            if (t1.v1 == t2.v2) sharedV++;
            if (t1.v1 == t2.v3) sharedV++;
            if (t1.v2 == t2.v1) sharedV++;
            if (t1.v2 == t2.v2) sharedV++;
            if (t1.v2 == t2.v3) sharedV++;
            if (t1.v3 == t2.v1) sharedV++;
            if (t1.v3 == t2.v2) sharedV++;
            if (t1.v3 == t2.v3) sharedV++;
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


        /// <summary>
        /// Convert a PrimMesher.PrimMesh to OpenSim.Region.Physics.Meshing.Mesh
        /// </summary>
        /// <param name="meshIn"></param>
        /// <returns></returns>
        public static Mesh PrimMeshToMesh(PrimMesh meshIn)
        {
            Mesh mesh = new Mesh();
            mesh.primMesh = meshIn;
            {
                List<Coord> coords = meshIn.coords;
                List<Face> faces = meshIn.faces;
                int numCoords = coords.Count;
                int numFaces = faces.Count;

                for (int i = 0; i < numCoords; i++)
                {
                    Coord c = coords[i];
                    mesh.vertices.Add(new Vertex(c.X, c.Y, c.Z));
                }

                List<Vertex> vertices = mesh.vertices;
                for (int i = 0; i < numFaces; i++)
                {
                    Face f = faces[i];
                    mesh.triangles.Add(new Triangle(vertices[f.v1], vertices[f.v2], vertices[f.v3]));
                }
            }
            return mesh;
        }


        public static Mesh PrimitiveToMesh(Primitive primitive, LevelOfDetail detail, Vector3 Scale, Quaternion rot)
        {

            if (primitive.Sculpt != null)
            {
                Primitive.SculptData SD = primitive.Sculpt;
                byte[] bytes = SimRegion.GetRegion(primitive.RegionHandle).TextureBytesToUUID(SD.SculptTexture);
                SculptMesh SM = ToSculptMesh(bytes, primitive.Sculpt.Type, Scale, rot);
                if (SM != null)
                    return ToMesh(SM.coords, SM.faces, SM.viewerFaces, primitive.Type == PrimType.Sphere);
            }

            bool UseExtremeDetail = Scale.X + Scale.Y + Scale.Z > UseExtremeDetailSize;

            PrimMesh primMesh = ConstructionDataToPrimMesh(primitive.PrimData, detail, UseExtremeDetail);
            primMesh.Scale(Scale.X, Scale.Y, Scale.Z);
            primMesh.AddRot(QuaternionToQuat(rot));
            return PrimMeshToMesh(primMesh);
        }

        public static Mesh ToMesh(List<Coord> coords, List<Face> faces, List<ViewerFace> viewerFaces, bool isSphere)
        {
            Mesh mesh = new Mesh();

            int numCoords = coords.Count;
            int numFaces = faces.Count;

            for (int i = 0; i < numCoords; i++)
            {
                Coord c = coords[i];
                mesh.vertices.Add(new Vertex(c.X, c.Y, c.Z));
            }

            List<Vertex> vertices = mesh.vertices;
            for (int i = 0; i < numFaces; i++)
            {
                Face f = faces[i];
                mesh.triangles.Add(new Triangle(vertices[f.v1], vertices[f.v2], vertices[f.v3]));
            }
            if (UseViewerMode && viewerFaces != null)
            {
                int numViewerFaces = viewerFaces.Count;
                for (uint i = 0; i < numViewerFaces; i++)
                {
                    ViewerFace vf = viewerFaces[(int)i];

                    if (isSphere)
                    {
                        vf.uv1.U = (vf.uv1.U - 0.5f) * 2.0f;
                        vf.uv2.U = (vf.uv2.U - 0.5f) * 2.0f;
                        vf.uv3.U = (vf.uv3.U - 0.5f) * 2.0f;
                    }
                }
            }
            return mesh;
        }

        // partly from OpenSim.Region.Physics.Meshing
        public static SculptMesh ToSculptMesh(byte[] sculptData, OpenMetaverse.SculptType sculptTypeIn, Vector3 size, Quaternion Rotation)
        {
            SculptMesh sculptMesh;
            if (sculptData == null || sculptData.Length == 0)
                return null;

            System.Drawing.Image idata = null;

            try
            {
                OpenMetaverse.Imaging.ManagedImage managedImage;  // we never use this
                OpenMetaverse.Imaging.OpenJPEG.DecodeToImage(sculptData, out managedImage, out idata);

            }
            catch (DllNotFoundException)
            {
                System.Console.WriteLine("[PHYSICS]: OpenJpeg is not installed correctly on this system. Physics Proxy generation failed.  Often times this is because of an old version of GLIBC.  You must have version 2.4 or above!");
                return null;
            }
            catch (IndexOutOfRangeException)
            {
                System.Console.WriteLine("[PHYSICS]: OpenJpeg was unable to decode this.   Physics Proxy generation failed");
                return null;
            }
            catch (Exception)
            {
                System.Console.WriteLine("[PHYSICS]: Unable to generate a Sculpty physics proxy.  Sculpty texture decode failed!");
                return null;
            }

            SculptMesh.SculptType sculptType;

            switch (sculptTypeIn)
            {
                case OpenMetaverse.SculptType.Cylinder:
                    sculptType = SculptMesh.SculptType.cylinder;
                    break;
                case OpenMetaverse.SculptType.Plane:
                    sculptType = SculptMesh.SculptType.plane;
                    break;
                case OpenMetaverse.SculptType.Torus:
                    sculptType = SculptMesh.SculptType.torus;
                    break;
                case OpenMetaverse.SculptType.Sphere:
                default:
                    sculptType = SculptMesh.SculptType.sphere;
                    break;
            }
            if (idata == null) return null;
            sculptMesh = new SculptMesh((System.Drawing.Bitmap)idata, sculptType, (int)64, false);

            idata.Dispose();

            //    sculptMesh.DumpRaw(baseDir, primName, "primMesh");

            sculptMesh.AddRot(QuaternionToQuat(Rotation));
            sculptMesh.Scale(size.X, size.Y, size.Z);
            return sculptMesh;
        }



        /// <summary>
        /// Convert a Openmetaverse.Primitive to a PrimMesh
        /// </summary>
        /// <param name="thePrim"></param>
        /// <param name="detail"></param>
        /// <param name="pos"></param>
        /// <param name="rot"></param>
        /// <returns></returns>
        public static PrimMesh PrimitiveToPrimMesh(Primitive thePrim, LevelOfDetail detail, Vector3 Scale, Quaternion rot)
        {
            bool UseExtremeDetail = Scale.X + Scale.Y + Scale.Z > UseExtremeDetailSize;
            PrimMesh mesh = ConstructionDataToPrimMesh(thePrim.PrimData, detail, UseExtremeDetail);
            mesh.Scale(Scale.X, Scale.Y, Scale.Z);
            // if (rot != Quaternion.Identity)                
            mesh.AddRot(QuaternionToQuat(rot));
            return mesh;
        }

        public static Quat QuaternionToQuat(Quaternion quaternion)
        {
            return new Quat(quaternion.X, quaternion.Y, quaternion.Z, quaternion.W);
        }


        // from IdealistViewer.PrimMesherG.cs
        public static PrimMesh ConstructionDataToPrimMesh(Primitive.ConstructionData primData, LevelOfDetail detail, bool UseExtremeDetail)
        {

            int sides = 4;
            int hollowsides = 4;

            float profileBegin = primData.ProfileBegin;
            float profileEnd = primData.ProfileEnd;
            bool isSphere = false;

            if ((ProfileCurve)(primData.profileCurve & 0x07) == ProfileCurve.Circle)
            {
                switch (detail)
                {
                    case LevelOfDetail.Low:
                        sides = 6;
                        break;
                    case LevelOfDetail.Medium:
                        sides = 12;
                        break;
                    default:
                        sides = 24;
                        break;
                }
            }
            else if ((ProfileCurve)(primData.profileCurve & 0x07) == ProfileCurve.EqualTriangle)
                sides = 3;
            else if ((ProfileCurve)(primData.profileCurve & 0x07) == ProfileCurve.HalfCircle)
            { // half circle, prim is a sphere
                isSphere = true;
                switch (detail)
                {
                    case LevelOfDetail.Low:
                        sides = 6;
                        break;
                    case LevelOfDetail.Medium:
                        sides = 12;
                        break;
                    default:
                        sides = 24;
                        break;
                }
                profileBegin = 0.5f * profileBegin + 0.5f;
                profileEnd = 0.5f * profileEnd + 0.5f;
            }

            if ((HoleType)primData.ProfileHole == HoleType.Same)
                hollowsides = sides;
            else if ((HoleType)primData.ProfileHole == HoleType.Circle)
            {
                switch (detail)
                {
                    case LevelOfDetail.Low:
                        hollowsides = 6;
                        break;
                    case LevelOfDetail.Medium:
                        hollowsides = 12;
                        break;
                    default:
                        hollowsides = 24;
                        break;
                }

            }
            else if ((HoleType)primData.ProfileHole == HoleType.Triangle)
                hollowsides = 3;

            if (UseExtremeDetail)
            {
                sides *= 2;
                hollowsides *= 2;
            }
            PrimMesh newPrim = new PrimMesh(sides, profileBegin, profileEnd, (float)primData.ProfileHollow, hollowsides);
            newPrim.viewerMode = UseViewerMode;
            newPrim.holeSizeX = primData.PathScaleX;
            newPrim.holeSizeY = primData.PathScaleY;
            newPrim.pathCutBegin = primData.PathBegin;
            newPrim.pathCutEnd = primData.PathEnd;
            newPrim.topShearX = primData.PathShearX;
            newPrim.topShearY = primData.PathShearY;
            newPrim.radius = primData.PathRadiusOffset;
            newPrim.revolutions = primData.PathRevolutions;
            newPrim.skew = primData.PathSkew;
            switch (detail)
            {
                case LevelOfDetail.Low:
                    newPrim.stepsPerRevolution = 6;
                    break;
                case LevelOfDetail.Medium:
                    newPrim.stepsPerRevolution = 12;
                    break;
                default:
                    newPrim.stepsPerRevolution = 24;
                    break;
            }

            if (UseExtremeDetail)
            {
                newPrim.stepsPerRevolution *= 2;
            }


            if (primData.PathCurve == PathCurve.Line)
            {
                newPrim.taperX = 1.0f - primData.PathScaleX;
                newPrim.taperY = 1.0f - primData.PathScaleY;
                newPrim.twistBegin = (int)(180 * primData.PathTwistBegin);
                newPrim.twistEnd = (int)(180 * primData.PathTwist);
                newPrim.ExtrudeLinear();
            }
            else
            {
                newPrim.taperX = primData.PathTaperX;
                newPrim.taperY = primData.PathTaperY;
                newPrim.twistBegin = (int)(360 * primData.PathTwistBegin);
                newPrim.twistEnd = (int)(360 * primData.PathTwist);
                newPrim.ExtrudeCircular();
            }


            if (UseViewerMode)
            {
                int numViewerFaces = newPrim.viewerFaces.Count;
                for (uint i = 0; i < numViewerFaces; i++)
                {
                    ViewerFace vf = newPrim.viewerFaces[(int)i];

                    if (isSphere)
                    {
                        vf.uv1.U = (vf.uv1.U - 0.5f) * 2.0f;
                        vf.uv2.U = (vf.uv2.U - 0.5f) * 2.0f;
                        vf.uv3.U = (vf.uv3.U - 0.5f) * 2.0f;
                    }
                }
            }
            return newPrim;
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

    }

    public class Box3Fill : IComparable<Box3Fill>, IEquatable<Box3Fill>
    {

        #region IEquatable<Box3Fill> Members

        public bool Equals(Box3Fill other)
        {
            if (other.MaxX == MaxX &&
                other.MaxY == MaxY &&
                other.MaxZ == MaxZ &&
                other.MinX == MinX &&
                other.MinY == MinY &&
                other.MinZ == MinZ) return true;
            return false;
        }

        #endregion

        public override bool Equals(object obj)
        {
            if (obj is Box3Fill)
            {
                return Equals((Box3Fill)obj);
            }
            return false;
        }

        public void AddPos(Vector3 offset)
        {
            MinX += offset.X;
            MaxX += offset.X;
            MinY += offset.Y;
            MaxY += offset.Y;
            MinZ += offset.Z;
            MaxZ += offset.Z;
        }

        public static bool operator ==(Box3Fill o1, Box3Fill o2)
        {
            if (Object.ReferenceEquals(o1, null)) return Object.ReferenceEquals(o2, null);
            if (Object.ReferenceEquals(o2, null)) return false;
            return o1.Equals(o2);
        }

        public static bool operator !=(Box3Fill o1, Box3Fill o2)
        {
            return !o1.Equals(o2);
        }


        public float MinX;// = float.MaxValue;
        public float MaxX;// = float.MinValue;
        public float MinY;// = float.MaxValue;
        public float MaxY;// = float.MinValue;
        public float MinZ;// = float.MaxValue;
        public float MaxZ;// = float.MinValue;
        /// <summary>
        /// Construct an infinately small box
        /// </summary>
        //public Box3Fill(bool b) { Reset(); }
        /// <summary>
        ///  Make the box infinatly small
        /// </summary>
        public Box3Fill(bool b)
        {
            MinX = float.MaxValue;
            MaxX = float.MinValue;
            MinY = float.MaxValue;
            MaxY = float.MinValue;
            MinZ = float.MaxValue;
            MaxZ = float.MinValue;
        }

        public void Reset()
        {
            MinX = float.MaxValue;
            MaxX = float.MinValue;
            MinY = float.MaxValue;
            MaxY = float.MinValue;
            MinZ = float.MaxValue;
            MaxZ = float.MinValue;
        }

        //const float PADXY = 0.33f;// SimPathStore.StepSize*0.75f;
        const float PADZ = 0.20f;// SimPathStore.StepSize*0.75f;

        public override int GetHashCode()
        {
            return MinEdge.GetHashCode() ^ MaxEdge.GetHashCode();
        }

        public override string ToString()
        {
            return "(" + MinEdge + " - " + MaxEdge + ")";
        }

        internal void SetOccupied(CallbackXY p, SimZMinMaxLevel MinMaxZ, float detail)
        {
            // detail /= 2f;
            //float MinX = this.MinX + offset.X;
            //float MaxX = this.MaxX + offset.X;
            //float MinY = this.MinY + offset.Y;
            //float MaxY = this.MaxY + offset.Y;
            //float MinZ = this.MinZ + offset.Z;
            //float MaxZ = this.MaxZ + offset.Z;

            float SimZMinLevel, SimZMaxLevel;

            // = SimPathStore.StepSize;
            for (float x = MinX; x <= MaxX; x += detail)
            {
                for (float y = MinY; y <= MaxY; y += detail)
                {
                    MinMaxZ(x, y, out SimZMinLevel, out SimZMaxLevel);
                    if (SimZMinLevel > MaxZ || SimZMaxLevel < MinZ)
                    {
                        // this box is not between the Z levels
                        continue;
                    }
                    p(x, y, MinZ, MaxZ);
                }
            }
            /// the for/next loop probably missed this last point
            MinMaxZ(MaxX, MaxY, out SimZMinLevel, out SimZMaxLevel);
            if (SimZMinLevel > MaxZ || SimZMaxLevel < MinZ)
            {
                // this box is not between the Z levels
                return;
            }
            p(MaxX, MaxY, MinZ, MaxZ);
        }

        internal void SetOccupied(CallbackXY p, float SimZMinLevel, float SimZMaxLevel, float detail)
        {
            //float MinX = this.MinX + offset.X;
            //float MaxX = this.MaxX + offset.X;
            //float MinY = this.MinY + offset.Y;
            //float MaxY = this.MaxY + offset.Y;
            //float MinZ = this.MinZ + offset.Z;
            //float MaxZ = this.MaxZ + offset.Z;


            if (SimZMinLevel > MaxZ || SimZMaxLevel < MinZ)
            {
                // this box is not between the Z levels
                return;
            }

            // = SimPathStore.StepSize;
            for (float x = MinX; x <= MaxX; x += detail)
            {
                for (float y = MinY; y <= MaxY; y += detail)
                {
                    p(x, y, MinZ, MaxZ);
                }
            }
            /// the for/next loop probably missed this last point
            p(MaxX, MaxY, MinZ, MaxZ);
        }


        public string ToString(Vector3 offset)
        {
            string s = "(" + (Vector3)(MinEdge + offset) + " - " + (Vector3)(MaxEdge + offset) + " mass= " + Mass() + ")";
            return s;
        }

        /// <summary>
        /// Make sure box is big enough for this vertex
        /// </summary>
        /// <param name="v"></param>
        /// <returns>true if the box has grown</returns>
        internal bool AddVertex(Vertex v, float PADXY)
        {
            return AddPoint(v.X, v.Y, v.Z, PADXY);
        }

        internal bool AddPoint(float x, float y, float z, float PADXY)
        {
            bool changed = false;
            if (x < MinX)
            {
                MinX = x - PADXY;
                changed = true;
            }
            if (y < MinY)
            {
                MinY = y - PADXY;
                changed = true;
            }
            if (z < MinZ)
            {
                MinZ = z;// -PADZ;
                changed = true;
            }

            if (x > MaxX)
            {
                MaxX = x + PADXY;
                changed = true;
            }
            if (y > MaxY)
            {
                MaxY = y + PADXY;
                changed = true;
            }
            if (z > MaxZ)
            {
                MaxZ = z + PADZ;
                changed = true;
            }
            return changed;
        }

        /// <summary>
        /// Add Triangle (this just pushes the size of the box outward if needed)
        /// </summary>
        /// <param name="t"></param>
        /// <returns>true if the boxsize was increased</returns>
        public bool AddTriange(Triangle t, float PADXY)
        {
            return AddVertex(t.v1, PADXY) ||
             AddVertex(t.v2, PADXY) ||
             AddVertex(t.v3, PADXY);
        }

        public Vector3 MinEdge
        {
            get
            {
                return new Vector3(MinX, MinY, MinZ);
            }
        }
        public Vector3 MaxEdge
        {
            get
            {
                return new Vector3(MaxX, MaxY, MaxZ);
            }
        }

        public bool IsInside(float x, float y, float z)
        {
            if (
             (x < MinX) ||
             (y < MinY) ||
             (z < MinZ) ||
             (x > MaxX) ||
             (y > MaxY) ||
             (z > MaxZ)) return false;
            return true;
        }

        public float Mass()
        {
            return (MaxX - MinX) * (MaxY - MinY) * (MaxZ - MinZ);
        }

        public bool IsCompletelyInside(Box3Fill inner)
        {
            if ((inner.MaxX > MaxX) ||
             (inner.MinX < MinX) ||
             (inner.MaxY > MaxY) ||
             (inner.MinY < MinY) ||
             (inner.MaxZ > MaxZ) ||
             (inner.MinZ < MinZ)) return false;
            return true;
        }

        public static List<Box3Fill> Simplify(List<Box3Fill> simpl)
        {
            simpl.Sort(Bigger);
            List<Box3Fill> retval = new List<Box3Fill>();
            int len = simpl.Count;
            int len1 = len - 1;
            for (int i = 0; i < len; i++)
            {
                Box3Fill bi = simpl[i];
                bool foundInside = false;
                for (int ii = len1; ii > i; ii--)
                {
                    if (simpl[ii].IsCompletelyInside(bi))
                    {
                        foundInside = true;
                        break;
                    }
                }
                if (!foundInside)
                {
                    retval.Add(bi);
                }
            }
            return retval;
        }

        #region IComparable<Box3Fill> Members

        public int CompareTo(Box3Fill other)
        {
            return Bigger(this, other);
        }

        #endregion

        static int Bigger(Box3Fill b1, Box3Fill b2)
        {
            if (b1 == b2) return 0;

            if (b1.MinX > b2.MinX)
            {
                return -1;
            }
            if (b1.MinY > b2.MinY)
            {
                return -1;
            }
            if (b1.MinZ > b2.MinZ)
            {
                return -1;
            }

            if (b1.MaxX < b2.MaxX)
            {
                return -1;
            }
            if (b1.MaxY < b2.MaxY)
            {
                return -1;
            }
            if (b1.MaxZ < b2.MaxZ)
            {
                return -1;
            }

            float f1 = b1.Mass();
            float f2 = b2.Mass();
            if (f1 == f2)
            {
                return 1;
            }
            return f1 < f2 ? -1 : 1;
        }

    }
}
