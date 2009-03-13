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
        static float UseExtremeDetailSize = 5f;
        static bool UseViewerMode = false;
        public static readonly ILog m_log = LogManager.GetLogger(MethodBase.GetCurrentMethod().DeclaringType);
     //   CollisionTest collider;
        List<Mesh> MeshList = new List<Mesh>();
        List<Triangle> TriangleList = new List<Triangle>();
        Vector3 Scale;
        //SimObject Object;
       // Vector3 Position;
        bool IsSolid = false;
        OpenMetaverse.Quaternion Rotation;
        string name;
        
        internal readonly Box3Fill OuterBox = new Box3Fill();
        List<Box3Fill> InnerBoxes = new List<Box3Fill>();

        public readonly bool IsSculpted;
        public SimMesh(SimObject simObject)
        {
           // collider = new CollisionTest(IsInside);
            name = simObject.thePrim.ID.ToString();
            IsSculpted = simObject.thePrim.Sculpt != null;
            Update(simObject);
        }

        public bool Update(SimObject simObject)
        {
            Rotation = simObject.GetSimRotation();
            //Position = simObject.Parent.GetSimPosition();
            Scale = simObject.GetSimScale();
            MeshList.Clear();

            if (simObject.thePrim.PrimData.ProfileHollow == 0.0f)
            {
                IsSolid = true;
            }

            //PrimMesh primMesh;

            //if (false)
            //{

            //    // Add Low PrimMesh (IdealistViewer code)
            //    primMesh = PrimitiveToPrimMesh(simObject.thePrim, LevelOfDetail.Low, Scale, Rotation);
            //    AddMesh(primMesh);

            //}

            // Add High PrimMesh (IdealistViewer code)
            Mesh mesh = PrimitiveToMesh(simObject.thePrim, LevelOfDetail.High, Scale, Rotation);
            AddMesh(mesh);


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

            CalcBoxesFromMeshes(simObject.GetSimPosition());
            return true;
        }

        private void AddMesh(PrimMesh primMesh)
        {
            AddMesh(PrimMeshToMesh(primMesh));
        }
        private void AddMesh(Mesh mesh)
        {
            MeshList.Add(mesh);
        }

        /// <summary>
        /// Build the Boxes
        /// </summary>
        void CalcBoxesFromMeshes(Vector3 pos)
        {
            InnerBoxes.Clear();           
            OuterBox.Reset();

            foreach (Mesh M in MeshList)
            {
                foreach (Vertex v in M.vertices)
                {
                    if (v != null)
                    {
                        OuterBox.AddVertex(v);
                    }
                }
                foreach (Triangle t in M.triangles)
                {
                    if (AddBox(t))
                    {
                        throw new IndexOutOfRangeException("Triangle outside of OuterBox");
                    }
                }
            }
            //int InnerBoxesCountBefore = InnerBoxes.Count;
            ////InnerBoxes = Box3Fill.Simplify(InnerBoxes);
            //if (InnerBoxesCountBefore != InnerBoxes.Count)
            //{
            //    Console.WriteLine("InnerBoxesCountBefore diff " + InnerBoxesCountBefore + "->" + InnerBoxes.Count);
            //}
            AddPos(pos);
        }

        internal string GetMeshInfo(Vector3 Position)
        {
            string MI = " ";
            int index = 1;
            foreach (Mesh M in MeshList)
            {
                if (M.primMesh != null)
                {
                    //String name = "primMesh" + Object.thePrim.LocalID + "_" + index++;
                    MI += "\n Mesh: "+name+" \n" + M.primMesh.ParamsToDisplayString();
                    M.primMesh.DumpRaw(".",name,"primMesher"+index);
                }
            }

            MI += "\n Outer: " + OuterBox.ToString(Position);
            MI += "\n Box Info:";
            foreach (Box3Fill B in InnerBoxes)
            {
                MI += "\n    Box: " + B.ToString(Position);
            }
            //foreach (Vector3 B in GetOccupiedList()){
            //    MI += "\n    Point: " + B;
            //}
            
            return MI;
        }


        public void AddPos(Vector3 offset) {
            OuterBox.AddPos(offset);
            foreach (Box3Fill B in InnerBoxes)
            {
                B.AddPos(offset);
            }
        }

        public bool AddBox(Triangle t)
        {
            if (t == null) return false;
            foreach (Triangle T in TriangleList)
            {
                // aready known?
                if (SharedVertexs(t, T) > 2) return false;
            }
            Box3Fill box = new Box3Fill();
            if (IsSolid)
            {
                foreach (Triangle T in TriangleList)
                {
                    int sharedVs = SharedVertexs(t, T);
                    if (sharedVs == 2)
                    {
                        box.AddTriange(T);
                        //TriangleList.Remove(T);
                        //break;
                    }
                }
            }
            box.AddTriange(t);
            if (!InnerBoxes.Contains(box))
            InnerBoxes.Add(box);    
            TriangleList.Add(t);
            return OuterBox.AddTriange(t);
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

        public bool IsInside(float x,float y,float z) {
            // Offset position

            if (OuterBox.IsInside(x, y, z)) // Is possible?
            {
                foreach (Box3Fill box in InnerBoxes)
                {
                    if (box.IsInside(x, y, z)) return true;
                }
            }
            return false;
        }

        internal void SetOccupied(CallbackXY p, float SimZLevel, float SimZMaxLevel, float detail)
        {
            //Vector3 loc = GetSimPosition();
            // TODO do we need High?
            // IrrlichtNETCP.Mesh mesh = IdealistViewer.PrimMesherG.PrimitiveToIrrMesh(thePrim, IdealistViewer.LevelOfDetail.High);
            //ProfileCurve pc = (ProfileCurve)Object.thePrim.PrimData.profileCurve;
            //PrimType pt = Object.thePrim.PrimData.Type;

            //if (pt == PrimType.Box)
            //{
            //    OuterBox.SetOccupied(p, SimZLevel, SimZMaxLevel,Position);
            //}
            //else
            {
                foreach (Box3Fill box in InnerBoxes)
                {
                    box.SetOccupied(p, SimZLevel, SimZMaxLevel,detail);
                }
            }
        }

        internal void SetOccupied(CallbackXY p, SimZMinMaxLevel MinMaxZ, float detail)
        {
            foreach (Box3Fill box in InnerBoxes)
            {
                box.SetOccupied(p, MinMaxZ, detail);
            }
        }

        //public List<Vector3>[] ZSlices = new List<Vector3>[10];
        ///// <summary>
        ///// </summary>
        ///// <param name="ZSlice"> right now ussually 22 or 23</param>
        ///// <returns>ICollection&lt;Vector3&gt; probly could be Vector2s but takes more time to wrap them</returns>
        //public virtual ICollection<Vector3> GetOccupied(float ZSlice)
        //{
        //    //  float SimZRange = (SimZHieght / 2)*1.5;
        //    int ZSliceIndex = (int)Math.Round((double)(ZSlice - 18) / SimPathStore.SimZHieght);
        //    float SimZLow = ZSlice;
        //    float SimZHigh = SimZLow + SimPathStore.SimZHieght;

        //    if (ZSliceIndex < 0)
        //    {
        //        ZSliceIndex = 0;
        //    }
        //    else if (ZSliceIndex > 9)
        //    {
        //        ZSliceIndex = 9;
        //    }
        //    lock (ZSlices)
        //    {
        //       // if (ZSlices[ZSliceIndex] == null)
        //        {
        //            List<Vector3> occpuied = new List<Vector3>();
        //            ZSlices[ZSliceIndex] = occpuied;
        //            foreach (Vector3 point in GetOccupiedList())
        //            {
        //                //if (SimZLow <= point.Z && SimZHigh >= point.Z)
        //                {
        //                    occpuied.Add(point);
        //                }
        //               // else
        //                {
        //                 //   occpuied.Add(point);
        //                }
        //            }
        //        }
        //        return ZSlices[ZSliceIndex];
        //    }
        //}


        ////////////////////////////////////////
        // Static Helpers
        ////////////////////////////////////////


        /// <summary>
        /// Make a List of Vector3 inside the bounding box
        /// </summary>
        /// <param name="Min"></param>
        /// <param name="Max"></param>
        /// <param name="detailLevel"></param>
        /// <param name="PointsOccupied"></param>
        /// <param name="PointsUnOccupied"></param>
        /// <param name="test"></param>
        public static void MakePointsList(Vector3 Min, Vector3 Max, int detailLevel, out ICollection<Vector3> PointsOccupied, out ICollection<Vector3> PointsUnOccupied, CollisionTest test)
        {
            PointsOccupied = new List<Vector3>();
            PointsUnOccupied = new List<Vector3>();
            float StepLevel = (float)(1f / (float)detailLevel);
            float MinX = (float)(Math.Round((double)Min.X * detailLevel) / detailLevel);
            float MinY = (float)(Math.Round((double)Min.Y * detailLevel) / detailLevel);
            float MinZ = (float)(Math.Round((double)Min.Z * detailLevel) / detailLevel);
            float MaxX = (float)(Math.Round((double)Max.X * detailLevel) / detailLevel);
            float MaxY = (float)(Math.Round((double)Max.Y * detailLevel) / detailLevel);
            float MaxZ = (float)(Math.Round((double)Max.Z * detailLevel) / detailLevel);
            if (MaxX < MinX || MaxZ < MinZ || MaxY < MinY)
            {
                throw new ArgumentException("is box3d.MinEdge and box3d.MaxEdge in the TopLeft-to-BottemRight Order? " + Min + " > " + Max);
            }
            for (float x = MinX; x <= MaxX; x += StepLevel)
            {
                for (float y = MinY; y <= MaxY; y += StepLevel)
                {
                    for (float z = MinZ; z <= MaxZ; z += StepLevel)
                    {
                        Vector3 v3 = new Vector3(x, y, z);
                        if (test(x, y, z))
                        {
                                PointsOccupied.Add(v3);
                        }
                        else
                        {
                                PointsUnOccupied.Add(v3);
                        }
                    }
                }
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
                SculptMesh SM = ToSculptMesh(bytes,primitive.Sculpt.Type,Scale,rot);
                if (SM!=null)                
                    return ToMesh(SM.coords, SM.faces, SM.viewerFaces,primitive.Type==PrimType.Sphere);
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
            if (sculptData==null || sculptData.Length == 0)
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



        //static public Mesh CreateMesh(String primName, PrimitiveBaseShape primShape, PhysicsVector size, float lod, bool isPhysical, Quat rot)
        //{
        //    Mesh mesh = (Mesh)Meshmerizer.CreateMesh(primName, primShape, size, lod, isPhysical, rot);
        //    return mesh;
        //}

       
        ///// <summary>
        ///// [05:31] <AFrisby> dmiles_afk, search my blog, I wrote a function for converting OpenMetaverse.Primitive to OpenSimulator.SceneObjectGroup
        ///// 
        ///// from: http://www.adamfrisby.com/blog/2008/10/code-snippet-converting-openmetaverseprimitive-to-opensimulatorsceneobjectpart/
        ///// </summary>
        ///// <param name="orig"></param>
        ///// <returns></returns>
        //static public PrimitiveBaseShape PrimToBaseShape(Primitive orig) {
        //    //bool root = orig.ParentID == 0;

        //    //SceneObjectPart sop = new SceneObjectPart();
        //    //sop.LastOwnerID = orig.OwnerID;
        //    //sop.OwnerID = orig.OwnerID;
        //    //sop.GroupID = orig.GroupID;

        //    //sop.CreatorID = orig.Properties.CreatorID;

        //    //sop.OwnershipCost = orig.Properties.OwnershipCost;
        //    //sop.ObjectSaleType = (byte)orig.Properties.SaleType;
        //    //sop.SalePrice = orig.Properties.SalePrice;
        //    //sop.CreationDate = (int)Utils.DateTimeToUnixTime(orig.Properties.CreationDate);

        //    //// Special   
        //    //sop.ParentID = 0;

        //    //sop.OwnerMask = (uint)orig.Properties.Permissions.OwnerMask;
        //    //sop.NextOwnerMask = (uint)orig.Properties.Permissions.NextOwnerMask;
        //    //sop.GroupMask = (uint)orig.Properties.Permissions.GroupMask;
        //    //sop.EveryoneMask = (uint)orig.Properties.Permissions.EveryoneMask;
        //    //sop.BaseMask = (uint)orig.Properties.Permissions.BaseMask;

        //    //sop.ParticleSystem = orig.ParticleSys.GetBytes();

        //    //// OS only   
        //    //sop.TimeStampFull = 0;
        //    //sop.TimeStampLastActivity = 0;
        //    //sop.TimeStampTerse = 0;

        //    //// Not sure nessecary   
        //    //sop.UpdateFlag = 2;

        //    //sop.InventorySerial = 0;
        //    //sop.UUID = orig.ID;
        //    //sop.LocalId = orig.LocalID;
        //    //sop.Name = orig.Properties.Name;
        //    //sop.Flags = orig.Flags;
        //    //sop.Material = 0;
        //    //sop.RegionHandle = orig.RegionHandle;

        //    //sop.GroupPosition = orig.Position;

        //    //if (!root)
        //    //    sop.OffsetPosition = orig.Position;
        //    //else
        //    //    sop.OffsetPosition = Vector3.Zero;

        //    //sop.RotationOffset = orig.Rotation;
        //    //sop.Velocity = orig.Velocity;
        //    //sop.RotationalVelocity = Vector3.Zero;
        //    //sop.AngularVelocity = Vector3.Zero;
        //    //sop.Acceleration = Vector3.Zero;

        //    //sop.Description = orig.Properties.Description;
        //    //sop.Color = Color.White;
        //    //sop.Text = orig.Text;
        //    //sop.SitName = orig.Properties.SitName;
        //    //sop.TouchName = orig.Properties.TouchName;
        //    //sop.ClickAction = (byte)orig.ClickAction;

        //    //sop.PayPrice = new int[1];

        //    PrimitiveBaseShape sopShape = new PrimitiveBaseShape(true);
        //    //sopShape.Scale = orig.Scale;
        //    sopShape.FlexiEntry = false;
        //    if (orig.Flexible != null)
        //    {
        //        sopShape.FlexiDrag = orig.Flexible.Drag;
        //        sopShape.FlexiEntry = false;
        //        sopShape.FlexiForceX = orig.Flexible.Force.X;
        //        sopShape.FlexiForceY = orig.Flexible.Force.Y;
        //        sopShape.FlexiForceZ = orig.Flexible.Force.Z;
        //        sopShape.FlexiGravity = orig.Flexible.Gravity;
        //        sopShape.FlexiSoftness = orig.Flexible.Softness;
        //        sopShape.FlexiTension = orig.Flexible.Tension;
        //        sopShape.FlexiWind = orig.Flexible.Wind;
        //    }

        //    Primitive.ConstructionData origPrimData = orig.PrimData;

        //    switch (origPrimData.ProfileHole)
        //    {
        //        case HoleType.Circle:
        //            sopShape.HollowShape = HollowShape.Circle;
        //            break;
        //        case HoleType.Square:
        //            sopShape.HollowShape = HollowShape.Square;
        //            break;
        //        case HoleType.Triangle:
        //            sopShape.HollowShape = HollowShape.Triangle;
        //            break;
        //        default:
        //        case HoleType.Same:
        //            sopShape.HollowShape = HollowShape.Same;
        //            break;
        //    }

        //    sopShape.LightEntry = false;
        //    if (orig.Light != null)
        //    {
        //        sopShape.LightColorA = orig.Light.Color.A;
        //        sopShape.LightColorB = orig.Light.Color.B;
        //        sopShape.LightColorG = orig.Light.Color.G;
        //        sopShape.LightColorR = orig.Light.Color.R;
        //        sopShape.LightCutoff = orig.Light.Cutoff;
        //        sopShape.LightEntry = false;
        //        sopShape.LightFalloff = orig.Light.Falloff;
        //        sopShape.LightIntensity = orig.Light.Intensity;
        //        sopShape.LightRadius = orig.Light.Radius;
        //    }

        //    sopShape.PathBegin = Primitive.PackBeginCut(origPrimData.PathBegin);
        //    sopShape.PathCurve = (byte)origPrimData.PathCurve;
        //    sopShape.PathEnd = Primitive.PackEndCut(origPrimData.PathEnd);
        //    sopShape.PathRadiusOffset = Primitive.PackPathTwist(origPrimData.PathRadiusOffset);
        //    sopShape.PathRevolutions = Primitive.PackPathRevolutions(origPrimData.PathRevolutions);
        //    sopShape.PathScaleX = Primitive.PackPathScale(origPrimData.PathScaleX);
        //    sopShape.PathScaleY = Primitive.PackPathScale(origPrimData.PathScaleY);
        //    sopShape.PathShearX = (byte)Primitive.PackPathShear(origPrimData.PathShearX);
        //    sopShape.PathShearY = (byte)Primitive.PackPathShear(origPrimData.PathShearY);
        //    sopShape.PathSkew = Primitive.PackPathTwist(origPrimData.PathSkew);
        //    sopShape.PathTaperX = Primitive.PackPathTaper(origPrimData.PathTaperX);
        //    sopShape.PathTaperY = Primitive.PackPathTaper(origPrimData.PathTaperY);
        //    sopShape.PathTwist = Primitive.PackPathTwist(origPrimData.PathTwist);
        //    sopShape.PathTwistBegin = Primitive.PackPathTwist(origPrimData.PathTwistBegin);
        //    sopShape.PCode = (byte)origPrimData.PCode;
        //    sopShape.ProfileBegin = Primitive.PackBeginCut(origPrimData.ProfileBegin);
        //    sopShape.ProfileCurve = origPrimData.profileCurve;
        //    sopShape.ProfileEnd = Primitive.PackEndCut(origPrimData.ProfileEnd);
        //    sopShape.ProfileHollow = Primitive.PackProfileHollow(origPrimData.ProfileHollow);
        //    sopShape.ProfileShape = (ProfileShape)(byte)origPrimData.ProfileCurve;

        //    sopShape.Textures = orig.Textures;

        //    return sopShape;
        //}

    }
    public class Box3Fill
    {

        public override bool Equals(object obj)
        {
            if (obj is Box3Fill)
            {
                Box3Fill other = obj as Box3Fill;
                if (other.MaxX == MaxX &&
                    other.MaxY == MaxY &&
                    other.MaxZ == MaxZ &&
                    other.MinX == MinX &&
                    other.MinY == MinY &&
                    other.MinZ == MinZ) return true;
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

        public float MinX = float.MaxValue;
        public float MaxX = float.MinValue;
        public float MinY = float.MaxValue;
        public float MaxY = float.MinValue;
        public float MinZ = float.MaxValue;
        public float MaxZ = float.MinValue;
        /// <summary>
        /// Construct an infinately small box
        /// </summary>
        public Box3Fill()
        {
        }
        /// <summary>
        ///  Make the box infinatly small
        /// </summary>
        public void Reset()
        {
            MinX = float.MaxValue;
            MaxX = float.MinValue;
            MinY = float.MaxValue;
            MaxY = float.MinValue;
            MinZ = float.MaxValue;
            MaxZ = float.MinValue;
        }

        const float PAD = 0.33f;// SimPathStore.StepSize*0.75f;

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
                    p(x, y);
                }
            }
            /// the for/next loop probably missed this last point
            MinMaxZ(MaxX, MaxY, out SimZMinLevel, out SimZMaxLevel);
            if (SimZMinLevel > MaxZ || SimZMaxLevel < MinZ)
            {
                // this box is not between the Z levels
                return;
            }
            p(MaxX, MaxY);
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
                    p(x, y);
                }
            }
            /// the for/next loop probably missed this last point
            p(MaxX, MaxY);
        }


        public string ToString(Vector3 offset)
        {
            string s = "(" + (Vector3)(MinEdge + offset) + " - " + (Vector3)(MaxEdge + offset) + " mass= "+Mass()+")";
            return s;
        }

        /// <summary>
        /// Make sure box is big enough for this vertex
        /// </summary>
        /// <param name="v"></param>
        /// <returns>true if the box has grown</returns>
        public bool AddVertex(Vertex v)
        {
            bool changed = false;
            if (v.X < MinX)
            {
                MinX = v.X -PAD;
                changed = true;
            }
            if (v.Y < MinY)
            {
                MinY = v.Y - PAD;
                changed = true;
            }
            if (v.Z < MinZ)
            {
                MinZ = v.Z - PAD;
                changed = true;
            }

            if (v.X > MaxX)
            {
                MaxX = v.X + PAD;
                changed = true;
            }
            if (v.Y > MaxY)
            {
                MaxY = v.Y + PAD;
                changed = true;
            }
            if (v.Z > MaxZ)
            {
                MaxZ = v.Z + PAD;
                changed = true;
            }
            return changed;
        }

        /// <summary>
        /// Add Triangle (this just pushes the size of the box outward if needed)
        /// </summary>
        /// <param name="t"></param>
        /// <returns>true if the boxsize was increased</returns>
        public bool AddTriange(Triangle t)
        {
            return AddVertex(t.v1) ||
             AddVertex(t.v2) ||
             AddVertex(t.v3);
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
            Vector3 size = MaxEdge - MinEdge;
            return size.X * size.Y * size.Z;
        }

        public bool IsCompletelyInside(Box3Fill inner) {
            if (inner.MaxX > MaxX) return false;
            if (inner.MinX < MinX) return false;
            if (inner.MaxY > MaxY) return false;
            if (inner.MinY < MinY) return false;
            if (inner.MaxZ > MaxZ) return false;
            if (inner.MinZ < MinZ) return false;
            return true;
        }

        public static List<Box3Fill> Simplify(List<Box3Fill> simpl)
        {
            List<Box3Fill> temp = new List<Box3Fill>(simpl);
            List<Box3Fill> retval = new List<Box3Fill>();

            temp.Sort(Bigger);
            temp.Reverse();
            for (int i = 0; i < temp.Count; i++)
            {
                Box3Fill bi = temp[i];
                bool foundInside = false;
                for (int ii = i + 1; ii < temp.Count; i++)
                {
                    if (temp[ii].IsCompletelyInside(bi))
                    {
                        foundInside = true;
                    }
                }
                if (!foundInside)
                {
                    retval.Add(bi);
                }
            }
            return retval;
        }
        static int Bigger(Box3Fill b1,Box3Fill b2) {
            if (b1 == b2) return 0;
            float f1 = b1.Mass();
            float f2 = b2.Mass();
            if (f1 == f2)
            {
                Vector3 v1 = b1.MinEdge;
                Vector3 v2 = b2.MinEdge;
                float l1 = v1.Length();
                float l2 = v2.Length();
                if (l1 < l2)
                {
                    return -1;
                }
                else if (l1 > l2)
                {
                    return 1;
                }
                if (v1.X < v2.X)
                {
                    return -1;
                }
                return 1;
            }
            return f1 < f2 ? -1 : 1;
        }
    }
}
