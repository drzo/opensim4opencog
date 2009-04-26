using System;
using System.Collections.Generic;
using System.Threading;
using PathSystem3D.Navigation;
using OpenMetaverse;
//using THIRDPARTY.OpenSim.Framework;
//using THIRDPARTY.OpenSim.Region.Physics.Manager;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
using THIRDPARTY.PrimMesher;
using cogbot.Listeners;
using cogbot.TheOpenSims;

namespace PathSystem3D.Mesher
{

    // from IdealistViewer
    public enum LevelOfDetail
    {
        Low,
        Medium,
        High
    }

    public delegate bool CollisionTest(float x, float y, float z);



    public class SimMesh : MeshedObject
    {
        public SimPosition RootObject;
        public Primitive Prim;

        public SimMesh(SimPosition simObject, Primitive prim, SimPathStore PS)
            : base(new Box3Fill(true), new List<Box3Fill>(), PS)
        {
            RootObject = simObject;
            Prim = prim;
            Update(simObject);
        }
        public override void RemeshObject(Box3Fill changed)
        {
            RemoveFromWaypoints(changed);
            Update(RootObject);
            UpdatePathOccupied(GetPathStore());
        }

        public SimPathStore GetPathStore()
        {
            return PathStore;
        }

        public override bool IsRegionAttached()
        {
            return RootObject.IsRegionAttached();
        }


        public override bool IsPassable
        {
            get { return RootObject.IsPassable; }
            set
            {
                if (RootObject.IsPassable != value)
                {
                    RootObject.IsPassable = value;
                    if (!value)
                    {
                        if (InnerBoxes.Count == 0)
                        {
                            Update(RootObject);
                        }

                    }
                    else
                        PathStore.Refresh(OuterBox);
                }
            }
        }

        private void AddPos(Vector3 offset)
        {
#if TRIANGE_MESH
            Vertex v3 = new Vertex(offset.X, offset.Y, offset.Z);
            foreach (Triangle tri in triangles)
            {
                tri.v1 = tri.v1 + v3;
                tri.v2 = tri.v2 + v3;
                tri.v3 = tri.v3 + v3;
            } 
#endif

            OuterBox.AddPos(offset);
            foreach (Box3Fill B in InnerBoxes)
            {
                B.AddPos(offset);
            }
        }

        public override bool Update(SimPosition simObject)
        {
            if (!simObject.IsRegionAttached()) return false;
            if (DoNotMeshPassable && simObject.IsPassable) return false;
            Quaternion Rotation = simObject.GetSimRotation();
            Vector3 Scale = Prim.Scale;//.GetSimScale();
            Vector3 Position = simObject.GetSimPosition();

            //List<Mesh> MeshList = new List<Mesh>();

            //PrimMesh primMesh;

            //if (false)
            //{

            //    // Add Low PrimMesh (IdealistViewer code)
            //    primMesh = PrimitiveToPrimMesh(simObject.thePrim, LevelOfDetail.Low, Scale, Rotation);
            //    AddMesh(primMesh);

            //}

            // Add High PrimMesh (IdealistViewer code)
            Mesh mesh = PrimitiveToMesh(Prim, Scale, Rotation);
            //MeshList.Add(mesh);


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
            CalcBoxesFromMeshes(mesh, InnerBoxes);
            // int b = InnerBoxes.Count;
            InnerBoxes = Box3Fill.Simplify((List<Box3Fill>)InnerBoxes);
            // Console.Write("Simplfy mesh {0} -> {1} ", b, InnerBoxes.Count);
            AddPos(Position);
            return true;
        }
        public static bool FastAndImpercise = false;
        /// <summary>
        /// UseExtremeDetailSize is compared to Scale X/Y/Z added together and if greater will try to
        ///   generate more faces
        /// </summary>
        static float UseExtremeDetailSize = 3f;
        static float UseLowDetailSize = 1f;//3f;
        static bool UseViewerMode = false;

        void CalcBoxesFromMeshes(Mesh M, IList<Box3Fill> InnerBoxes)
        {
#if TRIANGE_MESH
            triangles = M.triangles;     
#endif
            SimPathStore.TrianglesToBoxes(M.triangles, OuterBox, padXYZ, InnerBoxes);
        }


        public override string ToString()
        {
            string s = "Mesh " + OuterBox.ToString();
            return s + " for " + RootObject;
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


        static Dictionary<UUID, SculptMesh> SculptedMeshes = new Dictionary<UUID, SculptMesh>();

        public static Mesh PrimitiveToMesh(Primitive primitive, Vector3 Scale, Quaternion rot)
        {

            if (primitive.Sculpt != null)
            {
                Primitive.SculptData SD = primitive.Sculpt;
                UUID Id = SD.SculptTexture;
                SculptMesh SM;
                if (!SculptedMeshes.TryGetValue(Id, out SM))
                {
                    byte[] bytes = WorldObjects.Master.TextureBytesFormUUID(SD.SculptTexture);
                    SM = ToSculptMesh(bytes, primitive.Sculpt.Type);
                    SculptedMeshes[Id] = SM;
                    //  SM.DumpRaw(".", primitive.ID.ToString(), "sculptMesh" + primitive.LocalID);
                }
                if (SM != null)
                {
                    SM = SM.Copy();
                    SM.AddRot(QuaternionToQuat(rot));
                    SM.Scale(Scale.X, Scale.Y, Scale.Z);
                    return ToMesh(SM.coords, SM.faces, SM.viewerFaces, primitive.Type == PrimType.Sphere);
                }
            }

            float scaleSize = Scale.X + Scale.Y + Scale.Z;
            bool UseExtremeDetail = scaleSize > UseExtremeDetailSize;
            LevelOfDetail detail;
            if (scaleSize < UseLowDetailSize)
                detail = LevelOfDetail.Medium;
            else
                detail = LevelOfDetail.High;
            //if (!UseExtremeDetail)
            //{
            //    if (primitive.Type == PrimType.Box)
            //    {
            //        detail = LevelOfDetail.Medium;
            //    }
            //}

            PrimMesh primMesh = ConstructionDataToPrimMesh(primitive.PrimData, detail, UseExtremeDetail ? 2 : 1);
            primMesh.Scale(Scale.X, Scale.Y, Scale.Z);
            if (rot!=Quaternion.Identity) primMesh.AddRot(QuaternionToQuat(rot));
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
        public static SculptMesh ToSculptMesh(byte[] sculptData, OpenMetaverse.SculptType sculptTypeIn)//( Vector3 size, Quaternion Rotation)
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
            PrimMesh mesh = ConstructionDataToPrimMesh(thePrim.PrimData, detail, UseExtremeDetail ? 2 : 1);
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
        public static PrimMesh ConstructionDataToPrimMesh(Primitive.ConstructionData primData, LevelOfDetail detail, float detailMult)
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

            //  if (UseExtremeDetail)
            {
                sides = (int)(sides * detailMult);
                hollowsides = (int)(hollowsides * detailMult);
            }
            PrimMesh primMesh = new PrimMesh(sides, profileBegin, profileEnd, (float)primData.ProfileHollow, hollowsides);
            primMesh.viewerMode = UseViewerMode;
            primMesh.holeSizeX = primData.PathScaleX;
            primMesh.holeSizeY = primData.PathScaleY;
            primMesh.pathCutBegin = primData.PathBegin;
            primMesh.pathCutEnd = primData.PathEnd;
            primMesh.topShearX = primData.PathShearX;
            primMesh.topShearY = primData.PathShearY;
            primMesh.radius = primData.PathRadiusOffset;
            primMesh.revolutions = primData.PathRevolutions;
            primMesh.skew = primData.PathSkew;
            switch (detail)
            {
                case LevelOfDetail.Low:
                    primMesh.stepsPerRevolution = 6;
                    break;
                case LevelOfDetail.Medium:
                    primMesh.stepsPerRevolution = 12;
                    break;
                default:
                    primMesh.stepsPerRevolution = 24;
                    break;
            }

            //if (UseExtremeDetail)
            {
                primMesh.stepsPerRevolution = (int)(primMesh.stepsPerRevolution * detailMult);
            }


            if (primData.PathCurve == PathCurve.Line)
            {
                primMesh.taperX = 1.0f - primData.PathScaleX;
                primMesh.taperY = 1.0f - primData.PathScaleY;
                primMesh.twistBegin = (int)(180 * primData.PathTwistBegin);
                primMesh.twistEnd = (int)(180 * primData.PathTwist);
                primMesh.ExtrudeLinear();
            }
            else
            {
                primMesh.taperX = primData.PathTaperX;
                primMesh.taperY = primData.PathTaperY;
                primMesh.twistBegin = (int)(360 * primData.PathTwistBegin);
                primMesh.twistEnd = (int)(360 * primData.PathTwist);
                primMesh.ExtrudeCircular();
            }


            if (UseViewerMode)
            {
                int numViewerFaces = primMesh.viewerFaces.Count;
                for (uint i = 0; i < numViewerFaces; i++)
                {
                    ViewerFace vf = primMesh.viewerFaces[(int)i];

                    if (isSphere)
                    {
                        vf.uv1.U = (vf.uv1.U - 0.5f) * 2.0f;
                        vf.uv2.U = (vf.uv2.U - 0.5f) * 2.0f;
                        vf.uv3.U = (vf.uv3.U - 0.5f) * 2.0f;
                    }
                }
            }
            return primMesh;
        }


        internal void UpdateOccupied()
        {
            base.UpdatePathOccupied(GetPathStore());
            //throw new NotImplementedException();
        }

        internal void RemoveCollisions()
        {
            base.RemoveCollisions(GetPathStore());
        }
        public override bool xyMaxZ(float x, float y, float z, out float zout)
        {
            float izout;
            // x -= RootObject.GetSimPosition().X;
            bool b = base.xyMaxZ(x, y, z, out izout);
            zout = izout;
            return b;

        }
    }

}
