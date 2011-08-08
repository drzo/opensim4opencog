//#define COLLIDER_TRIANGLE
using System;
using System.Collections.Generic;
using System.Drawing;
using cogbot.Listeners;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Mesher;
using PathSystem3D.Navigation;
using THIRDPARTY.OpenSim.Region.Physics.Manager;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
using THIRDPARTY.PrimMesher;

#if COLLIDER_ODE  
using THIRDPARTY.OpenSim.Region.Physics.OdePlugin;
//using THIRDPARTY.OpenSim.Framework;
//using THIRDPARTY.OpenSim.Region.Physics.Manager;
#endif
namespace cogbot.TheOpenSims
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


#if COLLIDER_ODE  
        private PhysicsVector LastPosition;
        private PhysicsVector LastSize;
        private Quaternion LastRotation;      
        private Mesh mesh;
        private OdePrim physicsActor;
#endif
        public MeshableObject RootObject;
        public Primitive Prim;

        public SimMesh(MeshableObject simObject, Primitive prim, SimPathStore PS)
            : base(new Box3Fill(true), new List<Box3Fill>(), PS)
        {
            RootObject = simObject;
            Prim = prim;
            Update(RootObject);
        }
        protected SimMesh(Box3Fill o, IList<Box3Fill> i, SimPathStore R)
            : base(o, i, R)
        {
            
        }

        public override Color DebugColor()
        {
            if (RootObject is SimObjectImpl)
            {
                return ((SimObjectImpl) RootObject).DebugColor();
            }
            return Color.Empty;
        }
         
        public override void RemeshObject(Box3Fill changed)
        {
            RemoveFromWaypoints(changed);
            Update(RootObject);
            UpdateOccupied(PathStore);
        }

        public override bool IsRegionAttached()
        {
            return RootObject.IsRegionAttached;
        }


        public override bool IsSolid
        {
            get { return RootObject.IsSolid; }
            set
            {
                RootObject.IsSolid = value;
            }
        }

        public override void AddPos(Vector3 offset)
        {
#if COLLIDER_TRIANGLE
            Vertex v3 = new Vertex(offset.X, offset.Y, offset.Z);
            foreach (Triangle tri in triangles)
            {
                tri.v1 = tri.v1 + v3;
                tri.v2 = tri.v2 + v3;
                tri.v3 = tri.v3 + v3;
            } 
#endif

            OuterBox.AddPos(offset);
            foreach (CollisionObject B in InnerBoxes)
            {
                B.AddPos(offset);               
            }
        }
        
        public String GetObjectName()
        {
            return RootObject.ToString();
        }

        public override sealed bool Update(SimPosition simObject)
        {
            //if (!WorldObjects.MaintainCollisions) return false;
            if (!simObject.IsRegionAttached) return false;
            if (MeshOnlySolids && !((MeshableObject) simObject).IsSolid) return false;
            Quaternion Rotation = simObject.SimRotation;
            Vector3 Scale = Prim.Scale; //.GetSimScale();
            Vector3 Position = simObject.SimPosition;

#if COLLIDER_ODE  
            LastSize = new PhysicsVector(1, 1, 1); // we scaled the PrimMesh already!
            LastRotation = Quaternion.Identity;  // we rotated the PrimMesh already!
            LastPosition = ToPhysicsVector(Position); // we hadn't done position though
#endif
            //pbs.

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

#if COLLIDER_ODE  
            this.mesh = mesh;
            if (!RootObject.IsPhantom)
                physicsActor = GetPhysicsActor();
#endif
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
            if (InnerBoxes == null) InnerBoxes = new List<Box3Fill>();
            else
            InnerBoxes.Clear();
            OuterBox.Reset();
            CalcBoxesFromMeshes(mesh, InnerBoxes);
            bool verySmall = OuterBox.EdgeSize < WorldPathSystem.MinEdgeSizeOfSimplify;
            if (verySmall)
            {
                InnerBoxes.Clear();
                AddPos(Position);
                InnerBoxes.Add(OuterBox);
            }
            else
            {
                if (WorldObjects.SimplifyBoxes)
                {
                    int b = InnerBoxes.Count;
                    InnerBoxes = Box3Fill.Simplify((List<Box3Fill>) InnerBoxes);
                    if (b > 2000 || InnerBoxes.Count * 4 < b)
                        DLRConsole.DebugWriteLine("Simplfy mesh {0} -> {1} ", b,
                                                  InnerBoxes.Count + " " + OuterBox.Mass + " " + this.GetObjectName());
                }
                AddPos(Position);
            }
#if COLLIDER_TRIANGLE
            triangles = mesh.triangles;
#endif
            return true;
        }

#if COLLIDER_ODE  
        public OdePrim GetPhysicsActor()
        {
            if (!COLLIDER_ODE) return null;
            if (pbs == null) pbs = mesh.PBS;
            if (physicsActor == null)
            {
                physicsActor = (OdePrim)
                               PathStore.odeScene.AddPrim(GetObjectName(), LastPosition, LastSize, LastRotation, mesh,
                                                          pbs, false);
                physicsActor.UnSubscribeEvents();
            }
            return physicsActor;
        }
#endif

        static PhysicsVector ToPhysicsVector(Vector3 p)
        {
            return new PhysicsVector(p.X,p.Y,p.Z);
        }

        public static bool FastAndImpercise = false;
        private float scaleSize;
        /// <summary>
        /// UseExtremeDetailSize is compared to Scale X/Y/Z added together and if greater will try to
        ///   generate more faces
        /// </summary>
        static float UseExtremeDetailSize = 3f;
        static float UseLowDetailSize = 1f;//3f;
        static bool UseViewerMode = false;

        void CalcBoxesFromMeshes(Mesh M, IList<Box3Fill> innerBoxes)
        {
#if COLLIDER_TRIANGLE
            triangles = M.triangles;     
#endif
            SimPathStore.TrianglesToBoxes(M.triangles, OuterBox, PadXYZ, innerBoxes);
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
#if COLLIDER_ODE
               mesh.PBS = meshIn.PBS
#endif
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
        private static bool MaintainSculptPool = false;
        private int sides;
        private LevelOfDetail detail;
        private int hollowsides;
#if COLLIDER_ODE
        private PrimitiveBaseShape pbs;
#endif

        public Mesh PrimitiveToMesh(Primitive primitive, Vector3 Scale, Quaternion rot)
        {

            bool wasSculpt = primitive.Sculpt != null;
            if (wasSculpt && WorldPathSystem.SculptCollisions)
            {
                Primitive.SculptData SD = primitive.Sculpt;
                UUID Id = SD.SculptTexture;
                SculptMesh SM;
                if (!SculptedMeshes.TryGetValue(Id, out SM))
                {
                    byte[] bytes = WorldObjects.GridMaster.TextureBytesForUUID(SD.SculptTexture);
                    SM = ToSculptMesh(bytes, primitive.Sculpt, "" + primitive);
                    if (MaintainSculptPool) SculptedMeshes[Id] = SM;
                    //  SM.DumpRaw(".", primitive.ID.ToString(), "sculptMesh" + primitive.LocalID);
                }
                if (SM != null)
                {
                    SM = SM.Copy();
                    SM.Scale(Scale.X, Scale.Y, Scale.Z);
                    SM.AddRot(QuaternionToQuat(rot));
                    return ToMesh(
#if COLLIDER_ODE
                        PrimToBaseShape(primitive),
                        #endif

                        SM.coords, SM.faces, SM.viewerFaces, primitive.Type == PrimType.Sphere);
                }
            }

            this.scaleSize = Scale.X + Scale.Y + Scale.Z;
            bool UseExtremeDetail = scaleSize > UseExtremeDetailSize;
            //LevelOfDetail detail;
            if (scaleSize < UseLowDetailSize)
                detail = LevelOfDetail.Low;
            else
                detail = LevelOfDetail.Medium;
            if (UseExtremeDetail)
            {
                if (primitive.Type == PrimType.Box)
                {
                    detail = LevelOfDetail.Medium;
                } else
                {
                    detail = LevelOfDetail.High;
                }
            }

            PrimMesh primMesh = ConstructionDataToPrimMesh(primitive.PrimData, detail, UseExtremeDetail ? 2 : 1);
            primMesh.Scale(Scale.X, Scale.Y, Scale.Z);
            primMesh.AddRot(QuaternionToQuat(rot));
            Mesh m = PrimMeshToMesh(primMesh);
#if COLLIDER_ODE
            m.PBS = PrimToBaseShape(primitive);
#endif
            return m;
        }

        public static Mesh ToMesh(
#if (COLLIDER_ODE)
            PrimitiveBaseShape pbs, 
#endif
            List<Coord> coords, List<Face> faces, List<ViewerFace> viewerFaces, bool isSphere)
        {
            Mesh mesh = new Mesh(

#if COLLIDER_ODE 
                pbs
#endif

                );

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
        public static SculptMesh ToSculptMesh(byte[] sculptData, Primitive.SculptData sculptDataIn, string sculptDataString)//( Vector3 size, Quaternion Rotation)
        {
            SculptMesh sculptMesh;
            if (sculptData == null || sculptData.Length == 0)
            {
                Error("[PHYSICS]: Missing Sclupt Data ", sculptDataString);
                return null;
            }

            System.Drawing.Image idata = null;

            try
            {
                OpenMetaverse.Imaging.ManagedImage managedImage;  // we never use this
                if (!OpenMetaverse.Imaging.OpenJPEG.DecodeToImage(sculptData, out managedImage, out idata))
                {
                    Error("[PHYSICS]: OpenMetaverse.Imaging.OpenJPEG.DecodeToImage failed ", sculptDataString);
                    return null;
                }

            }
            catch (DllNotFoundException)
            {
                Error(
                    "[PHYSICS]: OpenJpeg is not installed correctly on this system. Physics Proxy generation failed.  Often times this is because of an old version of GLIBC.  You must have version 2.4 or above!",
                    sculptDataString);
                return null;
            }
            catch (IndexOutOfRangeException e)
            {
                Error("[PHYSICS]: OpenJpeg was unable to decode this.   Physics Proxy generation failed " + e, sculptDataString);
                return null;
            }
            catch (Exception e)
            {
                Error("[PHYSICS]: Unable to generate a Sculpty physics proxy.  Sculpty texture decode failed! " + e, sculptDataString);
                return null;
            }

            SculptMesh.SculptType sculptType;

            switch (sculptDataIn.Type)
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
            if (idata == null)
            {
                Error("[PHYSICS]: IData Null " , sculptDataString);
                return null;
            }
            sculptMesh = new SculptMesh((System.Drawing.Bitmap)idata, sculptType, (int)32, false, sculptDataIn.Mirror, sculptDataIn.Invert);

            idata.Dispose();

            //    sculptMesh.DumpRaw(baseDir, primName, "primMesh");
            return sculptMesh;
        }

        private static void Error(string s, string sculptDataString)
        {
            DLRConsole.DebugWriteLine("" + s + " ERROR: " + sculptDataString);
        }


        /// <summary>
        /// Convert a Openmetaverse.Primitive to a PrimMesh
        /// </summary>
        /// <param name="thePrim"></param>
        /// <param name="detail"></param>
        /// <param name="pos"></param>
        /// <param name="rot"></param>
        /// <returns></returns>
        public PrimMesh PrimitiveToPrimMesh(Primitive thePrim, LevelOfDetail detail, Vector3 Scale, Quaternion rot)
        {
            bool UseExtremeDetail = Scale.X + Scale.Y + Scale.Z > UseExtremeDetailSize;
            PrimMesh mesh = ConstructionDataToPrimMesh(thePrim.PrimData, detail, UseExtremeDetail ? 2 : 1);
#if COLLIDER_ODE
            mesh.PBS = PrimToBaseShape(thePrim);
#endif
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
        public PrimMesh ConstructionDataToPrimMesh(Primitive.ConstructionData primData, LevelOfDetail detail, float detailMult)
        {

            this.sides = 4;
            this.hollowsides = 4;

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


        internal void RemoveCollisions()
        {
            base.RemoveCollisions(PathStore);
        }

        public override bool xyMaxZ(float x, float y, float z, out float zout)
        {
            float izout;
            // x -= RootObject.GetSimPosition().X;
            bool b = base.xyMaxZ(x, y, z, out izout);
            zout = izout;
            return b;

        }

#if COLLIDER_ODE
        /// <summary>
        /// [05:31] <AFrisby> dmiles_afk, search my blog, I wrote a function for converting OpenMetaverse.Primitive to OpenSimulator.SceneObjectGroup
        /// 
        /// from: http://www.adamfrisby.com/blog/2008/10/code-snippet-converting-openmetaverseprimitive-to-opensimulatorsceneobjectpart/
        /// </summary>
        /// <param name="orig"></param>
        /// <returns></returns>
        static public PrimitiveBaseShape PrimToBaseShape(Primitive orig)
        {
            //bool root = orig.ParentID == 0;

            //SceneObjectPart sop = new SceneObjectPart();
            //sop.LastOwnerID = orig.OwnerID;
            //sop.OwnerID = orig.OwnerID;
            //sop.GroupID = orig.GroupID;

            //sop.CreatorID = orig.Properties.CreatorID;

            //sop.OwnershipCost = orig.Properties.OwnershipCost;
            //sop.ObjectSaleType = (byte)orig.Properties.SaleType;
            //sop.SalePrice = orig.Properties.SalePrice;
            //sop.CreationDate = (int)Utils.DateTimeToUnixTime(orig.Properties.CreationDate);

            //// Special   
            //sop.ParentID = 0;

            //sop.OwnerMask = (uint)orig.Properties.Permissions.OwnerMask;
            //sop.NextOwnerMask = (uint)orig.Properties.Permissions.NextOwnerMask;
            //sop.GroupMask = (uint)orig.Properties.Permissions.GroupMask;
            //sop.EveryoneMask = (uint)orig.Properties.Permissions.EveryoneMask;
            //sop.BaseMask = (uint)orig.Properties.Permissions.BaseMask;

            //sop.ParticleSystem = orig.ParticleSys.GetBytes();

            //// OS only   
            //sop.TimeStampFull = 0;
            //sop.TimeStampLastActivity = 0;
            //sop.TimeStampTerse = 0;

            //// Not sure nessecary   
            //sop.UpdateFlag = 2;

            //sop.InventorySerial = 0;
            //sop.UUID = orig.ID;
            //sop.LocalId = orig.LocalID;
            //sop.Name = orig.Properties.Name;
            //sop.Flags = orig.Flags;
            //sop.Material = 0;
            //sop.RegionHandle = orig.RegionHandle;

            //sop.GroupPosition = orig.Position;

            //if (!root)
            //    sop.OffsetPosition = orig.Position;
            //else
            //    sop.OffsetPosition = Vector3.Zero;

            //sop.RotationOffset = orig.Rotation;
            //sop.Velocity = orig.Velocity;
            //sop.RotationalVelocity = Vector3.Zero;
            //sop.AngularVelocity = Vector3.Zero;
            //sop.Acceleration = Vector3.Zero;

            //sop.Description = orig.Properties.Description;
            //sop.Color = Color.White;
            //sop.Text = orig.Text;
            //sop.SitName = orig.Properties.SitName;
            //sop.TouchName = orig.Properties.TouchName;
            //sop.ClickAction = (byte)orig.ClickAction;

            //sop.PayPrice = new int[1];

            PrimitiveBaseShape sopShape = new PrimitiveBaseShape(true);
            //sopShape.Scale = orig.Scale;
            sopShape.FlexiEntry = false;
            if (orig.Flexible != null)
            {
                sopShape.FlexiDrag = orig.Flexible.Drag;
                sopShape.FlexiEntry = false;
                sopShape.FlexiForceX = orig.Flexible.Force.X;
                sopShape.FlexiForceY = orig.Flexible.Force.Y;
                sopShape.FlexiForceZ = orig.Flexible.Force.Z;
                sopShape.FlexiGravity = orig.Flexible.Gravity;
                sopShape.FlexiSoftness = orig.Flexible.Softness;
                sopShape.FlexiTension = orig.Flexible.Tension;
                sopShape.FlexiWind = orig.Flexible.Wind;
            }

            Primitive.ConstructionData origPrimData = orig.PrimData;

            switch (origPrimData.ProfileHole)
            {
                case HoleType.Circle:
                    sopShape.HollowShape = HollowShape.Circle;
                    break;
                case HoleType.Square:
                    sopShape.HollowShape = HollowShape.Square;
                    break;
                case HoleType.Triangle:
                    sopShape.HollowShape = HollowShape.Triangle;
                    break;
                default:
                case HoleType.Same:
                    sopShape.HollowShape = HollowShape.Same;
                    break;
            }

            sopShape.LightEntry = false;
            if (orig.Light != null)
            {
                sopShape.LightColorA = orig.Light.Color.A;
                sopShape.LightColorB = orig.Light.Color.B;
                sopShape.LightColorG = orig.Light.Color.G;
                sopShape.LightColorR = orig.Light.Color.R;
                sopShape.LightCutoff = orig.Light.Cutoff;
                sopShape.LightEntry = false;
                sopShape.LightFalloff = orig.Light.Falloff;
                sopShape.LightIntensity = orig.Light.Intensity;
                sopShape.LightRadius = orig.Light.Radius;
            }

            sopShape.PathBegin = Primitive.PackBeginCut(origPrimData.PathBegin);
            sopShape.PathCurve = (byte)origPrimData.PathCurve;
            sopShape.PathEnd = Primitive.PackEndCut(origPrimData.PathEnd);
            sopShape.PathRadiusOffset = Primitive.PackPathTwist(origPrimData.PathRadiusOffset);
            sopShape.PathRevolutions = Primitive.PackPathRevolutions(origPrimData.PathRevolutions);
            sopShape.PathScaleX = Primitive.PackPathScale(origPrimData.PathScaleX);
            sopShape.PathScaleY = Primitive.PackPathScale(origPrimData.PathScaleY);
            sopShape.PathShearX = (byte)Primitive.PackPathShear(origPrimData.PathShearX);
            sopShape.PathShearY = (byte)Primitive.PackPathShear(origPrimData.PathShearY);
            sopShape.PathSkew = Primitive.PackPathTwist(origPrimData.PathSkew);
            sopShape.PathTaperX = Primitive.PackPathTaper(origPrimData.PathTaperX);
            sopShape.PathTaperY = Primitive.PackPathTaper(origPrimData.PathTaperY);
            sopShape.PathTwist = Primitive.PackPathTwist(origPrimData.PathTwist);
            sopShape.PathTwistBegin = Primitive.PackPathTwist(origPrimData.PathTwistBegin);
            sopShape.PCode = (byte)origPrimData.PCode;
            sopShape.ProfileBegin = Primitive.PackBeginCut(origPrimData.ProfileBegin);
            sopShape.ProfileCurve = origPrimData.profileCurve;
            sopShape.ProfileEnd = Primitive.PackEndCut(origPrimData.ProfileEnd);
            sopShape.ProfileHollow = Primitive.PackProfileHollow(origPrimData.ProfileHollow);
            sopShape.ProfileShape = (ProfileShape)(byte)origPrimData.ProfileCurve;

            sopShape.Textures = orig.Textures;

            return sopShape;
        }
#endif

    }
}