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
using cogbot.TheOpenSims.Mesher;
using cogbot.TheOpenSims.Navigation.Debug;
using OpenMetaverse;

namespace cogbot.TheOpenSims.Navigation
{
    public delegate void CallbackXY(float x, float y, float minZ, float maxZ);

    public delegate float SimZLevel(float x, float y);
    public delegate void SimZMinMaxLevel(float x, float y, out float minLevel, out float maxLevel);
    /// <summary>
    /// Graph structure. It is defined with :
    /// It is defined by a 2D matrix of CollisionIndex 
    /// It is used to perform operations on CollisionPlanes
    /// </summary>
    [Serializable]
    public class SimPathStore //: cogbot.TheOpenSims.Navigation.IPathStore
    {
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
            return String.Format("{0}: {1}", GetType().Name, GetSimRegion());// +" Level=" + SimZAverage;
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

        internal CollisionPlane GetCollisionPlane(float Z)
        {
            if (Z < 1) Z = 0;
            else if (Z > 1999) Z = 1998;
            int index = (int)Z;
            CollisionPlane found = Matrixes[index];
            if (index == 5)
            {
            }
            if (found != null)
            {
                found.ZLevel = Z;
                return found;
            }
            lock (Matrixes)
            {
                found = Matrixes[index];
                if (found != null)
                {
                    found.ZLevel = Z;
                    return found;
                }

                if (Z > 1)
                {
                    found = Matrixes[index - 1];
                    if (found != null)
                    {
                        if (found.Accepts(Z))
                        {
                            found.ZLevel = Z;
                            Matrixes[index] = found;
                            return found;
                        }
                        //Matrixes[index].ZLevel = Z;
                        //return Matrixes[index];
                    }
                    else
                        if (false && Matrixes[index + 1] != null)
                        {
                            Matrixes[index] = Matrixes[index + 1];
                            Matrixes[index].ZLevel = Z;
                            return Matrixes[index];
                        }
                }
                found = new CollisionPlane(MAPSPACE, MAPSPACE, Z, this);
                Debug("Created matrix[{0}] {1} for {2}", index, found, this);
                Matrixes[index] = found;
                return found;
            }
        }

        readonly CollisionPlane[] Matrixes = new CollisionPlane[2000];
        public byte[,] GetByteMatrix(float Z)
        {
            return GetCollisionPlane(Z).ByteMatrix;
        }

        readonly public CollisionIndex[,] MeshIndex;

        readonly string RegionFileName;
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


        public byte GetNodeQuality(Vector3 v3)
        {
            return GetByteMatrix(v3.Z)[ARRAY_X((v3.X)), ARRAY_Y((v3.Y))];
        }

        public void SetNodeQuality(Vector3 v3, byte v)
        {
            GetByteMatrix(v3.Z)[ARRAY_X((v3.X)), ARRAY_Y((v3.Y))] = v;
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
            if (b > 100 || b < 3)
            {
                return;
            }
            mMatrix[ix, iy] = STICKY_PASSABLE;
        }

        public CollisionIndex SetObjectAt(float x, float y, SimMesh simObject, float minZ, float maxZ)
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

        //static List<ISimObject> NOOBJECTS = new List<ISimObject>();
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



        public void SetBlocked(float x, float y, float z, SimMesh blocker)
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

        SimRegion TheSimRegion;
        public SimRegion GetSimRegion()
        {
            if (TheSimRegion == null)
            {
                TheSimRegion = SimRegion.GetRegion(Handle);
            }
            return TheSimRegion;
        }

        readonly ulong Handle;
        readonly Vector3d GlobalStart;
        readonly Vector3d GlobalEnd;
        readonly Vector3 Start;
        readonly Vector3 Size;
        /// <summary>
        /// Constructor.
        /// </summary>
        public SimPathStore(String simName, SimRegion region, Vector3 startBottem, Vector3 endTop)
        {
            RegionFileName = simName;
            TheSimRegion = region;
            Handle = region.RegionHandle;
            Start = startBottem;
            GlobalStart = region.LocalToGlobal(startBottem);
            GlobalEnd = region.LocalToGlobal(endTop);
            Size = endTop - startBottem;
            _XY256 = Size.X;
            _OuterBounds = new Box3Fill(true);
            OuterBounds.AddPoint(Start.X, Start.Y, Start.Z, 0);
            OuterBounds.AddPoint(endTop.X, endTop.Y, endTop.Z, 0);
            //TheSimZMinMaxLevel = new SimZMinMaxLevel(MinMaxLevel);
            StepSize = 1f / POINTS_PER_METER;
            _Max256 = XY256 - StepSize;
            MAPSPACE = (int)XY256 * ((int)POINTS_PER_METER);
            TheSimRegion.SetMapSpace(MAPSPACE);
            MeshIndex = TheSimRegion._mWaypoints;
            if (Size.X != Size.Y) throw new Exception("X and Y must be the same for " + this);
            //CreateDefaultRoutes();
          //  CurrentPlane = new CollisionPlane(MAPSPACE,MAPSPACE,0);
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
            IList<SimRoute> vectors = new List<SimRoute>();
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


        static public List<Vector3d> GetSimplifedRoute(Vector3d currentV3In, List<Vector3d> v3s, int MaxTurnDegrees, float MaxDist)
        {
            if (v3s.Count < 3) return v3s;
            Vector3d currentV3 = currentV3In;
            List<Vector3d> vectors = new List<Vector3d>();
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
        public IList<Vector3d> GetLocalPath(Vector3 start, Vector3 end)
        {
            PathFinderDemo panel = PathFinder;
            if (!IsPassable(start))
            {
                Debug("start is not passable: " + start);
            }
            if (!IsPassable(end))
            {
                Debug("end is not passable: " + start);
            }
            PunishChangeDirection = !PunishChangeDirection;    //toggle each time
            Point S = ToPoint(start);
            Point E = ToPoint(end);
            List<PathFinderNode> pfn = null;
            
            float Z = start.Z;

            CollisionPlane CP = GetCollisionPlane(Z);
            CP.EnsureUpToDate();
            try
            {
                PathFinderFasting pff = new PathFinderFasting(CP.ByteMatrix);
                if (panel != null) panel.SetStartEnd(S, E);
                // pff.Diagonals = false;
                //pff.ReopenCloseNodes = true;
                pff.SearchLimit = 10000000;
                pff.PunishChangeDirection = PunishChangeDirection;
                pfn = pff.FindPath(S, E);
            }
            catch (Exception e)
            {
                Debug("Cant do route! " + e);
            }
            if (pfn == null || pfn.Count == 0)
            {
                Debug("Cant do pfn!");
                List<Vector3d> temp = new List<Vector3d>();
                temp.Add(GetSimRegion().LocalToGlobal(end));
                return temp;
            }
            if (panel != null) panel.ShowPath(pfn);
            List<Vector3d> r = PathfinderNodesToV3s(pfn,Z);
            r.Reverse();
            return r;
        }

        public bool IsPassable(Vector3 end)
        {
            double Dist;
            if (GetNodeQuality(end) == BLOCKED) return false;
            // if (true) return true;
            SimWaypoint W = ClosestRegionNode(end.X, end.Y, end.Z, out Dist, true);
            return W.Passable;
        }



        private List<Vector3d> PathfinderNodesToV3s(List<PathFinderNode> pfn, float Z)
        {
            Vector3d V = GetGlobalCorner();
            List<Vector3d> v3s = new List<Vector3d>();
            foreach (PathFinderNode P in pfn)
            {
                float x = UNARRAY_X(P.X);
                float y = UNARRAY_Y(P.Y);
                Vector3d v3 = new Vector3d(x + V.X, y + V.Y, Z);
                v3s.Add(v3);
            }
            return GetSimplifedRoute(v3s[0], v3s, 10, 8f);
        }

        private Vector3d GetGlobalCorner()
        {
            return new Vector3d(GetSimRegion().LocalToGlobal(new Vector3(0, 0, 0)));
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
            Vector3d v3d = GetSimRegion().LocalToGlobal(v3);
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
            Vector3d v3d = GetSimRegion().LocalToGlobal(new Vector3(x, y, z));
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
            lock (LaskKnownPos) if (!LaskKnownPos.ContainsKey(uUID))
            {
                LaskKnownPos[uUID] = new MoverTracking(after, rot, this);
            }
            else
            {
                LaskKnownPos[uUID].Update(after, rot);
            }
        }

        private void Refresh(cogbot.TheOpenSims.Mesher.Box3Fill changed, CollisionPlane CurrentPlane)
        {
            int xs = (int)changed.MinX;
            int ys = (int)changed.MaxX;
            int xe = (int)changed.MinY;
            int ye = (int)changed.MaxY;
            if (ye < 0 || xe < 0) return;
            if (xs > 0) xs--;
            if (ys > 0) ys--;
            int MAX = MAPSPACE - 1;
            if (xe < MAX) xe++;
            if (ys < MAX) ye++;


            //  lock (mWaypoints)
            {
                for (int x = xs; x <= xe; x++)
                    for (int y = ys; y <= ye; y++)
                    {
                        CollisionIndex WP = MeshIndex[x, y];
                        if (WP != null) WP.TaintMatrix();
                    }
                for (int x = xs; x <= xe; x++)
                    for (int y = ys; y <= ye; y++)
                    {
                        CollisionIndex WP = MeshIndex[x, y];
                        if (WP != null) WP.UpdateMatrix(CurrentPlane);
                    }
            }
        }

        internal void Refresh(Box3dFill changed)
        {
            throw new NotImplementedException();
        }

        PathFinderDemo PathFinder;

        public void ShowDebugger()
        {
            if (PathFinder == null)
            {
                PathFinder = new PathFinderDemo(this);
            }
            PathFinder.Show();
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
                Vector3 dif = LastPosition - nextPosition;
                if (!(dif.X == 0 && dif.Y == 0))
                {
                    int stepsNeeded = (int)(dist * Store.POINTS_PER_METER) + 1;
                    if (Settings.LOG_LEVEL == Helpers.LogLevel.Debug) Console.WriteLine("MakeMovement " + LastPosition + " -> " + stepsNeeded + " -> " + nextPosition + " " + Store);
                    Vector3 vstep = dif / stepsNeeded;
                    Vector3 traveled = nextPosition;
                    Store.SetTraveled(nextPosition.X, nextPosition.Y, nextPosition.Z);
                    for (int i = 0; i < stepsNeeded; i++)
                    {
                        traveled = traveled + vstep;
                        if (stepsNeeded > 10) Store.SetTraveled(traveled.X, traveled.Y, traveled.Z);
                        else Store.SetPassable(traveled.X, traveled.Y, traveled.Z);
                    }
                }
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
