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
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Collections;
using System.IO;
using System.Threading;
using cogbot.TheOpenSims.Navigation.Debug;
using System.Runtime.Serialization.Formatters.Binary;
using System.Drawing;

namespace cogbot.TheOpenSims.Navigation
{
    public delegate void CallbackXY(float x, float y, float minZ, float maxZ);

    public delegate float SimZLevel(float x, float y);
    public delegate void SimZMinMaxLevel(float x, float y, out float minLevel, out float maxLevel);
    /// <summary>
    /// Graph structure. It is defined with :
    /// It is defined with both a list of nodes and a list of arcs.
    /// </summary>
    [Serializable]
    public class SimPathStore
    {
        public float _TheSimZ = -1f;
        public float SimZAverage
        {
            get
            {
                if (_TheSimZ == -1f)
                {
                    _TheSimZ = SimLevel(127f, 127f);
                }
                return _TheSimZ;
            }
        }

        public float TheSimZMinLevel
        {
            get { return SimZAverage + 1f; }
        }
        public float TheSimZMaxLevel
        {
            get { return SimZAverage + 2f; }
        }
        public void MinMaxLevel(float x, float y, out float minLevel, out float maxLevel)
        {
            minLevel = SimLevel(x, y) + 1f;
            maxLevel = minLevel + 1f;
        }

        public const byte BLOCKED = 0;
        public const byte PASSABLE = 1;
        public const byte STICKY_PASSABLE = 2;
        public float POINTS_PER_METER = 8f;
        public float LargeScale = 1f;//StepSize;//0.2f;

        public float SimLevel(float vx, float vy)
        {
            int ix = ARRAY_IDX(RangeCheck(vx));
            int iy = ARRAY_IDX(RangeCheck(vy));
            SimWaypoint WP = mWaypoints[ix, iy];
            if (WP == null) return GetSimRegion().GetGroundLevel(vx, vy);
            return WP.GetZLevel();
        }

        public SimZMinMaxLevel TheSimZMinMaxLevel;
        public static float PI2 = (float)(Math.PI * 2f);
        public static float RAD2DEG = 360f / PI2;

        public float StepSize;// = 1f / POINTS_PER_METER;
        public int MAPSPACE;// = 256 * ((int)POINTS_PER_METER);
        public readonly bool LargeScaleRound;// = LargeScale == (float)Math.Round(LargeScale, 0);
        public readonly byte[,] mMatrix;// = new byte[MAPSPACE, MAPSPACE];
        public readonly SimWaypoint[,] mWaypoints;// = new SimWaypoint[MAPSPACE, MAPSPACE];

        readonly string RegionFileName;
        //readonly byte[,] paths = PathFinding.PathFinderDemo.Instance


        public float UNARRAY_IDX(int p)
        {
            return p / POINTS_PER_METER;
        }

        public int ARRAY_IDX(double x)
        {
            if (x == 0.0) return 0;
            double i = Math.Round(x * POINTS_PER_METER, 1);
            int ii = (int)i;
            if ((double)i != ii)
            {
                throw new ArgumentException("ARRAY_IDX " + i + "!=" + ii);
            }
            if (ii < 0) return 0;
            return ii;
        }

        Color[] lastColour = new Color[256];//(Color.Black);

        internal Color GetColor(int x, int y)
        {
            byte p = mMatrix[x, y];
            if (p == BLOCKED)
            {
                return BlockedColor(mWaypoints[x, y]);
            }

            // if (lastsb != null)
            {
                //if (p == lastColorByte) return lastsb;
            }
            Color sb = lastColour[p];
            if (sb == Color.Empty)
            {
                if (p == PASSABLE) return (Color.Yellow);
                if (p == STICKY_PASSABLE) return (Color.Green);
                int colorIndex = 240 - ((int)(Math.Log10(p) * 127));
                colorIndex = colorIndex < 0 ? 0 : colorIndex > 255 ? 255 : colorIndex;
                sb = Color.FromArgb(255, colorIndex, colorIndex, colorIndex);
                lastColour[p] = sb;
            }
            return sb;
        }

        private Color BlockedColor(SimWaypoint simWaypoint)
        {
            Color c = Color.Olive;
            if (simWaypoint != null)
            {
                int dense = simWaypoint.OccupiedCount;
                int A = 240 - 10 * dense;
                if (A < 0) A = 20;

                return Color.FromArgb(A, c.R, c.G, c.B);
            }
            return c;
        }


        public byte GetNodeQuality(Vector3 v3)
        {
            return mMatrix[ARRAY_IDX(RangeCheck(v3.X)), ARRAY_IDX(RangeCheck(v3.Y))];
        }

        public void SetNodeQuality(Vector3 v3, byte v)
        {
            mMatrix[ARRAY_IDX(RangeCheck(v3.X)), ARRAY_IDX(RangeCheck(v3.Y))] = v;
        }

        public void SetTraveled(float x, float y)
        {
            SetPassable(x, y);
        }

        public void SetObjectAt(float x, float y, SimObject simObject, float minZ, float maxZ)
        {
            int ix = ARRAY_IDX(RangeCheck(x));
            int iy = ARRAY_IDX(RangeCheck(y));

            SimWaypoint W = Waypoint(ix, iy);
            if (W.AddOccupied(simObject,minZ,maxZ))
            {
                if (mMatrix[ix, iy] > 9)
                {
                    {
                        if (mMatrix[ix, iy] < 30)
                            mMatrix[ix, iy] += 2;
                    }
                }
            }
        }

        public void SetPassable(float x, float y)
        {
            x = RangeCheck(x); y = RangeCheck(y);
            ///Debug("SetBlocked: {0} {1}", x, y);
            int ix = ARRAY_IDX(x);
            int iy = ARRAY_IDX(y);
            if (mMatrix[ix, iy] > 100)
            {
                return;
            }
            mMatrix[ix, iy] = STICKY_PASSABLE;
        }

        //static List<SimObject> NOOBJECTS = new List<SimObject>();
        //internal IEnumerable<SimObject> ObjectsAt(float x, float y)
        //{
        //    x = RangeCheck(x); y = RangeCheck(y);
        //    ///Debug("SetBlocked: {0} {1}", x, y);
        //    int ix = ARRAY_IDX(x);
        //    int iy = ARRAY_IDX(y);
        //    SimWaypoint P = mWaypoints[ix, iy];
        //    if (P == null)
        //    {
        //        return NOOBJECTS;
        //    }
        //    return P.OccupiedListObject;
        //}



        public void SetBlocked(float x, float y, SimObject blocker)
        {
            x = RangeCheck(x); y = RangeCheck(y);
            if (blocker != null) SetObjectAt(x, y, blocker,SimZAverage,SimZAverage+1f);

            int ix = ARRAY_IDX(x);
            int iy = ARRAY_IDX(y);
            ///Debug("SetBlocked: {0} {1}", x, y);
            // if was set Passable dont re-block
            if (mMatrix[ix, iy] == STICKY_PASSABLE)
            {
                //  return;
            }
            mMatrix[ix, iy] = 0;
         //   SetBubbleBlock(ix, iy, blocker);
        }

        //private void SetBubbleBlock(int ix, int iy, SimObject blocker)
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
        //                    if (mMatrix[x, y] != 0)
        //                    {
        //                        if (blocker != null)
        //                        {
        //                            SimWaypoint W = Waypoint(x, y);
        //                            if (W.AddShadow(blocker) > 1)
        //                            {
        //                                mMatrix[x, y] = 0;
        //                            }
        //                            else
        //                            {
        //                                int newV = mMatrix[x, y] * 2;
        //                                if (newV > 200) newV = 200;
        //                                mMatrix[x, y] = (byte)newV;
        //                            }
        //                        }
        //                    }
        //                }
        //            }
        //        }
        //    }
        //}
    
        SimRegion TheSimRegion;
        internal SimRegion GetSimRegion()
        {
            if (TheSimRegion == null)
            {
                TheSimRegion = SimRegion.GetRegion(Handle);
            }
            return TheSimRegion;
        }

        ulong Handle;
        /// <summary>
        /// Constructor.
        /// </summary>
        internal SimPathStore(String simName, ulong regionHandle)           
        {
            Handle = regionHandle;
            TheSimZMinMaxLevel = new SimZMinMaxLevel(MinMaxLevel);
            StepSize = 1f / POINTS_PER_METER;
            MAPSPACE = 256 * ((int)POINTS_PER_METER);
            LargeScaleRound = LargeScale == (float)Math.Round(LargeScale, 0);
            mMatrix = new byte[MAPSPACE, MAPSPACE];
            mWaypoints = new SimWaypoint[MAPSPACE, MAPSPACE];

            RegionFileName = simName;
            //CreateDefaultRoutes();
            for (int y = 0; y < mMatrix.GetUpperBound(1); y++)
                for (int x = 0; x < mMatrix.GetUpperBound(0); x++)
                {
                    mMatrix[x, y] = 10;
                    //  mWaypoints[x, y] = SimWaypoint.Create(x / POINTS_PER_METER, y / POINTS_PER_METER, SimZLevel, this);
                }
        }

        public float RangeCheck(double PtY)
        {
            if (PtY > 255)
            {
                return 255f;
            }
            else if (PtY < 0)
            {
                return 0f;
            }
            else
            {
                return (float)(Math.Round(PtY * POINTS_PER_METER, 0) / POINTS_PER_METER);
            }
        }

        private SimWaypoint FindNode(float x, float y)
        {
            return mWaypoints[ARRAY_IDX(RangeCheck(x)), ARRAY_IDX(RangeCheck(y))];
        }

        public SimWaypoint CreateFirstNode(float x, float y)
        {
            x = RangeCheck(x);
            y = RangeCheck(y);
            int ix = ARRAY_IDX(x);
            int iy = ARRAY_IDX(y);
            return Waypoint(ix, iy);
        }

        internal SimWaypoint Waypoint(int ix, int iy)
        {
            SimWaypoint wp = mWaypoints[ix, iy];
            if (wp == null)
            {
                float x = ix / POINTS_PER_METER;
                float y = iy / POINTS_PER_METER;
                wp = SimWaypoint.CreateLocal(x, y, SimLevel(x, y), this);
                mWaypoints[ix, iy] = wp;
            }
            return wp;
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


        public List<Vector3d> GetSimplifedRoute(Vector3d currentV3In, List<Vector3d> v3s, int MaxTurnDegrees, float MaxDist)
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
        public IList<Vector3d> GetLocalPath(Vector3 start, Vector3 end, PathFinderDemo panel)
        {
            if (!IsPassable(start))
            {
                throw new ArgumentException("start is not passable: " + start);
            }
            if (!IsPassable(end))
            {
                throw new ArgumentException("end is not passable: " + start);
            }
            PunishChangeDirection = !PunishChangeDirection;    //toggle each time
            Point S = ToPoint(start);
            Point E = ToPoint(end);
            List<PathFinderNode> pfn = null;
            try
            {
                PathFinderFast pff = new PathFinderFast(mMatrix);
                if (panel != null) panel.SetStartEnd(S, E);
                pff.Diagonals = false;
                //pff.ReopenCloseNodes = true;
                pff.SearchLimit = 10000000;
                pff.PunishChangeDirection = PunishChangeDirection;
                pfn = pff.FindPath(S, E);
            }
            catch (Exception e)
            {
                Debug("Cant do route! " +e);
            }
            if (pfn == null || pfn.Count == 0)
            {
                Debug("Cant do pfn!");
                List<Vector3d> temp = new List<Vector3d>();
                temp.Add(GetSimRegion().LocalToGlobal(end));
                return temp;
            }
            if (panel != null) panel.ShowPath(pfn);
            List<Vector3d> r = PathfinderNodesToV3s(pfn);
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



        private List<Vector3d> PathfinderNodesToV3s(List<PathFinderNode> pfn)
        {
            Vector3d V = GetGlobalCorner();
            List<Vector3d> v3s = new List<Vector3d>();
            foreach (PathFinderNode P in pfn)
            {
                float x = UNARRAY_IDX(P.X);
                float y = UNARRAY_IDX(P.Y);
                Vector3d v3 = new Vector3d(x + V.X, y+V.Y, SimLevel(x, y));
                v3s.Add(v3);
            }
            return GetSimplifedRoute(v3s[0],v3s,10,8f);
        }

        private Vector3d GetGlobalCorner()
        {
            return new Vector3d(GetSimRegion().LocalToGlobal(new Vector3(0, 0, 0)));
        }

        public Point ToPoint(Vector3 start)
        {
            return new Point(ARRAY_IDX(RangeCheck(start.X)), ARRAY_IDX(RangeCheck(start.Y)));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="point"></param>
        /// <param name="rotation"></param>
        public void Update(UUID agentID, Vector3 point, Quaternion rotation)
        {
            SetTraveled(point.X, point.Y);
        }


        public void UpdateFromImage(Image image)
        {
            if (image == null) return;
            Bitmap edges = new Bitmap((Image)image.Clone());
            new DisplayImage("Edges", edges).Activate();
            Debug("START Edge detection " + image);
            Bitmap e = EdgeDetection(edges, 34f, delegate(int x, int y)
            {
                edges.SetPixel(x, y, Color.Yellow);
                SetBlocked((float)x, (float)y, null);
            });
            Debug("END Edge detection");
            new DisplayImage("Clone", e).Activate();
        }


        public delegate void EdgeAction(int x, int y);

        public static Bitmap EdgeDetection(Bitmap Image, double Threshold, EdgeAction EdgeColor)
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
            Console.WriteLine("[SimPathStore] " + format, arg);
        }


        internal void TaintMatrix()
        {
            for (int x = 0; x < MAPSPACE; x++)
            {
                for (int y = 0; y < MAPSPACE; y++)
                {
                    SimWaypoint W = mWaypoints[x, y];
                    if (W != null)
                        W.TaintMatrix();
                }
            }
        }
        internal void UpdateMatrix()
        {
            TaintMatrix();
            System.GC.Collect();
            GetSimRegion().BakeTerrain();
            for (int x = 0; x < MAPSPACE; x++)
            {
                for (int y = 0; y < MAPSPACE; y++)
                {
                        SimWaypoint W = mWaypoints[x, y];
                        if (W != null)
                            W.UpdateMatrix();                    
                }
            }
            System.GC.Collect();
        }

        internal SimRoute InternArc(SimWaypoint StartNode, SimWaypoint EndNode, double Weight)
        {
            return SimGlobalRoutes.Instance.InternArc(StartNode,EndNode,Weight);
        }

        internal SimWaypoint CreateClosestRegionWaypoint(Vector3 v3, double maxDist)
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

        internal SimRoute Intern2Arc(SimWaypoint StartNode, SimWaypoint EndNode, double Weight)
        {
            return SimGlobalRoutes.Instance.Intern2Arc(StartNode, EndNode, Weight);
        }

        internal SimWaypoint ClosestRegionNode(float x, float y, float z, out double Dist, bool IgnorePassable)
        {
            Vector3d v3d = GetSimRegion().LocalToGlobal(new Vector3(x, y, z));
            return SimGlobalRoutes.Instance.ClosestNode(v3d.X, v3d.Y, v3d.Z, out Dist, IgnorePassable);
        }

        internal void Clear()
        {
            SimGlobalRoutes.Instance.Clear();
        }

        internal IList<SimRoute> GetRoute(SimWaypoint from, SimWaypoint to, out bool IsFake)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        internal void AddArc(SimRoute R)
        {
            SimGlobalRoutes.Instance.AddArc(R);
        }


        internal void SetNodeQualityTimer(Vector3 vector3, int value, int seconds)
        {
            SimPathStore PathStore = this;
            Point P = PathStore.ToPoint(vector3);
            Debug("SetNodeQualityTimer {0},{1} to {2}", P.X / PathStore.POINTS_PER_METER, P.Y / PathStore.POINTS_PER_METER,value);
            byte oldValue = PathStore.GetNodeQuality(vector3);
            if (oldValue == value) // aready blocked
                return;
            PathStore.SetNodeQuality(vector3, 0);
            new Thread(new ThreadStart(delegate()
            {
                Thread.Sleep(60000);
                byte newValue = PathStore.GetNodeQuality(vector3);
                if (newValue != value)
                {
                    // its been changed by something else since we set to Zero
                    Debug("SetNodeQualityTimer Thread out of date {0} value changed to {1}", vector3, newValue);
                }
                else
                {
                    PathStore.SetNodeQuality(vector3, oldValue);
                    Debug("ResetNodeQualityTimer {0} value reset to {1}", vector3, oldValue);
                }
            })).Start();

        }

        Dictionary<UUID, MoverTracking> LaskKnownPos = new Dictionary<UUID, MoverTracking>();

        internal void UpdateTraveled(UUID uUID, Vector3 after, Quaternion rot)
        {
            if (!LaskKnownPos.ContainsKey(uUID))
            {
                LaskKnownPos[uUID] = new MoverTracking(after, rot, this);
            }
            else
            {
                LaskKnownPos[uUID].Update(after, rot);
            }
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
                int stepsNeeded = (int)(dist * Store.POINTS_PER_METER)+1;
                Console.WriteLine("MakeMovement " + LastPosition + " -> " + stepsNeeded + " -> " + nextPosition);
                Vector3 vstep = dif / stepsNeeded;
                Vector3 traveled = nextPosition;
                Store.SetTraveled(nextPosition.X, nextPosition.Y);
                for (int i = 0; i < stepsNeeded; i++)
                {
                    traveled = traveled + vstep;
                    Store.SetTraveled(traveled.X, traveled.Y);
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
