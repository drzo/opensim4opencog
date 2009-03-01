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
    delegate void PassibleType(float x, float y);

    /// <summary>
    /// Graph structure. It is defined with :
    /// It is defined with both a list of nodes and a list of arcs.
    /// </summary>
    [Serializable]
    public class SimPathStore
    {
        public static bool OtherPathFinder = true;
        public float POINTS_PER_METER = 8f;
        public float LargeScale = 1f;//StepSize;//0.2f;
        public float SimZLevel = 22.25f;
        public float SimZHieght = 1f;
        public float SimZMaxLevel = 23.25f;

        public static float PI2 = (float)(Math.PI * 2f);
        public static float RAD2DEG = 360f / PI2;

        public float StepSize;// = 1f / POINTS_PER_METER;
        public int MAPSPACE;// = 256 * ((int)POINTS_PER_METER);
        public readonly bool LargeScaleRound;// = LargeScale == (float)Math.Round(LargeScale, 0);
        public readonly byte[,] mMatrix;// = new byte[MAPSPACE, MAPSPACE];
        public readonly SimWaypoint[,] mWaypoints;// = new SimWaypoint[MAPSPACE, MAPSPACE];
        IList<SimWaypoint> SimWaypoints;
        IList<SimRoute> SimRoutes;
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
            return ii;
        }

        Color[] lastColour = new Color[256];//(Color.Black);

        internal Color GetColor(int x, int y)
        {
            byte p = mMatrix[x, y];
            if (p == 0)
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
                if (p == 1) return (Color.White);
                if (p == 2) return (Color.Green);
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
                int dense = simWaypoint.OccupiedList.Count;
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

        //public void SetWieght(float fx, float fy, float ZeroToTwo)
        //{
        //    if (ZeroToTwo > 1.6) SetBlocked(fx, fy);
        //    else
        //    {
        //        int x = ARRAY_IDX(RangeCheck(fx));
        //        int y = ARRAY_IDX(RangeCheck(fy));
        //        mMatrix[x, y] = (byte)(1 + (int)Math.Round(ZeroToTwo / 2 * 254));
        //    }
        //}

        public void SetTraveled(float x, float y)
        {
            SetPassable(x, y);
        }

        public void SetObjectAt(float x, float y, SimObject simObject)
        {
            int ix = ARRAY_IDX(RangeCheck(x));
            int iy = ARRAY_IDX(RangeCheck(y));

            SimWaypoint W = Waypoint(ix, iy);
            if (W.AddOccupied(simObject))
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
            if (OtherPathFinder)
            {
                ///Debug("SetBlocked: {0} {1}", x, y);
                int ix = ARRAY_IDX(x);
                int iy = ARRAY_IDX(y);
                if (mMatrix[ix, iy] > 100)
                {
                    return;
                }
                mMatrix[ix, iy] = 2;
                //  PathFinder.SetWieght(ix, iy + 1, 0.8);
                //PathFinder.SetWieght(ix + 1, iy, 0.8);
                return;
            }
            float Dist;
            SimWaypoint waypoint = ClosestNode(x, y, SimZLevel, out Dist, false);
            if (Dist < 1f)
            {
                waypoint.EnsureAtLeastOnePath();

            }
        }



        public void SetBlocked(float x, float y, SimObject blocker)
        {
            SetObjectAt(x, y, blocker);
            //SetObjectAt(x, y, obj);
            x = RangeCheck(x); y = RangeCheck(y);
            if (OtherPathFinder)
            {
                int ix = ARRAY_IDX(x);
                int iy = ARRAY_IDX(y);
                ///Debug("SetBlocked: {0} {1}", x, y);
                // if was set Passable dont block
                if (mMatrix[ix, iy] == 2)
                {
                    //  return;
                }
                mMatrix[ix, iy] = 0;
                SetBubbleBlock(ix, iy, blocker);
                //PathFinder.SetBlocked(ix, iy + 1);
                // PathFinder.SetBlocked(ix, iy - 1);
                // PathFinder.SetWieght(ix + 1, iy, 1.5);
                return;
            }
            EnsureFillSmallSteps(x, y);
            //    if (LargeScaleRound) if (x != (float)Math.Round(x, 0) && y != (float)Math.Round(y, 0)) return;
            SimWaypoint point = mWaypoints[ARRAY_IDX(x), ARRAY_IDX(y)];
            if (point != null)
            {
                //lock (SimRoutes) foreach (SimRoute A in point.IncomingArcs)
                //    {
                //        if (A.Passable)
                //        {
                //            SimRoute R = A.Reverse;
                //            A.Passable = false;
                //            Debug("SetBlocked: {0}  the {1}", point, R);
                //            R.Passable = false;
                //        }
                //    }
                point.Passable = false;
                return;
            }
            Vector3 P = new Vector3(x, y, SimZLevel);
            lock (SimRoutes) foreach (SimRoute A in SimRoutes)
                {
                    if (A.BlockedPoint(P))
                    {
                        Debug("SetBlocked: {0}  the {1}", P, A);
                    }
                }
        }

        private void SetBubbleBlock(int ix, int iy, SimObject blocker)
        {
            int TWO = (int)Math.Round(POINTS_PER_METER / 2);
            if (ix > TWO && iy > TWO && ix < MAPSPACE - TWO && iy < MAPSPACE - TWO)
            {
                for (int x = ix - TWO; x < ix + TWO; x++)
                {
                    for (int y = iy - TWO; y < iy + TWO; y++)
                    {
                        if (x != ix || y != iy)
                        {
                            if (mMatrix[x, y] != 0)
                            {
                                if (blocker != null)
                                {
                                    SimWaypoint W = Waypoint(x, y);
                                    if (W.AddShadow(blocker) > 1)
                                    {
                                        mMatrix[x, y] = 0;
                                    }
                                    else
                                    {
                                        mMatrix[x, y] = (byte)(mMatrix[x, y] * 2);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }


        public void UpdateFromObject(SimObject obj)
        {
            obj.UpdatePaths(this);
        }


        public void EnsureFillSmallSteps(float x, float y)
        {
            if (OtherPathFinder)
            {
                return;
            }
            x = RangeCheck(x); y = RangeCheck(y);
            int lx = (int)(Math.Round(x * LargeScale) / LargeScale);
            int ly = (int)(Math.Round(y * LargeScale) / LargeScale);
            SimWaypoint point = mWaypoints[ARRAY_IDX(lx + StepSize + StepSize), ARRAY_IDX(ly + StepSize + StepSize)];
            if (point != null) return;
            Debug("EnsureFillSmallSteps: {0} {1}", x, y);
            float MX = lx + LargeScale;
            float MY = ly + LargeScale;
            float Weight = 1f;
            for (float xx = MX; xx >= x; xx -= StepSize)
            {
                for (float yy = MY; yy >= y; yy -= StepSize)
                {
                    /*

                     Draws two-way Routes StepSize meters appart

                             * - *  
                             |
                             * 

                    */
                    SimWaypoint C, S, E;
                    bool MC = false;
                    C = CreateFirstNode(xx, yy, out MC);
                    S = CreateFirstNode(xx, yy + StepSize);
                    E = CreateFirstNode(xx + StepSize, yy);
                    if (MC)
                    {
                        AddInitalArcsNoCheck(C, S, Weight);
                        AddInitalArcsNoCheck(C, E, Weight);
                    }
                    else
                    {
                        Intern2Arc(C, S, Weight); //dirrection  |
                        Intern2Arc(C, E, Weight); //dirrection  -
                    }
                }
            }
        }

        /// <summary>
        /// Constructor.
        /// </summary>
        internal SimPathStore(String simName)
        {
            StepSize = 1f / POINTS_PER_METER;
            MAPSPACE = 256 * ((int)POINTS_PER_METER);
            LargeScaleRound = LargeScale == (float)Math.Round(LargeScale, 0);
            mMatrix = new byte[MAPSPACE, MAPSPACE];
            mWaypoints = new SimWaypoint[MAPSPACE, MAPSPACE];

            RegionFileName = simName;
            SimWaypoints = new List<SimWaypoint>();
            SimRoutes = new List<SimRoute>();
            //CreateDefaultRoutes();
            for (int y = 0; y < mMatrix.GetUpperBound(1); y++)
                for (int x = 0; x < mMatrix.GetUpperBound(0); x++)
                {
                    mMatrix[x, y] = 10;
                    //  mWaypoints[x, y] = SimWaypoint.Create(x / POINTS_PER_METER, y / POINTS_PER_METER, SimZLevel, this);
                }
            LoadFromFile();
            //PathFinder.Show();
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

        ///// <summary>
        ///// Adds a grid of LargeScale
        ///// </summary>
        ///// <param name="LargeScale"></param>
        //public void CreateDefaultRoutes(float LargeScale)
        //{
        //    Debug("START CreateDefaultRoutes StepSize={0}", LargeScale);
        //    float MX = 255f - LargeScale - StepSize;
        //    float MY = 255f - LargeScale - StepSize;
        //    float Weight = 0.05f;
        //    for (float x = StepSize; x < MX; x += LargeScale)
        //    {
        //        for (float y = StepSize; y < MY; y += LargeScale)
        //        {
        //            /*

        //             Draws two-way Routes StepSize meters appart

        //                     * - *  
        //                     |
        //                     * 

        //            */
        //            SimWaypoint C, S, E;
        //            C = InternNode(x, y);
        //            S = InternNode(x, y + LargeScale);
        //            E = InternNode(x + LargeScale, y);
        //            AddInitalArcs(C, S, Weight); //dirrection  |
        //            AddInitalArcs(C, E, Weight); //dirrection  -
        //        }
        //    }
        //    Debug("END CreateDefaultRoutes StepSize={0}", LargeScale);
        //}

        public void CreateDefaultRoutes()
        {
            float ThisStepSize = LargeScale;// StepSize; //should be LargeScale;
            Debug("START CreateDefaultRoutes StepSize={0}", ThisStepSize);
            float MX = 255f - ThisStepSize - StepSize;
            float MY = 255f - ThisStepSize - StepSize;
            float Weight = 1f;
            for (float x = 0; x < MX; x += ThisStepSize)
            {
                for (float y = 0; y < MY; y += ThisStepSize)
                {
                    /*

                     Draws two-way Routes StepSize meters appart

                             * - *  
                             |
                             * 
                     
                    */
                    SimWaypoint C, S, E;
                    C = CreateFirstNode(x, y);
                    S = CreateFirstNode(x, y + ThisStepSize);
                    E = CreateFirstNode(x + ThisStepSize, y);
                    AddInitalArcsNoCheck(C, S, Weight); //dirrection  |
                    AddInitalArcsNoCheck(C, E, Weight); //dirrection  -
                }
            }
            Debug("END CreateDefaultRoutes StepSize={0}", ThisStepSize);
        }


        private SimWaypoint FindNode(float x, float y)
        {
            return mWaypoints[ARRAY_IDX(RangeCheck(x)), ARRAY_IDX(RangeCheck(y))];
        }

        //public SimWaypoint CreateFirstNode(float x, float y) {
        //    bool MadeIt;
        //    return CreateFirstNode(x, y, out MadeIt);
        //}

        public SimWaypoint CreateFirstNode(float x, float y)
        {
            x = RangeCheck(x);
            y = RangeCheck(y);
            int ix = ARRAY_IDX(x);
            int iy = ARRAY_IDX(y);
            return Waypoint(ix, iy);
        }

        private SimWaypoint Waypoint(int ix, int iy)
        {
            SimWaypoint wp = mWaypoints[ix, iy];
            if (wp == null)
            {
                wp = SimWaypoint.Create(ix / POINTS_PER_METER, iy / POINTS_PER_METER, SimZLevel, this);
                lock (SimWaypoints) SimWaypoints.Add(wp);
                mWaypoints[ix, iy] = wp;
            }
            return wp;
        }


        public SimWaypoint CreateFirstNode(float x, float y, out bool madeIt)
        {
            x = RangeCheck(x);
            y = RangeCheck(y);
            int ix = ARRAY_IDX(x);
            int iy = ARRAY_IDX(y);
            SimWaypoint wp = mWaypoints[ix, iy];
            if (wp == null)
            {
                wp = SimWaypoint.Create(x, y, SimZLevel, this);
                lock (SimWaypoints)
                    SimWaypoints.Add(wp);
                mWaypoints[ix, iy] = wp;
                madeIt = true;
            }
            else
            {
                madeIt = false;
            }
            return wp;
        }


        ///// <summary>
        /////  This Fills a LargeScale area with StepSize
        ///// </summary>
        ///// <param name="xx"></param>
        ///// <param name="yy"></param>
        ///// <returns></returns>
        //public SimWaypoint InternNodeFill(float xx, float yy)
        //{
        //    if (StepSize >= LargeScale) return CreateFirstNode(xx, yy);
        //    float MX = xx + LargeScale;
        //    float MY = yy + LargeScale;
        //    float Weight = 1f;
        //    for (float x = xx; x < MX; x += StepSize)
        //    {
        //        for (float y = yy; y < MY; y += StepSize)
        //        {
        //            SimWaypoint C = InternNode(x, y);
        //            SimWaypoint S = InternNode(x, y + StepSize);
        //            SimWaypoint E = InternNode(x + StepSize, y);
        //            /*

        //             Draws two-way Routes StepSize meters appart

        //                     * - *  
        //                     |
        //                     * 


        //            */
        //            AddInitalArcs(C, S, Weight); //dirrection  |
        //            AddInitalArcs(C, E, Weight); //dirrection  -
        //        }
        //    }
        //    return InternNode(xx, yy);
        //}

        ///// <summary>
        /////  Create 4 StepSizes around the Node
        ///// </summary>
        ///// <param name="x"></param>
        ///// <param name="y"></param>
        ///// <returns></returns>
        //public SimWaypoint InternNode(float x, float y)
        //{
        //    SimWaypoint C = CreateFirstNode(x, y);
        //    if (StepSize >= LargeScale) return C;
        //    float Weight = 1f;
        //    if (C.ArcCount() < 2)
        //    {
        //        SimWaypoint S = CreateFirstNode(x, y + StepSize);
        //        SimWaypoint E = CreateFirstNode(x + StepSize, y);
        //        SimWaypoint N = CreateFirstNode(x, y - StepSize);
        //        SimWaypoint W = CreateFirstNode(x - StepSize, y);
        //        //SimWaypoint sw11 = InternNode(x + StepSize, y + StepSize);
        //        /*

        //         Draws two-way Routes StepSize meters appart

        //                 * 
        //                 | 
        //             * - * - *  
        //                 |    
        //                 *   
        //        */
        //        AddInitalArcs(C, N, Weight); //dirrection  |
        //        AddInitalArcs(C, W, Weight); //dirrection  -
        //        AddInitalArcs(C, S, Weight); //dirrection  |
        //        AddInitalArcs(C, E, Weight); //dirrection  -
        //    }
        //    return C;
        //}

        private void AddInitalArcsNoCheck(SimWaypoint s, SimWaypoint e, float Weight)
        {
            if (s == e) return;
            SimRoute se, es;
            {
                se = new SimRoute(s, e);
                se.Weight = Weight;
                SimRoutes.Add(se);
            }
            {
                es = new SimRoute(e, s);
                SimRoutes.Add(es);
                es.Weight = Weight;
            }
            {
                es._Reverse = se;
                se._Reverse = es;
            }
        }

        public void AddInitalArcs(SimWaypoint s, SimWaypoint e, float Weight)
        {
            if (s == e) return;
            SimRoute se = null, es = null;
            if (!s.GoesTo(e))
            {
                se = new SimRoute(s, e);
                se.Weight = Weight;
                SimRoutes.Add(se);
            }
            if (!e.GoesTo(s))
            {
                es = new SimRoute(e, s);
                SimRoutes.Add(es);
                es.Weight = Weight;
            }
            if (es != null && se != null)
            {
                es._Reverse = se;
                se._Reverse = es;
            }
        }


        public SimRoute InternArc(SimWaypoint s, SimWaypoint e, float Weight)
        {
            if (s == e) throw new ArgumentException("s and e the same!" + s);
            SimRoute NewArc = FindArc(s, e);
            if (NewArc == null)
            {
                NewArc = new SimRoute(s, e);
                AddArc(NewArc);

            }
            NewArc.ReWeight(Weight);
            return NewArc;
        }

        public SimRoute Intern2Arc(SimWaypoint s, SimWaypoint e, float Weight)
        {
            InternArc(e, s, Weight);
            Debug("Intern2Arc: " + s + " <-> " + e);
            return InternArc(s, e, Weight);
        }

        //private SimRoute FindArc(SimWaypoint s, SimWaypoint e)
        //{
        //    int hash = s.GetHashCode() ^ e.GetHashCode(); 
        //    lock (SimRoutes) foreach (SimRoute R in SimRoutes.GetHashedCollection(hash))
        //        {
        //            if (R.IsSame(s, e)) return R;
        //        }
        //    return null;
        //}

        private SimRoute FindArc(SimWaypoint s, SimWaypoint e)
        {
            foreach (SimRoute R in s.OutgoingArcs)
            {
                if (R._EndNode == e) return R;
            }
            foreach (SimRoute R in e.IncomingArcs)
            {
                if (R._StartNode == s) return R;
            }
            //lock (SimRoutes) for (int i = SimRoutes.Count; i != 0; )
            //    {
            //        SimRoute sr = SimRoutes[--i];
            //        if (sr.IsSame(s, e))
            //        {
            //            return sr;
            //        }
            //    }
            return null;
        }


        public IList<SimRoute> GetSimplifedRoute(IList<SimRoute> v3s)
        {
            IList<SimRoute> vectors = new List<SimRoute>();
            List<SimRoute> skipped = new List<SimRoute>();
            float ZAngle = float.NaN;
            bool ZAngleValid = false;
            Vector3 currentV3 = v3s[0].StartNode.Position;
            vectors.Add(v3s[0]);
            for (int Current = 0; Current < v3s.Count; Current++)
            {
                Vector3 compare = v3s[Current].EndNode.Position;
                Vector3 dif = compare - currentV3;
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

        public IList<Vector3> GetSimplifedRoute(Vector3 currentV3, IList<Vector3> v3s, int MaxTurnDegrees, float MaxDist)
        {
            IList<Vector3> vectors = new List<Vector3>();
            float ZAngle = float.NaN;
            bool ZAngleValid = false;
            int Max = v3s.Count - 1;
            for (int Current = 0; Current < Max; Current++)
            {
                bool UsePoint = false;
                Vector3 compare = v3s[Current];
                Vector3 dif = compare - currentV3;
                float NewZAngle = ComparableAngle(Math.Atan2(dif.X, dif.Y));
                if (!ZAngleValid)
                {
                    ZAngleValid = true;
                    ZAngle = NewZAngle;
                }
                else
                {
                    float adif = Math.Abs(NewZAngle - ZAngle);
                    if (adif * RAD2DEG > MaxTurnDegrees)
                        UsePoint = true;
                    else
                        if (Vector3.Distance(currentV3, v3s[Current]) > MaxDist)
                            UsePoint = true;
                }
                if (UsePoint)
                {
                    vectors.Add(v3s[Current]);
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
        public IList<Vector3> GetV3Route(Vector3 start, Vector3 end)
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
            PathFinderFast pff = new PathFinderFast(mMatrix);
            //TODO            PathFinder.SetStartEnd(S, E);
            //pff.ReopenCloseNodes = true;
            pff.SearchLimit = 10000000;
            pff.PunishChangeDirection = PunishChangeDirection;
            //pff.PunishChangeDirection = true;            
            List<PathFinderNode> pfn = pff.FindPath(S, E);
            if (pfn == null || pfn.Count == 0)
            {
                List<Vector3> temp = new List<Vector3>();
                temp.Add(end);
                return temp;
            }
            //TODOPathFinder.ShowPath(pfn);
            return PathfinderNodesToV3s(pfn);
        }

        public bool IsPassable(Vector3 end)
        {
            float Dist;
            if (GetNodeQuality(end) == 0) return false;
            // if (true) return true;
            SimWaypoint W = ClosestNode(end.X, end.Y, end.Z, out Dist, true);
            return W.Passable;
        }

        private IList<Vector3> PathfinderNodesToV3s(List<PathFinderNode> pfn)
        {
            List<Vector3> v3s = new List<Vector3>();
            foreach (PathFinderNode P in pfn)
            {
                Vector3 v3 = new Vector3(UNARRAY_IDX(P.X), UNARRAY_IDX(P.Y), SimZLevel);
                v3s.Add(v3);
            }
            return v3s;// GetSimplifedRoute(v3s);
        }

        public Point ToPoint(Vector3 start)
        {
            return new Point(ARRAY_IDX(RangeCheck(start.X)), ARRAY_IDX(RangeCheck(start.Y)));
        }

        public IList<SimRoute> GetRoute(SimWaypoint StartNode, SimWaypoint EndNode, out bool IsFake)
        {
            SimMovement AS = new SimMovement(this);
            AS.Initialize(StartNode, EndNode);
            while (AS.NextStep()) { }
            if (AS.PathFound)
            {
                // Full Path
                IsFake = false;
                return //GetSimplifedRoute
                    (AS.PathByArcs);
            }
            // Partial Path
            IsFake = true;

            //int Nb = AS._LeafToGoBackUp.NbArcsVisited;
            //SimRoute[] Path = new SimRoute[Nb];
            //Track Cur = _LeafToGoBackUp;
            //for (int i = Nb - 1; i >= 0; i--, Cur = Cur.Queue)
            //    Path[i] = Cur.Queue.EndNode.ArcGoingTo(Cur.EndNode);
            //return Path;

            //AS.Open.Length, AS.Closed.Length, AS.StepCounter
            SimRoute[] PathByArcs = AS.PathByArcs;
            if (PathByArcs == null || PathByArcs.Length == 0)
            {
                return FakeRoute(StartNode, EndNode);
            }
            List<SimRoute> list = new List<SimRoute>();
            list.AddRange(PathByArcs);
            SimRoute LastArc = PathByArcs[PathByArcs.Length - 1];
            list.AddRange(FakeRoute(LastArc.EndNode, EndNode));
            return list.ToArray();
        }

        private SimRoute[] FakeRoute(SimWaypoint StartNode, SimWaypoint EndNode)
        {
            SimRoute[] route = new SimRoute[1];//
            SimRoute NewArc = Intern2Arc(StartNode, EndNode, 1.2f);
            //NewArc.Passable = true;
            route[0] = NewArc;
            return route;
        }

        /// <summary>
        /// Gets the List interface of the nodes in the graph.
        /// </summary>
        public ICollection<SimWaypoint> Nodes { get { return SimWaypoints; } }

        /// <summary>
        /// Gets the List interface of the arcs in the graph.
        /// </summary>
        public ICollection<SimRoute> Arcs { get { return SimRoutes; } }

        /// <summary>
        /// Empties the graph.
        /// </summary>
        public void Clear()
        {
            SimWaypoints.Clear();
            SimRoutes.Clear();
        }

        /// <summary>
        /// Directly Adds a node to the graph.
        /// </summary>
        /// <param name="NewNode">The node to add.</param>
        /// <returns>'true' if it has actually been added / 'false' if the node is null or if it is already in the graph.</returns>
        public bool AddNode(SimWaypoint NewNode)
        {
            Vector3 v3 = NewNode.Position;
            float x = RangeCheck(v3.X);
            float y = RangeCheck(v3.Y);
            float z = SimZLevel;
            SimWaypoint NodeMin = mWaypoints[ARRAY_IDX((x)), ARRAY_IDX((y))];
            if (NodeMin != null)
            {
                if (NewNode != NodeMin) throw new InvalidOperationException("Dup" + NewNode);
                return false;
            }
            lock (SimWaypoints) SimWaypoints.Add(NewNode);
            return true;
        }

        /// <summary>
        /// Creates a node, adds to the graph and returns its reference.
        /// </summary>
        /// <param name="x">X coordinate.</param>
        /// <param name="y">Y coordinate.</param>
        /// <param name="z">Z coordinate.</param>
        /// <returns>The reference of the new node / null if the node is already in the graph.</returns>
        public SimWaypoint AddNode(float x, float y, float z)
        {

            SimWaypoint NewNode = SimWaypoint.Create(x, y, z, this);
            lock (SimWaypoints) SimWaypoints.Add(NewNode);
            return NewNode;
        }

        /// <summary>
        /// Directly Adds an arc to the graph.
        /// </summary>
        /// <exception cref="ArgumentException">Cannot add an arc if one of its extremity nodes does not belong to the graph.</exception>
        /// <param name="NewArc">The arc to add.</param>
        /// <returns>'true' if it has actually been added / 'false' if the arc is null or if it is already in the graph.</returns>
        public bool AddArc(SimRoute NewArc)
        {
            if (NewArc == null || SimRoutes.Contains(NewArc)) return false;
            if (!SimWaypoints.Contains(NewArc.StartNode) || !SimWaypoints.Contains(NewArc.EndNode))
                throw new ArgumentException("Cannot add an arc if one of its extremity nodes does not belong to the graph.");
            lock (SimRoutes) SimRoutes.Add(NewArc);
            return true;
        }

        /// <summary>
        /// Creates an arc between two nodes that are already registered in the graph, adds it to the graph and returns its reference.
        /// </summary>
        /// <exception cref="ArgumentException">Cannot add an arc if one of its extremity nodes does not belong to the graph.</exception>
        /// <param name="StartNode">Start node for the arc.</param>
        /// <param name="EndNode">End node for the arc.</param>
        /// <param name="Weight">Weight for the arc.</param>
        /// <returns>The reference of the new arc / null if the arc is already in the graph.</returns>
        public SimRoute AddArc(SimWaypoint StartNode, SimWaypoint EndNode, float Weight)
        {
            SimRoute NewArc = FindArc(StartNode, EndNode);
            if (NewArc == null)
            {
                NewArc = new SimRoute(StartNode, EndNode);
                NewArc.Weight = Weight;
                return AddArc(NewArc) ? NewArc : null;
            }
            else
            {
                NewArc.Weight = Weight;
                return AddArc(NewArc) ? NewArc : null;
            }
            ///SimRoute NewArc = new SimRoute(StartNode, EndNode);
        }

        /// <summary>
        /// Adds the two opposite arcs between both specified nodes to the graph.
        /// </summary>
        /// <exception cref="ArgumentException">Cannot add an arc if one of its extremity nodes does not belong to the graph.</exception>
        /// <param name="Node1"></param>
        /// <param name="Node2"></param>
        /// <param name="Weight"></param>
        public void Add2Arcs(SimWaypoint Node1, SimWaypoint Node2, float Weight)
        {
            AddArc(Node1, Node2, Weight);
            AddArc(Node2, Node1, Weight);
        }

        /// <summary>
        /// Removes a node from the graph as well as the linked arcs.
        /// </summary>
        /// <param name="NodeToRemove">The node to remove.</param>
        /// <returns>'true' if succeeded / 'false' otherwise.</returns>
        public bool RemoveNode(SimWaypoint NodeToRemove)
        {
            if (NodeToRemove == null) return false;
            try
            {
                foreach (SimRoute A in NodeToRemove.IncomingArcs)
                {
                    lock (A.StartNode.OutgoingArcs) A.StartNode.OutgoingArcs.Remove(A);
                    lock (SimRoutes) SimRoutes.Remove(A);
                }
                foreach (SimRoute A in NodeToRemove.OutgoingArcs)
                {
                    lock (A.EndNode.IncomingArcs) A.EndNode.IncomingArcs.Remove(A);
                    lock (SimRoutes) SimRoutes.Remove(A);
                }
                lock (SimWaypoints) SimWaypoints.Remove(NodeToRemove);
            }
            catch { return false; }
            return true;
        }

        /// <summary>
        /// Removes a node from the graph as well as the linked arcs.
        /// </summary>
        /// <param name="ArcToRemove">The arc to remove.</param>
        /// <returns>'true' if succeeded / 'false' otherwise.</returns>
        public bool RemoveArc(SimRoute ArcToRemove)
        {
            if (ArcToRemove == null) return false;
            try
            {
                lock (SimRoutes) SimRoutes.Remove(ArcToRemove);
                lock (ArcToRemove.StartNode.OutgoingArcs) ArcToRemove.StartNode.OutgoingArcs.Remove(ArcToRemove);
                lock (ArcToRemove.StartNode.IncomingArcs) ArcToRemove.EndNode.IncomingArcs.Remove(ArcToRemove);
            }
            catch { return false; }
            return true;
        }

        /// <summary>
        /// Determines the bounding box of the entire graph.
        /// </summary>
        /// <exception cref="InvalidOperationException">Impossible to determine the bounding box for this graph.</exception>
        /// <param name="MinPoint">The point of minimal coordinates for the box.</param>
        /// <param name="MaxPoint">The point of maximal coordinates for the box.</param>
        public void BoundingBox(out float[] MinPoint, out float[] MaxPoint)
        {
            try
            {
                SimWaypoint.BoundingBox(Nodes, out MinPoint, out MaxPoint);
            }
            catch (ArgumentException e)
            { throw new InvalidOperationException("Impossible to determine the bounding box for this graph.\n", e); }
        }

        /// <summary>
        /// This function will find the closest node from a geographical position in space.
        /// </summary>
        /// <param name="PtX">X coordinate of the point from which you want the closest node.</param>
        /// <param name="PtY">Y coordinate of the point from which you want the closest node.</param>
        /// <param name="PtZ">Z coordinate of the point from which you want the closest node.</param>
        /// <param name="Distance">The distance to the closest node.</param>
        /// <param name="IgnorePassableProperty">if 'false', then nodes whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The closest node that has been found.</returns>
        public SimWaypoint ClosestNode(float PtX, float PtY, float PtZ, out float Distance, bool IgnorePassableProperty)
        {
            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            SimWaypoint NodeMin = mWaypoints[ARRAY_IDX(PtX), ARRAY_IDX(PtY)];
            if (NodeMin != null)
            {
                if (IgnorePassableProperty || NodeMin.Passable)
                {
                    Distance = 0;
                    return NodeMin;
                }
            }
            float DistanceMin = -1;
            PtZ = SimZLevel;
            Vector3 P = new Vector3(PtX, PtY, PtZ);


            lock (SimWaypoints) foreach (SimWaypoint N in SimWaypoints)
                {
                    if (IgnorePassableProperty && N.Passable == false) continue;
                    float DistanceTemp = N.Distance(P);
                    if (DistanceMin == -1 || DistanceMin > DistanceTemp)
                    {
                        DistanceMin = DistanceTemp;
                        NodeMin = N;
                    }
                }
            Distance = DistanceMin;
            
            
            return NodeMin;
        }

        /// <summary>
        /// This function will find the nodes from a geographical position in space.
        /// </summary>
        /// <param name="P">Waypoint node.</param>
        /// <param name="MinDistance">The min distance to the node.</param>
        /// <param name="MaxDistance">The max distance to the node.</param>
        /// <param name="IgnorePassableProperty">if 'false', then nodes whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The nodes that has been found.</returns>
        public IList<SimWaypoint> ClosestNodes(float PtX, float PtY, float PtZ, float DistanceMin, float DistanceMax, bool IgnorePassableProperty)
        {
            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            PtZ = SimZLevel;
            Vector3 P = new Vector3(PtX, PtY, PtZ);
            List<SimWaypoint> waypoints = new List<SimWaypoint>();
            lock (SimWaypoints) foreach (SimWaypoint N in SimWaypoints)
                {
                    //if (P == N) continue;
                    if (IgnorePassableProperty && N.Passable == false) continue;
                    float Distance = N.Distance(P);
                    if (Distance < DistanceMin || DistanceMax < Distance) continue;
                    waypoints.Add(N);
                }
            return waypoints;
        }

        /// <summary>
        /// This function will find the routes from a geographical position in space.
        /// </summary>
        /// <param name="P">Waypoint route.</param>
        /// <param name="MinDistance">The min distance to the route.</param>
        /// <param name="MaxDistance">The max distance to the route.</param>
        /// <param name="IgnorePassableProperty">if 'false', then routes whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The routes that has been found.</returns>
        public IList<SimRoute> ClosestArcs(float PtX, float PtY, float PtZ, float DistanceMin, float DistanceMax, bool IgnorePassableProperty)
        {
            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            PtZ = SimZLevel;
            Vector3 P = new Vector3(PtX, PtY, PtZ);
            List<SimRoute> waypoints = new List<SimRoute>();
            lock (SimWaypoints) foreach (SimRoute N in SimRoutes)
                {
                    //if (P == N) continue;
                    if (IgnorePassableProperty && N.Passable == false) continue;
                    float Distance = N.Distance(P);
                    if (Distance < DistanceMin || DistanceMax < Distance) continue;
                    waypoints.Add(N);
                }
            return waypoints;
        }


        /// <summary>
        /// This function will find the closest arc from a geographical position in space using projection.
        /// </summary>
        /// <param name="PtX">X coordinate of the point from which you want the closest arc.</param>
        /// <param name="PtY">Y coordinate of the point from which you want the closest arc.</param>
        /// <param name="PtZ">Z coordinate of the point from which you want the closest arc.</param>
        /// <param name="Distance">The distance to the closest arc.</param>
        /// <param name="IgnorePassableProperty">if 'false', then arcs whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The closest arc that has been found.</returns>
        public SimRoute ClosestArc(float PtX, float PtY, float PtZ, out float Distance, bool IgnorePassableProperty)
        {
            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            PtZ = SimZLevel;

            SimRoute ArcMin = null;
            float DistanceMin = -1;
            Vector3 P = new Vector3(PtX, PtY, PtZ);
            lock (SimRoutes) foreach (SimRoute A in SimRoutes)
                {
                    if (IgnorePassableProperty && A.Passable == false) continue;
                    float DistanceTemp = A.Distance(P);//s, Projection);
                    if (DistanceMin == -1 || DistanceMin > DistanceTemp)
                    {
                        DistanceMin = DistanceTemp;
                        ArcMin = A;
                    }
                }
            Distance = DistanceMin;
            return ArcMin;
        }

        Dictionary<uint, PrimTracker> TrackedAgents = new Dictionary<uint, PrimTracker>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="point"></param>
        /// <param name="rotation"></param>
        public void Update(uint agentID, Vector3 point, Quaternion rotation)
        {
            if (OtherPathFinder)
            {
                SetTraveled(point.X, point.Y);
                return;
            }
            lock (TrackedAgents) // the lock is more for the little PrimTrackers sequentuality than for this dictionary
            {
                // return;
                if (!TrackedAgents.ContainsKey(agentID))
                {
                    TrackedAgents[agentID] = new PrimTracker(SimWaypoint.Create(point, this), "agentID" + agentID, rotation, this);
                }
                else
                {
                    PrimTracker tracker = TrackedAgents[agentID];
                    tracker.Update(point, rotation);
                }
            }
        }

        //public SimRoute GetBeginToEndMovement(Vector3 begin, Vector3 end, float maxDist)
        //{
        //    SimRoute b = GetBeginMovement(begin, maxDist);
        //    SimRoute e = GetEndMovement(end, maxDist);
        //    if (b == null || e == null) return null;
        //    SimRoute m = new SimRoute(b.EndNode, e.StartNode);
        //    List<SimRoute> list = new List<SimRoute>();
        //    list.Add(b);
        //    list.Add(m);
        //    list.Add(e);
        //    return new SimRouteMovement(list);
        //}

        //public SimRoute GetBeginMovement(Vector3 vect, float maxDist)
        //{
        //    SimRoute bestBegin = null;
        //    float distToBegin = maxDist;
        //    foreach (SimRoute move in SimRoutes)
        //    {
        //        float close = Vector3.Distance(vect, move.StartNode.GetSimPosition());
        //        if (close < distToBegin)
        //        {
        //            bestBegin = move;
        //            distToBegin = close;
        //        }
        //        else
        //        {
        //            if (!move.IsOneDirrection)
        //            {
        //                close = Vector3.Distance(vect, move.EndNode.GetSimPosition());
        //                if (close < distToBegin)
        //                {
        //                    bestBegin = move.Reverse();
        //                    distToBegin = close;
        //                }
        //            }
        //        }
        //    }
        //    return bestBegin;
        //}

        //public SimRoute GetEndMovement(Vector3 vect, float maxDist)
        //{
        //    SimRoute bestEnd = null;
        //    float distToEnd = maxDist;
        //    foreach (SimRoute move in SimRoutes)
        //    {
        //        float close = Vector3.Distance(vect, move.EndNode.GetSimPosition());
        //        if (close < distToEnd)
        //        {
        //            bestEnd = move;
        //            distToEnd = close;
        //        }
        //        else
        //        {
        //            if (!move.IsOneDirrection)
        //            {
        //                close = Vector3.Distance(vect, move.StartNode.GetSimPosition());
        //                if (close < distToEnd)
        //                {
        //                    bestEnd = move.Reverse();
        //                    distToEnd = close;
        //                }
        //            }
        //        }
        //    }
        //    return bestEnd;
        //}

        //public SimPathStore(string regionFileName)
        //{
        //    RegionFileName = regionFileName;
        //    LoadFromFile();
        //}
        /// <summary>
        /// 
        /// </summary>
        void SaveToFile()
        {
            FileInfo save = new FileInfo(RegionFileName);
            if (save.Exists)
            {
                save.Delete();
            }
            SaveFile(RegionFileName);
            //FileStream stream = save.Open(FileMode.OpenOrCreate, FileAccess.Write);
            //StreamWriter sw = new StreamWriter(stream);
            //foreach (SimRoute sm in SimRoutes)
            //{
            //    sw.WriteLine(sm.ToFileString());
            //}
            //sw.Close();
        }
        /// <summary>
        /// 
        /// </summary>
        void LoadFromFile()
        {
            FileInfo read = new FileInfo(RegionFileName);
            if (!read.Exists)
            {
                Logger.Log("Not loading file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Info);
                return;
            }
            //FileStream stream = read.Open(FileMode.Open, FileAccess.Read);
            //TextReader tr = new StreamReader(stream);
            //while (tr.Peek() != -1)
            //{
            //    string s = tr.ReadLine();
            //    LoadFromLine(s);
            //}
            //tr.Close();
            LoadFile(RegionFileName);
        }

        //private void LoadFromLine(string s)
        //{
        //    SimRoute sm = new SimRoute(s);
        //    AddArc(sm);
        //}


        //public static void EnsureKnown(SimWaypoint wp)
        //{
        //    if (Instance.SimWaypoints.Contains(wp)) return;
        //    Instance.SimWaypointsAdd(wp);
        //}
        /// <summary>
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public bool SaveFile(string filename)
        {
            FileStream StreamWrite = (new FileInfo(filename)).OpenWrite();
            if (StreamWrite != null)
            {
                BinaryFormatter BinaryWrite = new BinaryFormatter();
                BinaryWrite.Serialize(StreamWrite, this);
                StreamWrite.Close();
                Logger.Log("Success saving file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Info);
                return true;
            }
            Logger.Log("Error saving file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Error);
            return false;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="pathName"></param>
        /// <returns></returns>
        public bool LoadFile(string pathName)
        {
            Stream StreamRead = (new FileInfo(pathName)).OpenRead();
            if (StreamRead != null)
            {
                BinaryFormatter BinaryRead = new BinaryFormatter();
                SimPathStore G = (SimPathStore)BinaryRead.Deserialize(StreamRead);
                StreamRead.Close();
                Clear();
                //   RegionFileName = pathName;
                SimRoutes = G.SimRoutes;
                SimWaypoints = G.SimWaypoints;
                Logger.Log("Loaded file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Info);
                return true;
            }
            Logger.Log("Error loading file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Error);
            return false;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public SimWaypoint CreateClosestWaypoint(Vector3 v3)
        {
            EnsureFillSmallSteps(v3.X, v3.Y);
            float Dist;
            bool Made = false;
            SimWaypoint Closest = ClosestNode(v3.X, v3.Y, SimZLevel, out Dist, true);
            if (Dist > LargeScale / 2)
            {
                Dist = StepSize;
                Made = true;
                Closest = CreateFirstNode(v3.X, v3.Y);
            }
            else
            {
                Closest.EnsureAtLeastOnePath();
                if (Closest.OutgoingArcs.Count > 1) return Closest;
                if (Closest.IncomingArcs.Count > 1) return Closest;
            }

            if (OtherPathFinder) return Closest;

            IList<SimWaypoint> more = new List<SimWaypoint>();

            float Biggest = LargeScale;
            while (more.Count < 4)
            {
                more = ClosestNodes(v3.X, v3.Y, SimZLevel, Dist, Biggest, false);
                Biggest += StepSize;
            }

            foreach (SimWaypoint P in more)
            {
                if (P != Closest)
                    if (Made)
                    {
                        SimRoute route = new SimRoute(P, Closest);
                        route.Weight = 1.1f;
                        lock (SimRoutes)
                        {
                            SimRoutes.Add(route);
                            route = route.Reverse;
                            route.Weight = 1.2f;
                            SimRoutes.Add(route);
                        }
                    }
                    else Intern2Arc(P, Closest, 1.2f);
            }
            if (Closest.EnsureAtLeastOnePath())
            {
                Debug("EnsureAtLeastOnePath {0}", Closest);
            }
            return Closest;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="v3"></param>
        /// <param name="radius"></param>
        /// <param name="numPoints"></param>
        /// <param name="Weight"></param>
        /// <returns></returns>
        public SimWaypoint CreateClosestWaypointBoxHistorical(Vector3 v3, float radius, int numPoints, float Weight)
        {
            SimWaypoint node = SimWaypoint.Create(v3, this);
            double radiansStep = PI2 / numPoints;
            SimWaypoint Last = node;
            Dictionary<SimWaypoint, List<SimWaypoint>> newWaypoints = new Dictionary<SimWaypoint, List<SimWaypoint>>();
            for (int Step = 0; Step < numPoints; Step++)
            {
                double ThisAngle = Step * radiansStep;
                Vector3 vectNew = (new Vector3((float)Math.Cos(ThisAngle), (float)Math.Sin(ThisAngle), 0) * radius) + v3;
                SimWaypoint nodeNew = SimWaypoint.Create(v3, this);
                List<SimWaypoint> closeNodes = new List<SimWaypoint>();
                newWaypoints[nodeNew] = closeNodes;
                float Dist;
                closeNodes.Add(node);
                closeNodes.Add(Last);
                closeNodes.Add(ClosestNode(vectNew.X, vectNew.Y, vectNew.Z, out Dist, false));
                Last = nodeNew;

            }
            foreach (SimWaypoint P in newWaypoints.Keys)
            {
                AddNode(P);
                foreach (SimWaypoint V in newWaypoints[P])
                {
                    if (V == null) continue;
                    AddNode(V);
                    if (P != V) Intern2Arc(P, V, Weight);
                }
            }
            return node;
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

        public SimWaypoint CreateTiedNode(Vector3 WayPoint)
        {
            float Dist;
            SimWaypoint Closest = ClosestNode(WayPoint.X, WayPoint.Y, WayPoint.Z, out Dist, true);
            if (Dist > StepSize)
            {
                SimWaypoint P = CreateFirstNode(WayPoint.X, WayPoint.Y);
                SimRoute route = new SimRoute(P, Closest);
                route.Weight = 1.0f;
                lock (SimRoutes)
                {
                    SimRoutes.Add(route);
                    route = route.Reverse;
                    route.Weight = 1.0f;
                    SimRoutes.Add(route);
                }
                Closest = P;
            }
            Closest.EnsureAtLeastOnePath();
            return Closest;
        }

        internal void CleanUnblocked()
        {
            for (int x = 0; x < MAPSPACE; x++)
            {
                for (int y = 0; y < MAPSPACE; y++)
                {
                    if (mMatrix[x, y] == 0)
                    {
                        SimWaypoint W = mWaypoints[x, y];
                        if (W != null)
                        {
                            if (W.NoObjectsBlock())
                            {
                                mMatrix[x, y] = 30;
                            }
                        }
                    }
                }
            }
        }
    }

    public class PrimTracker
    {
        protected float MovedAllot;// = SimPathStore.LargeScale;
        SimWaypoint WayPoint;
        Quaternion Orientation;
        SimPathStore Store;
        Queue<Vector3> Positions = new Queue<Vector3>();
        Thread RouteMaker;
        public PrimTracker(SimPosition firstP, String name, Quaternion firtsR, SimPathStore store)
        {
            Store = store;
            MovedAllot = store.LargeScale;
            WayPoint = Store.CreateTiedNode(firstP.GetSimPosition());
            Orientation = firtsR;
            RouteMaker = new Thread(new ThreadStart(MakeRoutes));
            RouteMaker.Name = "RouteMaker for " + name;
            RouteMaker.Priority = ThreadPriority.Lowest;
            RouteMaker.Start();
        }

        void MakeRoutes()
        {
            while (true)
            {
                Vector3 next;
                lock (Positions)
                {
                    if (Positions.Count == 0)
                    {
                        Thread.Sleep(2000);
                        continue;
                    }
                    //  Thread.Sleep(500);
                    next = Positions.Dequeue();
                }
                // if (next != null)                 
                MakeMovement(next);
            }
        }
        public void Update(Vector3 point, Quaternion rotation)
        {
            return;
            float dist = Vector3.Distance(WayPoint.Position, point);
            if (dist > MovedAllot)
            {
                lock (Positions) Positions.Enqueue(point);
            }
            else
                if (RotationDiffernt(rotation, Orientation))
                {
                    lock (Positions) Positions.Enqueue(point);
                    //  MakeMovement(point);
                    Orientation = rotation;
                }
        }


        public void MakeMovement(Vector3 point)
        {
            if (Vector3.Distance(WayPoint.Position, point) > 8f)
            {
                Console.WriteLine("SKIP WAYPOINT " + WayPoint + " -> " + point);
                WayPoint = Store.CreateTiedNode(point);
            }
            if (Vector3.Distance(WayPoint.Position, point) > MovedAllot)
            {
                Console.WriteLine("WAYPOINT " + WayPoint + " -> " + point);
                SimWaypoint tieIn1 = WayPoint;
                SimWaypoint tieIn2 = Store.CreateTiedNode(point);
                if (tieIn1 == tieIn2) return;
                Store.Intern2Arc(tieIn1, tieIn2, 0.01f); //Cheap
                WayPoint = tieIn2;
            }
        }

        static bool RotationDiffernt(Quaternion rotation, Quaternion Orientation)
        {
            Quaternion diff = rotation - Orientation;
            return (diff.Length() > 0.2);
        }
    }
}
