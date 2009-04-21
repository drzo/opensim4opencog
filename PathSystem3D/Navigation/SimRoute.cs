// Copyleft 2009 Douglas R. Miles (Daxtron Labs) - <dmiles@daxtron.com>
// Copyright 2003 Eric Marchesin - <eric.marchesin@laposte.net>
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

namespace PathSystem3D.Navigation
{
    /// <summary>
    /// An arc is defined with its two extremity nodes StartNode and EndNode therefore it is oriented.
    /// It is also characterized by a crossing factor named 'Weight'.
    /// This value represents the difficulty to reach the ending node from the starting one.
    /// </summary>
    [Serializable]
    public class SimRoute
    {
        double _Weight = 1f;
        protected bool _Passable;
        double _Length;
        bool _LengthUpdated;
        static double StepSize = 0.2;

        internal SimWaypoint _StartNode, _EndNode;

        public int BumpyCount = 0;

        /// <summary>
        /// Arc constructor.
        /// </summary>
        /// <exception cref="ArgumentNullException">Extremity nodes cannot be null.</exception>
        /// <exception cref="ArgumentException">StartNode and EndNode must be different.</exception>
        /// <param name="Start">The node from which the arc starts.</param>
        /// <param name="End">The node to which the arc ends.</param>
        public SimRoute(SimWaypoint Start, SimWaypoint End)
        {
            StartNode = Start;
            EndNode = End;
            Weight = 1;
            LengthUpdated = false;
            _Passable = true;
        }

        /// <summary>
        /// Gets/Sets the node from which the arc starts.
        /// </summary>
        /// <exception cref="ArgumentNullException">StartNode cannot be set to null.</exception>
        /// <exception cref="ArgumentException">StartNode cannot be set to EndNode.</exception>
        public SimWaypoint StartNode
        {
            set
            {
                if (value == null) throw new ArgumentNullException("StartNode");
                if (EndNode != null && value.Equals(EndNode)) throw new ArgumentException("StartNode and EndNode must be different");
                if (_StartNode != null) _StartNode.OutgoingArcs.Remove(this);
                _StartNode = value;
                if (_EndNode != null) UpdateBounds();
                _StartNode.OutgoingArcs.Add(this);
            }
            get { return _StartNode; }
        }

        double minX, maxX, minY, maxY;

        internal void UpdateBounds()
        {
            Vector3d start = _StartNode.Position;
            Vector3d end = _EndNode.Position;
            minX = Math.Min(start.X, end.X);
            maxX = Math.Max(start.X, end.X);
            minY = Math.Min(start.Y, end.Y);
            maxY = Math.Max(start.Y, end.Y);
        }

        /// <summary>
        /// Gets/Sets the node to which the arc ends.
        /// </summary>
        /// <exception cref="ArgumentNullException">EndNode cannot be set to null.</exception>
        /// <exception cref="ArgumentException">EndNode cannot be set to StartNode.</exception>
        public SimWaypoint EndNode
        {
            set
            {
                if (value == null) throw new ArgumentNullException("EndNode");
                if (StartNode != null && value.Equals(StartNode))
                {
                    throw new ArgumentException("StartNode and EndNode must be different");
                }
                if (_EndNode != null) _EndNode.IncomingArcs.Remove(this);
                _EndNode = value;
                if (_StartNode != null) UpdateBounds();
                _EndNode.IncomingArcs.Add(this);
            }
            get { return _EndNode; }
        }

        /// <summary>
        /// Sets/Gets the weight of the arc.
        /// This value is used to determine the cost of moving through the arc.
        /// </summary>
        public double Weight
        {
            set { _Weight = value; }
            get { return _Weight; }
        }

        /// <summary>
        /// Gets/Sets the functional state of the arc.
        /// 'true' means that the arc is in its normal state.
        /// 'false' means that the arc will not be taken into account (as if it did not exist or if its cost were infinite).
        /// </summary>
        public bool Passable
        {
            set
            {
                _Passable = value;
                foreach (SimRoute R in Dependants)
                {
                    R._Passable = value;
                }
                Reverse._Passable = value;
            }
            get
            {
                return _Passable;
            }
        }

        public bool LengthUpdated
        {
            set
            {
                _LengthUpdated = value;
                if (!value)
                {
                    foreach (SimRoute R in Dependants)
                    {
                        R.LengthUpdated = value;
                    }
                }
            }
            get { return _LengthUpdated; }
        }

        /// <summary>
        /// Gets arc's length.
        /// </summary>
        public double Length
        {
            get
            {
                if (LengthUpdated == false)
                {
                    _Length = CalculateLength();
                    LengthUpdated = true;
                }
                return _Length;
            }
        }

        /// <summary>
        /// Performs the calculous that returns the arc's length
        /// Can be overriden for derived types of arcs that are not linear.
        /// </summary>
        /// <returns></returns>
        virtual protected double CalculateLength()
        {
            return _StartNode.Distance( _EndNode);
        }

        /// <summary>
        /// Gets the cost of moving through the arc.
        /// Can be overriden when not simply equals to Weight*Length.
        /// </summary>
        virtual public double Cost
        {
            get { return Weight * Length; }
        }

        /// <summary>
        /// Returns the textual description of the arc.
        /// object.ToString() override.
        /// </summary>
        /// <returns>String describing this arc.</returns>
        public override string ToString()
        {
            return ToInfoString();
        }

        /// <summary>
        /// Object.Equals override.
        /// Tells if two arcs are equal by comparing StartNode and EndNode.
        /// </summary>
        /// <exception cref="ArgumentException">Cannot compare an arc with another type.</exception>
        /// <param name="O">The arc to compare with.</param>
        /// <returns>'true' if both arcs are equal.</returns>
        public override bool Equals(object O)
        {
            SimRoute A = (SimRoute)O;
            if (A == null) throw new ArgumentException("Cannot compare type " + GetType() + " with type " + O.GetType() + " !");
            return _StartNode.Equals(A._StartNode) && _EndNode.Equals(A._EndNode);
        }

        /// <summary>
        /// Object.GetHashCode override.
        /// </summary>
        /// <returns>HashCode value.</returns>
        public override int GetHashCode() { return _StartNode.GetHashCode() ^ _EndNode.GetHashCode(); }
        //public override int GetHashCode() { return (int)Length; }

        //public static SimRoute PointsToMovement(IList<SimWaypoint> list, double fudge)
        //{
        //    SimRoute moveNow = new SimRoute(list[0], list[1]);
        //    if (list.Count > 2)
        //    {
        //        SimWaypoint Last = list[1];
        //        int listIndex = 2;
        //        while (listIndex < list.Count)
        //        {
        //            if (SimWaypoint.Distance(Last, list[listIndex]) > fudge)
        //                moveNow = moveNow.AppendPoint(list[listIndex], fudge);
        //            Last = list[listIndex++];
        //        }
        //    }
        //    return moveNow;
        //}

        bool MustFly = false;
        bool MustCrouch = false;

        public bool IsCrossRegion
        {
            get
            {
                return StartNode.GetPathStore() != EndNode.GetPathStore();
            }
        }

        //bool MustAutoPilot = false;
        public bool IsBlocked
        {
            get { return !Passable; }
            set { Passable = !value; }
        }
        //public bool IsOneDirrection = true;

        public static SimRouteMulti CopyProperties(SimRoute simMovement, SimRouteMulti movement)
        {
            return (SimRouteMulti)CopyProperties(simMovement, (SimRoute)movement);
        }

        public virtual IList<SimWaypoint> GetWayPoints(double apart, SimPathStore PathStore)
        {
            IList<SimWaypoint> points = new List<SimWaypoint>();
            double len = Length;
            double way = 0.0f;
            while (way < len)
            {
                points.Add(GetPointAt(way, PathStore));
                way += apart;
            }
            points.Add(EndNode);
            return points;
        }

        public static SimRoute CopyProperties(SimRoute simMovement, SimRoute movement)
        {
            //if (simMovement.MustAutoPilot) movement.MustAutoPilot = simMovement.MustAutoPilot;
            if (simMovement.MustCrouch) movement.MustCrouch = simMovement.MustCrouch;
            if (simMovement.MustFly) movement.MustFly = simMovement.MustFly;
            //if (simMovement.IsBlocked) movement.IsBlocked = simMovement.IsBlocked;
            //if (simMovement.IsOneDirrection) movement.IsOneDirrection = simMovement.IsOneDirrection;
            movement.Weight = simMovement.Weight;
            movement._Length = simMovement._Length;
            movement.LengthUpdated = simMovement.LengthUpdated;
            return movement;
        }

        //public SimRoute(String s)
        //{
        //    FromFileString(s);
        //}

        //public SimRoute(Vector3d from, Vector3d to)
        //    : this(SimWaypoint.Create(from), SimWaypoint.Create(to))
        //{
        //}

        public SimRoute _Reverse;
        public SimPathStore PathStore;
        public virtual SimRoute Reverse
        {
            get
            {
                if (_Reverse == null)
                {
                    _Reverse = PathStore.InternArc(EndNode, StartNode, Weight);
                    SimRoute.CopyProperties(this, _Reverse);
                    _Reverse._Reverse = this;
                    //movement.Cost = Cost;
                }
                return _Reverse;
            }
        }

        public virtual SimRoute FillIn(double maxDist)
        {
            return this;
        }

        public virtual SimRoute GetSegment(double start, double distlen, SimPathStore PathStore)
        {
            IList<SimRoute> newmoves = new List<SimRoute>();
            double len = Length;

            if (distlen <= 0.0 || start + distlen > len)
            {
                distlen = len - start;
            }
            SimWaypoint First = StartNode;
            SimWaypoint Last = EndNode;
            foreach (SimRoute move in GetSegments())
            {

                double mlen = move.Length;
                if (mlen > start)
                {
                    First = move.GetPointAt(start, PathStore);
                    if (distlen + start < mlen)
                    {
                        Last = move.GetPointAt(distlen + start, PathStore);
                        newmoves.Add(new SimRoute(First, EndNode));
                        break; // start and end in a single segment
                    }
                    Last = move.EndNode;
                    newmoves.Add(new SimRoute(First, EndNode));
                    distlen -= (mlen - start);
                    start = 0.0f;
                    continue; // have a start but need distlen more
                }
                if (start > 0)
                {
                    start -= mlen;
                    continue; // still scanning for start
                }
                else
                {
                    if (distlen > mlen)
                    {
                        distlen -= mlen;
                        newmoves.Add(move);
                        continue; // add whole segment and get distlen more
                    }
                    else
                    {
                        First = move.StartNode;
                        EndNode = move.GetPointAt(mlen, PathStore);
                        newmoves.Add(new SimRoute(First, EndNode));
                        break; // this completed it
                    }
                }
            }
            return new SimRouteMulti(newmoves);
        }

        public virtual SimWaypoint NextPoint(double start)
        {
            return EndNode;
        }

        public virtual SimRouteMulti Divide(int by, SimPathStore PathStore)
        {
            IList<SimRoute> moves = new List<SimRoute>();
            double len = Length;
            double seglen = len / by;
            SimWaypoint beg = StartNode;
            int current = 1;
            while (current < by)
            {
                SimWaypoint end = GetPointAt(seglen * current,  PathStore);
                SimRoute move = new SimRoute(beg, end);
                moves.Add(move);
                beg = end;
            }
            return CopyProperties(this, new SimRouteMulti(moves));
        }

        public virtual SimWaypoint GetPointAt(double p, SimPathStore PathStore)
        {
            double len = Length;
            if (p <= 0.0f) return StartNode;
            if (p >= len) return EndNode;
            Vector3d dir = EndNode.GetWorldPosition() - StartNode.GetWorldPosition();
            double X = (dir.X / len) * p;
            double Y = (dir.Y / len) * p;
            double Z = (dir.Z / len) * p;
            return SimWaypointImpl.CreateGlobal(new Vector3d(X, Y, Z));
        }

        public virtual IList<SimRoute> GetSegments()
        {
            IList<SimRoute> moves = new List<SimRoute>();
            moves.Add(this);
            return moves;
        }

        public virtual SimRouteMulti ToSegmentCopy()
        {
            IList<SimRoute> moves = new List<SimRoute>();
            moves.Add(this);
            return new SimRouteMulti(moves);
        }

        public virtual SimRouteMulti Append(SimRoute extra)
        {
            if (extra is SimRouteMulti)
            {
                return extra.Prepend(this);
            }
            IList<SimRoute> MS = new List<SimRoute>();
            MS.Add(this);
            MS.Add(extra);
            return new SimRouteMulti(MS);
        }

        public virtual SimRouteMulti Prepend(SimRoute extra)
        {
            if (extra is SimRouteMulti)
            {
                return extra.Append(this);
            }
            IList<SimRoute> MS = new List<SimRoute>();
            MS.Add(extra);
            MS.Add(this);
            return new SimRouteMulti(MS);
        }

        public virtual IList<SimWaypoint> GetPoints()
        {
            IList<SimWaypoint> points = new List<SimWaypoint>();
            points.Add(StartNode);
            points.Add(EndNode);
            return points;
        }

        public virtual SimRoute AppendPoint(SimWaypoint vector3, double fudge)
        {
            if (EndNode.Distance(vector3) < fudge)
            {
                return new SimRoute(StartNode, vector3);
            }
            return Append(new SimRoute(EndNode, vector3));
        }

        public virtual string ToInfoString()
        {
            string s = StartNode.GetWorldPosition().ToRawString() + " -> " + EndNode.GetWorldPosition().ToRawString() + " ";
            //if (MustAutoPilot) s += " MustAutoPilot";
            if (MustFly) s += " MustFly";
            if (MustCrouch) s += " MustCrouch";
            if (IsBlocked) s += " IsBlocked";
           // if (IsOneDirrection) s += " IsOneDirrection";
            return s;
        }

        //public void FromFileString(String s)
        //{
        //    string[] args = s.Split(null);
        //    StartNode = SimWaypoint.Create(new Vector3d(double.Parse(args[0]), double.Parse(args[1]), double.Parse(args[2])));
        //    EndNode = SimWaypoint.Create(new Vector3d(double.Parse(args[4]), double.Parse(args[5]), double.Parse(args[6])));
        //   // if (s.Contains("MustAutoPilot")) MustAutoPilot = true;
        //    if (s.Contains("MustFly")) MustFly = true;
        //    if (s.Contains("MustCrouch")) MustCrouch = true;
        //    if (s.Contains("IsBlocked")) IsBlocked = true;
        //   // if (s.Contains("IsOneDirrection")) IsOneDirrection = true;
        //}

        public virtual bool NearPoint(SimWaypoint e, double maxDist)
        {
            if (StartNode.Distance(e) < maxDist) return true;
            if (EndNode.Distance( e) < maxDist) return true;
            return false;
        }

        public bool IsSame(SimWaypoint s, SimWaypoint e)
        {
            return s == StartNode && e == EndNode;
        }

        public virtual void ReWeight(double p)
        {
            Weight = Weight * p;
        }

        /// <summary>
        /// Quick min/max check to decide if w/in bounds
        /// </summary>
        /// <param name="point"></param>
        /// <returns></returns>
        public virtual bool InRouteBox(Vector3d point)
        {
            double x = point.X, y = point.Y;
            if (x < minX || x > maxX || y < minY || y > maxY)
            {
                return false;
            }
            return true;
        }

        /// <summary>
        /// Check to see if this route contains the point
        /// </summary>
        /// <param name="point">point to be checked</param>
        /// <returns>return true when the point is on the route</returns>
        public virtual bool OnRoute(Vector3d point)
        {
            double x = point.X, y = point.Y;
            if (x < minX || x > maxX || y < minY || y > maxY)
            {
                return false; //outside box
            }
            if (minX == maxX || minY == maxY) return true;// vertical or horizontal
            return Distance(point) <= StepSize;
        }

        public virtual double Distance(Vector3d P)
        {
            if (minX == maxX || minY == maxY)// vertical or horizontal
            {
                if (InRouteBox(P)) // not outside the box
                    return 0;
            }
            Vector3d Projection = ProjectOnLine(P, _StartNode.Position, _EndNode.Position);
            return Vector3d.Distance(Projection, P);
        }

        /// <summary>
        /// Returns the projection of a point on the line defined with two other points.
        /// When the projection is out of the segment, then the closest extremity is returned.
        /// </summary>
        /// <exception cref="ArgumentNullException">None of the arguments can be null.</exception>
        /// <exception cref="ArgumentException">P1 and P2 must be different.</exception>
        /// <param name="Pt">Point to project.</param>
        /// <param name="P1">First point of the line.</param>
        /// <param name="P2">Second point of the line.</param>
        /// <returns>The projected point if it is on the segment / The closest extremity otherwise.</returns>
        public static Vector3d ProjectOnLine(Vector3d Pt, Vector3d P1, Vector3d P2)
        {
            if (Pt == P1 || Pt == P2) return Pt;
            //if (Pt == null || P1 == null || P2 == null) throw new ArgumentNullException("None of the arguments can be null.");
            if (P1.Equals(P2)) throw new ArgumentException("P1 and P2 must be different.");
            Vector3d VLine = MakeDiff(P1, P2);
            Vector3d V1Pt = MakeDiff(P1, Pt);
            Vector3d Translation = VLine * VectOR(VLine, V1Pt) / SquareNorm(VLine);
            Vector3d Projection = P1 + Translation;

            Vector3d V1Pjt = MakeDiff(P1, Projection);
            double D1 = VectOR(V1Pjt, VLine);
            if (D1 < 0) return P1;

            Vector3d V2Pjt = MakeDiff(P2, Projection);
            double D2 = VectOR(V2Pjt, VLine);
            if (D2 > 0) return P2;

            return Projection;
        }

        /// <summary>
        /// Scalar product between two vectors.
        /// </summary>
        /// <param name="V1">First vector.</param>
        /// <param name="V2">Second vector.</param>
        /// <returns>Value resulting from the scalar product.</returns>
        public static double VectOR(Vector3d V1, Vector3d V2)
        {
            double ScalarProduct = 0;
            ScalarProduct += V1.X * V2.X;
            ScalarProduct += V1.Y * V2.Y;
            ScalarProduct += V1.Z * V2.Z;
            return ScalarProduct;
        }

        /// <summary>
        /// Constructs a Vector3D with two points.
        /// </summary>
        /// <param name="P1">First point of the vector.</param>
        /// <param name="P2">Second point of the vector.</param>
        public static Vector3d MakeDiff(Vector3d P1, Vector3d P2)
        {
            Vector3d point = new Vector3d(0, 0, 0);
            point.X = P2.X - P1.X; point.Y = P2.Y - P1.Y; point.Z = P2.Z - P1.Z;
            return point;
        }

        /// <summary>
        ///  Gets the square norm of the vector.
        /// </summary>
        /// <param name="point">vector.</param>
        static public double SquareNorm(Vector3d point)
        {
            double Sum = 0;
            //               for (int i = 0; i < 3; i++) 
            Sum += point.X * point.X;
            Sum += point.Y * point.Y;
            Sum += point.Z * point.Z;
            return Sum;
        }
        /// <summary>
        /// When something is changed the Dependant Routes that must be updated
        /// </summary>
        protected ICollection<SimRoute> Dependants = new List<SimRoute>();

        /// <summary>
        ///  When something is changed the simRouteMovement that must be updated
        /// </summary>
        /// <param name="simRouteMovement"></param>
        public virtual void AddDependant(SimRoute simRouteMovement)
        {
            Dependants.Add(simRouteMovement);
        }

        /// <summary>
        /// Searches route(s) for the point
        /// </summary>
        /// <param name="point"></param>
        /// <returns>the route element with the point point</returns>
        public virtual SimRoute WhichRoute(Vector3d point)
        {
            if (OnRoute(point)) return this;
            return null;
        }

        /// <summary>
        /// If this route contains the point then the route is blocked
        /// </summary>
        /// <param name="point"></param>
        /// <returns>return true if an element was marked blocked</returns>
        public virtual bool BlockedPoint(Vector3d point)
        {
            if (OnRoute(point))
            {
                Passable = false;
                return true;
            }
            return false;
        }
    }
    /// <summary>
    /// A SimRoute containing other SimRoutes
    /// </summary>
    public class SimRouteMulti : SimRoute
    {
        IList<SimRoute> MoveList;
        public SimRouteMulti(IList<SimRoute> ms)
            : base(ms[0].StartNode, ms[ms.Count - 1].EndNode)
        {
            MoveList = ms;
            foreach (SimRoute R in MoveList) {
                R.AddDependant(this);
            }
        }

        public override void  ReWeight(double p)
        { 	
            foreach (SimRoute R in MoveList)
            {
            R.ReWeight(p);
            }
            Weight = p;
        }

        public override bool InRouteBox(Vector3d point)
        {
            foreach (SimRoute R in MoveList)
            {
                if (R.InRouteBox(point)) return true;
            }
            return false;
        }

        public override bool OnRoute(Vector3d point)
        {
            foreach (SimRoute R in MoveList)
            {
                if (R.OnRoute(point)) return true;
            }
            return false;
        }

        public override bool BlockedPoint(Vector3d point)
        {
            bool didBlock = false;
            foreach (SimRoute R in MoveList)
            {
                if (R.BlockedPoint(point))
                {
                    didBlock = true;
                }
            }
            if (didBlock) _Passable = false;
            return didBlock;
        }

        public override SimRoute WhichRoute(Vector3d point)
        {
            foreach (SimRoute R in MoveList)
            {
                SimRoute W = R.WhichRoute(point);
                if (W != null) return W;
            }
            return null;
        }

        public override double Distance(Vector3d P)
        {
            double result = double.MaxValue;
            foreach (SimRoute R in MoveList)
            {
                double temp = R.Distance(P);
                if (temp < result) result = temp;
            }
            return result;
        }

        public override bool NearPoint(SimWaypoint e, double maxDist)
        {
            if (StartNode.Distance(e) < maxDist) return true;
            if (EndNode.Distance(e) < maxDist) return true;
            foreach (SimRoute move in MoveList)
            {
                if (move.NearPoint(e, maxDist)) return true;
            }
            return false;
        }

        public override SimRoute Reverse
        {
            get
            {
                if (_Reverse == null)
                {
                    IList<SimRoute> ReverseMoveList = new List<SimRoute>();
                    //  SimRoute[] Rs = MoveList.ToArray();
                    int ri = MoveList.Count;// Length;
                    while (ri > 0)
                        ReverseMoveList.Add(MoveList[--ri].Reverse);
                    _Reverse = new SimRouteMulti(ReverseMoveList);
                    _Reverse._Reverse = this;
                    CopyProperties(this, _Reverse);
                }
                return _Reverse;
            }
        }

        public override IList<SimWaypoint> GetPoints()
        {
            return SimPathStore.RouteListToPoints(GetSegments());
        }

        public override SimRouteMulti ToSegmentCopy()
        {
            IList<SimRoute> moves = GetSegments();
            return new SimRouteMulti(moves);
        }

        public override SimRouteMulti Append(SimRoute extra)
        {
            IList<SimRoute> MS = new List<SimRoute>();
            MS.Add(this);
            MS.Add(extra);
            return new SimRouteMulti(MS);
        }

        public override SimRouteMulti Prepend(SimRoute extra)
        {
            IList<SimRoute> MS = new List<SimRoute>();
            MS.Add(extra);
            MS.Add(this);
            return new SimRouteMulti(MS);
        }


        public override SimRoute FillIn(double maxDist)
        {
            IList<SimRoute> moves = new List<SimRoute>();
            SimWaypoint at = StartNode;
            bool filled = false;
            foreach (SimRoute move in GetSegments())
            {
                if (at.Distance(move.StartNode) > maxDist)
                {
                    moves.Add(new SimRoute(at, move.StartNode));
                    filled = true;
                }
                moves.Add(move);
                at = move.EndNode;
            }
            if (filled) return new SimRouteMulti(moves);
            return this;
        }

        public override IList<SimRoute> GetSegments()
        {
            IList<SimRoute> moves = new List<SimRoute>();
            foreach (SimRoute move in MoveList)
            {
                foreach (SimRoute one in move.GetSegments())
                {
                    moves.Add(one);
                }
            }
            return moves;
        }

        public override SimWaypoint GetPointAt(double p, SimPathStore PathStore)
        {
            if (p <= 0.0f) return StartNode;
            foreach (SimRoute move in MoveList)
            {
                double mlen = move.Length;
                if (mlen > p) return move.GetPointAt(p, PathStore);
                p -= mlen;
            }
            return EndNode;
        }

        protected override double CalculateLength()
        {
            double len = 0f;
            foreach (SimRoute mv in MoveList)
            {
                len += mv.Length;
            }
            return len;
        }
    }
}
