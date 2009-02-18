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

namespace cogbot.TheOpenSims.Navigation
{
    /// <summary>
    /// An arc is defined with its two extremity nodes StartNode and EndNode therefore it is oriented.
    /// It is also characterized by a crossing factor named 'Weight'.
    /// This value represents the difficulty to reach the ending node from the starting one.
    /// </summary>
    [Serializable]
    public class SimRoute
    {
        SimWaypoint _StartNode, _EndNode;
        float _Weight = 1f;
        bool _Passable;
        float _Length;
        bool _LengthUpdated;

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
            Passable = true;
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
                if (EndNode != null) UpdateBounds();
                _StartNode.OutgoingArcs.Add(this);
            }
            get { return _StartNode; }
        }

        float minX;
        float maxX;
        float minY;
        float maxY;

        private void UpdateBounds()
        {
            Vector3 start = _StartNode.Position;
            Vector3 end = _EndNode.Position;
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
        public float Weight
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
            set { _Passable = value; }
            get { return _Passable; }
        }

        internal bool LengthUpdated
        {
            set { _LengthUpdated = value; }
            get { return _LengthUpdated; }
        }

        /// <summary>
        /// Gets arc's length.
        /// </summary>
        public float Length
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
        virtual protected float CalculateLength()
        {
            return SimWaypoint.Distance(_StartNode, _EndNode);
        }

        /// <summary>
        /// Gets the cost of moving through the arc.
        /// Can be overriden when not simply equals to Weight*Length.
        /// </summary>
        virtual public float Cost
        {
            get { return Weight * Length ; }
        }

        /// <summary>
        /// Returns the textual description of the arc.
        /// object.ToString() override.
        /// </summary>
        /// <returns>String describing this arc.</returns>
        public override string ToString()
        {
            return ToFileString();
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
        public override int GetHashCode() { return (int)Length; }

        //public static SimRoute PointsToMovement(List<SimWaypoint> list, float fudge)
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
        bool MustAutoPilot = false;
        public bool IsBlocked
        {
            get { return !Passable; }
            set { Passable = !value; }
        }
        public bool IsOneDirrection = true;

        public static SimRouteMovement CopyProperties(SimRoute simMovement, SimRouteMovement movement)
        {
            return (SimRouteMovement)CopyProperties(simMovement, (SimRoute)movement);
        }

        public virtual List<SimWaypoint> GetWayPoints(float apart)
        {
            List<SimWaypoint> points = new List<SimWaypoint>();
            float len = Length;
            float way = 0.0f;
            while (way < len)
            {
                points.Add(GetPointAt(way));
                way += apart;
            }
            points.Add(EndNode);
            return points;
        }

        public static SimRoute CopyProperties(SimRoute simMovement, SimRoute movement)
        {
            if (simMovement.MustAutoPilot) movement.MustAutoPilot = simMovement.MustAutoPilot;
            if (simMovement.MustCrouch) movement.MustCrouch = simMovement.MustCrouch;
            if (simMovement.MustFly) movement.MustFly = simMovement.MustFly;
            if (simMovement.IsBlocked) movement.IsBlocked = simMovement.IsBlocked;
            if (simMovement.IsOneDirrection) movement.IsOneDirrection = simMovement.IsOneDirrection;
            movement.Weight = simMovement.Weight;
            movement._Length = simMovement._Length;
            movement.LengthUpdated = simMovement.LengthUpdated;
            return movement;
        }

        //public SimRoute(String s)
        //{
        //    FromFileString(s);
        //}

        //public SimRoute(Vector3 from, Vector3 to)
        //    : this(SimWaypoint.Create(from), SimWaypoint.Create(to))
        //{
        //}

        internal SimRoute _Reverse;
        public virtual SimRoute Reverse()
        {
            if (_Reverse == null)
            {
                _Reverse = SimPathStore.Instance.InternArc(EndNode, StartNode, Weight);
                SimRoute.CopyProperties(this, _Reverse);
                _Reverse._Reverse = this;
                //movement.Cost = Cost;
            }
            return _Reverse;
        }

        public virtual SimRoute FillIn(float maxDist)
        {
            return this;
        }

        public virtual SimRoute GetSegment(float start, float distlen)
        {
            List<SimRoute> newmoves = new List<SimRoute>();
            float len = Length;

            if (distlen <= 0.0 || start + distlen > len)
            {
                distlen = len - start;
            }
            SimWaypoint First = StartNode;
            SimWaypoint Last = EndNode;
            foreach (SimRoute move in GetSegments())
            {

                float mlen = move.Length;
                if (mlen > start)
                {
                    First = move.GetPointAt(start);
                    if (distlen + start < mlen)
                    {
                        Last = move.GetPointAt(distlen + start);
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
                        EndNode = move.GetPointAt(mlen);
                        newmoves.Add(new SimRoute(First, EndNode));
                        break; // this completed it
                    }
                }
            }
            return new SimRouteMovement(newmoves);
        }

        public virtual SimWaypoint NextPoint(float start)
        {
            return EndNode;
        }

        public virtual SimRouteMovement Divide(int by)
        {
            List<SimRoute> moves = new List<SimRoute>();
            float len = Length;
            float seglen = len / by;
            SimWaypoint beg = StartNode;
            int current = 1;
            while (current < by)
            {
                SimWaypoint end = GetPointAt(seglen * current);
                SimRoute move = new SimRoute(beg, end);
                moves.Add(move);
                beg = end;
            }
            return CopyProperties(this, new SimRouteMovement(moves));
        }

        public virtual SimWaypoint GetPointAt(float p)
        {
            float len = Length;
            if (p <= 0.0f) return StartNode;
            if (p >= len) return EndNode;
            Vector3 dir = EndNode.GetSimPosition() - StartNode.GetSimPosition();
            float X = (dir.X / len) * p;
            float Y = (dir.Y / len) * p;
            float Z = (dir.Z / len) * p;
            return SimWaypoint.Create(new Vector3(X, Y, Z));
        }

        public virtual List<SimRoute> GetSegments()
        {
            List<SimRoute> moves = new List<SimRoute>();
            moves.Add(this);
            return moves;
        }

        public virtual SimRouteMovement ToSegmentCopy()
        {
            List<SimRoute> moves = new List<SimRoute>();
            moves.Add(this);
            return new SimRouteMovement(moves);
        }

        public virtual SimRouteMovement Append(SimRoute extra)
        {
            if (extra is SimRouteMovement)
            {
                return extra.Prepend(this);
            }
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(this);
            MS.Add(extra);
            return new SimRouteMovement(MS);
        }

        public virtual SimRouteMovement Prepend(SimRoute extra)
        {
            if (extra is SimRouteMovement)
            {
                return extra.Append(this);
            }
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(extra);
            MS.Add(this);
            return new SimRouteMovement(MS);
        }

        public virtual List<SimWaypoint> GetPoints()
        {
            List<SimWaypoint> points = new List<SimWaypoint>();
            points.Add(StartNode);
            points.Add(EndNode);
            return points;
        }

        public virtual SimRoute AppendPoint(SimWaypoint vector3, float fudge)
        {
            if (SimWaypoint.Distance(EndNode, vector3) < fudge)
            {
                return new SimRoute(StartNode, vector3);
            }
            return Append(new SimRoute(EndNode, vector3));
        }

        public virtual string ToFileString()
        {
            string s = StartNode.GetSimPosition().ToRawString() + " -> " + EndNode.GetSimPosition().ToRawString() + " ";
            if (MustAutoPilot) s += " MustAutoPilot";
            if (MustFly) s += " MustFly";
            if (MustCrouch) s += " MustCrouch";
            if (IsBlocked) s += " IsBlocked";
            if (IsOneDirrection) s += " IsOneDirrection";
            return s;
        }

        public void FromFileString(String s)
        {
            string[] args = s.Split(null);
            StartNode = SimWaypoint.Create(new Vector3(float.Parse(args[0]), float.Parse(args[1]), float.Parse(args[2])));
            EndNode = SimWaypoint.Create(new Vector3(float.Parse(args[4]), float.Parse(args[5]), float.Parse(args[6])));
            if (s.Contains("MustAutoPilot")) MustAutoPilot = true;
            if (s.Contains("MustFly")) MustFly = true;
            if (s.Contains("MustCrouch")) MustCrouch = true;
            if (s.Contains("IsBlocked")) IsBlocked = true;
            if (s.Contains("IsOneDirrection")) IsOneDirrection = true;
        }

        public virtual bool NearPoint(SimWaypoint e, float maxDist)
        {
            if (SimWaypoint.Distance(StartNode, e) < maxDist) return true;
            if (SimWaypoint.Distance(EndNode, e) < maxDist) return true;
            return false;
        }

        internal bool IsSame(SimWaypoint s, SimWaypoint e)
        {
            return s == StartNode && e == EndNode;
        }

        internal void ReWeight(float p)
        {
            Weight = Weight * p;
        }

        internal string ToInfoString()
        {
            return ToFileString();
        }

        internal bool InRouteBox(Vector3 v3)
        {
            float x = v3.X, y = v3.Y;
            if (x < minX || x > maxX || y < minY || y > maxY)
            {
                return false;
            }
            return true;
        }

        internal bool OnRoute(Vector3 v3)
        {
            float x = v3.X, y = v3.Y;
            if (x < minX || x > maxX || y < minY || y > maxY)
            {
                return false;
            }           
            return Distance(v3)<=SimPathStore.StepSize;
        }

        internal float Distance(Vector3 P)
        {
            Vector3 Projection = ProjectOnLine(P, _StartNode.Position, _EndNode.Position);
            return Vector3.Distance(Projection,P);
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
        public static Vector3 ProjectOnLine(Vector3 Pt, Vector3 P1, Vector3 P2)
        {
            if (Pt == P1 || Pt == P2) return Pt;
            //if (Pt == null || P1 == null || P2 == null) throw new ArgumentNullException("None of the arguments can be null.");
            if (P1.Equals(P2)) throw new ArgumentException("P1 and P2 must be different.");
            Vector3 VLine = MakeDiff(P1, P2);
            Vector3 V1Pt = MakeDiff(P1, Pt);
            Vector3 Translation = VLine * VectOR(VLine, V1Pt) / SquareNorm(VLine);
            Vector3 Projection = P1 + Translation;

            Vector3 V1Pjt = MakeDiff(P1, Projection);
            float D1 = VectOR(V1Pjt, VLine);
            if (D1 < 0) return P1;

            Vector3 V2Pjt = MakeDiff(P2, Projection);
            float D2 = VectOR(V2Pjt, VLine);
            if (D2 > 0) return P2;

            return Projection;
        }

        /// <summary>
        /// Scalar product between two vectors.
        /// </summary>
        /// <param name="V1">First vector.</param>
        /// <param name="V2">Second vector.</param>
        /// <returns>Value resulting from the scalar product.</returns>
        public static float VectOR(Vector3 V1, Vector3 V2)
        {
            float ScalarProduct = 0;
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
        public static Vector3 MakeDiff(Vector3 P1, Vector3 P2)
        {
            Vector3 v3 = new Vector3(0, 0, 0);
            v3.X = P2.X - P1.X; v3.Y = P2.Y - P1.Y; v3.Z = P2.Z - P1.Z;
            return v3;
        }

        /// <summary>
        ///  Gets the square norm of the vector.
        /// </summary>
        /// <param name="v3">vector.</param>
        static public float SquareNorm(Vector3 v3)
        {
            float Sum = 0;
            //               for (int i = 0; i < 3; i++) 
            Sum += v3.X * v3.X;
            Sum += v3.Y * v3.Y;
            Sum += v3.Z * v3.Z;
            return Sum;
        }

    }

    public class SimRouteMovement : SimRoute
    {
        public override bool NearPoint(SimWaypoint e, float maxDist)
        {
            if (SimWaypoint.Distance(StartNode, e) < maxDist) return true;
            if (SimWaypoint.Distance(EndNode, e) < maxDist) return true;
            foreach (SimRoute move in MoveList)
            {
                if (move.NearPoint(e, maxDist)) return true;
            }
            return false;
        }

        public override SimRoute Reverse()
        {
            List<SimRoute> ReverseMoveList = new List<SimRoute>();
            foreach (SimRoute move in MoveList)
            {
                ReverseMoveList.Insert(0, move.Reverse());
            }
            SimRouteMovement movement = new SimRouteMovement(ReverseMoveList);
            CopyProperties(this, movement);
            return movement;
        }

        List<SimRoute> MoveList;
        public SimRouteMovement(List<SimRoute> ms)
            : base(ms[0].StartNode, ms[ms.Count - 1].EndNode)
        {
            MoveList = ms;
        }

        public override List<SimWaypoint> GetPoints()
        {
            List<SimWaypoint> points = new List<SimWaypoint>();
            points.Add(StartNode);
            SimWaypoint Last = StartNode;
            foreach (SimRoute move in GetSegments())
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
            if (EndNode != Last)
            {
                Last = EndNode;
                points.Add(Last);
            }
            return points;
        }

        public override SimRouteMovement ToSegmentCopy()
        {
            List<SimRoute> moves = GetSegments();
            return new SimRouteMovement(moves);
        }

        public override SimRouteMovement Append(SimRoute extra)
        {
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(this);
            MS.Add(extra);
            return new SimRouteMovement(MS);
        }

        public override SimRouteMovement Prepend(SimRoute extra)
        {
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(extra);
            MS.Add(this);
            return new SimRouteMovement(MS);
        }


        public override SimRoute FillIn(float maxDist)
        {
            List<SimRoute> moves = new List<SimRoute>();
            SimWaypoint at = StartNode;
            bool filled = false;
            foreach (SimRoute move in GetSegments())
            {
                if (SimWaypoint.Distance(at, move.StartNode) > maxDist)
                {
                    moves.Add(new SimRoute(at, move.StartNode));
                    filled = true;
                }
                moves.Add(move);
                at = move.EndNode;
            }
            if (filled) return new SimRouteMovement(moves);
            return this;
        }

        public override List<SimRoute> GetSegments()
        {
            List<SimRoute> moves = new List<SimRoute>();
            foreach (SimRoute move in MoveList)
            {
                foreach (SimRoute one in move.GetSegments())
                {
                    moves.Add(one);
                }
            }
            return moves;
        }

        public override SimWaypoint GetPointAt(float p)
        {
            if (p <= 0.0f) return StartNode;
            foreach (SimRoute move in MoveList)
            {
                float mlen = move.Length;
                if (mlen > p) return move.GetPointAt(p);
                p -= mlen;
            }
            return EndNode;
        }

        protected override float CalculateLength()
        {
            float len = 0f;
            foreach (SimRoute mv in MoveList)
            {
                len += mv.Length;
            }
            return len;
        }
    }
}
