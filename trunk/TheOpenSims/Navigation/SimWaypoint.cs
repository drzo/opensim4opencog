using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Collections;
using cogbot.TheOpenSims.Navigation.Debug;

namespace cogbot.TheOpenSims.Navigation
{
    /// <summary>
    /// Basically a node is defined with a geographical position in space.
    /// It is also characterized with both collections of outgoing arcs and incoming arcs.
    /// </summary>
    [Serializable]
    public class SimWaypoint : SimPosition
    {
        public SimWaypoint GetWaypoint()
        {
            return this;
        }


        public bool CanGetSimPosition()
        {
            return true;
        }
         
        Vector3 _Position;
        bool _Passable;
        ArrayList _IncomingArcs, _OutgoingArcs;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="PositionX">X coordinate.</param>
        /// <param name="PositionY">Y coordinate.</param>
        /// <param name="PositionZ">Z coordinate.</param>
        static public SimWaypoint Create(float PositionX, float PositionY, float PositionZ)
        {
            Vector3 _Position = RoundPoint(new Vector3(PositionX, PositionY, PositionZ));
            return Create(_Position);
        }

        public int ArcCount()
        {
            return _OutgoingArcs.Count;
        }
        /// <summary>
        /// Gets the list of the arcs that lead to this node.
        /// </summary>
        public IList IncomingArcs { get { return _IncomingArcs; } }

        /// <summary>
        /// Gets the list of the arcs that start from this node.
        /// </summary>
        public IList OutgoingArcs { get { return _OutgoingArcs; } }

        /// Gets/Sets the functional state of the node.
        /// 'true' means that the node is in its normal state.
        /// 'false' means that the node will not be taken into account (as if it did not exist).
        public bool Passable
        {
            set
            {
                foreach (SimRoute A in _IncomingArcs) A.Passable = value;
                foreach (SimRoute A in _OutgoingArcs) A.Passable = value;
                _Passable = value;
                if (GraphFormer.DEBUGGER != null)
                {
                    GraphFormer.DEBUGGER.RepaintNow();
                }

            }
            get { return _Passable; }
        }

        /// <summary>
        /// Gets X coordinate.
        /// </summary>
        public double X { get { return Position.X * GraphFormer.DSCALE; } }

        /// <summary>
        /// Gets Y coordinate.
        /// </summary>
        public double Y { get { return Position.Y * GraphFormer.DSCALE; } }

        /// <summary>
        /// Gets Z coordinate.
        /// </summary>
        public double Z { get { return Position.Z * GraphFormer.DSCALE; } }

        /// <summary>
        /// Modifies X, Y and Z coordinates
        /// </summary>
        /// <param name="PositionX">X coordinate.</param>
        /// <param name="PositionY">Y coordinate.</param>
        /// <param name="PositionZ">Z coordinate.</param>
        public void ChangeXYZDebug(float PositionX, float PositionY, float PositionZ)
        {
            Position = new Vector3((float)PositionX, (float)PositionY,(float) PositionZ);
        }

        /// <summary>
        /// Gets/Sets the geographical position of the node.
        /// </summary>
        /// <exception cref="ArgumentNullException">Cannot set the Position to null.</exception>
        public Vector3 Position
        {
            set
            {
                if (value == null) throw new ArgumentNullException();
                foreach (SimRoute A in _IncomingArcs) A.LengthUpdated = false;
                foreach (SimRoute A in _OutgoingArcs) A.LengthUpdated = false;
                _Position = value;
            }
            get { return _Position; }
        }

        /// <summary>
        /// Gets the array of nodes that can be directly reached from this one.
        /// </summary>
        public SimWaypoint[] AccessibleNodes
        {
            get
            {
                SimWaypoint[] Tableau = new SimWaypoint[_OutgoingArcs.Count];
                int i = 0;
                foreach (SimRoute A in OutgoingArcs) Tableau[i++] = A.EndNode;
                return Tableau;
            }
        }

        /// <summary>
        /// Gets the array of nodes that can directly reach this one.
        /// </summary>
        public SimWaypoint[] AccessingNodes
        {
            get
            {
                SimWaypoint[] Tableau = new SimWaypoint[_IncomingArcs.Count];
                int i = 0;
                foreach (SimRoute A in IncomingArcs) Tableau[i++] = A.StartNode;
                return Tableau;
            }
        }

        /// <summary>
        /// Gets the array of nodes directly linked plus this one.
        /// </summary>
        public SimWaypoint[] Molecule
        {
            get
            {
                int NbNodes = 1 + _OutgoingArcs.Count + _IncomingArcs.Count;
                SimWaypoint[] Tableau = new SimWaypoint[NbNodes];
                Tableau[0] = this;
                int i = 1;
                foreach (SimRoute A in OutgoingArcs) Tableau[i++] = A.EndNode;
                foreach (SimRoute A in IncomingArcs) Tableau[i++] = A.StartNode;
                return Tableau;
            }
        }

        /// <summary>
        /// Unlink this node from all current connected arcs.
        /// </summary>
        public void Isolate()
        {
            UntieIncomingArcs();
            UntieOutgoingArcs();
        }

        /// <summary>
        /// Unlink this node from all current incoming arcs.
        /// </summary>
        public void UntieIncomingArcs()
        {
            foreach (SimRoute A in _IncomingArcs)
                A.StartNode.OutgoingArcs.Remove(A);
            _IncomingArcs.Clear();
        }

        /// <summary>
        /// Unlink this node from all current outgoing arcs.
        /// </summary>
        public void UntieOutgoingArcs()
        {
            foreach (SimRoute A in _OutgoingArcs)
                A.EndNode.IncomingArcs.Remove(A);
            _OutgoingArcs.Clear();
        }

        /// <summary>
        /// Returns the arc that leads to the specified node if it exists.
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument node must not be null.</exception>
        /// <param name="N">A node that could be reached from this one.</param>
        /// <returns>The arc leading to N from this / null if there is no solution.</returns>
        public SimRoute ArcGoingTo(SimWaypoint N)
        {
            if (N == null) throw new ArgumentNullException();
            foreach (SimRoute A in _OutgoingArcs)
                if (A.EndNode == N) return A;
            return null;
        }

        /// <summary>
        /// Returns the arc that arc that comes to this from the specified node if it exists.
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument node must not be null.</exception>
        /// <param name="N">A node that could reach this one.</param>
        /// <returns>The arc coming to this from N / null if there is no solution.</returns>
        public SimRoute ArcComingFrom(SimWaypoint N)
        {
            if (N == null) throw new ArgumentNullException();
            foreach (SimRoute A in _IncomingArcs)
                if (A.StartNode == N) return A;
            return null;
        }

        void Invalidate()
        {
            foreach (SimRoute A in _IncomingArcs) A.LengthUpdated = false;
            foreach (SimRoute A in _OutgoingArcs) A.LengthUpdated = false;
        }

        /// <summary>
        /// object.ToString() override.
        /// Returns the textual description of the node.
        /// </summary>
        /// <returns>String describing this node.</returns>
        public override string ToString() { return "(" + Position.ToRawString() + ")"; }

        /// <summary>
        /// Object.Equals override.
        /// Tells if two nodes are equal by comparing positions.
        /// </summary>
        /// <exception cref="ArgumentException">A Node cannot be compared with another type.</exception>
        /// <param name="O">The node to compare with.</param>
        /// <returns>'true' if both nodes are equal.</returns>
        public override bool Equals(object O)
        {
            if (O is SimPosition)
            {
                return GetSimPosition() == ((SimPosition)O).GetSimPosition();
            }
            if (O is Vector3)
            {
                return Create((Vector3)O).Equals(this);
            }
            
            throw new ArgumentException("Type " + O.GetType() + " cannot be compared with type " + GetType() + " !");
        }


        /// <summary>
        /// Returns a copy of this node.
        /// </summary>
        /// <returns>The reference of the new object.</returns>
        public object Clone()
        {
            SimWaypoint N = new SimWaypoint(_Position);
            N._Passable = _Passable;
            return N;
        }

        /// <summary>
        /// Object.GetHashCode override.
        /// </summary>
        /// <returns>HashCode value.</returns>
        public override int GetHashCode() { return Position.GetHashCode(); }

        /// <summary>
        /// Returns the euclidian distance between two nodes : Sqrt(Dx²+Dy²+Dz²)
        /// </summary>
        /// <param name="N1">First node.</param>
        /// <param name="N2">Second node.</param>
        /// <returns>Distance value.</returns>
        public static float EuclidianDistance(SimWaypoint N1, SimWaypoint N2)
        {
            return (float)Math.Sqrt(SquareEuclidianDistance(N1, N2));
        }

        /// <summary>
        /// Returns the square euclidian distance between two nodes : Dx²+Dy²+Dz²
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument nodes must not be null.</exception>
        /// <param name="N1">First node.</param>
        /// <param name="N2">Second node.</param>
        /// <returns>Distance value.</returns>
        public static float SquareEuclidianDistance(SimWaypoint N1, SimWaypoint N2)
        {
            if (N1 == null || N2 == null) throw new ArgumentNullException();
            float DX = N1.Position.X - N2.Position.X;
            float DY = N1.Position.Y - N2.Position.Y;
            float DZ = N1.Position.Z - N2.Position.Z;
            return DX * DX + DY * DY + DZ * DZ;
        }

        /// <summary>
        /// Returns the manhattan distance between two nodes : |Dx|+|Dy|+|Dz|
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument nodes must not be null.</exception>
        /// <param name="N1">First node.</param>
        /// <param name="N2">Second node.</param>
        /// <returns>Distance value.</returns>
        public static float ManhattanDistance(SimWaypoint N1, SimWaypoint N2)
        {
            if (N1 == null || N2 == null) throw new ArgumentNullException();
            float DX = N1.Position.X - N2.Position.X;
            float DY = N1.Position.Y - N2.Position.Y;
            float DZ = N1.Position.Z - N2.Position.Z;
            return Math.Abs(DX) + Math.Abs(DY) + Math.Abs(DZ);
        }

        /// <summary>
        /// Returns the maximum distance between two nodes : Max(|Dx|, |Dy|, |Dz|)
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument nodes must not be null.</exception>
        /// <param name="N1">First node.</param>
        /// <param name="N2">Second node.</param>
        /// <returns>Distance value.</returns>
        public static float MaxDistanceAlongAxis(SimWaypoint N1, SimWaypoint N2)
        {
            if (N1 == null || N2 == null) throw new ArgumentNullException();
            float DX = Math.Abs(N1.Position.X - N2.Position.X);
            float DY = Math.Abs(N1.Position.Y - N2.Position.Y);
            float DZ = Math.Abs(N1.Position.Z - N2.Position.Z);
            return Math.Max(DX, Math.Max(DY, DZ));
        }

        /// <summary>
        /// Returns the bounding box that wraps the specified list of nodes.
        /// </summary>
        /// <exception cref="ArgumentException">The list must only contain elements of type Node.</exception>
        /// <exception cref="ArgumentException">The list of nodes is empty.</exception>
        /// <param name="NodesGroup">The list of nodes to wrap.</param>
        /// <param name="MinPoint">The point of minimal coordinates for the box.</param>
        /// <param name="MaxPoint">The point of maximal coordinates for the box.</param>
        static public void BoundingBox(IList<SimWaypoint> NodesGroup, out float[] MinPoint, out float[] MaxPoint)
        {
            SimWaypoint N1 = NodesGroup[0] as SimWaypoint;
            if (N1 == null) throw new ArgumentException("The list must only contain elements of type Node.");
            if (NodesGroup.Count == 0) throw new ArgumentException("The list of nodes is empty.");
            int Dim = 3;
            MinPoint = new float[Dim];
            MaxPoint = new float[Dim];
            for (int i = 0; i < Dim; i++) MinPoint[i] = MaxPoint[i] = N1.PositionXYZ(i);
            foreach (SimWaypoint N in NodesGroup)
            {
                for (int i = 0; i < Dim; i++)
                {
                    if (MinPoint[i] > N.PositionXYZ(i)) MinPoint[i] = N.PositionXYZ(i);
                    if (MaxPoint[i] < N.PositionXYZ(i)) MaxPoint[i] = N.PositionXYZ(i);
                }
            }
        }

        private float PositionXYZ(int i)
        {
            switch (i)
            {
                case 0:
                    return Position.X;
                case 1:
                    return Position.Y;
                case 2:
                    return Position.Z;
                default:
                    throw new ArgumentException("Waypoints dont have arg " + i);
            }
        }

        //public static implicit operator Vector3(SimWaypoint m)
        //{
        //    return m.GetSimPosition();
        //}
        //public static implicit operator Vector2(SimWaypoint m)
        //{
        //    Vector3 v3 = m.GetSimPosition();
        //    return new Vector2(v3.X, v3.Y);
        //}
        //public static implicit operator Vector3d(SimWaypoint m)
        //{
        //    Vector3 v3 = m.GetSimPosition();
        //    return new Vector3d(v3.X, v3.Y, v3.Z);
        //}

        private SimWaypoint(Vector3 firstP)
        {
            _Position = RoundPoint(firstP);
        }

        //protected Vector3 _Position;

        public static Vector3 RoundPoint(Vector3 point)
        {
            double POINTS_PER_METER = SimPathStore.POINTS_PER_METER;
            Vector3 vect3 = new Vector3(point);
            vect3.X = (float)(Math.Round(vect3.X * POINTS_PER_METER, 0) / POINTS_PER_METER);
            vect3.Y = (float)(Math.Round(vect3.Y * POINTS_PER_METER, 0) / POINTS_PER_METER);
            vect3.Z = (float)Math.Round(vect3.Z);
            return vect3;
        }

        static Dictionary<Vector3, SimWaypoint> InternedPoints = new Dictionary<Vector3, SimWaypoint>();
        internal static SimWaypoint Create(Vector3 from)
        {
            Vector3 rounded = RoundPoint(from);
            if (InternedPoints.ContainsKey(rounded))
            {
                return InternedPoints[rounded];
            }
            SimWaypoint wp = new SimWaypoint(rounded);
            wp._Passable = true;
            wp._IncomingArcs = new ArrayList();
            wp._OutgoingArcs = new ArrayList();
            InternedPoints[rounded] = wp;
          //  SimPathStore.EnsureKnown(wp);
            return wp;
        }

        public Vector3 GetSimPosition()
        {
            return _Position;
        }

        #region SimPosition Members

        public Vector3 GetUsePosition()
        {
            return GetSimPosition();
        }

        public float GetSizeDistance()
        {
            return 0.7f;
        }

        #endregion

        internal static float Distance(SimWaypoint wp1, SimWaypoint wp2)
        {
            return Vector3.Distance(wp1.GetSimPosition(), wp2.GetSimPosition());
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns>true if something needed to be changed</returns>
        public bool EnsureAtLeastOnePath()
        {
            bool needIt = true;
            foreach (SimRoute A in _IncomingArcs)
            {
                if (A.Passable)
                {
                    needIt = false;
                }
            }
            foreach (SimRoute A in _OutgoingArcs)
            {
                if (A.Passable)
                {
                    needIt = false;
                }
            }
            if (needIt)
            {
                Passable = true;
            }
            return needIt;
        }

        internal bool GoesTo(SimWaypoint e)
        {
            foreach (SimRoute r in _OutgoingArcs)
            {
                if (r.EndNode == e) return true;
            }
            return false;
        }
    }

    //public class SimMovementPoints : SimMovement
    //{
    //    List<Vector3> ThePoints;// = new List<Vector3>();
    //    public SimMovementPoints(List<Vector3> points)
    //        : base(points[0], points[points.Count - 1])
    //    {
    //        ThePoints = points;
    //    }

    //    public override List<Vector3> GetPoints()
    //    {
    //        return ThePoints;
    //    }
    //}
}
