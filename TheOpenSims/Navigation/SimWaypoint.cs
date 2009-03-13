using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Collections;
using cogbot.TheOpenSims.Navigation.Debug;
using System.Drawing;
using cogbot.TheOpenSims.Mesher;

namespace cogbot.TheOpenSims.Navigation
{
    /// <summary>
    /// Basically a node is defined with a geographical position in space.
    /// It is also characterized with both collections of outgoing arcs and incoming arcs.
    /// </summary>
    [Serializable]
    public class SimWaypoint : SimPosition
    {
        public byte GetMatrix()
        {
            return PathStore.mMatrix[PX, PY];
        }

        public void SetMatrix(int v)
        {
            if (v < 0) v = 0; else if (v > 255) v = 255;
            PathStore.mMatrix[PX, PY] = (byte)v;
        }


        public bool _BlockedKnown = false;
        public bool _Blocked = false;
        public bool IsSolid
        {
            get
            {
                if (!_BlockedKnown)
                {
                    _BlockedKnown = true;
                    _Blocked = false;
                    foreach (SimObject O in OccupiedList)
                    {
                        if (!O.IsPassable)
                        {
                            _Blocked = true;
                        }
                    }
                    _BlockedKnown = true;
                }
                return _Blocked;
            }
        }
        public void UpdateMatrix()
        {
            bool WasBlocked = GetMatrix() == 0;
            bool Blocked = false;
           if (IsSolid)
            {
                float groundLevel = LowestSurrounding();
                float ground = GetZLevel();

                if (ground + 1.2 < groundLevel || ground > groundLevel + 1)
                    Blocked = true;
                //      if (O.Mesh.IsInside(_LocalPos.X, _LocalPos.Y, groundLevel + 1f))

            }
            if (WasBlocked && !Blocked)
            {
                //  Passable = !IsSolid;
                SetMatrix(2 + OccupiedList.Count * 2);
                return;
            }
            else if (!WasBlocked && Blocked)
            {
                //Passable = !IsSolid;
                SetMatrix(0);
            }
        }

        private float LowestSurrounding()
        {
            float original = GetZLevel();
            float mostDiff = original;

            //return GetGroundLevel();
            if (PX < 1 || PY < 1 || _LocalPos.X > 254 || _LocalPos.Y > 254)
                return mostDiff;
            

            SimWaypoint O = null;

            SimWaypoint[,] mWaypoints = PathStore.mWaypoints;

            O = mWaypoints[PX, PY + 1];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            O = mWaypoints[PX + 1, PY + 1];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            O = mWaypoints[PX + 1, PY];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            O = mWaypoints[PX + 1, PY - 1];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            O = mWaypoints[PX, PY - 1];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            O = mWaypoints[PX - 1, PY - 1];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            O = mWaypoints[PX - 1, PY];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            O = mWaypoints[PX - 1, PY + 1];
            if (O != null)
                mostDiff = MostDiff(O.GetZLevel(), mostDiff, original);

            float gl = GetGroundLevel();
            if (mostDiff == original) return gl;

            return mostDiff;
        }

        static float MostDiff(float other, float mostDiff, float original)
        {
            if (Math.Abs(original - mostDiff) > Math.Abs(original - other)) return mostDiff;
            return other;
        }

        private float GetGroundLevel()
        {
            return GetSimRegion().GetGroundLevel(_LocalPos.X, _LocalPos.Y);
        }

        internal int AddShadow(SimObject blocker)
        {
            IList<SimObject> ShadowList = OccupiedList;
            if (!ShadowList.Contains(blocker))
            {
                ShadowList.Add(blocker);
                _ZLevel = float.MinValue;
            }
            return ShadowList.Count;
        }

        internal bool AddOccupied(SimObject simObject)
        {
            if (OccupiedList.Contains(simObject)) return false;
            OccupiedList.Add(simObject);
            _ZLevel = float.MinValue;
            return true;
        }

        // private IList ShadowList = new List<SimObject>();
        internal IList<SimObject> OccupiedList = new List<SimObject>();

        public bool NoObjectsBlock()
        {
            foreach (SimObject O in OccupiedList)
            {
                if (!O.IsPassable)
                {
                    return false;
                }
            }
            return true;
        }

        public string OccupiedString()
        {
            GetZLevel();
            string S = "";
            foreach (SimObject O in OccupiedList)
            {
                S += O.ToString();
                S += "\n";
            }
            return S + this.ToString() + "\nZLevel=" + GetZLevel();
        }


        public SimWaypoint GetWaypoint()
        {
            return this;
        }


        public bool IsRegionAttached()
        {
            return PathStore != null;
        }

        Vector3d _GlobalPos;
        bool _Passable = true;
        ArrayList _IncomingArcs, _OutgoingArcs;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="PositionX">X coordinate.</param>
        /// <param name="PositionY">Y coordinate.</param>
        /// <param name="PositionZ">Z coordinate.</param>
        static public SimWaypoint CreateLocal(float PositionX, float PositionY, float PositionZ, SimPathStore PathStore)
        {
            return CreateLocal(new Vector3(PositionX, PositionY, PositionZ), PathStore);
        }

        public int ArcCount()
        {
            if (_OutgoingArcs != null)
                return _OutgoingArcs.Count;
            return 0;
        }
        /// <summary>
        /// Gets the list of the arcs that lead to this node.
        /// </summary>
        public IList IncomingArcs
        {
            get
            {
                if (_IncomingArcs == null) _IncomingArcs = new ArrayList();
                return _IncomingArcs;
            }
        }

        /// <summary>
        /// Gets the list of the arcs that start from this node.
        /// </summary>
        public IList OutgoingArcs
        {
            get
            {
                if (_OutgoingArcs == null) _OutgoingArcs = new ArrayList();
                return _OutgoingArcs;
            }
        }

        /// Gets/Sets the functional state of the node.
        /// 'true' means that the node is in its normal state.
        /// 'false' means that the node will not be taken into account (as if it did not exist).
        public bool Passable
        {
            set
            {
                if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs) A.Passable = value;
                if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs) A.Passable = value;
                _Passable = value;
                SetMatrix((byte)(value ? 2 : 0));
            }
            get
            {
                if (!_Passable) return false;
                if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs) if (A.Passable) return true;
                if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs) if (A.Passable) return true;
                return GetMatrix() != 0;
            }
        }

        /// <summary>
        /// Gets Point.X coordinate on the PathStore.
        /// </summary>
        public int PX;// { get { return (int)Math.Round(Position.X * PathStore.POINTS_PER_METER); } }

        /// <summary>
        /// Gets Point.Y coordinate on the PathStore.
        /// </summary>
        public int PY;// { get { return (int)Math.Round(Position.Y * PathStore.POINTS_PER_METER); } }


        /// <summary>
        /// Gets X coordinate on the Graph debugger (whole world position).
        /// </summary>
        public double DX { get { return (Position.X / GraphFormer.DSCALE) - GraphFormer.StartX; } }

        /// <summary>
        /// Gets Y coordinate on the Graph debugger
        /// </summary>
        public double DY { get { return (Position.Y / GraphFormer.DSCALE) - GraphFormer.StartY; } }

        /// <summary>
        /// Gets Z coordinate.
        /// </summary>
        ///public double DZ { get { return Position.Z; } }

        /// <summary>
        /// Modifies X, Y and Z coordinates
        /// </summary>
        /// <param name="PositionX">X coordinate.</param>
        /// <param name="PositionY">Y coordinate.</param>
        /// <param name="PositionZ">Z coordinate.</param>
        public void ChangeXYZDebug(double PositionX, double PositionY, double PositionZ)
        {
            SetGlobalPos(new Vector3d(PositionX, PositionY, PositionZ));
        }

        private void SetGlobalPos(Vector3d v3d)
        {
            SimRegion R = SimRegion.GetRegion(v3d);
            PathStore = R.PathStore;
            _LocalPos = SimRegion.GlobalToLocal(v3d);
            _GlobalPos = R.LocalToGlobal(_LocalPos);
            PX = (int)Math.Round(_LocalPos.X * PathStore.POINTS_PER_METER);
            PY = (int)Math.Round(_LocalPos.Y * PathStore.POINTS_PER_METER);
            if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs) A.LengthUpdated = false;
            if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs) A.LengthUpdated = false;
        }

        /// <summary>
        /// Gets/Sets the geographical position of the node.
        /// </summary>
        /// <exception cref="ArgumentNullException">Cannot set the Position to null.</exception>
        public Vector3d Position
        {
            set
            {
                //if (value == null) throw new ArgumentNullException();
                SetGlobalPos(value);
            }
            get { return _GlobalPos; }
        }

        /// <summary>
        /// Gets the array of nodes that can be directly reached from this one.
        /// </summary>
        public SimWaypoint[] AccessibleNodes
        {
            get
            {
                SimWaypoint[] Tableau = new SimWaypoint[OutgoingArcs.Count];
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
                SimWaypoint[] Tableau = new SimWaypoint[IncomingArcs.Count];
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
                int NbNodes = 1 + OutgoingArcs.Count + IncomingArcs.Count;
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
            if (_IncomingArcs != null)
            {
                foreach (SimRoute A in _IncomingArcs)
                    A.StartNode.OutgoingArcs.Remove(A);
                _IncomingArcs.Clear();
            }
        }

        /// <summary>
        /// Unlink this node from all current outgoing arcs.
        /// </summary>
        public void UntieOutgoingArcs()
        {
            if (_OutgoingArcs != null)
            {
                foreach (SimRoute A in _OutgoingArcs)
                    A.EndNode.IncomingArcs.Remove(A);
                _OutgoingArcs.Clear();
            }
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
            if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs)
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
            if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs)
                    if (A.StartNode == N) return A;
            return null;
        }

        void Invalidate()
        {
            if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs) A.LengthUpdated = false;
            if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs) A.LengthUpdated = false;
        }

        /// <summary>
        /// object.ToString() override.
        /// Returns the textual description of the node.
        /// </summary>
        /// <returns>String describing this node.</returns>
        public override string ToString()
        {
            //    return "(" + Position.ToRawString() + ")"; 
            SimRegion R = GetSimRegion();
            Vector3 loc = GetSimPosition();
            return String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

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
                return Position == ((SimPosition)O).GetWorldPosition();
            }
            //if (O is Vector3d)
            //{
            //    return Create((Vector3d)O).Equals(this);
            //}

            throw new ArgumentException("Type " + O.GetType() + " cannot be compared with type " + GetType() + " !");
        }


        /// <summary>
        /// Returns a copy of this node.
        /// </summary>
        /// <returns>The reference of the new object.</returns>
        public object Clone()
        {
            SimWaypoint N = new SimWaypoint(_LocalPos, _GlobalPos, PathStore);
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
        public static double EuclidianDistance(SimWaypoint N1, SimWaypoint N2)
        {
            return (double)Math.Sqrt(SquareEuclidianDistance(N1, N2));
        }

        /// <summary>
        /// Returns the square euclidian distance between two nodes : Dx²+Dy²+Dz²
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument nodes must not be null.</exception>
        /// <param name="N1">First node.</param>
        /// <param name="N2">Second node.</param>
        /// <returns>Distance value.</returns>
        public static double SquareEuclidianDistance(SimWaypoint N1, SimWaypoint N2)
        {
            if (N1 == null || N2 == null) throw new ArgumentNullException();
            double DX = N1.Position.X - N2.Position.X;
            double DY = N1.Position.Y - N2.Position.Y;
            double DZ = N1.Position.Z - N2.Position.Z;
            return (double)(DX * DX + DY * DY + DZ * DZ);
        }

        /// <summary>
        /// Returns the manhattan distance between two nodes : |Dx|+|Dy|+|Dz|
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument nodes must not be null.</exception>
        /// <param name="N1">First node.</param>
        /// <param name="N2">Second node.</param>
        /// <returns>Distance value.</returns>
        public static double ManhattanDistance(SimWaypoint N1, SimWaypoint N2)
        {
            if (N1 == null || N2 == null) throw new ArgumentNullException();
            double DX = N1.Position.X - N2.Position.X;
            double DY = N1.Position.Y - N2.Position.Y;
            double DZ = N1.Position.Z - N2.Position.Z;
            return (double)(Math.Abs(DX) + Math.Abs(DY) + Math.Abs(DZ));
        }

        /// <summary>
        /// Returns the maximum distance between two nodes : Max(|Dx|, |Dy|, |Dz|)
        /// </summary>
        /// <exception cref="ArgumentNullException">Argument nodes must not be null.</exception>
        /// <param name="N1">First node.</param>
        /// <param name="N2">Second node.</param>
        /// <returns>Distance value.</returns>
        public static double MaxDistanceAlongAxis(SimWaypoint N1, SimWaypoint N2)
        {
            if (N1 == null || N2 == null) throw new ArgumentNullException();
            double DX = Math.Abs(N1.Position.X - N2.Position.X);
            double DY = Math.Abs(N1.Position.Y - N2.Position.Y);
            double DZ = Math.Abs(N1.Position.Z - N2.Position.Z);
            return (double)Math.Max(DX, Math.Max(DY, DZ));
        }

        /// <summary>
        /// Returns the bounding box that wraps the specified list of nodes.
        /// </summary>
        /// <exception cref="ArgumentException">The list must only contain elements of type Node.</exception>
        /// <exception cref="ArgumentException">The list of nodes is empty.</exception>
        /// <param name="NodesGroup">The list of nodes to wrap.</param>
        /// <param name="MinPoint">The point of minimal coordinates for the box.</param>
        /// <param name="MaxPoint">The point of maximal coordinates for the box.</param>
        static public void BoundingBox(ICollection<SimWaypoint> NodesGroup, out double[] MinPoint, out double[] MaxPoint)
        {
            IEnumerator E = NodesGroup.GetEnumerator();
            E.MoveNext();
            SimWaypoint N1 = E.Current as SimWaypoint;
            if (N1 == null) throw new ArgumentException("The list must only contain elements of type Node.");
            if (NodesGroup.Count == 0) throw new ArgumentException("The list of nodes is empty.");
            int Dim = 3;
            MinPoint = new double[Dim];
            MaxPoint = new double[Dim];
            for (int i = 0; i < Dim; i++) MinPoint[i] = MaxPoint[i] = N1.GlobalXYZ(i);
            foreach (SimWaypoint N in NodesGroup)
            {
                for (int i = 0; i < Dim; i++)
                {
                    if (MinPoint[i] > N.GlobalXYZ(i)) MinPoint[i] = N.GlobalXYZ(i);
                    if (MaxPoint[i] < N.GlobalXYZ(i)) MaxPoint[i] = N.GlobalXYZ(i);
                }
            }
        }

        private double GlobalXYZ(int i)
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

        //public static implicit operator Vector3d(SimWaypoint m)
        //{
        //    return m.GetSimPosition();
        //}
        //public static implicit operator Vector2(SimWaypoint m)
        //{
        //    Vector3d v3 = m.GetSimPosition();
        //    return new Vector2(v3.X, v3.Y);
        //}
        //public static implicit operator Vector3d(SimWaypoint m)
        //{
        //    Vector3d v3 = m.GetSimPosition();
        //    return new Vector3d(v3.X, v3.Y, v3.Z);
        //}

        SimPathStore PathStore;
        Vector3 _LocalPos;
        private SimWaypoint(Vector3 local, Vector3d global, SimPathStore pathStore)
        {
            PathStore = pathStore;
            _GlobalPos = global;// RoundPoint(firstP, PathStore);
            _LocalPos = local;
            PX = (int)Math.Round(_LocalPos.X * PathStore.POINTS_PER_METER);
            PY = (int)Math.Round(_LocalPos.Y * PathStore.POINTS_PER_METER);
        }

        //protected Vector3d _GlobalPos;

        public static Vector3 RoundPoint(Vector3 point, SimPathStore PathStore)
        {
            double POINTS_PER_METER = PathStore.POINTS_PER_METER;
            Vector3 vect3 = new Vector3(point);
            vect3.X = (float)(Math.Round(vect3.X * POINTS_PER_METER, 0) / POINTS_PER_METER);
            vect3.Y = (float)(Math.Round(vect3.Y * POINTS_PER_METER, 0) / POINTS_PER_METER);
            vect3.Z = (float)(Math.Round(vect3.Z * POINTS_PER_METER, 0) / POINTS_PER_METER);
            return vect3;
        }

        public static SimWaypoint CreateLocal(Vector3 from, SimPathStore PathStore)
        {
            int PX = (int)Math.Round(from.X * PathStore.POINTS_PER_METER);
            int PY = (int)Math.Round(from.Y * PathStore.POINTS_PER_METER);
            SimWaypoint WP = PathStore.mWaypoints[PX, PY];
            if (WP != null) return WP;
            Vector3 rounded = new Vector3(PX / PathStore.POINTS_PER_METER, PY / PathStore.POINTS_PER_METER, from.Z);
            Vector3d GlobalPos = PathStore.GetSimRegion().LocalToGlobal(rounded);
            WP = new SimWaypoint(rounded, GlobalPos, PathStore);
            PathStore.mWaypoints[PX, PY] = WP;
            WP._Passable = true;
            // wp._IncomingArcs = new ArrayList();
            // wp._OutgoingArcs = new ArrayList();
            //  PathStore.EnsureKnown(wp);
            return WP;
        }

        public Vector3 GetSimPosition()
        {
            return _LocalPos;
        }

        #region SimPosition Members

        //public Vector3 GetUsePosition()
        //{
        //    return GetSimPosition();
        //}

        public float GetSizeDistance()
        {
            return 0.7f;
        }

        #endregion

        public static double Distance(SimWaypoint wp1, SimWaypoint wp2)
        {
            return Vector3d.Distance(wp1.Position, wp2.Position);
        }

        // <summary>

        // </summary>
        // <returns>true if something needed to be changed</returns>
        public bool EnsureAtLeastOnePath()
        {
            bool needIt = true;
            if (_IncomingArcs != null)
                foreach (SimRoute A in _IncomingArcs)
                {
                    if (A.Passable)
                    {
                        needIt = false;
                    }
                }
            if (_OutgoingArcs != null)
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

        public bool GoesTo(SimWaypoint e)
        {

            if (_OutgoingArcs != null)
                foreach (SimRoute r in _OutgoingArcs)
                {
                    if (r.EndNode == e) return true;
                }
            return false;
        }

        public double Distance(Vector3d P)
        {
            return Vector3d.Distance(P, Position);
        }


        #region SimPosition Members


        public Quaternion GetSimRotation()
        {
            return Quaternion.Identity;
        }

        #endregion

        #region SimPosition Members


        public Vector3d GetWorldPosition()
        {
            return Position;
        }

        #endregion

        #region SimPosition Members


        public SimRegion GetSimRegion()
        {
            return PathStore.GetSimRegion();
        }

        #endregion

        internal static SimWaypoint CreateGlobal(double gx, double gy, double gz)
        {
            return CreateGlobal(new Vector3d(gx, gy, gz));
        }

        internal static SimWaypoint CreateGlobal(Vector3d v3d)
        {
            SimRegion R = SimRegion.GetRegion(v3d);
            return CreateLocal(SimRegion.GlobalToLocal(v3d), R.PathStore);
        }

        float _ZLevel = float.MinValue;
        internal float GetZLevel()
        {
            // when the Two Zs are differnt that means global Pos has been computed
            if (_ZLevel > -10) return _ZLevel;
            float ground = GetGroundLevel();
            lock (OccupiedList)
            {
                foreach (SimObject O in OccupiedList)
                {
                    if (O.IsPassable) continue;
                    Box3Fill box = O.OuterBox;
                    if (box.MinZ < ground + 1)
                        if (ground + 1 < box.MaxZ)
                        {
                            ground = box.MaxZ;
                            _LocalPos.Z = ground;
                            _GlobalPos.Z = ground + 1;
                        }
                }
            }
            _ZLevel = ground;
            return ground;
        }
    }

    //public class SimMovementPoints : SimMovement
    //{
    //    List<Vector3d> ThePoints;// = new List<Vector3d>();
    //    public SimMovementPoints(List<Vector3d> points)
    //        : base(points[0], points[points.Count - 1])
    //    {
    //        ThePoints = points;
    //    }

    //    public override List<Vector3d> GetPoints()
    //    {
    //        return ThePoints;
    //    }
    //}
}
