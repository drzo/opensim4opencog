using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Collections;
using PathSystem3D.Navigation.Debug;
using System.Drawing;
using PathSystem3D.Mesher;
using System.Threading;

namespace PathSystem3D.Navigation
{
    /// <summary>
    /// Basically a node is defined with a geographical position in space.
    /// It is also characterized with both collections of outgoing arcs and incoming arcs.
    /// </summary>
    [Serializable]
    public class SimWaypointImpl : PathSystem3D.Navigation.SimWaypoint,SimPosition
    {

        public SimPosition UsePosition
        {
            get
            {
                return this;
            }
        }
        public string DistanceVectorString(SimPosition pos)
        {
            Vector3 loc = pos.SimPosition;
            SimPathStore R = pos.PathStore;
            return String.Format("{0:0.0#}m ", Vector3d.Distance(GlobalPosition, pos.GlobalPosition))
               + String.Format("{0}/{1:0.0#}/{2:0.0#}/{3:0.0#}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public double Distance(SimPosition other)
        {
            return Vector3d.Distance(GlobalPosition, other.GlobalPosition);
        }

        public bool IsGroundLevel()
        {
            return GetZLevel(Plane) == GetGroundLevel();
        }

        public bool IsUnderWater()
        {
            return GetZLevel(Plane) < PathStore.WaterHeight;
        }

        private float _MinZ;

        public float MinZ
        {
            get { return _MinZ; }
            set { _MinZ = value; }
        }
        private float _MaxZ;

        public float MaxZ
        {
            get { return _MaxZ; }
            set { _MaxZ = value; }
        }

        public bool IsFlyZone()
        {
            return IsUnderWater();
        }

        public string OccupiedString(CollisionPlane cp)
        {
            IEnumerable<CollisionObject> OccupiedListObject = GetOccupied();
            string S = "";
            lock (OccupiedListObject)
            {
                foreach (CollisionObject O in OccupiedListObject)
                {
                    S += O.ToString();
                    S += "\r\n";
                }
            }
            return S + this.ToString() + " " + ExtraInfoString(cp);
        }

        private IEnumerable<CollisionObject> GetOccupied()
        {
            return CIndex.GetOccupied(MinZ, MaxZ);
        }

        public string ExtraInfoString(CollisionPlane cp)
        {
            return CIndex.ExtraInfoString(cp);
        }

        private float GetMatrix(CollisionPlane CP)
        {
            return CIndex.GetMatrix(CP);
        }

        private void SetMatrix(CollisionPlane CP, int v)
        {
            CIndex.SetMatrixForced(CP, v);
        }

        private float GetZLevel(CollisionPlane CP)
        {
            return CIndex.GetZLevel(MinZ, MaxZ);
        }

        private CollisionPlane _Plane;
        public CollisionPlane Plane
        {
            get
            {
                return _Plane;
            }
            set
            {
                _Plane = value;
            }
        }

        public float GetGroundLevel()
        {
            return CIndex.GetGroundLevel();
        }


        public SimWaypoint GetWaypoint()
        {
            return this;
        }

        public bool TryGetSimPosition(out Vector3 pos)
        {
            if (!IsRegionAttached)
            {
                pos = default(Vector3);
                return false;
            }
            pos = SimPosition;
            return true;
        }

        public bool IsRegionAttached
        {
            get { return PathStore != null; }
        }

        Vector3d _GlobalPos;
        bool _Passable;
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
        public bool IsPassable
        {
            set
            {
                if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs) A.Passable = value;
                if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs) A.Passable = value;
                _Passable = value;
                SetMatrix(Plane, (byte)(value ? SimPathStore.PASSABLE : SimPathStore.BLOCKED));
            }
            get
            {
                if (!_Passable) return false;
                if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs) if (A.Passable) return true;
                if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs) if (A.Passable) return true;
                return GetMatrix(Plane) != SimPathStore.BLOCKED;
            }
        }

        /// <summary>
        /// Gets X coordinate on the Graph debugger (whole world position).
        /// </summary>
        public double DX { get { return (_GlobalPos.X / GraphFormer.DSCALE) - GraphFormer.StartX; } }

        /// <summary>
        /// Gets Y coordinate on the Graph debugger
        /// </summary>
        public double DY { get { return (_GlobalPos.Y / GraphFormer.DSCALE) - GraphFormer.StartY; } }

        /// <summary>
        /// Gets Z coordinate.
        /// </summary>
        ///public double DZ { get { return _GlobalPos.Z; } }

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

        public void SetGlobalPos(Vector3d v3d)
        {
            SimPathStore R = SimPathStore.GetPathStore(v3d);
            _LocalPos = SimPathStore.GlobalToLocal(v3d);
            PathStore = R.GetPathStore3D(_LocalPos);
            _GlobalPos = R.LocalToGlobal(_LocalPos);
            //PX = (int)Math.Round(_LocalPos.X * PathStore.POINTS_PER_METER);
            //PY = (int)Math.Round(_LocalPos.Y * PathStore.POINTS_PER_METER);
            if (_IncomingArcs != null) foreach (SimRoute A in _IncomingArcs) A.LengthUpdated = false;
            if (_OutgoingArcs != null) foreach (SimRoute A in _OutgoingArcs) A.LengthUpdated = false;
        }

        /// <summary>
        /// Gets/Sets the geographical position of the node.
        /// </summary>
        /// <exception cref="ArgumentNullException">Cannot set the _GlobalPos to null.</exception>
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
            //    return "(" + _GlobalPos.ToRawString() + ")"; 
            SimPathStore R = PathStore;
            Vector3 loc = SimPosition;
            return String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        ///// <summary>
        ///// Object.Equals override.
        ///// Tells if two nodes are equal by comparing positions.
        ///// </summary>
        ///// <exception cref="ArgumentException">A Node cannot be compared with another type.</exception>
        ///// <param name="O">The node to compare with.</param>
        ///// <returns>'true' if both nodes are equal.</returns>
        //public override bool Equals(object O)
        //{
        //    if (O is MeshableObject)
        //    {
        //        return _GlobalPos == ((MeshableObject)O).GlobalPosition();
        //    }
        //    //if (O is Vector3d)
        //    //{
        //    //    return Create((Vector3d)O).Equals(this);
        //    //}

        //    throw new ArgumentException("Type " + O.GetType() + " cannot be compared with type " + GetType() + " !");
        //}


        /// <summary>
        /// Returns a copy of this node.
        /// </summary>
        /// <returns>The reference of the new object.</returns>
        public object Clone()
        {
            SimWaypointImpl N = new SimWaypointImpl(_LocalPos, _GlobalPos, CIndex, Plane, PathStore);
            N._Passable = _Passable;
            return N;
        }

        /// <summary>
        /// Object.GetHashCode override.
        /// </summary>
        /// <returns>HashCode value.</returns>
        public override int GetHashCode() { return _GlobalPos.GetHashCode(); }

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

        public double GlobalXYZ(int i)
        {
            switch (i)
            {
                case 0:
                    return _GlobalPos.X;
                case 1:
                    return _GlobalPos.Y;
                case 2:
                    return _GlobalPos.Z;
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

        Vector3 _LocalPos;
        CollisionIndex CIndex;
        private SimWaypointImpl(Vector3 local, Vector3d global, CollisionIndex Ci, CollisionPlane Cp, SimPathStore pathStore)
        {
            _MinZ = local.Z;
            _MaxZ = _MinZ + 1.5f;
            CIndex = Ci;
            _Plane = Cp;
            _PathStore = pathStore;
            _GlobalPos = global;// RoundPoint(firstP, PathStore);
            _LocalPos = local;
            //PX = PX0;
            //PY = PY0;
            //PathStore.SimWaypoint[PX, PY] = this;
            //TaintMatrix();
            //UpdateMatrix(pathStore.GL);
            _Passable = true;
            _IncomingArcs = null;
            _OutgoingArcs = null;
        }

        //public static Vector3 RoundPoint(Vector3 vect3, SimPathStore PathStore)
        //{
        //    double POINTS_PER_METER = PathStore.POINTS_PER_METER;            
        //    vect3.X = (float)(Math.Round(vect3.X * POINTS_PER_METER, 0) / POINTS_PER_METER);
        //    vect3.Y = (float)(Math.Round(vect3.Y * POINTS_PER_METER, 0) / POINTS_PER_METER);
        //    vect3.Z = (float)(Math.Round(vect3.Z * POINTS_PER_METER, 0) / POINTS_PER_METER);
        //    return vect3;
        //}

        public static SimWaypoint CreateLocal(Vector3 from, SimPathStore PathStore)
        {
            return CreateLocal(from, PathStore.GetCollisionPlane(from.Z));
        }

        public static SimWaypoint CreateLocal(Vector3 from, CollisionPlane CP)
        {
            SimPathStore PathStore = CP.PathStore;
            float POINTS_PER_METER = PathStore.POINTS_PER_METER;
            int PX = PathStore.ARRAY_X(from.X);
            int PY = PathStore.ARRAY_Y(from.Y);
            SimWaypoint WP;
            CollisionIndex CI = CollisionIndex.CreateCollisionIndex(from, PathStore);
            lock (CI)
            {
                WP = CI.FindWayPoint(from.Z);
                if (WP != null) return WP;
                from.X = PX / POINTS_PER_METER;
                from.Y = PY / POINTS_PER_METER;
                Vector3d GlobalPos = PathStore.LocalToGlobal(from);
                if (GlobalPos.X < 256 || GlobalPos.Y < 256)
                {
                    Console.WriteLine("bad global " + GlobalPos);
                }
                WP = new SimWaypointImpl(from, GlobalPos, CI, CP, PathStore);
                WP.IsPassable = true;
            }
            // wp._IncomingArcs = new ArrayList();
            // wp._OutgoingArcs = new ArrayList();
            //  PathStore.EnsureKnown(wp);
            return WP;
        }

        public Vector3 SimPosition
        {
            get { return _LocalPos; }
        }

        #region MeshableObject Members

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
                IsPassable = true;
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
            return Vector3d.Distance(P, _GlobalPos);
        }


        #region MeshableObject Members

        public Quaternion SimRotation
        {
            get { return Quaternion.Identity; }
        }


        public Vector3d GlobalPosition
        {
            get { return _GlobalPos; }
        }

        #endregion

        public static SimWaypoint CreateGlobal(double gx, double gy, double gz)
        {
            return CreateGlobal(new Vector3d(gx, gy, gz));
        }

        public static SimWaypoint CreateGlobal(Vector3d v3d)
        {
            Vector3 v3 = SimPathStore.GlobalToLocal(v3d);
            SimPathStore PathStore = SimPathStore.GetPathStore(v3d);
            return CreateLocal(v3, PathStore);
        }

        private void Debug(string format, params object[] objs)
        {
            SimPathStore.Debug(format, objs);
        }

        #region MeshableObject Members

        private SimPathStore _PathStore;
        public SimPathStore PathStore
        {
            get
            {
                return _PathStore;
            }
            set
            {
                _PathStore = value;
            }
        }

        #endregion
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
    public interface SimWaypoint : SimPosition
    {
        SimWaypoint[] AccessibleNodes { get; }
        SimWaypoint[] AccessingNodes { get; }
        SimRoute ArcComingFrom(SimWaypoint N);
        int ArcCount();
        SimRoute ArcGoingTo(SimWaypoint N);
        void ChangeXYZDebug(double PositionX, double PositionY, double PositionZ);
        object Clone();
        double Distance(OpenMetaverse.Vector3d P);
        double Distance(SimPosition e);
        double DX { get; }
        double DY { get; }
        bool EnsureAtLeastOnePath();
        int GetHashCode();
        SimWaypoint GetWaypoint();
        double GlobalXYZ(int i);
        bool GoesTo(SimWaypoint e);
        System.Collections.IList IncomingArcs { get; }
        SimWaypoint[] Molecule { get; }
        System.Collections.IList OutgoingArcs { get; }
        bool IsPassable { get; set; }
        CollisionPlane Plane { get; set; }
        void SetGlobalPos(OpenMetaverse.Vector3d v3d);
        string ToString();
        void UntieIncomingArcs();
        void UntieOutgoingArcs();

        OpenMetaverse.Vector3d Position { get; set; }
        float MinZ { get; set; }
        float MaxZ { get; set; }
        bool IsFlyZone();
        bool IsGroundLevel();
        void Isolate();
        bool IsUnderWater();
        string OccupiedString(CollisionPlane cp);
       // SimPathStore GetSimRegion();
    }
}
