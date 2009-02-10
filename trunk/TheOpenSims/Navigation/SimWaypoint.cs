using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.TheOpenSims.Navigation
{
    public class SimWaypoint
    {
        public override bool Equals(object obj)
        {
            if (obj is SimWaypoint)
            {
                return ToVector() == ((SimWaypoint)obj).ToVector();
            }
            if (obj is Vector3)
            {
                return  Create((Vector3)obj).Equals(this);
            }
            return base.Equals(obj);
        }

        public override int GetHashCode()
        {
            return SimPosition.GetHashCode();
        }

        public List<SimMovement> BeginMovements = new List<SimMovement>();
        public List<SimMovement> EndMovements = new List<SimMovement>();
        private SimWaypoint(Vector3 firstP)
        {
            SimPosition = RoundPoint(firstP);
        }

        public static implicit operator Vector3(SimWaypoint m)
        {
            return m.ToVector();
        }
        public static implicit operator Vector2(SimWaypoint m)
        {
            return new Vector2(m.SimPosition.X, m.SimPosition.Y);
        }
        public static implicit operator Vector3d(SimWaypoint m)
        {
            return new Vector3d(m.SimPosition.X, m.SimPosition.Y,m.SimPosition.Z);
        }

        protected Vector3 SimPosition;

        public static Vector3 RoundPoint(Vector3 point)
        {
            Vector3 vect3 = new Vector3(point);
            vect3.X = (float)Math.Round(vect3.X, 1);
            vect3.Y = (float)Math.Round(vect3.Y, 1);
            vect3.Z = (float)Math.Round(vect3.Z, 0);
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
            InternedPoints[rounded] = wp;
            return wp;
        }

        public Vector3 ToVector()
        {
            return SimPosition;
        }

        internal void HasBegin(SimMovement simMovement)
        {
            BeginMovements.Add(simMovement);
        }
        internal void HasEnd(SimMovement simMovement)
        {
            EndMovements.Add(simMovement);
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
