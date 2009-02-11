using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.TheOpenSims.Navigation
{
    public class SimWaypoint : SimPosition
    {

        public static implicit operator Vector3(SimWaypoint m)
        {
            return m.GetSimPosition();
        }
        public static implicit operator Vector2(SimWaypoint m)
        {
            Vector3 v3 = m.GetSimPosition();
            return new Vector2(v3.X, v3.Y);
        }
        public static implicit operator Vector3d(SimWaypoint m)
        {
            Vector3 v3 = m.GetSimPosition();
            return new Vector3d(v3.X, v3.Y, v3.Z);
        }

        public override string ToString()
        {
            return GetSimPosition().ToRawString();
        }
        public override bool Equals(object obj)
        {
            if (obj is SimPosition)
            {
                return GetSimPosition() == ((SimPosition)obj).GetSimPosition();
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

        public List<SimRoute> BeginMovements = new List<SimRoute>();
        public List<SimRoute> EndMovements = new List<SimRoute>();
        private SimWaypoint(Vector3 firstP)
        {
            SimPosition = RoundPoint(firstP);
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

        public Vector3 GetSimPosition()
        {
            return SimPosition;
        }

        internal void HasBegin(SimRoute simMovement)
        {
            BeginMovements.Add(simMovement);
        }
        internal void HasEnd(SimRoute simMovement)
        {
            EndMovements.Add(simMovement);
        }

        #region SimPosition Members

        public Vector3 GetUsePosition()
        {
            return GetSimPosition();
        }

        public float GetSizeDistance()
        {
            return 0.17f;
        }

        #endregion
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
