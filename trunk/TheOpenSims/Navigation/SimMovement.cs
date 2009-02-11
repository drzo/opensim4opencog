using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;
using System.Windows.Forms;

namespace cogbot.TheOpenSims.Navigation
{
    public class SimMovement : SimRoute
    {
        public override bool NearPoint(Vector3 e, float maxDist)
        {
            if (Vector3.Distance(Begin, e) < maxDist) return true;
            if (Vector3.Distance(End, e) < maxDist) return true;
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
            SimMovement movement = new SimMovement(ReverseMoveList);
            CopyProperties(this, movement);
            return movement;
        }

        List<SimRoute> MoveList;
        public SimMovement(List<SimRoute> ms)
            : base(ms[0].Begin, ms[ms.Count - 1].End)
        {
            MoveList = ms;
        }

        public override List<Vector3> GetPoints()
        {
            List<Vector3> points = new List<Vector3>();
            points.Add(Begin);
            Vector3 Last = Begin;
            foreach (SimRoute move in GetSegments())
            {
                if (move.Begin != Last)
                {
                    Last = move.Begin;
                    points.Add(Last);
                }
                if (move.End != Last)
                {
                    Last = move.End;
                    points.Add(Last);
                }
            }
            if (End != Last)
            {
                Last = End;
                points.Add(Last);
            }
            return points;
        }

        public override SimMovement ToSegmentCopy()
        {
            List<SimRoute> moves = GetSegments();
            return new SimMovement(moves);
        }

        public override SimMovement Append(SimRoute extra)
        {
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(this);
            MS.Add(extra);
            return new SimMovement(MS);
        }

        public override SimMovement Prepend(SimRoute extra)
        {
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(extra);
            MS.Add(this);
            return new SimMovement(MS);
        }


        public override SimRoute FillIn(float maxDist)
        {
            List<SimRoute> moves = new List<SimRoute>();
            SimWaypoint at = Begin;
            bool filled = false;
            foreach (SimRoute move in GetSegments())
            {
                if (Vector3.Distance(at, move.Begin) > maxDist)
                {
                    moves.Add(new SimRoute(at, move.Begin));
                    filled = true;
                }
                moves.Add(move);
                at = move.End;
            }
            if (filled) return new SimMovement(moves);
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
            if (p <= 0.0f) return Begin;
            foreach (SimRoute move in MoveList)
            {
                float mlen = move.GetLength();
                if (mlen > p) return move.GetPointAt(p);
                p -= mlen;
            }
            return End;
        }

        public override float GetLength()
        {
            float len = 0f;
            foreach (SimRoute mv in MoveList)
            {
                len += mv.GetLength();
            }
            return len;
        }
    }

}
