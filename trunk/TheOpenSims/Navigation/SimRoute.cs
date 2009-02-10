using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.TheOpenSims.Navigation
{
    public class SimRoute : SimMovement
    {
        public override bool NearPoint(Vector3 e, float maxDist)
        {
            if (Vector3.Distance(Begin, e) < maxDist) return true;
            if (Vector3.Distance(End, e) < maxDist) return true;
            foreach (SimMovement move in MoveList)
            {
                if (move.NearPoint(e, maxDist)) return true;
            }
            return false;
        }

        public override SimMovement Reverse()
        {
            List<SimMovement> ReverseMoveList = new List<SimMovement>();
            foreach (SimMovement move in MoveList)
            {
                ReverseMoveList.Insert(0, move.Reverse());
            }
            SimRoute movement = new SimRoute(ReverseMoveList);
            CopyProperties(this, movement);
            return movement;
        }

        List<SimMovement> MoveList;
        public SimRoute(List<SimMovement> ms)
            : base(ms[0].Begin, ms[ms.Count - 1].End)
        {
            MoveList = ms;
        }

        public override List<Vector3> GetPoints()
        {
            List<Vector3> points = new List<Vector3>();
            points.Add(Begin);
            Vector3 Last = Begin;
            foreach (SimMovement move in GetSegments())
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

        public override SimRoute ToSegmentCopy()
        {
            List<SimMovement> moves = GetSegments();
            return new SimRoute(moves);
        }

        public override SimRoute Append(SimMovement extra)
        {
            List<SimMovement> MS = new List<SimMovement>();
            MS.Add(this);
            MS.Add(extra);
            return new SimRoute(MS);
        }

        public override SimRoute Prepend(SimMovement extra)
        {
            List<SimMovement> MS = new List<SimMovement>();
            MS.Add(extra);
            MS.Add(this);
            return new SimRoute(MS);
        }


        public override SimMovement FillIn(float maxDist)
        {
            List<SimMovement> moves = new List<SimMovement>();
            SimWaypoint at = Begin;
            bool filled = false;
            foreach (SimMovement move in GetSegments())
            {
                if (Vector3.Distance(at, move.Begin) > maxDist)
                {
                    moves.Add(new SimMovement(at, move.Begin));
                    filled = true;
                }
                moves.Add(move);
                at = move.End;
            }
            if (filled) return new SimRoute(moves);
            return this;
        }

        public override List<SimMovement> GetSegments()
        {
            List<SimMovement> moves = new List<SimMovement>();
            foreach (SimMovement move in MoveList)
            {
                foreach (SimMovement one in move.GetSegments())
                {
                    moves.Add(one);
                }
            }
            return moves;
        }


        public override SimWaypoint GetPointAt(float p)
        {
            if (p <= 0.0f) return Begin;
            foreach (SimMovement move in MoveList)
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
            foreach (SimMovement mv in MoveList)
            {
                len += mv.GetLength();
            }
            return len;
        }
    }
}
