using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.TheOpenSims.Navigation
{
    public class SimRoute
    {
        public static SimRoute PointsToMovement(List<Vector3> list, float fudge)
        {
            SimRoute moveNow = new SimRoute(list[0], list[1]);
            if (list.Count > 2)
            {
                Vector3 Last = list[1];
                int listIndex = 2;
                while (listIndex < list.Count)
                {
                    if (Vector3.Distance(Last, list[listIndex]) > fudge)
                        moveNow = moveNow.AppendPoint(list[listIndex], fudge);
                    Last = list[listIndex++];
                }
            }
            return moveNow;
        }

        public SimWaypoint Begin;
        public SimWaypoint End;
        bool MustFly = false;
        bool MustCrouch = false;
        bool MustAutoPilot = false;
        public bool IsBlocked = false;
        public bool IsOneDirrection = false;

        public static SimMovement CopyProperties(SimRoute simMovement, SimMovement movement)
        {
            return (SimMovement)CopyProperties(simMovement, (SimRoute)movement);
        }

        public virtual List<Vector3> GetWayPoints(float apart)
        {
            List<Vector3> points = new List<Vector3>();
            float len = GetLength();
            float way = 0.0f;
            while (way < len)
            {
                points.Add(GetPointAt(way).GetSimPosition());
                way += apart;
            }
            points.Add(End.GetSimPosition());
            return points;
        }

        public static SimRoute CopyProperties(SimRoute simMovement, SimRoute movement)
        {
            if (simMovement.MustAutoPilot) movement.MustAutoPilot = simMovement.MustAutoPilot;
            if (simMovement.MustCrouch) movement.MustCrouch = simMovement.MustCrouch;
            if (simMovement.MustFly) movement.MustFly = simMovement.MustFly;
            if (simMovement.IsBlocked) movement.IsBlocked = simMovement.IsBlocked;
            if (simMovement.IsOneDirrection) movement.IsOneDirrection = simMovement.IsOneDirrection;
            return movement;
        }

        public SimRoute(String s)
        {
            FromFileString(s);
        }

        public SimRoute(SimWaypoint from, SimWaypoint to)
        {
            Begin = from;
            Begin.HasBegin(this);
            End = to;
            End.HasEnd(this);
        }

        public SimRoute(Vector3 from, Vector3 to)
            : this(SimWaypoint.Create(from), SimWaypoint.Create(to))
        {
        }

        public virtual SimRoute Reverse()
        {
            SimRoute movement = new SimRoute(End, Begin);
            SimRoute.CopyProperties(this, movement);
            return movement;
        }

        public virtual float GetLength()
        {
            return Vector3.Distance(Begin, End);
        }

        public override string ToString()
        {
            return ToFileString();
        }

        public virtual SimRoute FillIn(float maxDist)
        {
            return this;
        }

        public virtual SimRoute GetSegment(float start, float distlen)
        {
            List<SimRoute> newmoves = new List<SimRoute>();
            float len = GetLength();

            if (distlen <= 0.0 || start + distlen > len)
            {
                distlen = len - start;
            }
            Vector3 First = Begin.GetSimPosition();
            Vector3 Last = End.GetSimPosition();
            foreach (SimRoute move in GetSegments())
            {

                float mlen = move.GetLength();
                if (mlen > start)
                {
                    First = move.GetPointAt(start);
                    if (distlen + start < mlen)
                    {
                        Last = move.GetPointAt(distlen + start);
                        newmoves.Add(new SimRoute(First, End));
                        break; // start and end in a single segment
                    }
                    Last = move.End;
                    newmoves.Add(new SimRoute(First, End));
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
                        First = move.Begin;
                        End = move.GetPointAt(mlen);
                        newmoves.Add(new SimRoute(First, End));
                        break; // this completed it
                    }
                }
            }
            return new SimMovement(newmoves);
        }

        public virtual Vector3 NextPoint(float start)
        {
            return End;
        }

        public virtual SimMovement Divide(int by)
        {
            List<SimRoute> moves = new List<SimRoute>();
            float len = GetLength();
            float seglen = len / by;
            Vector3 beg = Begin;
            int current = 1;
            while (current < by)
            {
                Vector3 end = GetPointAt(seglen * current);
                SimRoute move = new SimRoute(beg, end);
                moves.Add(move);
                beg = end;
            }
            return CopyProperties(this, new SimMovement(moves));
        }

        public virtual SimWaypoint GetPointAt(float p)
        {
            float len = GetLength();
            if (p <= 0.0f) return Begin;
            if (p >= len) return End;
            Vector3 dir = End.GetSimPosition() - Begin.GetSimPosition();
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

        public virtual SimMovement ToSegmentCopy()
        {
            List<SimRoute> moves = new List<SimRoute>();
            moves.Add(this);
            return new SimMovement(moves);
        }

        public virtual SimMovement Append(SimRoute extra)
        {
            if (extra is SimMovement)
            {
                return extra.Prepend(this);
            }
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(this);
            MS.Add(extra);
            return new SimMovement(MS);
        }

        public virtual SimMovement Prepend(SimRoute extra)
        {
            if (extra is SimMovement)
            {
                return extra.Append(this);
            }
            List<SimRoute> MS = new List<SimRoute>();
            MS.Add(extra);
            MS.Add(this);
            return new SimMovement(MS);
        }

        public virtual List<Vector3> GetPoints()
        {
            List<Vector3> points = new List<Vector3>();
            points.Add(Begin);
            points.Add(End);
            return points;
        }

        public virtual SimRoute AppendPoint(Vector3 vector3, float fudge)
        {
            if (Vector3.Distance(End, vector3) < fudge)
            {
                return new SimRoute(Begin, vector3);
            }
            return Append(new SimRoute(End, vector3));
        }

        public virtual string ToFileString()
        {
            string s = Begin.GetSimPosition().ToRawString() + " " + End.GetSimPosition().ToRawString() + " ";
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
            Begin = SimWaypoint.Create(new Vector3(float.Parse(args[0]), float.Parse(args[1]), float.Parse(args[2])));
            Begin.HasBegin(this);
            End = SimWaypoint.Create(new Vector3(float.Parse(args[3]), float.Parse(args[4]), float.Parse(args[5])));
            End.HasEnd(this);
            if (s.Contains("MustAutoPilot")) MustAutoPilot = true;
            if (s.Contains("MustFly")) MustFly = true;
            if (s.Contains("MustCrouch")) MustCrouch = true;
            if (s.Contains("IsBlocked")) IsBlocked = true;
            if (s.Contains("IsOneDirrection")) IsOneDirrection = true;
        }

        public virtual bool NearPoint(Vector3 e, float maxDist)
        {
            if (Vector3.Distance(Begin, e) < maxDist) return true;
            if (Vector3.Distance(End, e) < maxDist) return true;
            return false;
        }
    }

}
