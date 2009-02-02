using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;

namespace cogbot.TheOpenSims
{

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

    public class SimMovement
    {
        public static SimMovement PointsToMovement(List<Vector3> list, float fudge)
        {
            SimMovement moveNow = new SimMovement(list[0], list[1]);
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

        public Vector3 Begin;
        public Vector3 End;
        bool MustFly = false;
        bool MustCrouch = false;
        bool MustAutoPilot = false;
        bool IsBlocked = false;
        bool IsOneDirrection = false;
        public static SimMovementComplex CopyProperties(SimMovement simMovement, SimMovementComplex movement)
        {
            return (SimMovementComplex)CopyProperties(simMovement, (SimMovement)movement);
        }

        public virtual List<Vector3> GetWayPoints(float apart)
        {
            List<Vector3> points = new List<Vector3>();
            float len = GetLength();
            float way = 0.0f;
            while (way < len)
            {
                points.Add(GetPointAt(way));
                way += apart;
            }
            points.Add(End);
            return points;
        }

        public static SimMovement CopyProperties(SimMovement simMovement, SimMovement movement)
        {
            if (simMovement.MustAutoPilot) movement.MustAutoPilot = simMovement.MustAutoPilot;
            if (simMovement.MustCrouch) movement.MustCrouch = simMovement.MustCrouch;
            if (simMovement.MustFly) movement.MustFly = simMovement.MustFly;
            if (simMovement.IsBlocked) movement.IsBlocked = simMovement.IsBlocked;
            if (simMovement.IsOneDirrection) movement.IsOneDirrection = simMovement.IsOneDirrection;
            return movement;
        }
        public SimMovement(Vector3 from, Vector3 to)
        {
            Begin = from;
            End = to;
        }
        public SimMovement GetReverse()
        {
            SimMovement movement = new SimMovement(End, Begin);
            SimMovement.CopyProperties(this, movement);
            return movement;
        }

        public virtual float GetLength()
        {
            return Vector3.Distance(Begin, End);
        }
        public override string ToString()
        {
            return "" + Begin + "->" + End;
        }

        public virtual SimMovement FillIn(float maxDist)
        {
            return this;
        }

        public virtual SimMovement GetSegment(float start, float distlen)
        {
            List<SimMovement> newmoves = new List<SimMovement>();
            float len = GetLength();

            if (distlen <= 0.0 || start + distlen > len)
            {
                distlen = len - start;
            }
            Vector3 First = Begin;
            Vector3 Last = End;
            foreach (SimMovement move in GetSegments())
            {

                float mlen = move.GetLength();
                if (mlen > start)
                {
                    First = move.GetPointAt(start);
                    if (distlen + start < mlen)
                    {
                        Last = move.GetPointAt(distlen + start);
                        newmoves.Add(new SimMovement(First, End));
                        break; // start and end in a single segment
                    }
                    Last = move.End;
                    newmoves.Add(new SimMovement(First, End));
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
                        newmoves.Add(new SimMovement(First, End));
                        break; // this completed it
                    }
                }
            }
            return new SimMovementComplex(newmoves);
        }

        public virtual Vector3 NextPoint(float start)
        {
            return End;
        }

        public virtual SimMovementComplex Divide(int by)
        {
            List<SimMovement> moves = new List<SimMovement>();
            float len = GetLength();
            float seglen = len / by;
            Vector3 beg = Begin;
            int current = 1;
            while (current < by)
            {
                Vector3 end = GetPointAt(seglen*current);
                SimMovement move = new SimMovement(beg, end);
                moves.Add(move);
                beg = end;
            }
            return CopyProperties(this,new SimMovementComplex(moves));
            
        }

        public virtual Vector3 GetPointAt(float p)
        {
            float len = GetLength();
            if (p <= 0.0f) return Begin;
            if (p >= len) return End;
            Vector3 dir = End - Begin;
            float X = (dir.X / len) * p;
            float Y = (dir.Y / len) * p;
            float Z = (dir.Z / len) * p;
            return new Vector3(X, Y, Z);
        }
        
        public virtual List<SimMovement> GetSegments() {
            List<SimMovement> moves = new List<SimMovement>();
            moves.Add(this);
            return moves;
        }

        public virtual SimMovementComplex ToSegmentCopy()
        {
            List<SimMovement> moves = new List<SimMovement>();
            moves.Add(this);
            return new SimMovementComplex(moves);
        }

        public virtual SimMovementComplex Append(SimMovement extra)
        {
            if (extra is SimMovementComplex)
            {
                return extra.Prepend(this);
            }
            List<SimMovement> MS = new List<SimMovement>();
            MS.Add(this);
            MS.Add(extra);
            return new SimMovementComplex(MS);
        }

        public virtual SimMovementComplex Prepend(SimMovement extra)
        {
            if (extra is SimMovementComplex)
            {
                return extra.Append(this);
            }
            List<SimMovement> MS = new List<SimMovement>();
            MS.Add(extra);
            MS.Add(this);
            return new SimMovementComplex(MS);
        }

        public virtual List<Vector3> GetPoints()
        {
            List<Vector3> points = new List<Vector3>();
            points.Add(Begin);
            points.Add(End);
            return points;
        }

        public virtual SimMovement AppendPoint(Vector3 vector3, float fudge)
        {
            if (Vector3.Distance(End, vector3) < fudge)
            {
                return new SimMovement(Begin, vector3);
            }
            return Append(new SimMovement(End, vector3));
        }
    }

    public class SimMovementComplex : SimMovement
    {
        List<SimMovement> MoveList;
        public SimMovementComplex(List<SimMovement> ms)
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

        public override SimMovementComplex ToSegmentCopy()
        {
            List<SimMovement> moves = GetSegments();
            return new SimMovementComplex(moves);
        }

        public override SimMovementComplex Append(SimMovement extra)
        {
            List<SimMovement> MS = new List<SimMovement>();
            MS.Add(this);
            MS.Add(extra);
            return new SimMovementComplex(MS);
        }

        public override SimMovementComplex Prepend(SimMovement extra)
        {
            List<SimMovement> MS = new List<SimMovement>();
            MS.Add(extra);
            MS.Add(this);
            return new SimMovementComplex(MS);
        }


        public override SimMovement FillIn(float maxDist)
        {
            List<SimMovement> moves = new List<SimMovement>();
            Vector3 at = Begin;
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
            if (filled) return new SimMovementComplex(moves);
            return this;
        }

        public override List<SimMovement>  GetSegments(){ 	       
            List<SimMovement> moves = new List<SimMovement>();
            foreach(SimMovement move in MoveList) {
                foreach (SimMovement one in move.GetSegments()) {
                    moves.Add(one);
                }
            }
            return moves;
        }


        public override Vector3 GetPointAt(float p)
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

    public class MovementToVector
    {
        public static bool MoveTo(SimAvatar bc, Vector3 targ, float dist)
        {
            MovementToVector mtv = new MovementToVector(bc, targ, dist);
            mtv.Goto();
            if (mtv.GetDistance() > dist) return false;
            return true;
        }
        SimAvatar theAvatar;
        Vector3 Destination;
        Vector3 LastPosition;
        BotClient Client;
        //private AutoResetEvent Ready = new AutoResetEvent(false);
        Boolean justStopped = false;
        float lastDistance = Single.MaxValue;
        int autoPilotsRemaining = 6;

        float followDist = 2.0F;
        public MovementToVector(SimAvatar bc, Vector3 targ, float fd)
        {
            theAvatar = bc;
            Client = bc.GetGridClient();
            Destination = targ;
            followDist = fd;
        }

        public void Goto()
        {
            float d = GetDistance();
            if (d < followDist)
            {
                followDist = d / 2;
            }
            //Client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
            tracker();
            StopMoving();
            Client.Self.Movement.TurnToward(Destination);
            if (madePhantom.Count > 0)
            {
                foreach (SimObject obj in madePhantom)
                {
                    obj.RestoreEnterable();
                }
                madePhantom.Clear();
            }
        }

        private float GetDistance()
        {
            return Vector3.Distance(Client.Self.SimPosition, Destination);
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            //{
            //    if (Vector3.Distance(Client.Self.BotPosition, Destination) > followDist)
            //    {
            //        //if (Vector3.Dist(LastTarget, Destination) > 1)
            //        //{
            //        //   LastTarget = Destination;
            //        //    Client.Self.Movement.TurnToward(Destination);
            //        //    Client.Self.Movement.AtPos = true;
            //        //    //Client.Self.AutoPilotCancel();
            //        //      Client.Self.Movement.UpdateInterval = 0;
            //        //    Client.Self.Movement.SendUpdate();
            //        //}
            //        //      Client.Self.AutoPilotLocal((int)Destination.X,
            //        //          (int)Destination.Y, Destination.Z);
            //    }
            //    else
            //    {
            //        //Client.Self.AutoPilotCancel();
            //    }
            //}
        }

        readonly ListAsSet<SimObject> madePhantom = new ListAsSet<SimObject>();

        void tracker()
        {
            float curDist = GetDistance();
            bool UseAutoPilot = false;
            float traveled = 10f;
            while (curDist > followDist && autoPilotsRemaining > 0)
            {
                LastPosition = Client.Self.SimPosition;
                if (UseAutoPilot)
                {
                    autoPilotsRemaining--;
                    if (autoPilotsRemaining > 0)
                    {
                        Console.WriteLine("AutoPilot due to traveled=" + traveled);
                        PhantomizeArea();
                        Client.Self.AutoPilot(Destination.X, Destination.Y, Destination.Z);
                        Thread.Sleep(2000);
                    }
                    else
                    {
                        UseAutoPilot = false;
                    }

                }
                if (!UseAutoPilot)
                {
                    Client.Self.AutoPilotCancel();
                    UpdateHeading();
                }
                Thread.Sleep(250);
                traveled = Vector3.Distance(LastPosition, Client.Self.SimPosition);
                if (traveled < 0.1)
                {
                    UseAutoPilot = true;
                }
                else
                {
                    UseAutoPilot = false;
                }

                curDist = GetDistance();

            }
            Client.Self.AutoPilotCancel();
        }

        private void PhantomizeArea()
        {
                foreach (SimObject obj in theAvatar.GetNearByObjects(2.0f, true)) //should be false
                {
                    madePhantom.AddTo(obj);
                    obj.MakeEnterable();
                }
        }

        private void UpdateHeading()
        {
            Random somthing = new Random(Environment.TickCount);// We do stuff randomly here
            float curDist = GetDistance();

            if (lastDistance <= curDist)
            {
                //    StopMoving();
                //    followDist = curDist + 1.0F;
            }
            lastDistance = curDist;

            if (curDist > followDist)
            {

                //Client.Self.AnimationStop(Animations.WALK, true);
                //Client.Self.AnimationStart(Animations.WALK, true);
                //Client.Self.Movement.SendUpdate();
                if (curDist < (followDist * 1.25))
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(125);
                    Client.Self.Movement.Stop = true;
                    Client.Self.Movement.AtPos = false;
                    Client.Self.Movement.NudgeAtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(100);
                    Client.Self.Movement.NudgeAtPos = false;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(100);
                }
                else
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.UpdateInterval = 0; //100
                    Client.Self.Movement.SendUpdate(true);
                    //(int)(25 * (1 + (curDist / followDist)))
                    Thread.Sleep(somthing.Next(25, 100));
                }
                justStopped = true;
            }
            else
            {
                if (justStopped)
                {
                    StopMoving();

                    Thread.Sleep(25);
                    justStopped = false;
                }
                else
                {
                    Thread.Sleep(100);
                }


            }
        }

        private void StopMoving()
        {
            Client.Self.Movement.TurnToward(Destination);
            Client.Self.Movement.AtPos = false;
            //Client.Self.Movement.UpdateInterval = 0;
            Client.Self.Movement.StandUp = true;
            //Client.Self.Movement.SendUpdate();
            Client.Self.Movement.FinishAnim = true;
            Client.Self.Movement.Stop = true;
            Client.Self.Movement.SendUpdate(true);

        }
    }
    
}
