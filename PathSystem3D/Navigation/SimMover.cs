using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;
using System.Drawing;
using System.Windows.Forms;

namespace PathSystem3D.Navigation
{
    public interface SimMover : PathSystem3D.Navigation.SimPosition
    {
        void TurnToward(Vector3d targetPosition);
        //void SetMoveTarget(MeshableObject v3);
        void StopMoving();
        bool MoveTo(Vector3d end, double maxDistance, int maxSeconds);
        // Quaternion GetSimRotation();
        void Debug(string format, params object[] args);
        //double Distance(MeshableObject v3);
       // List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly);

        ///oid Touch(MeshableObject simObject);

        void OpenNearbyClosedPassages();
    }

    public enum SimMoverState : byte
    {
        PAUSED = 0,
        MOVING = 1,
        BLOCKED = 2,
        COMPLETE = 3,
        TRYAGAIN = 4
    }



    public class SimAbstractMover
    {

        static Dictionary<SimMover, CollisionPlane> MoverPlanes = new Dictionary<SimMover, CollisionPlane>();
            
            
        public SimMoverState STATE
        {
            get { return _STATE; }
            set { _STATE = value; }
        }

        protected SimMoverState _STATE = SimMoverState.PAUSED;
        protected SimMover Mover;
        protected readonly Vector3d FinalLocation;
        protected readonly double FinalDistance;
        protected double CloseDistance = 1f;// SimPathStore.LargeScale-SimPathStore.StepSize;

        protected SimPathStore PathStore
        {
            get { return Mover.GetPathStore(); }
        }
        CollisionPlane MoverPlane = null;
        static protected double TurnAvoid = 0f;
        bool UseTurnAvoid = false;  // this toggles each time

        public SimAbstractMover(SimMover mover, Vector3d finalGoal, double finalDistance)
        {
            Mover = mover;
            FinalDistance = finalDistance;
            FinalLocation = finalGoal;
            lock (MoverPlanes)
            {
                if (!MoverPlanes.ContainsKey(mover))
                {
                    MoverPlanes[mover] = PathStore.CreateMoverPlane(mover.GetSimPosition().Z);
                }
                MoverPlane = MoverPlanes[mover];
            }
        }


        private Vector3 GetSimPosition()
        {
            return Mover.GetSimPosition();
        }

        public bool FollowPathTo(Vector3d globalEnd, double distance)
        {
            bool OnlyStart = true;
            bool MadeIt = true;

            while (OnlyStart && MadeIt)
            {
                if (Vector3d.Distance(GetWorldPosition(), globalEnd) < distance) return true;
                IList<Vector3d> route = SimPathStore.GetPath(MoverPlane,GetWorldPosition(), globalEnd, distance, out OnlyStart);
                MadeIt = FollowPathTo(route, globalEnd, distance);
            }
            return MadeIt;
        }

        bool UseSkipping = false;
        bool UseReverse = false;
        public bool FollowPathTo(IList<Vector3d> v3s, Vector3d finalTarget, double finalDistance)
        {
            Vector3d vstart = v3s[0];
            Vector3 vv3 = SimPathStore.GlobalToLocal(vstart);

            v3s = SimPathStore.GetSimplifedRoute(vstart, v3s, 45, 4f);

            Debug("FollowPath: {0} -> {1} for {2}", v3s.Count, DistanceVectorString(finalTarget), finalDistance);
            int CanSkip = UseSkipping ? 1 : 0; //never right now
            int Skipped = 0;
            UseSkipping =  !UseSkipping;
            int v3sLength = v3s.Count;
            int at = 0;
            while (at < v3sLength)
            {
                Vector3d v3 = v3s[at];
                // try to get there first w/in StepSize 0.2f 
                if (!MoveTo(v3, PathStore.StepSize, 3))
                {
                    if (Vector3d.Distance(GetWorldPosition(), finalTarget) < finalDistance) return true;
                    int nbest = ClosestAt(v3s);
                    if (nbest > at)
                    {
                        Debug("Fast-forward {0} -> {1} ", at, nbest);
                        at = nbest + 1;
                        continue;
                    }
                    if (!MoveTo(v3, PathStore.StepSize*3, 2))
                    {
                        if (Skipped++ <= CanSkip)
                        {
                            MoveToPassableArround(GetSimPosition());
                            Skipped++;
                            at++;
                            continue;
                        }
                        Vector3 l3 = SimPathStore.GlobalToLocal(v3);
                        BlockTowardsVector(l3);
                        Debug("!MoveTo: {0} -> {1} -> {2} ", DistanceVectorString(GetWorldPosition()), DistanceVectorString(v3), DistanceVectorString(finalTarget));
                        MoveToPassableArround(l3);
                        return false;
                    }
                }
                else
                {
                    Skipped = 0;
                }
                if (Vector3d.Distance(GetWorldPosition(), finalTarget) < finalDistance) return true;
                int best = ClosestAt(v3s);
                if (best > at)
                {
                    Debug("Fastforward {0} -> {1} ", at, best);
                }
                else if (best < at)
                {
                    Debug("Reverse {0} -> {1} ", at, best);
                    if (UseReverse)
                    {
                        best = at;
                    }
                    UseReverse = !UseReverse;
                }
                at = best+1;

            }
            Debug("Complete: {0} -> {1}", DistanceVectorString(GetWorldPosition()), DistanceVectorString(finalTarget));
            return true;
        }



        private int ClosestAt(IList<Vector3d> v3s)
        {
            Vector3d newPos = GetWorldPosition();
            int v3sLength = v3s.Count-1;
            double bestDist = Vector3d.Distance(newPos, v3s[v3sLength]);
            int best = v3sLength;
            while (v3sLength-->0)
            {
                double lenNat = Vector3d.Distance(newPos, v3s[v3sLength]);
                if (lenNat < bestDist)
                {
                    best = v3sLength;
                    bestDist = lenNat;
                }
            }
            return best;
        }

        public string DistanceVectorString(Vector3d loc3d)
        {
            Vector3 loc = SimPathStore.GlobalToLocal(loc3d);
            SimPathStore R = SimPathStore.GetPathStore(loc3d);
            return String.Format("{0:0.00}m ", Vector3d.Distance(GetWorldPosition(), loc3d))
               + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public string DistanceVectorString(Vector3 loc)
        {    
            return String.Format("{0:0.00}m ", Vector3.Distance(GetSimPosition(), loc))
               + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", PathStore.RegionName, loc.X, loc.Y, loc.Z);
        }

        public bool MoveTo(Vector3d finalTarget, double maxDistance, int maxSeconds)
        {
            return Mover.MoveTo(finalTarget, maxDistance, maxSeconds);
        }


        //    internal Vector3d GetUsePosition()
        //    {
        //        return Mover.GetUsePosition();
        //        //throw new Exception("The method or operation is not implemented.");
        //    }


        public Vector3 MoveToPassableArround(Vector3 start)
        {
            if (!UseTurnAvoid)
            {
                UseTurnAvoid = !UseTurnAvoid;
                return start;
            }

            UseTurnAvoid = !UseTurnAvoid;
            double A45 = 45f / SimPathStore.RAD2DEG;
            for (double angle = A45; angle < SimPathStore.PI2; angle += A45)
            {
                Vector3 next = ZAngleVector(angle + TurnAvoid) * 2 + start;
                if (PathStore.IsPassable(next))
                {
                    Vector3d v3d = PathStore.LocalToGlobal(start);
                    if (MoveTo(v3d, 1, 4))
                    {
                        TurnAvoid += angle;  // update for next use
                        if (TurnAvoid > SimPathStore.PI2)
                            TurnAvoid -= SimPathStore.PI2;
                        return next;
                    }
                }
            }
            return start;
        }

        //    internal bool MoveTo(Vector3d endVect)
        //    {
        //        bool MadeIt = Mover.MoveTo(endVect, CloseDistance, 6);
        //        if (!MadeIt)
        //        {
        //            Debug("!MadeIt " + endVect);
        //            return MadeIt;
        //            Vector3d vectMover = Mover.GetWorldPosition();
        //            List<ISimObject> nears = ((ISimObject)Mover).GetNearByObjects(CloseDistance / 2, false);
        //            while (!MadeIt && nears.Count == 0)
        //            {
        //                MadeIt = Mover.MoveTo(endVect, CloseDistance, 6);
        //                vectMover = Mover.GetWorldPosition();
        //                nears = ((ISimObject)Mover).GetNearByObjects(2f, false);
        //            }
        //        }
        //        if (!MadeIt)
        //        {
        //            Mover.StopMoving();

        //        }
        //        else
        //        {              
        //        }
        //        return MadeIt;
        //    }

        public override string ToString()
        {

            string s = GetType().Name + "::" + Mover;// +" " + point.ToString() + " to " + end;
            return s;
            //SimWaypoint point = Routes[0].StartNode;
            //int c = Routes.Count;
            //SimWaypoint end = Routes[c - 1].EndNode;
            //foreach (SimRoute A in Routes)
            //{
            //    SimWaypoint next = A.StartNode;
            //    if (next != point)
            //    {
            //        s += " -> " + next.ToString();
            //        point = next;
            //    }
            //    next = A.EndNode;
            //    if (next != point)
            //    {
            //        s += " -> " + next.ToString();
            //        point = next;
            //    }
            //}
            //return s;
        }

        //    public Vector3d InFrontOf()
        //    {
        //        return FrontOf(Mover.GetWorldPosition(), CloseDistance, Mover.GetSimRotation());
        //    }

        //    static Vector3d FrontOf(Vector3d c, double distance, Quaternion r)
        //    {
        //        double ax, ay, az;
        //        r.GetEulerAngles(out ax, out ay, out az);
        //        double xmul = (double)Math.Cos(az);
        //        double ymul = (double)Math.Sin(az);
        //        Vector3d v3 = new Vector3d(xmul, ymul, 0);
        //        v3 = v3 * distance;
        //        Vector3d front = c + v3;
        //        return front;
        //    }

        internal void Debug(string p, params object[] args)
        {
            Mover.Debug(p + " for " + this.ToString(), args);
        }

        internal Vector3d GetWorldPosition()
        {
            return Mover.GetWorldPosition();
        }

        //    public abstract SimMoverState Goto();


        /// <summary>
        /// Blocks points -45 to +45 degrees in front of Bot (assumes the bot is heading toward V3)
        /// </summary>
        /// <param name="v3"></param>
        public void BlockTowardsVector(Vector3 l3)
        {
            OpenNearbyClosedPassages();
            PathStore.SetBlockedTemp(GetSimPosition(), l3);
        }

        internal void OpenNearbyClosedPassages()
        {
            Mover.OpenNearbyClosedPassages();
        }

        internal Vector3 ZAngleVector(double ZAngle)
        {
            while (ZAngle < 0)
            {
                ZAngle += SimPathStore.PI2;
            }
            while (ZAngle > SimPathStore.PI2)
            {
                ZAngle -= SimPathStore.PI2;
            }
            return new Vector3((float)Math.Sin(ZAngle), (float)Math.Cos(ZAngle), 0);
        }



        //    //internal void BlockForwardPos()
        //    //{
        //    //    Point P1 = PathStore.ToPoint(GetWorldPosition());
        //    //    Point Last = Point.Empty;
        //    //    for (double dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    //    {
        //    //        Vector3d blocked = GetLeftPos(0, dist);
        //    //        Point P2 = PathStore.ToPoint(blocked);
        //    //        if (P2 != P1 && Last != P2)
        //    //        {
        //    //            BlockPoint(blocked);
        //    //            Debug("Blocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //    //            Last = P2;
        //    //        }
        //    //    }
        //    //    for (double dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    //    {
        //    //        Vector3d blocked = GetLeftPos(45, dist);
        //    //        Point P2 = PathStore.ToPoint(blocked);
        //    //        if (P2 != P1 && Last != P2)
        //    //        {
        //    //            BlockPoint(blocked);
        //    //            Debug("Blocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //    //            Last = P2;
        //    //        }
        //    //    }
        //    //    for (double dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    //    {
        //    //        Vector3d blocked = GetLeftPos(360 - 45, dist);
        //    //        Point P2 = PathStore.ToPoint(blocked);
        //    //        if (P2 != P1 && Last != P2)
        //    //        {
        //    //            BlockPoint(blocked);
        //    //            Debug("Blocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //    //            Last = P2;
        //    //        }
        //    //    }

        //    //    Last = Point.Empty;
        //    //    for (double dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    //    {
        //    //        Vector3d blocked = GetLeftPos(0, dist);
        //    //        Point P2 = PathStore.ToPoint(blocked);
        //    //        if (P2 != P1 && Last != P2)
        //    //        {
        //    //            PathStore.SetPassable(blocked.X, blocked.Y);
        //    //            Debug("Unblocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //    //            Last = P2;
        //    //        }
        //    //    }
        //    //    //Last = Point.Empty;
        //    //    //for (double dist = 0.0f; dist < 1f; dist += 0.14f)
        //    //    //{
        //    //    //    Vector3d blocked = GetLeftPos(180, dist);
        //    //    //    Point P2 = PathStore.ToPoint(blocked);
        //    //    //    if (P2 != Last)
        //    //    //    {
        //    //    //        PathStore.SetPassable(blocked.X, blocked.Y);
        //    //    //        Debug("Unblocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //    //    //        Last = P2;
        //    //    //    }
        //    //    //}
        //    //}

        //}
        //public class SimReRouteMover : SimAbstractMover
        //{
        //    public SimReRouteMover(SimMover mover, MeshableObject finalGoal, double finalDistance)
        //        : base(mover, finalGoal.GetWorldPosition(), finalDistance)
        //    {

        //    }


        //    public IList<SimRoute> GetRouteList(SimWaypoint to, out bool IsFake)
        //    {
        //        SimWaypoint from = GetWaypoint();

        //        //IList<SimRoute> route = PathStore.GetRoute(from, to, out IsFake);
        //        //if (false)
        //        //{
        //        //    ////pathByNodes
        //        //    //if (GraphFormer.DEBUGGER != null)
        //        //    //{
        //        //    //    new Thread(new ThreadStart(delegate()
        //        //    //    {
        //        //    //        //GraphFormer.DEBUGGER.Invalidate();
        //        //    //        //      GraphFormer.DEBUGGER.SetTryPathNow(from, to, pathByNodes);
        //        //    //    })).Start();
        //        //    //}
        //        //}
        //        return PathStore.GetRoute(from, to, out IsFake);
        //    }

        //    private SimWaypoint GetWaypoint()
        //    {
        //        throw new Exception("The method or operation is not implemented.");
        //    }


        //    /// <summary>
        //    /// 
        //    /// </summary>
        //    /// <param name="pos"></param>
        //    /// <param name="IsFake"></param>
        //    /// <returns></returns>
        //    public bool TryGotoTarget(MeshableObject pos, out bool IsFake)
        //    {
        //        IsFake = false;
        //        SimMoverState state = SimMoverState.TRYAGAIN;
        //        while (state == SimMoverState.TRYAGAIN)
        //        {
        //            SimWaypoint target = pos.GetWaypoint();
        //            IList<SimRoute> routes = (IList<SimRoute>)GetRouteList(target, out IsFake);
        //            if (routes == null) return false;
        //            SimRouteMover ApproachPlan = new SimRouteMover(Mover, routes, pos.GetWorldPosition(), pos.GetSizeDistance());
        //            state = ApproachPlan.Goto();
        //            if (state == SimMoverState.COMPLETE) return true;
        //        }
        //        return false;

        //        //== SimMoverState.COMPLETE;
        //    }

        //    private bool GotoSimRoute(MeshableObject pos)
        //    {
        //        bool IsFake;
        //        for (int i = 0; i < 19; i++)
        //        {
        //            Debug("PLAN GotoTarget: " + pos);
        //            // StopMoving();
        //            if (TryGotoTarget(pos, out IsFake))
        //            {
        //                Mover.StopMoving();
        //                Mover.TurnToward(pos.GetWorldPosition());
        //                Debug("SUCCESS GotoTarget: " + pos);
        //                return true;
        //            }

        //            //TurnToward(pos);
        //            double posDist = Vector3d.Distance(GetWorldPosition(), pos.GetWorldPosition());
        //            if (posDist <= pos.GetSizeDistance() + 0.5)
        //            {
        //                Debug("OK GotoTarget: " + pos);
        //                return true;
        //            }
        //            TurnAvoid += 115;
        //            while (TurnAvoid > 360)
        //            {
        //                TurnAvoid -= 360;
        //            }
        //            //Vector3d newPost = GetLeftPos(TurnAvoid, 4f);

        //            //StopMoving();
        //            //Debug("MOVELEFT GotoTarget: " + pos);
        //            //MoveTo(newPost, 2f, 4);
        //            if (IsFake) break;
        //        }
        //        Debug("FAILED GotoTarget: " + pos);
        //        return false;
        //    }

        //    public override SimMoverState Goto()
        //    {
        //        GotoSimRoute(SimWaypointImpl.CreateGlobal(FinalLocation));
        //        return STATE;
        //    }
        //}

        //public class SimRouteMover : SimAbstractMover
        //{

        //    IList<SimRoute> Routes;
        //    int CurrentRouteIndex = 0;
        //    SimRoute OuterRoute = null;


        //    public SimRouteMover(SimMover mover, IList<SimRoute> routes, Vector3d finalGoal, double finalDistance)
        //        : base(mover, finalGoal, finalDistance)
        //    {
        //        Routes = routes;
        //        OuterRoute = new SimRouteMulti(routes);
        //    }

        //    public override SimMoverState Goto()
        //    {
        //        _STATE = SimMoverState.MOVING;
        //        int CanSkip = 0;
        //        int Skipped = 0;
        //        int tried = 0;
        //        SimRoute prev = null;

        //        for (int cI = CurrentRouteIndex; cI < Routes.Count; cI++)
        //        {
        //            CurrentRouteIndex = cI;
        //            if (cI > 0)
        //            {
        //                prev = Routes[cI - 1];
        //            }

        //            SimRoute route = Routes[cI];
        //            if (route.IsBlocked)
        //            {
        //                STATE = SimMoverState.BLOCKED;
        //                continue;
        //            }

        //            tried++;
        //            // TRY
        //            STATE = FollowRoute(route);

        //            double distance = Vector3d.Distance(Mover.GetWorldPosition(), FinalLocation);
        //            if (STATE == SimMoverState.BLOCKED)
        //            {
        //                Mover.StopMoving();
        //                //  SetBlocked(route);
        //                if (distance < FinalDistance)
        //                {
        //                    return SimMoverState.COMPLETE;
        //                }
        //                //CreateSurroundWaypoints();
        //                route.ReWeight(1.1f);
        //                route.BumpyCount++;
        //                if (CanSkip > 0)
        //                {
        //                    CanSkip--;
        //                    Skipped++;
        //                    continue;
        //                }
        //                if (route.BumpyCount > 0 && Skipped==0)
        //                {
        //                    SetBlocked(route);
        //                  //  if (prev!=null) if (FollowRoute(prev.Reverse) == SimMoverState.COMPLETE)
        //                   // {
        //                     //   return SimMoverState.TRYAGAIN;
        //                   // }
        //                }
        //                return STATE;
        //            }
        //            if (STATE == SimMoverState.PAUSED)
        //            {
        //                if (distance < FinalDistance)
        //                {
        //                    return SimMoverState.COMPLETE;
        //                }
        //                return SimMoverState.TRYAGAIN;
        //            }
        //            if (STATE == SimMoverState.COMPLETE)
        //            {
        //                // if made it here then the prev was very good
        //                if (prev!=null)
        //                    prev.ReWeight(0.8f);
        //            }

        //            if (distance < FinalDistance)
        //            {
        //                return SimMoverState.COMPLETE;
        //            }
        //        }
        //        if (STATE != SimMoverState.COMPLETE)
        //        {

        //            if (tried == 0)
        //            {
        //                return SimMoverState.TRYAGAIN;
        //            }
        //            return STATE;
        //        }
        //        OuterRoute.ReWeight(0.7f); // Reward
        //        Mover.GetSimRegion().AddArc(OuterRoute);
        //        STATE = SimMoverState.COMPLETE;
        //        return STATE;
        //    }

        //    internal void SetBlocked(SimRoute StuckAt)
        //    {
        //        BlockTowardsVector(StuckAt._EndNode.GetWorldPosition());

        //        Vector3d pos = Mover.GetWorldPosition();
        //        if (StuckAt.BlockedPoint(GetGlobal(pos)))
        //        {
        //            StuckAt.ReWeight(1.1f);
        //            Debug("BLOCKED: " + StuckAt);
        //        }
        //        else
        //        {
        //            SimRoute StuckAt2 = OuterRoute.WhichRoute(GetGlobal(pos));
        //            if (StuckAt2 == null)
        //            {
        //                StuckAt.ReWeight(1.3f);
        //                //OuterRoute.ReWeight(1.2f);
        //                Debug("INACESSABLE: " + StuckAt);
        //                StuckAt.Passable = false;
        //            }
        //            else
        //            {
        //                StuckAt2.ReWeight(1.1f);
        //                Debug("ROUTE BLOCKED: " + StuckAt2);
        //                StuckAt2.BlockedPoint(GetGlobal(pos));
        //                StuckAt2.Passable = false;
        //                StuckAt2.Reverse.Passable = false;
        //            }
        //        }

        //        // SimPathStore.Instance.RemoveArc(StuckAt);
        //        ///SimPathStore.Instance.RemoveArc(StuckAt.Reverse);

        //        STATE = SimMoverState.BLOCKED;
        //    }

        //    private Vector3d GetGlobal(Vector3d pos)
        //    {
        //        return Mover.GetSimRegion().LocalToGlobal(pos);
        //    }


        //    internal SimMoverState FollowRoute(SimRoute route)
        //    {
        //        Vector3d vectStart = route.StartNode.GetWorldPosition();
        //        Vector3d vectMover = Mover.GetWorldPosition();
        //        double currentDistFromStart = Vector3d.Distance(vectMover, vectStart);
        //        if (currentDistFromStart > CloseDistance)
        //        {
        //            Debug("FollowRoute: TRYING for Start " + vectMover + " -> " + vectStart);
        //            if (!MoveTo(vectStart))
        //            {
        //                Debug("FollowRoute: FAILED Start " + vectMover + " -> " + vectStart);
        //                return SimMoverState.TRYAGAIN;
        //            }
        //        }
        //        Vector3d endVect = route.EndNode.GetWorldPosition();

        //        bool MadeIt = MoveTo(endVect);

        //        if (!MadeIt)
        //        {               
        //            Debug("FollowRoute: BLOCKED ROUTE " + vectMover + "-> " + endVect);
        //            //route.Passable = false;
        //            return SimMoverState.BLOCKED;
        //        }

        //        Vector3d endVectMover = Mover.GetWorldPosition();
        //        double currentDistFromfinish = Vector3d.Distance(endVectMover, endVect);
        //        if (currentDistFromfinish > CloseDistance)
        //        {
        //            Debug("FollowRoute: CANNOT FINISH " + endVectMover + " -> " + endVect);
        //            return SimMoverState.PAUSED;
        //        }
        //        Debug("FollowRoute: SUCCEED " + vectStart + " -> " + endVectMover);
        //        return SimMoverState.COMPLETE;
        //    }

        //}

        //public class SimVectorMover : SimAbstractMover
        //{

        //    public SimVectorMover(SimMover mover, Vector3d finalGoal, double finalDistance)
        //        : base(mover, finalGoal, finalDistance)           
        //    {
        //    }

        //    private bool GotoSimVector(Vector3d vector3, double finalDistance)
        //    {

        //        int OneCount = 0;
        //        Mover.TurnToward(vector3);
        //        if (Vector3d.Distance(GetWorldPosition(), vector3) < finalDistance) return true;
        //        for (int trial = 0; trial < 25; trial++)
        //        {
        //            Mover.StopMoving();
        //            Application.DoEvents();
        //            Vector3d start = GetUsePosition();

        //            if (!PathStore.IsPassable(start))
        //            {
        //                start = MoveToPassableArround(start);
        //            }
        //            Vector3d end = vector3;


        //            List<Vector3d> v3s = (List<Vector3d>)Mover.GetSimRegion().GetV3Route(start, end);
        //            if (v3s.Count > 1)
        //            {
        //                if (Vector3d.Distance(v3s[0], start) > Vector3d.Distance(v3s[v3s.Count - 1], start))
        //                    v3s.Reverse();
        //            }
        //            else
        //            {
        //                MoveToPassableArround(GetWorldPosition());
        //                //  GetUsePosition();
        //                if (OneCount > 3) return false;
        //                OneCount++;
        //            }

        //            Debug("Path {1}: {0} ", v3s.Count, trial);
        //            if (FollowPath(v3s, vector3, finalDistance)) return true;
        //            if (Vector3d.Distance(GetWorldPosition(), vector3) < finalDistance) return true;

        //        }
        //        return false;
        //    }


        //    private bool FollowPath(List<Vector3d> v3sIn, Vector3d finalTarget, double finalDistance)
        //    {
        //        IList<Vector3d> v3s = PathStore.GetSimplifedRoute(GetWorldPosition(), v3sIn, 10, 8f);
        //        Debug("FollowPath: {0} -> {1}", v3sIn.Count, v3s.Count);
        //        int CanSkip = 2;
        //        int Skipped = 0;
        //        foreach (Vector3d v3 in v3s)
        //        {
        //            STATE = SimMoverState.MOVING;
        //            //  if (Vector3d.Distance(v3, GetWorldPosition()) < dist) continue;
        //            if (!MoveTo(v3))
        //            {
        //                STATE = SimMoverState.TRYAGAIN;
        //                if (Vector3d.Distance(GetWorldPosition(), finalTarget) < finalDistance) return true;
        //                if (!Mover.MoveTo(v3,PathStore.LargeScale,4))
        //                {
        //                    if (Skipped++ <= CanSkip)
        //                    {
        //                        MoveToPassableArround(GetWorldPosition());
        //                        Skipped++;
        //                        continue;
        //                    }
        //                    BlockTowardsVector(v3);
        //                    STATE = SimMoverState.BLOCKED;
        //                    return false;
        //                }
        //            }
        //            else
        //            {
        //                STATE = SimMoverState.MOVING;
        //                Skipped = 0;
        //            }

        //        }
        //        STATE = SimMoverState.COMPLETE;
        //        return true;
        //    }



        //    public override SimMoverState Goto()
        //    {
        //        if (GotoSimVector(FinalLocation, FinalDistance))
        //        {
        //            return SimMoverState.COMPLETE;
        //        }
        //        return SimMoverState.TRYAGAIN;
        //    }
        //}
    }
}
