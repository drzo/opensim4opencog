using System;
using System.Collections.Generic;
using OpenMetaverse;

namespace PathSystem3D.Navigation
{
    public interface SimMover : PathSystem3D.Navigation.SimPosition
    {
        void TurnToward(Vector3d targetPosition);
        void StopMoving();
        bool MoveTo(Vector3d end, double maxDistance, int maxSeconds);
        void Debug(string format, params object[] args);
        //List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly);
        /*
         
         Inherited from SimPosition:
         
                    bool IsPassable { get; set; }
                    string DistanceVectorString(SimPosition RootObject);
                    Vector3 GetSimPosition();
                    float GetSizeDistance();
                    bool IsRegionAttached();
                    Quaternion GetSimRotation();
                    Vector3d GetWorldPosition();
                    SimPathStore GetPathStore();
         
         */
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


    public abstract class SimAbstractMover
    {
        public SimMoverState _STATE = SimMoverState.PAUSED;
        public SimMover Mover;
        public Vector3d FinalLocation;
        public double FinalDistance;
        public double CloseDistance = 1f;// SimPathStore.LargeScale-SimPathStore.StepSize;
        static public double TurnAvoid = 0f;
        public bool UseTurnAvoid = false;  // this toggles each time
        public SimPosition FinalPosition;
        public bool UseSkipping = false;
        public bool UseReverse = false;
        public abstract SimMoverState Goto();

        public SimAbstractMover(SimMover mover, SimPosition finalGoal, double finalDistance)
        {
            Mover = mover;
            FinalDistance = finalDistance;
            FinalLocation = finalGoal.GetWorldPosition();
            FinalPosition = finalGoal;
        }

        public SimMoverState STATE
        {
            get { return _STATE; }
            set { _STATE = value; }
        }

        public SimPathStore PathStore
        {
            get { return Mover.GetPathStore(); }
        }


        public static SimAbstractMover CreateSimPathMover(SimMover mover, SimPosition finalGoal, double finalDistance)
        {
            return new SimCollisionPlaneMover(mover, finalGoal, finalDistance);
        }

        public Vector3 GetSimPosition()
        {
            return Mover.GetSimPosition();
        }

        public SimMoverState FollowPathTo(IList<Vector3d> v3s, Vector3d finalTarget, double finalDistance)
        {
            _STATE = SimMoverState.MOVING;
            Vector3d vstart = v3s[0];
            Vector3 vv3 = SimPathStore.GlobalToLocal(vstart);

            v3s = SimPathStore.GetSimplifedRoute(vstart, v3s, 45, 4f);

            int maxReverse = 2;
            Debug("FollowPath: {0} -> {1} for {2}", v3s.Count, DistanceVectorString(finalTarget), finalDistance);
            int CanSkip = UseSkipping ? 1 : 0; //never right now
            int Skipped = 0;
            UseSkipping = !UseSkipping;
            int v3sLength = v3s.Count;
            int at = 0;
            while (at < v3sLength)
            {
                Vector3d v3 = v3s[at];
                // try to get there first w/in StepSize 0.2f 
                if (!MoveTo(v3, PathStore.StepSize, 3))
                {
                    // didn't make it but are we close enough for govt work?
                    if (Vector3d.Distance(GetWorldPosition(), finalTarget) < finalDistance) return SimMoverState.COMPLETE;
                    // See what point in the list we are closest to
                    int nbest = ClosestAt(v3s);
                    if (nbest > at)
                    {
                        Debug("Fast-forward {0} -> {1} ", at, nbest);
                        at = nbest + 1;
                        continue;
                    }
                    // Try again with distance of 0.6f
                    if (!MoveTo(v3, PathStore.StepSize * 3, 2))
                    {
                        // still did not make it
                        OpenNearbyClosedPassages();
                        if (Skipped++ <= CanSkip)
                        {
                            // move around
                            MoveToPassableArround(GetSimPosition());
                            Skipped++;
                            // move onto the next point
                            at++;
                            continue;
                        }
                        Vector3 l3 = SimPathStore.GlobalToLocal(v3);
                        BlockTowardsVector(l3);
                        Debug("!MoveTo: {0} ", DistanceVectorString(v3));
                        MoveToPassableArround(l3);
                        return SimMoverState.TRYAGAIN;
                    }
                }
                else
                {
                    Skipped = 0;
                }
                if (Vector3d.Distance(GetWorldPosition(), finalTarget) < finalDistance) return SimMoverState.COMPLETE;
                int best = ClosestAt(v3s);
                if (best > at)
                {
                    Debug("Fast forward {0} -> {1} ", at, best);
                }
                else if (best < at)
                {
                    if (!UseReverse || maxReverse-- < 0)
                    {
                        // not using reverse
                      //  Debug("Wont Reverse {0} -> {1} ", at, best);
                        best = at;
                    }
                    else
                    {
                        Debug("Reverse {0} -> {1} ", at, best);
                    }
                    UseReverse = !UseReverse;
                }
                at = best + 1;

            }
            Debug("Complete: {0} -> {1}", DistanceVectorString(GetWorldPosition()), DistanceVectorString(finalTarget));
            return SimMoverState.COMPLETE;
        }

        public int ClosestAt(IList<Vector3d> v3s)
        {
            Vector3d newPos = GetWorldPosition();
            int v3sLength = v3s.Count - 1;
            double bestDist = Vector3d.Distance(newPos, v3s[v3sLength]);
            int best = v3sLength;
            while (v3sLength-- > 0)
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

        public string DistanceVectorString(SimPosition loc3d)
        {
            return DistanceVectorString(loc3d.GetWorldPosition());
        }

        public string DistanceVectorString(Vector3 loc)
        {
            return String.Format("{0:0.00}m ", Vector3.Distance(GetSimPosition(), loc))
                   + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", PathStore.RegionName, loc.X, loc.Y, loc.Z);
        }

        public bool MoveTo(Vector3d finalTarget)
        {
            return MoveTo(finalTarget, PathStore.StepSize*2, 4);
        }


        public bool MoveTo(Vector3d finalTarget, double maxDistance, int maxSeconds)
        {
            return Mover.MoveTo(finalTarget, maxDistance, maxSeconds);
        }

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

        public SimWaypoint GetWaypoint()
        {
            return PathStore.CreateClosestRegionWaypoint(GetSimPosition(), 2f);
        }

        public override string ToString()
        {

            string s = GetType().Name + "::" + Mover + " -> " + DistanceVectorString(FinalPosition) + " to " + FinalPosition;
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

        public void Debug(string p, params object[] args)
        {
            Mover.Debug(p + " for " + this.ToString(), args);
        }

        public Vector3d GetWorldPosition()
        {
            return Mover.GetWorldPosition();
        }

        /// <summary>
        /// Blocks points -45 to +45 degrees in front of Bot (assumes the bot is heading toward V3)
        /// </summary>
        /// <param name="v3"></param>
        public void BlockTowardsVector(Vector3 l3)
        {
            OpenNearbyClosedPassages();
            PathStore.SetBlockedTemp(GetSimPosition(), l3);
        }

        public void OpenNearbyClosedPassages()
        {
            Mover.OpenNearbyClosedPassages();
        }

        public static Vector3 ZAngleVector(double ZAngle)
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

        //public abstract bool FollowPathTo(Vector3d globalEnd, double distance);
    }

    public class SimCollisionPlaneMover : SimAbstractMover
    {
        public SimCollisionPlaneMover(SimMover mover, SimPosition finalGoal, double finalDistance) :
            base(mover, finalGoal, finalDistance)
        {
            MoverPlane.WalkZLevel = mover.GetSimPosition().Z;
            MoverPlane.WalkZLevel = finalGoal.GetSimPosition().Z;
        }
        public static Dictionary<SimMover, CollisionPlane> MoverPlanes = new Dictionary<SimMover, CollisionPlane>();

        public CollisionPlane MoverPlane
        {
            get
            {
                CollisionPlane _MoverPlane;
                lock (MoverPlanes)
                {
                    if (!MoverPlanes.ContainsKey(Mover))
                    {
                        MoverPlanes[Mover] = PathStore.GetCollisionPlane(Mover.GetSimPosition().Z);
                    }
                    _MoverPlane = MoverPlanes[Mover];
                    if (_MoverPlane.PathStore != Mover.GetPathStore())
                    {
                        _MoverPlane = PathStore.GetCollisionPlane(Mover.GetSimPosition().Z);
                        MoverPlanes[Mover] = _MoverPlane;
                    }
                }
                return _MoverPlane;
            }
        }

        public override SimMoverState Goto()
        {
            if (FollowPathTo(FinalLocation, FinalDistance)) return SimMoverState.COMPLETE;
            return SimMoverState.TRYAGAIN;
        }

        public bool FollowPathTo(Vector3d globalEnd, double distance)
        {
            bool OnlyStart = true;
            bool MadeIt = true;

            int maxTryAgains = 2;
            while (OnlyStart && MadeIt)
            {
                Vector3d v3d = GetWorldPosition();
                if (Vector3d.Distance(v3d, globalEnd) < distance) return true;
                MoverPlane.WalkZLevel = (float)globalEnd.Z;
                MoverPlane.WalkZLevel = (float)v3d.Z;
                MoverPlane.EnsureUpToDate();
                IList<Vector3d> route = SimPathStore.GetPath(MoverPlane, GetWorldPosition(), globalEnd, distance, out OnlyStart);
                STATE = FollowPathTo(route, globalEnd, distance);
                switch (STATE)
                {
                    case SimMoverState.TRYAGAIN:
                        {
                            if (maxTryAgains-- > 0)
                            {
                                OnlyStart = true;
                                continue;
                            }
                            MadeIt = false;
                            continue;
                        }
                    case SimMoverState.COMPLETE:
                        {
                            MadeIt = true;
                            continue;
                        }
                    case SimMoverState.BLOCKED:
                        {
                            OnlyStart = false;
                            MadeIt = false;
                            continue;
                        }
                    case SimMoverState.PAUSED:
                    case SimMoverState.MOVING:
                    default:
                        {
                            //MadeIt = true;
                            OnlyStart = true;
                            continue;
                        }
                }

            }
            return MadeIt;
        }

    }


        public class SimReRouteMover : SimAbstractMover
        {
            public SimReRouteMover(SimMover mover, SimPosition finalGoal, double finalDistance)
                : base(mover, finalGoal, finalDistance)
            {

            }

            public IList<SimRoute> GetRouteList(SimWaypoint to, out bool IsFake)
            {
                SimWaypoint from = GetWaypoint();

                //IList<SimRoute> route = PathStore.GetRoute(from, to, out IsFake);
                //if (false)
                //{
                //    ////pathByNodes
                //    //if (GraphFormer.DEBUGGER != null)
                //    //{
                //    //    new Thread(new ThreadStart(delegate()
                //    //    {
                //    //        //GraphFormer.DEBUGGER.Invalidate();
                //    //        //      GraphFormer.DEBUGGER.SetTryPathNow(from, to, pathByNodes);
                //    //    })).Start();
                //    //}
                //}
                // TODO return PathStore.GetRoute(from, to, out IsFake);
                throw new Exception("The method or operation is not implemented.");
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="pos"></param>
            /// <param name="IsFake"></param>
            /// <returns></returns>s
            public bool TryGotoTarget(SimPosition pos, out bool IsFake)
            {
                IsFake = false;
                SimMoverState state = SimMoverState.TRYAGAIN;
                while (state == SimMoverState.TRYAGAIN)
                {
                    SimWaypoint target = (SimWaypoint)pos;
                    IList<SimRoute> routes = (IList<SimRoute>)GetRouteList(target, out IsFake);
                    if (routes == null) return false;
                    SimRouteMover ApproachPlan = new SimRouteMover(Mover, routes, pos.GetWorldPosition(), pos.GetSizeDistance());
                    state = ApproachPlan.Goto();
                    if (state == SimMoverState.COMPLETE) return true;
                }
                return false;

                //== SimMoverState.COMPLETE;
            }

            public bool GotoSimRoute(SimPosition pos)
            {
                bool IsFake;
                for (int i = 0; i < 19; i++)
                {
                    Debug("PLAN GotoTarget: " + pos);
                    // StopMoving();
                    if (TryGotoTarget(pos, out IsFake))
                    {
                        Mover.StopMoving();
                        Mover.TurnToward(pos.GetWorldPosition());
                        Debug("SUCCESS GotoTarget: " + pos);
                        return true;
                    }

                    //TurnToward(pos);
                    double posDist = Vector3d.Distance(GetWorldPosition(), pos.GetWorldPosition());
                    if (posDist <= pos.GetSizeDistance() + 0.5)
                    {
                        Debug("OK GotoTarget: " + pos);
                        return true;
                    }
                    TurnAvoid += 115;
                    while (TurnAvoid > 360)
                    {
                        TurnAvoid -= 360;
                    }
                    //Vector3d newPost = GetLeftPos(TurnAvoid, 4f);

                    //StopMoving();
                    //Debug("MOVELEFT GotoTarget: " + pos);
                    //MoveTo(newPost, 2f, 4);
                    if (IsFake) break;
                }
                Debug("FAILED GotoTarget: " + pos);
                return false;
            }

            public override SimMoverState Goto()
            {
                GotoSimRoute(SimWaypointImpl.CreateGlobal(FinalLocation));
                return STATE;
            }
        }

        public class SimRouteMover : SimAbstractMover
        {

            IList<SimRoute> Routes;
            int CurrentRouteIndex = 0;
            SimRoute OuterRoute = null;


            public SimRouteMover(SimMover mover, IList<SimRoute> routes, Vector3d finalGoal, double finalDistance)
                : base(mover, mover.GetPathStore().CreateClosestWaypoint(finalGoal, finalDistance), finalDistance)
            {
                Routes = routes;
                OuterRoute = new SimRouteMulti(routes);
            }

            public override SimMoverState Goto()
            {
                _STATE = SimMoverState.MOVING;
                int CanSkip = 0;
                int Skipped = 0;
                int tried = 0;
                SimRoute prev = null;

                for (int cI = CurrentRouteIndex; cI < Routes.Count; cI++)
                {
                    CurrentRouteIndex = cI;
                    if (cI > 0)
                    {
                        prev = Routes[cI - 1];
                    }

                    SimRoute route = Routes[cI];
                    if (route.IsBlocked)
                    {
                        STATE = SimMoverState.BLOCKED;
                        continue;
                    }

                    tried++;
                    // TRY
                    STATE = FollowRoute(route);

                    double distance = Vector3d.Distance(Mover.GetWorldPosition(), FinalLocation);
                    if (STATE == SimMoverState.BLOCKED)
                    {
                        Mover.StopMoving();
                        //  SetBlocked(route);
                        if (distance < FinalDistance)
                        {
                            return SimMoverState.COMPLETE;
                        }
                        //CreateSurroundWaypoints();
                        route.ReWeight(1.1f);
                        route.BumpyCount++;
                        if (CanSkip > 0)
                        {
                            CanSkip--;
                            Skipped++;
                            continue;
                        }
                        if (route.BumpyCount > 0 && Skipped == 0)
                        {
                            SetBlocked(route);
                            //  if (prev!=null) if (FollowRoute(prev.Reverse) == SimMoverState.COMPLETE)
                            // {
                            //   return SimMoverState.TRYAGAIN;
                            // }
                        }
                        return STATE;
                    }
                    if (STATE == SimMoverState.PAUSED)
                    {
                        if (distance < FinalDistance)
                        {
                            return SimMoverState.COMPLETE;
                        }
                        return SimMoverState.TRYAGAIN;
                    }
                    if (STATE == SimMoverState.COMPLETE)
                    {
                        // if made it here then the prev was very good
                        if (prev != null)
                            prev.ReWeight(0.8f);
                    }

                    if (distance < FinalDistance)
                    {
                        return SimMoverState.COMPLETE;
                    }
                }
                if (STATE != SimMoverState.COMPLETE)
                {

                    if (tried == 0)
                    {
                        return SimMoverState.TRYAGAIN;
                    }
                    return STATE;
                }
                OuterRoute.ReWeight(0.7f); // Reward
                //TODO Mover.PathStore.AddArc(OuterRoute);
                STATE = SimMoverState.COMPLETE;
                return STATE;
            }

            public void SetBlocked(SimRoute StuckAt)
            {
                BlockTowardsVector(StuckAt._EndNode.GetSimPosition());

                Vector3d pos = Mover.GetWorldPosition();
                if (StuckAt.BlockedPoint(GetGlobal(pos)))
                {
                    StuckAt.ReWeight(1.1f);
                    Debug("BLOCKED: " + StuckAt);
                }
                else
                {
                    SimRoute StuckAt2 = OuterRoute.WhichRoute(GetGlobal(pos));
                    if (StuckAt2 == null)
                    {
                        StuckAt.ReWeight(1.3f);
                        //OuterRoute.ReWeight(1.2f);
                        Debug("INACESSABLE: " + StuckAt);
                        StuckAt.Passable = false;
                    }
                    else
                    {
                        StuckAt2.ReWeight(1.1f);
                        Debug("ROUTE BLOCKED: " + StuckAt2);
                        StuckAt2.BlockedPoint(GetGlobal(pos));
                        StuckAt2.Passable = false;
                        StuckAt2.Reverse.Passable = false;
                    }
                }

                // SimPathStore.Instance.RemoveArc(StuckAt);
                ///SimPathStore.Instance.RemoveArc(StuckAt.Reverse);

                STATE = SimMoverState.BLOCKED;
            }

            public Vector3d GetGlobal(Vector3d pos)
            {
                return pos;// PathStore.LocalToGlobal(pos);
            }


            public SimMoverState FollowRoute(SimRoute route)
            {
                Vector3d vectStart = route.StartNode.GetWorldPosition();
                Vector3d vectMover = Mover.GetWorldPosition();
                double currentDistFromStart = Vector3d.Distance(vectMover, vectStart);
                if (currentDistFromStart > CloseDistance)
                {
                    Debug("FollowRoute: TRYING for Start " + vectMover + " -> " + vectStart);
                    if (!MoveTo(vectStart))
                    {
                        Debug("FollowRoute: FAILED Start " + vectMover + " -> " + vectStart);
                        return SimMoverState.TRYAGAIN;
                    }
                }
                Vector3d endVect = route.EndNode.GetWorldPosition();

                bool MadeIt = MoveTo(endVect);

                if (!MadeIt)
                {
                    Debug("FollowRoute: BLOCKED ROUTE " + vectMover + "-> " + endVect);
                    //route.Passable = false;
                    return SimMoverState.BLOCKED;
                }

                Vector3d endVectMover = Mover.GetWorldPosition();
                double currentDistFromfinish = Vector3d.Distance(endVectMover, endVect);
                if (currentDistFromfinish > CloseDistance)
                {
                    Debug("FollowRoute: CANNOT FINISH " + endVectMover + " -> " + endVect);
                    return SimMoverState.PAUSED;
                }
                Debug("FollowRoute: SUCCEED " + vectStart + " -> " + endVectMover);
                return SimMoverState.COMPLETE;
            }
        }

        //}

        //public class SimVectorMover : SimAbstractMover
        //{

        //    public SimVectorMover(SimMover mover, Vector3d finalGoal, double finalDistance)
        //        : base(mover, finalGoal, finalDistance)           
        //    {
        //    }

        //    public bool FollowPathTo(Vector3d vector3, double finalDistance)
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


        //            List<Vector3d> v3s = (List<Vector3d>)Mover.PathStore.GetV3Route(start, end);
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


        //    public bool FollowPath(List<Vector3d> v3sIn, Vector3d finalTarget, double finalDistance)
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
        //        if (FollowPathTo(FinalLocation, FinalDistance))
        //        {
        //            return SimMoverState.COMPLETE;
        //        }
        //        return SimMoverState.TRYAGAIN;
        //    }
        //}
}
