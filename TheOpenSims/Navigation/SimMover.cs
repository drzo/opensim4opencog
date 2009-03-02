using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;
using System.Drawing;
using System.Windows.Forms;

namespace cogbot.TheOpenSims.Navigation
{
    public interface SimMover : SimPosition
    {
        void TurnToward(Vector3 targetPosition);
         //void SetMoveTarget(SimPosition v3);
        void StopMoving();
        bool MoveTo(Vector3 end, float maxDistance, int maxSeconds);
       // Quaternion GetSimRotation();
        void Debug(string format, params object[] args);
        SimPathStore GetPathSystem();
        //float Distance(SimPosition v3);

    }

    public enum SimMoverState : byte
    {
        PAUSED = 0,
        MOVING = 1,
        BLOCKED = 2,
        COMPLETE = 3,
        TRYAGAIN = 4
    }



    abstract public class SimAbstractMover
    {
        public SimMoverState STATE
        {
            get { return _STATE; }
            set { _STATE = value; }
        }

        protected SimMoverState _STATE = SimMoverState.PAUSED;
        protected SimAvatar Mover;
        protected readonly Vector3 FinalLocation;
        protected readonly float FinalDistance;
        protected float CloseDistance = 1f;// SimPathStore.LargeScale-SimPathStore.StepSize;
        protected SimPathStore PathStore;
        protected float TurnAvoid = 0f;

        public SimAbstractMover(SimMover mover, Vector3 finalGoal, float finalDistance)
        {
            Mover = (SimAvatar)mover;
            FinalDistance = finalDistance;
            FinalLocation = finalGoal;
            PathStore = mover.GetPathSystem();
        }

        internal Vector3 GetUsePosition()
        {
            return Mover.GetUsePosition();
            //throw new Exception("The method or operation is not implemented.");
        }


        public Vector3 MoveToPassableArround(Vector3 start)
        {
            float A45 = 45f / SimPathStore.RAD2DEG;
            for (float angle = A45; angle < SimPathStore.PI2; angle += A45)
            {
                Vector3 next = ZAngleVector(angle + TurnAvoid) * 2 + start;
                if (PathStore.IsPassable(next))
                {
                    if (MoveTo(next))
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

        internal bool MoveTo(Vector3 endVect)
        {
            bool MadeIt = Mover.MoveTo(endVect, CloseDistance, 6);
            if (!MadeIt)
            {
                Debug("!MadeIt " + endVect);
                return MadeIt;
                Vector3 vectMover = Mover.GetSimPosition();
                List<SimObject> nears = ((SimObject)Mover).GetNearByObjects(CloseDistance / 2, false);
                while (!MadeIt && nears.Count == 0)
                {
                    MadeIt = Mover.MoveTo(endVect, CloseDistance, 6);
                    vectMover = Mover.GetSimPosition();
                    nears = ((SimObject)Mover).GetNearByObjects(2f, false);
                }
            }
            if (!MadeIt)
            {
                Mover.StopMoving();

            }
            else
            {              
            }
            return MadeIt;
        }

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

        public Vector3 InFrontOf()
        {
            return FrontOf(Mover.GetSimPosition(), CloseDistance, Mover.GetSimRotation());
        }

        static Vector3 FrontOf(Vector3 c, float distance, Quaternion r)
        {
            float ax, ay, az;
            r.GetEulerAngles(out ax, out ay, out az);
            float xmul = (float)Math.Cos(az);
            float ymul = (float)Math.Sin(az);
            Vector3 v3 = new Vector3(xmul, ymul, 0);
            v3 = v3 * distance;
            Vector3 front = c + v3;
            return front;
        }

        internal void Debug(string p, params object[] args)
        {
            Mover.Debug(p + " for " + this.ToString(), args);
        }

        internal Vector3 GetSimPosition()
        {
            return Mover.GetSimPosition();
        }

        public abstract SimMoverState Goto();


        /// <summary>
        /// Blocks points -45 to +45 degrees in front of Bot (assumes the bot is heading toward V3)
        /// </summary>
        /// <param name="v3"></param>
        internal void BlockTowardsVector(Vector3 v3)
        {
            OpenNearbyClosedPassages();
            Point P1 = PathStore.ToPoint(GetSimPosition());
            Vector3 cp = GetSimPosition();
            Vector3 offset = v3 - cp;
            float ZAngle = (float)Math.Atan2(offset.Y, offset.X);
            Point Last = PathStore.ToPoint(v3);
            float Dist = 0.3f;
            Vector3 b1 = v3;
            while (offset.Length() > 0.1)
            {
                offset *= 0.75f;
                Vector3 blocked = cp + offset;
                Point P2 = PathStore.ToPoint(blocked);
                if (P2 != P1)
                {
                    Dist = offset.Length();
                    Last = P2;
                    b1 = blocked;
                }
            }
            float x = Last.X / PathStore.POINTS_PER_METER;
            float y = Last.Y / PathStore.POINTS_PER_METER;
            BlockPoint(new Vector3(x, y, v3.Z));
            float A45 = 45f / SimPathStore.RAD2DEG;
            Debug("Blocked {0},{1}", x, y);
            Vector3 middle = ZAngleVector(ZAngle) * Dist;
            middle += cp;
            float mdist = Vector3.Distance(middle, b1);
            if (mdist > 0.1)
            {
                Debug("Wierd mdist=" + mdist);
            }
            Dist = 0.4f;
            BlockPoint(ZAngleVector(ZAngle) * Dist + cp);
            BlockPoint(ZAngleVector(ZAngle - A45 * 0.5) * Dist + cp);
            BlockPoint(ZAngleVector(ZAngle + A45 * 0.5) * Dist + cp);
            BlockPoint(ZAngleVector(ZAngle - A45) * Dist + cp);
            BlockPoint(ZAngleVector(ZAngle + A45) * Dist + cp);
            BlockPoint(ZAngleVector(ZAngle - A45 * 1.5) * Dist + cp);
            BlockPoint(ZAngleVector(ZAngle + A45 * 1.5) * Dist + cp);
            //Dont Run back
            //MoveTo(cp + ZAngleVector(ZAngle - Math.PI) * 2, 1f, 2);
        }

        internal void OpenNearbyClosedPassages()
        {
            SimObjectType DOOR = SimTypeSystem.DOOR;
            // look for closed doors
            foreach (SimObject O in ((SimAvatar)Mover).GetNearByObjects(2, false))
            {
                if (O.IsTypeOf(DOOR) != null)
                {
                    O.MakeEnterable((SimAvatar)Mover);
                }
            }


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

        /// <summary>
        /// Blocks a point temporarilly (one minute)
        /// </summary>
        /// <param name="vector3"></param>
        internal void BlockPoint(Vector3 vector3)
        {
            Point P = PathStore.ToPoint(vector3);
            Debug("BlockPoint {0},{1}", P.X / PathStore.POINTS_PER_METER, P.Y / PathStore.POINTS_PER_METER);
            byte oldValue = PathStore.GetNodeQuality(vector3);
            if (oldValue == 0) // aready blocked
                return;
            PathStore.SetNodeQuality(vector3, 0);
            new Thread(new ThreadStart(delegate()
            {
                Thread.Sleep(60000);
                byte newValue = PathStore.GetNodeQuality(vector3);
                if (newValue != 0)
                {
                    // its been changed by something else since we set to Zero
                    Debug("BlockPoint Thread out of date {0} value changed to {1}", vector3, newValue);
                }
                else
                {
                    PathStore.SetNodeQuality(vector3, oldValue);
                    Debug("Unblock {0} value reset to {1}", vector3, oldValue);
                }
            })).Start();
        }

        //internal void BlockForwardPos()
        //{
        //    Point P1 = PathStore.ToPoint(GetSimPosition());
        //    Point Last = Point.Empty;
        //    for (float dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    {
        //        Vector3 blocked = GetLeftPos(0, dist);
        //        Point P2 = PathStore.ToPoint(blocked);
        //        if (P2 != P1 && Last != P2)
        //        {
        //            BlockPoint(blocked);
        //            Debug("Blocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //            Last = P2;
        //        }
        //    }
        //    for (float dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    {
        //        Vector3 blocked = GetLeftPos(45, dist);
        //        Point P2 = PathStore.ToPoint(blocked);
        //        if (P2 != P1 && Last != P2)
        //        {
        //            BlockPoint(blocked);
        //            Debug("Blocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //            Last = P2;
        //        }
        //    }
        //    for (float dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    {
        //        Vector3 blocked = GetLeftPos(360 - 45, dist);
        //        Point P2 = PathStore.ToPoint(blocked);
        //        if (P2 != P1 && Last != P2)
        //        {
        //            BlockPoint(blocked);
        //            Debug("Blocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //            Last = P2;
        //        }
        //    }

        //    Last = Point.Empty;
        //    for (float dist = 0.1f; dist < 0.75f; dist += 0.14f)
        //    {
        //        Vector3 blocked = GetLeftPos(0, dist);
        //        Point P2 = PathStore.ToPoint(blocked);
        //        if (P2 != P1 && Last != P2)
        //        {
        //            PathStore.SetPassable(blocked.X, blocked.Y);
        //            Debug("Unblocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //            Last = P2;
        //        }
        //    }
        //    //Last = Point.Empty;
        //    //for (float dist = 0.0f; dist < 1f; dist += 0.14f)
        //    //{
        //    //    Vector3 blocked = GetLeftPos(180, dist);
        //    //    Point P2 = PathStore.ToPoint(blocked);
        //    //    if (P2 != Last)
        //    //    {
        //    //        PathStore.SetPassable(blocked.X, blocked.Y);
        //    //        Debug("Unblocked {0},{1}", P2.X / PathStore.POINTS_PER_METER, P2.Y / PathStore.POINTS_PER_METER);
        //    //        Last = P2;
        //    //    }
        //    //}
        //}

    }
    public class SimReRouteMover : SimAbstractMover
    {
        public SimReRouteMover(SimMover mover, SimPosition finalGoal, float finalDistance)
            : base(mover, finalGoal.GetSimPosition(), finalDistance)
        {

        }


        public IList<SimRoute> GetRouteList(SimWaypoint to, out bool IsFake)
        {
            SimWaypoint from = Mover.GetWaypoint();
         
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
            return PathStore.GetRoute(from, to, out IsFake);
        }


        /// <summary>
        /// 
        /// </summary>
        /// <param name="pos"></param>
        /// <param name="IsFake"></param>
        /// <returns></returns>
        public bool TryGotoTarget(SimPosition pos, out bool IsFake)
        {
            IsFake = false;
            SimMoverState state = SimMoverState.TRYAGAIN;
            while (state == SimMoverState.TRYAGAIN)
            {
                SimWaypoint target = pos.GetWaypoint();
                IList<SimRoute> routes = GetRouteList(target, out IsFake);
                if (routes == null) return false;
                SimRouteMover ApproachPlan = new SimRouteMover(Mover, routes, pos.GetSimPosition(), pos.GetSizeDistance());
                state = ApproachPlan.Goto();
                if (state == SimMoverState.COMPLETE) return true;
            }
            return false;

            //== SimMoverState.COMPLETE;
        }

        private bool GotoSimRoute(SimPosition pos)
        {
            bool IsFake;
            for (int i = 0; i < 19; i++)
            {
                Debug("PLAN GotoTarget: " + pos);
                // StopMoving();
                if (TryGotoTarget(pos, out IsFake))
                {
                    Mover.StopMoving();
                    Mover.TurnToward(pos.GetSimPosition());
                    Debug("SUCCESS GotoTarget: " + pos);
                    return true;
                }

                //TurnToward(pos);
                float posDist = Vector3.Distance(GetSimPosition(), pos.GetSimPosition());
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
                //Vector3 newPost = GetLeftPos(TurnAvoid, 4f);

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
            GotoSimRoute(PathStore.CreateClosestWaypoint(FinalLocation));
            return STATE;
        }
    }

    public class SimRouteMover : SimAbstractMover
    {

        IList<SimRoute> Routes;
        int CurrentRouteIndex = 0;
        SimRoute OuterRoute = null;


        public SimRouteMover(SimMover mover, IList<SimRoute> routes, Vector3 finalGoal, float finalDistance)
            : base(mover, finalGoal, finalDistance)
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

                float distance = Vector3.Distance(Mover.GetSimPosition(), FinalLocation);
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
                    if (route.BumpyCount > 0 && Skipped==0)
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
                    if (prev!=null)
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
            Mover.GetPathSystem().AddArc(OuterRoute);
            STATE = SimMoverState.COMPLETE;
            return STATE;
        }

        internal void SetBlocked(SimRoute StuckAt)
        {
            BlockTowardsVector(StuckAt._EndNode.Position);

            Vector3 pos = Mover.GetSimPosition();
            if (StuckAt.BlockedPoint(pos))
            {
                StuckAt.ReWeight(1.1f);
                Debug("BLOCKED: " + StuckAt);
            }
            else
            {
                SimRoute StuckAt2 = OuterRoute.WhichRoute(pos);
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
                    StuckAt2.BlockedPoint(pos);
                    StuckAt2.Passable = false;
                    StuckAt2.Reverse.Passable = false;
                }
            }

            // SimPathStore.Instance.RemoveArc(StuckAt);
            ///SimPathStore.Instance.RemoveArc(StuckAt.Reverse);

            STATE = SimMoverState.BLOCKED;
        }


        internal SimMoverState FollowRoute(SimRoute route)
        {
            Vector3 vectStart = route.StartNode.GetSimPosition();
            Vector3 vectMover = Mover.GetSimPosition();
            float currentDistFromStart = Vector3.Distance(vectMover, vectStart);
            if (currentDistFromStart > CloseDistance)
            {
                Debug("FollowRoute: TRYING for Start " + vectMover + " -> " + vectStart);
                if (!MoveTo(vectStart))
                {
                    Debug("FollowRoute: FAILED Start " + vectMover + " -> " + vectStart);
                    return SimMoverState.TRYAGAIN;
                }
            }
            Vector3 endVect = route.EndNode.GetSimPosition();

            bool MadeIt = MoveTo(endVect);

            if (!MadeIt)
            {               
                Debug("FollowRoute: BLOCKED ROUTE " + vectMover + "-> " + endVect);
                //route.Passable = false;
                return SimMoverState.BLOCKED;
            }

            Vector3 endVectMover = Mover.GetSimPosition();
            float currentDistFromfinish = Vector3.Distance(endVectMover, endVect);
            if (currentDistFromfinish > CloseDistance)
            {
                Debug("FollowRoute: CANNOT FINISH " + endVectMover + " -> " + endVect);
                return SimMoverState.PAUSED;
            }
            Debug("FollowRoute: SUCCEED " + vectStart + " -> " + endVectMover);
            return SimMoverState.COMPLETE;
        }

    }

    public class SimVectorMover : SimAbstractMover
    {

        public SimVectorMover(SimMover mover, Vector3 finalGoal, float finalDistance)
            : base(mover, finalGoal, finalDistance)           
        {
        }

        private bool GotoSimVector(Vector3 vector3, float finalDistance)
        {

            int OneCount = 0;
            Mover.TurnToward(vector3);
            if (Vector3.Distance(GetSimPosition(), vector3) < finalDistance) return true;
            for (int trial = 0; trial < 25; trial++)
            {
                Mover.StopMoving();
                Application.DoEvents();
                Vector3 start = GetUsePosition();

                if (!PathStore.IsPassable(start))
                {
                    start = MoveToPassableArround(start);
                }
                Vector3 end = vector3;


                List<Vector3> v3s = (List<Vector3>)Mover.CurrentRegion.GetV3Route(start, end);
                if (v3s.Count > 1)
                {
                    if (Vector3.Distance(v3s[0], start) > Vector3.Distance(v3s[v3s.Count - 1], start))
                        v3s.Reverse();
                }
                else
                {
                    MoveToPassableArround(GetSimPosition());
                    //  GetUsePosition();
                    if (OneCount > 3) return false;
                    OneCount++;
                }

                Debug("Path {1}: {0} ", v3s.Count, trial);
                if (FollowPath(v3s, vector3, finalDistance)) return true;
                if (Vector3.Distance(GetSimPosition(), vector3) < finalDistance) return true;

            }
            return false;
        }


        private bool FollowPath(List<Vector3> v3sIn, Vector3 finalTarget, float finalDistance)
        {
            IList<Vector3> v3s = PathStore.GetSimplifedRoute(GetSimPosition(), v3sIn, 10, 8f);
            Debug("FollowPath: {0} -> {1}", v3sIn.Count, v3s.Count);
            int CanSkip = 2;
            int Skipped = 0;
            foreach (Vector3 v3 in v3s)
            {
                STATE = SimMoverState.MOVING;
                //  if (Vector3.Distance(v3, GetSimPosition()) < dist) continue;
                if (!MoveTo(v3))
                {
                    STATE = SimMoverState.TRYAGAIN;
                    if (Vector3.Distance(GetSimPosition(), finalTarget) < finalDistance) return true;
                    if (!Mover.MoveTo(v3,PathStore.LargeScale,4))
                    {
                        if (Skipped++ <= CanSkip)
                        {
                            MoveToPassableArround(GetSimPosition());
                            Skipped++;
                            continue;
                        }
                        BlockTowardsVector(v3);
                        STATE = SimMoverState.BLOCKED;
                        return false;
                    }
                }
                else
                {
                    STATE = SimMoverState.MOVING;
                    Skipped = 0;
                }

            }
            STATE = SimMoverState.COMPLETE;
            return true;
        }



        public override SimMoverState Goto()
        {
            if (GotoSimVector(FinalLocation, FinalDistance))
            {
                return SimMoverState.COMPLETE;
            }
            return SimMoverState.TRYAGAIN;
        }
    }
}
