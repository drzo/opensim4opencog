using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;

namespace cogbot.TheOpenSims.Navigation
{
    public interface SimMover : SimPosition
    {
        //void TurnToward(SimPosition targetPosition);
         //void SetMoveTarget(SimPosition v3);
        void StopMoving();
        bool MoveTo(Vector3 end, float maxDistance, int maxSeconds);
        Quaternion GetSimRotation();
        void Debug(string p, params object[] args);
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

    public class SimRouteMover
    {

        SimMoverState _STATE = SimMoverState.PAUSED;
        SimMover Mover;
        IList<SimRoute> Routes;
        readonly Vector3 FinalLocation;
        readonly float FinalDistance;
        int CurrentRouteIndex = 0;
        SimRoute StuckAt = null;
        SimRoute OuterRoute = null;
        float CloseDistance = SimPathStore.LargeScale;

        public SimMoverState STATE
        {
            get { return _STATE; }
            set { _STATE = value; }
        }

        public SimRouteMover(SimMover mover, IList<SimRoute> routes, Vector3 finalGoal, float finalDistance)
        {
            Mover = (SimAvatar)mover;
            Routes = routes;
            OuterRoute = new SimRouteMulti(routes);
            FinalDistance = finalDistance;
            FinalLocation = finalGoal;
        }

        public SimMoverState Goto()
        {
            _STATE = SimMoverState.MOVING;
            int CanSkip = 0;
            int Skipped = 0;
            SimRoute prev = null;

            for (int cI = CurrentRouteIndex; cI < Routes.Count; cI++)
            {
                CurrentRouteIndex = cI;
                if (cI > 0)
                {
                    prev = Routes[cI - 1];
                }

                SimRoute route = Routes[cI];

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
                if (distance < FinalDistance)
                {
                    return SimMoverState.COMPLETE;
                }
            }
            OuterRoute.ReWeight(0.7f); // Reward
            SimPathStore.Instance.AddArc(OuterRoute);
            STATE = SimMoverState.COMPLETE;
            return STATE;
        }

        private void SetBlocked(SimRoute StuckAt)
        {
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
                    OuterRoute.ReWeight(1.2f);
                    Debug("INACESSABLE: " + StuckAt);
                }
                else
                {
                    StuckAt2.ReWeight(1.1f);
                    Debug("ROUTE BLOCKED: " + StuckAt2);
                    StuckAt2.BlockedPoint(pos);
                }
            }

            // SimPathStore.Instance.RemoveArc(StuckAt);
            ///SimPathStore.Instance.RemoveArc(StuckAt.Reverse);

            STATE = SimMoverState.BLOCKED;
        }

        private SimMoverState FollowRoute(SimRoute route)
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
            route.ReWeight(0.9f);
            Debug("FollowRoute: SUCCEED " + vectStart + " -> " + endVectMover);
            return SimMoverState.COMPLETE;
        }

        private bool MoveTo(Vector3 endVect)
        {
            bool MadeIt = Mover.MoveTo(endVect, CloseDistance, 6);
            if (!MadeIt)
            {
                return MadeIt;
				Vector3 vectMover = Mover.GetSimPosition();
                List<SimObject> nears = ((SimObject)Mover).GetNearByObjects(CloseDistance/2, false);
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
            return MadeIt;
        }

        public override string ToString()
        {

            string s = GetType().Name + "::" + Mover;// +" " + point.ToString() + " to " + end;
            return s;
            SimWaypoint point = Routes[0].StartNode;
            int c = Routes.Count;
            SimWaypoint end = Routes[c - 1].EndNode;
            foreach (SimRoute A in Routes)
            {
                SimWaypoint next = A.StartNode;
                if (next != point)
                {
                    s += " -> " + next.ToString();
                    point = next;
                }
                next = A.EndNode;
                if (next != point)
                {
                    s += " -> " + next.ToString();
                    point = next;
                }
            }
            return s;

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

        private void Debug(string p, params object[] args)
        {
            Mover.Debug(p + " for " + this.ToString(),args);
        }
    }
}
