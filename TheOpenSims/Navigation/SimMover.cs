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
            FinalDistance = finalDistance;
            FinalLocation = finalGoal;
        }

        public SimMoverState Goto()
        {
            SimMoverState ST =  Goto1();
            return ST;
        }

        public SimMoverState Goto1()
        {
            _STATE = SimMoverState.MOVING;
            int CanSkip = 0;
            int Skipped = 0;
            SimRoute prev = null;

            for (int cI = CurrentRouteIndex; cI < Routes.Count;cI++)
            {
                CurrentRouteIndex = cI;

                if (false)
                {
                    // Auto Advance when we are closer to the end if its in the future
                    // but cant trust doing this if we are circling arround something
                    int indexClosest = ClosestToInRoute(Routes[cI]);
                    if (indexClosest > cI)
                    {
                        cI = indexClosest;
                    }
                }
                if (cI > 0)
                {
                    prev = Routes[cI - 1];
                }

                SimRoute route = Routes[cI];

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
                        if (prev!=null) if (FollowRoute(prev.Reverse) == SimMoverState.COMPLETE)
                        {
                            return SimMoverState.TRYAGAIN;
                        }
                    }
                    return STATE;
                }
                if (STATE == SimMoverState.PAUSED)
                {
                    CreateSurroundWaypoints();
                    if (distance < FinalDistance)
                    {
                        return SimMoverState.COMPLETE;
                    }
                    if (CanSkip > 0)
                    {
                        CanSkip--;
                        Skipped++;
                        continue;
                    }
                    Mover.StopMoving();
                    return SimMoverState.PAUSED;
                }
                if (distance < FinalDistance)
                {
                    return SimMoverState.COMPLETE;
                }
            }
            STATE = SimMoverState.COMPLETE;
            return STATE;
        }

        private void CreateSurroundWaypoints()
        {
            //SimPathStore.Instance.CreateClosestWaypointBox(Mover.GetSimPosition(), 4, 5, 1.0f);
        }

        private int ClosestToInRoute(SimRoute BestR)
        {
            Vector3 current = Mover.GetSimPosition();
            float BestDist = Vector3.Distance(current, BestR.StartNode.GetSimPosition());
            int index = -1;
            foreach (SimRoute R in Routes) {
                index++;
                float testDist = Vector3.Distance(current, R.StartNode.GetSimPosition());
                if (testDist < BestDist)
                {
                    BestR = R;
                    BestDist = testDist;
                }
            }
            return index;
        }

        private void SetBlocked(SimRoute route)
        {
            StuckAt = route;
            Debug("INACESSABLE: " + StuckAt);
            //route.EndNode.Passable = false;
            StuckAt.Passable = false;
            StuckAt.ReWeight(1.1f);
            
            SimRoute reversed = StuckAt.Reverse;//
            
            reversed.Passable = false;
            reversed.ReWeight(1.1f);
           // SimPathStore.Instance.RemoveArc(StuckAt);
            ///SimPathStore.Instance.RemoveArc(reversed);

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
            Vector3 vectMover = Mover.GetSimPosition();
            if (!MadeIt)
            {
                return MadeIt;
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
