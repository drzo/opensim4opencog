using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;

namespace cogbot.TheOpenSims.Navigation
{
    public interface SimMover : SimPosition
    {
        void TurnToward(SimPosition targetPosition);
        void SetMoveTarget(SimPosition v3);
        float Distance(SimPosition v3);

        void StopMoving();
    }

    public enum SimMoverState : byte
    {
        PAUSED = 0,
        MOVING = 1,
        BLOCKED = 2,
        COMPLETE = 3
    }

    public class SimRouteMover
    {

        SimMoverState _STATE = SimMoverState.PAUSED;
        SimAvatar Mover;
        SimRoute[] Routes;

        int CurrentRouteIndex = -1;
        SimRoute StuckAt = null;


        public SimMoverState STATE
        {
            get { return _STATE; }
            set { _STATE = value; }
        }

        public SimRouteMover(SimMover mover, SimRoute[] routes)
        {
            Mover = (SimAvatar)mover;
            Routes = routes;
        }


        public SimMoverState Goto()
        {
            _STATE = SimMoverState.MOVING;
            CurrentRouteIndex = -1;
            foreach (SimRoute route in Routes)
            {
                CurrentRouteIndex++;
                STATE = FollowRoute(route);
                if (STATE == SimMoverState.BLOCKED)
                {
                    SetBlocked(route);
                    return STATE;
                }
                if (STATE == SimMoverState.PAUSED)
                {
                    return STATE;
                }
            }
            STATE = SimMoverState.COMPLETE;
            return STATE;
        }

        private void SetBlocked(SimRoute route)
        {
            StuckAt = route;
            StuckAt.Passable = false;
            StuckAt.ReWeigth(1.1f);
            Debug("INACESSABLE: " + StuckAt);
            StuckAt.Reverse().Passable = false;
            SimPathStore.Instance.RemoveArc(StuckAt);
            STATE = SimMoverState.BLOCKED;
        }

        private SimMoverState FollowRoute(SimRoute route)
        {
            Vector3 startGoto = route.StartNode.GetSimPosition();
            Vector3 start = Mover.GetSimPosition();
            float maxDistFromStart = route.StartNode.GetSizeDistance();
            float currentDistFromStart = Vector3.Distance(start, startGoto);            
            if (currentDistFromStart > maxDistFromStart)
            {
                Debug("FollowRoute: trying to " + start + " > " + startGoto);
                if (!MoveTo(route.StartNode, maxDistFromStart))
                {
                    Debug("FollowRoute: cant start " + currentDistFromStart + " > " + maxDistFromStart);
                    return SimMoverState.PAUSED;
                }
            }

            Vector3 finishGoto = route.EndNode.GetSimPosition();
            float maxDistFromfinish = route.EndNode.GetSizeDistance();

            bool MadeIt = MoveTo(route.EndNode, maxDistFromfinish);

            Vector3 finish = Mover.GetSimPosition();
            float currentDistFromfinish = Vector3.Distance(finish, finishGoto);
            if (currentDistFromfinish > maxDistFromfinish)
            {
                Debug("FollowRoute: cant finish " + currentDistFromfinish + " > " + maxDistFromfinish);
                return SimMoverState.PAUSED;
                //return false;
            }
            if (!MadeIt)
            {
                route.ReWeigth(1.1f);
                Debug("FollowRoute: NOT MadeIt " + route);
                return SimMoverState.BLOCKED;
            }
            else
            {
                route.ReWeigth(0.8f); //Cheapen
                Debug("FollowRoute: Success " + route);
                return SimMoverState.COMPLETE;
            }
        }

        private bool MoveTo(SimWaypoint finishGoto, float maxDistance)
        {
            Vector3 start = Mover.GetSimPosition();
            Vector3 end = finishGoto.GetSimPosition();
            Mover.TurnToward(finishGoto);
            Mover.SetMoveTarget(finishGoto);
            // Monitor for 6 seconds
            for (int i = 0; i < 6; i++)
            {
                Thread.Sleep(1000);
                float currentDist = Vector3.Distance(end, Mover.GetSimPosition());
                if (currentDist > maxDistance)
                {         
                    continue;
                }
                else
                {
                    return true;
                }
            }
            return false;
        }

        public override string ToString()
        {
            string s = GetType().Name + "::" + Mover + " to " + Routes[0].StartNode;
            SimWaypoint point = Routes[0].StartNode;
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

        private void Debug(string p)
        {
            ((SimAvatar)Mover).Debug(p + " for " + this.ToString());
        }
    }
}
