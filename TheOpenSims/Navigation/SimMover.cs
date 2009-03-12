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
        void TurnToward(Vector3d targetPosition);
         //void SetMoveTarget(SimPosition v3);
        void StopMoving();
        bool MoveTo(Vector3d end, double maxDistance, int maxSeconds);
       // Quaternion GetSimRotation();
        void Debug(string format, params object[] args);
        SimPathStore GetPathSystem();
        //double Distance(SimPosition v3);

    }

    public enum SimMoverState : byte
    {
        PAUSED = 0,
        MOVING = 1,
        BLOCKED = 2,
        COMPLETE = 3,
        TRYAGAIN = 4
    }



    //abstract public class SimAbstractMover
    //{
    //    public SimMoverState STATE
    //    {
    //        get { return _STATE; }
    //        set { _STATE = value; }
    //    }

    //    protected SimMoverState _STATE = SimMoverState.PAUSED;
    //    protected SimAvatar Mover;
    //    protected readonly Vector3d FinalLocation;
    //    protected readonly double FinalDistance;
    //    protected double CloseDistance = 1f;// SimPathStore.LargeScale-SimPathStore.StepSize;
    //    protected SimPathStore PathStore;
    //    protected double TurnAvoid = 0f;

    //    public SimAbstractMover(SimMover mover, Vector3d finalGoal, double finalDistance)
    //    {
    //        Mover = (SimAvatar)mover;
    //        FinalDistance = finalDistance;
    //        FinalLocation = finalGoal;
    //        PathStore = mover.GetPathSystem();
    //    }

    //    internal Vector3d GetUsePosition()
    //    {
    //        return Mover.GetUsePosition();
    //        //throw new Exception("The method or operation is not implemented.");
    //    }


    //    public Vector3d MoveToPassableArround(Vector3d start)
    //    {
    //        double A45 = 45f / SimPathStore.RAD2DEG;
    //        for (double angle = A45; angle < SimPathStore.PI2; angle += A45)
    //        {
    //            Vector3d next = ZAngleVector(angle + TurnAvoid) * 2 + start;
    //            if (PathStore.IsPassable(next))
    //            {
    //                if (MoveTo(next))
    //                {
    //                    TurnAvoid += angle;  // update for next use
    //                    if (TurnAvoid > SimPathStore.PI2)
    //                        TurnAvoid -= SimPathStore.PI2;
    //                    return next;
    //                }
    //            }
    //        }
    //        return start;
    //    }

    //    internal bool MoveTo(Vector3d endVect)
    //    {
    //        bool MadeIt = Mover.MoveTo(endVect, CloseDistance, 6);
    //        if (!MadeIt)
    //        {
    //            Debug("!MadeIt " + endVect);
    //            return MadeIt;
    //            Vector3d vectMover = Mover.GetWorldPosition();
    //            List<SimObject> nears = ((SimObject)Mover).GetNearByObjects(CloseDistance / 2, false);
    //            while (!MadeIt && nears.Count == 0)
    //            {
    //                MadeIt = Mover.MoveTo(endVect, CloseDistance, 6);
    //                vectMover = Mover.GetWorldPosition();
    //                nears = ((SimObject)Mover).GetNearByObjects(2f, false);
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

    //    public override string ToString()
    //    {

    //        string s = GetType().Name + "::" + Mover;// +" " + point.ToString() + " to " + end;
    //        return s;
    //        //SimWaypoint point = Routes[0].StartNode;
    //        //int c = Routes.Count;
    //        //SimWaypoint end = Routes[c - 1].EndNode;
    //        //foreach (SimRoute A in Routes)
    //        //{
    //        //    SimWaypoint next = A.StartNode;
    //        //    if (next != point)
    //        //    {
    //        //        s += " -> " + next.ToString();
    //        //        point = next;
    //        //    }
    //        //    next = A.EndNode;
    //        //    if (next != point)
    //        //    {
    //        //        s += " -> " + next.ToString();
    //        //        point = next;
    //        //    }
    //        //}
    //        //return s;

    //    }

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

    //    internal void Debug(string p, params object[] args)
    //    {
    //        Mover.Debug(p + " for " + this.ToString(), args);
    //    }

    //    internal Vector3d GetWorldPosition()
    //    {
    //        return Mover.GetWorldPosition();
    //    }

    //    public abstract SimMoverState Goto();


    //    /// <summary>
    //    /// Blocks points -45 to +45 degrees in front of Bot (assumes the bot is heading toward V3)
    //    /// </summary>
    //    /// <param name="v3"></param>
    //    internal void BlockTowardsVector(Vector3d v3)
    //    {
    //        OpenNearbyClosedPassages();
    //        Point P1 = PathStore.ToPoint(GetWorldPosition());
    //        Vector3d cp = GetWorldPosition();
    //        Vector3d offset = v3 - cp;
    //        double ZAngle = (double)Math.Atan2(offset.Y, offset.X);
    //        Point Last = PathStore.ToPoint(v3);
    //        double Dist = 0.3f;
    //        Vector3d b1 = v3;
    //        while (offset.Length() > 0.1)
    //        {
    //            offset *= 0.75f;
    //            Vector3d blocked = cp + offset;
    //            Point P2 = PathStore.ToPoint(blocked);
    //            if (P2 != P1)
    //            {
    //                Dist = offset.Length();
    //                Last = P2;
    //                b1 = blocked;
    //            }
    //        }
    //        double x = Last.X / PathStore.POINTS_PER_METER;
    //        double y = Last.Y / PathStore.POINTS_PER_METER;
    //        BlockPoint(new Vector3d(x, y, v3.Z));
    //        double A45 = 45f / SimPathStore.RAD2DEG;
    //        Debug("Blocked {0},{1}", x, y);
    //        Vector3d middle = ZAngleVector(ZAngle) * Dist;
    //        middle += cp;
    //        double mdist = Vector3d.Distance(middle, b1);
    //        if (mdist > 0.1)
    //        {
    //            Debug("Wierd mdist=" + mdist);
    //        }
    //        Dist = 0.4f;
    //        BlockPoint(ZAngleVector(ZAngle) * Dist + cp);
    //        BlockPoint(ZAngleVector(ZAngle - A45 * 0.5) * Dist + cp);
    //        BlockPoint(ZAngleVector(ZAngle + A45 * 0.5) * Dist + cp);
    //        BlockPoint(ZAngleVector(ZAngle - A45) * Dist + cp);
    //        BlockPoint(ZAngleVector(ZAngle + A45) * Dist + cp);
    //        BlockPoint(ZAngleVector(ZAngle - A45 * 1.5) * Dist + cp);
    //        BlockPoint(ZAngleVector(ZAngle + A45 * 1.5) * Dist + cp);
    //        //Dont Run back
    //        //MoveTo(cp + ZAngleVector(ZAngle - Math.PI) * 2, 1f, 2);
    //    }

    //    internal void OpenNearbyClosedPassages()
    //    {
    //        SimObjectType DOOR = SimTypeSystem.DOOR;
    //        // look for closed doors
    //        foreach (SimObject O in ((SimAvatar)Mover).GetNearByObjects(2, false))
    //        {
    //            if (O.IsTypeOf(DOOR) != null)
    //            {
    //                O.MakeEnterable((SimAvatar)Mover);
    //            }
    //        }


    //    }

    //    internal Vector3d ZAngleVector(double ZAngle)
    //    {
    //        while (ZAngle < 0)
    //        {
    //            ZAngle += SimPathStore.PI2;
    //        }
    //        while (ZAngle > SimPathStore.PI2)
    //        {
    //            ZAngle -= SimPathStore.PI2;
    //        }
    //        return new Vector3d((double)Math.Sin(ZAngle), (double)Math.Cos(ZAngle), 0);
    //    }

    //    /// <summary>
    //    /// Blocks a point temporarilly (one minute)
    //    /// </summary>
    //    /// <param name="vector3"></param>
    //    internal void BlockPoint(Vector3d vector3)
    //    {
    //        Point P = PathStore.ToPoint(vector3);
    //        Debug("BlockPoint {0},{1}", P.X / PathStore.POINTS_PER_METER, P.Y / PathStore.POINTS_PER_METER);
    //        byte oldValue = PathStore.GetNodeQuality(vector3);
    //        if (oldValue == 0) // aready blocked
    //            return;
    //        PathStore.SetNodeQuality(vector3, 0);
    //        new Thread(new ThreadStart(delegate()
    //        {
    //            Thread.Sleep(60000);
    //            byte newValue = PathStore.GetNodeQuality(vector3);
    //            if (newValue != 0)
    //            {
    //                // its been changed by something else since we set to Zero
    //                Debug("BlockPoint Thread out of date {0} value changed to {1}", vector3, newValue);
    //            }
    //            else
    //            {
    //                PathStore.SetNodeQuality(vector3, oldValue);
    //                Debug("Unblock {0} value reset to {1}", vector3, oldValue);
    //            }
    //        })).Start();
    //    }

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
    //    public SimReRouteMover(SimMover mover, SimPosition finalGoal, double finalDistance)
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
    //    public bool TryGotoTarget(SimPosition pos, out bool IsFake)
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

    //    private bool GotoSimRoute(SimPosition pos)
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
    //        GotoSimRoute(SimWaypoint.CreateGlobal(FinalLocation));
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
    //        Mover.GetPathSystem().AddArc(OuterRoute);
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
