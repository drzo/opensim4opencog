using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Actions.Pathfinder;
using cogbot.Listeners;
using java.lang;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;
using PathSystem3D.Navigation;
using Boolean=System.Boolean;
using Exception=System.Exception;
using Math=System.Math;
using Object=System.Object;
using String=System.String;
using Thread=System.Threading.Thread;
using System.Drawing;
using ControlFlags = OpenMetaverse.AgentManager.ControlFlags;

namespace cogbot.TheOpenSims
{
    public partial class SimAvatarClient : SimMover, SimControllableAvatar
    {
        public override void ThreadJump()
        {
            return;
            ///  all the anims here are mainly so we can see what the bot is doing
            (new Thread(
                WithAnim(Animations.SHRUG,
                         WithAnim(Animations.WORRY, () =>
                         {
                             //Client.Self.Fly(false);
                             Thread.Sleep(10);
                             Client.Self.Jump(true);
                             Thread.Sleep(500);
                             Client.Self.Jump(false);
                             CogPush(Vector3.UnitZ);
                         })))).Start();
        }

        public ThreadStart WithAnim(UUID uUID, ThreadStart threadStart)
        {
            return WithAnim(SimAssetStore.FindOrCreateAsset(uUID, AssetType.Animation), threadStart);
        }

        private DateTime ThisUpdateShown;
        public static int PipesAlive = 0;
        public static int PipesNumberNext = 0;
        public static string PipeMaker = "SEC";
        public static bool UseSimpleTurnToward = false;
        public static bool ResetOnDestination = true;

        public override void IndicateRoute(IEnumerable<Vector3d> list, Color color)
        {
            var PS = GetSimRegion();
            var llist = new List<Vector3>();
            double Z = GlobalPosition.Z;
            foreach (var v3d in list)
            {
                Z = v3d.Z;
                llist.Add(PS.GlobalToLocal(v3d));
            }
            bool haveFirst = false;
            bool haveSecond = false;
            Vector3 prev = Vector3.Zero;
            double atan = 999;

            var CP = PathStore.GetCollisionPlane((float)Z);
            if (CP.lastUpdate != ThisUpdateShown)
            {
                ThisUpdateShown = CP.lastUpdate;
                ChatCogPrim("http://logicmoo.dyndns.org:5580/cogpath/path.gif");
                //ChatCogPrim("hi " + Client.Self.SimPosition.Z);
            }
            if (string.IsNullOrEmpty(PipeMaker)) return;
            bool throttle = llist.Count < 200;
            foreach (var next in llist)
            {
                if (!haveFirst)
                {
                    haveFirst = true;
                    prev = next;
                    continue;
                }

                Vector3 diff = prev - next;
                double thisAtan = Math.Atan2(diff.X, diff.Y);
                if (Math.Abs(atan - thisAtan) < 0.10)
                {
                    atan = thisAtan;
                    continue;
                }
                if (throttle) Thread.Sleep(3);
                string send = "";
                foreach (char s in PipeMaker)
                {
                    switch (char.ToUpper(s))
                    {
                        case 'S':
                            {
                                send += String.Format(",{0},{1},{2}", prev.X, prev.Y, prev.Z);
                                continue;

                            }
                        case 'E':
                            {
                                send += String.Format(",{0},{1},{2}", next.X, next.Y, next.Z);
                                continue;
                            }
                        case 'C':
                            {
                                send += String.Format(",{0},{1},{2}", color.R, color.G, color.B);
                                continue;
                            }
                        case 'A':
                            {
                                send += String.Format(",{0}", color.A);
                                continue;
                            }
                        case 'P':
                            {
                                send += String.Format(",{0}", PipesNumberNext);
                                continue;
                            }
                        default:
                            break;
                    }
                }
                if (send.Length > 1)
                {
                    ChatCogPrim(String.Format(send.Substring(1)));
                    PipesNumberNext++;
                    PipesAlive++;
                }
                prev = next;
            }
        }

        private void ChatCogPrim(string s)
        {
            Client.Self.Chat(s, 100, ChatType.Normal);
        }

        public void KillPipes()
        {
            ChatCogPrim("die");
            PipesAlive = 0;
            PipesNumberNext = 0;
        }

        public override bool OpenNearbyClosedPassages()
        {
            bool changed = false;
            foreach (var s in GetNearByObjects(20, false))
            {
                if (s.PathFinding.AddCollisions()) changed = true;
            }
            if (!changed)
                foreach (var s in GetNearByObjects(40, false))
                {
                    if (s.PathFinding.AddCollisions()) changed = true;
                }
            //WithAnim(Animations.AFRAID,()=> base.OpenNearbyClosedPassages()).Invoke();
            return OpenNearbyClosedPassages2() || changed;
        }
        public bool OpenNearbyClosedPassages2()
        {
            bool changed = false;
            SimObjectType DOOR = SimTypeSystem.DOOR;
            // look for closed doors

            var UnEnterables = new List<SimObjectPathFinding>();
            foreach (SimObject O in GetNearByObjects(3, false))
            {
                var P = O.PathFinding;
                if (P.AddCollisions()) changed = true;
                if (!O.IsPhantom)
                {
                    if (O.Affordances.IsTypeOf(DOOR) != null)
                    {
                        P.MakeEnterable(this);
                        UnEnterables.Add(P);
                        continue;
                    }
                    if (O.IsSculpted)
                    {
                        P.MakeEnterable(this);
                        UnEnterables.Add(P);
                        continue;
                    }
                }
            }
            if (UnEnterables.Count > 0)
            {
                changed = true;
                new Thread(delegate()
                {
                    Thread.Sleep(90000); // 90 seconds
                    foreach (var O in UnEnterables)
                    {
                        O.RestoreEnterable(this);
                    }
                }).Start();
            }
            return changed;
        }

        private SimMoverState old;
        private readonly static Dictionary<SimMoverState, UUID> State2Anim = new Dictionary<SimMoverState, UUID>();
        protected override void OnMoverStateChange(SimMoverState obj)
        {

            //return; // todo make sure it doesnt mess up turning animations while moving
            if (State2Anim.Count == 0)
            {
                State2Anim[SimMoverState.THINKING] = Animations.EXPRESS_TOOTHSMILE;
                State2Anim[SimMoverState.TRYAGAIN] = Animations.SURPRISE;
                State2Anim[SimMoverState.BLOCKED] = Animations.WORRY;
            }

            if (old != obj)
            {
                UUID oldAnim;
                if (State2Anim.TryGetValue(old, out oldAnim))
                {
                    if (oldAnim != UUID.Zero) Client.Self.AnimationStop(oldAnim, true);
                }
            }
            UUID newAnim;
            if (State2Anim.TryGetValue(obj, out newAnim))
            {
                if (newAnim != UUID.Zero)
                {
                    Client.Self.AnimationStart(newAnim, true);
                    Thread.Sleep(100);
                }
            }
            old = obj;
        }

        public override bool Flying
        {
            get
            {
                if (IsControllable)
                    return Client.Self.Movement.Fly;
                if (base.Flying) return true;
                return IsFlying;
            }
            set
            {
                if (Flying != value)
                {
                    if (IsControllable)
                    {
                        ConsumerSetFight = value;
                        Client.Self.Fly(value);
                    }
                }
            }
        }

        public override void StopMoving(bool fullStop)
        {
            if (lastStoppedMoving.AddSeconds(2) < DateTime.Now)
            {
                //return;
            }
            lastStoppedMoving = DateTime.Now;
            StopMovingReal();
        }
        public override void StopMovingIsProblem()
        {
            StopMovingReal();
        }

        private DateTime lastStoppedMoving = DateTime.Now;
        public void StopMovingReal()
        {
            lock (TrackerLoopLock)
            {
                //ApproachPosition = null;
                lastDistance = float.MaxValue;
                ApproachVector3D = Vector3d.Zero;
            }
            ResetMoveContols();
            AgentManager ClientSelf = Client.Self;
            AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
            ClientMovement.ResetControlFlags();
            //ClientMovement.FinishAnim = true;
            ClientMovement.Stop = true;
            //AtoPilotCancle sends update ClientMovement.SendUpdate();
            Client.Self.AutoPilotCancel();
        }

        private void ResetMoveContols()
        {
            AgentManager ClientSelf = Client.Self;

            AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
            ///   ClientMovement. AlwaysRun = false;
            ClientMovement.AtNeg = false;
            ClientMovement.AtPos = false;
            /// ClientMovement.AutoResetControls = true;
            ///    ClientMovement. Away = false;
            ClientMovement.FastAt = false;
            ClientMovement.FastLeft = false;
            ClientMovement.FastUp = false;

            ///   ClientMovement. Fly = false;
            ClientMovement.LButtonDown = false;
            ClientMovement.LButtonUp = false;
            ClientMovement.LeftNeg = false;
            ClientMovement.LeftPos = false;
            ClientMovement.MLButtonDown = false;
            ClientMovement.MLButtonUp = false;
            ///  ClientMovement. Mouselook = false;
            ClientMovement.NudgeAtNeg = false;
            ClientMovement.NudgeAtPos = false;
            ClientMovement.NudgeLeftNeg = false;
            ClientMovement.NudgeLeftPos = false;
            ClientMovement.NudgeUpNeg = false;
            ClientMovement.NudgeUpPos = false;
            ClientMovement.PitchNeg = false;
            ClientMovement.PitchPos = false;
            /// ClientMovement. SitOnGround = false;
            /// ClientMovement. StandUp = false;
            ClientMovement.Stop = false;
            ClientMovement.TurnLeft = false;
            ClientMovement.TurnRight = false;
            //ClientMovement.UpdateInterval = 0;
            ClientMovement.UpNeg = false;
            ClientMovement.UpPos = false;
            ClientMovement.YawNeg = false;
            ClientMovement.YawPos = false;
        }


        /// <summary>
        ///  
        /// </summary>
        /// <param name="obj"></param>
        /// <param name="maxDistance"></param>
        /// <returns></returns>
        public double Approach(SimObject obj, double maxDistance)
        {
            OnlyMoveOnThisThread();
            ///  stand up first
            SimObject UnPhantom = StandUp();
            ///  make sure it not going somewhere
            ///  set the new target
            ApproachDistance = maxDistance;
            string str = "Approaching " + obj + " " + DistanceVectorString(obj) + " to get " + ApproachDistance;
            Debug(str);
            try
            {
                if (DebugLevel > 1) IndicateTarget(obj, true);
                obj.PathFinding.MakeEnterable(this);
                ///  if (!MoveTo(obj.GlobalPosition(), obj.GetSizeDistance() + 0.5f, 12))
                SalientGoto(obj);
                TurnToward(obj);
                SimpleMoveTo(obj.GlobalPosition, maxDistance, 1);
            }
            finally
            {
                if (UnPhantom != null)
                    UnPhantom.PathFinding.RestoreEnterable(this);
                if (DebugLevel > 1) IndicateTarget(obj, false);
            }
            return (double)Distance(obj);
        }



        private readonly object TrackerLoopLock = new object();

        private bool IsBlocked = false;
        private double lastDistance = float.MaxValue;

        private void TrackerLoop()
        {
            Random MyRandom = new Random(DateTime.Now.Millisecond);
            //Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = false;
            //Client.Self.Movement.AutoResetControls = true;
            //Client.Self.Movement.UpdateInterval = 10;
            //DateTime lastDateTime = DateTime.Now;
            DateTime currentDateTime = DateTime.Now;
            while (true)
            {
                DateTime lastDateTime = currentDateTime;
                currentDateTime = DateTime.Now;

                Vector3d targetPosition = ApproachVector3D;
                lock (TrackerLoopLock)
                {
                    ///  Debug("TrackerLoop: " + Thread.CurrentThread);
                    if (ApproachVector3D == Vector3d.Zero)
                    {
                        //if (ApproachPosition == null)
                        {
                            lastDistance = float.MaxValue;
                            Thread.Sleep(100);
                            continue;
                        }
                    }
                    //Client.Settings.SEND_AGENT_UPDATES = false;
                    //if (ApproachPosition != null)
                    //{
                    //    targetPosition = ApproachPosition.GlobalPosition;
                    //} else
                    {
                        targetPosition = ApproachVector3D;
                    }
                }
                double realTargetZ = targetPosition.Z;
                Vector3d worldPosition = GlobalPosition;
                /// ApproachDistance = ApproachPosition.GetSizeDistance();
                try
                {

                    double curDist001 = Vector3d.Distance(worldPosition, targetPosition);
                    if (curDist001 < ApproachDistance)
                    {
                        if (SimAvatarClient.ResetOnDestination)
                        {
                            ApproachVector3D = Vector3d.Zero;
                            continue;
                        }
                    }



                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = false;

                    AgentManager ClientSelf = Client.Self;
                    AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;

                    if (SimpleMoveToMovementProceedure == MovementProceedure.AutoPilot)
                    {
                        if (!InAutoPilot)
                        {
                            ClientSelf.AutoPilot(targetPosition.X, targetPosition.Y, targetPosition.Z);
                            InAutoPilot = true;
                        }
                        Thread.Sleep(1000);
                        continue;
                    }
                    if (SimpleMoveToMovementProceedure == MovementProceedure.Teleport)
                    {
                        // Just Waiting
                        Thread.Sleep(1000);
                        continue;
                    }
                    if (SimpleMoveToMovementProceedure == MovementProceedure.CogPusher && false)
                    {
                        double curDist00 = Vector3d.Distance(worldPosition, targetPosition);
                        if (curDist00 < ApproachDistance)
                        {
                            Thread.Sleep(75);
                            continue;
                        }
                        ///var cp = WorldSystem.GetObject("CogPusher");
                        //if (cp != null)
                        {
                            TurnToward(targetPosition);
                            SendUpdate(10);
                            var gloffset = targetPosition - worldPosition;
                            Vector3 g3offset = new Vector3((float)gloffset.X, (float)gloffset.Y, (float)gloffset.Z);
                            if (g3offset.Length() < 1) g3offset = g3offset + g3offset;
                            bool pusherFound = CogPush(g3offset);
                            Parcel parcel = GetParcel();
                            if (pusherFound && CanPush)
                            {
                                // pusher working
                                Thread.Sleep(1000);
                                continue;
                            }
                            // moves onto TurnToAndWalk
                        }
                    }


                    ClientMovement.AutoResetControls = false;
                    ClientMovement.UpdateInterval = 0; /// 100
                    SimRegion R = GetSimRegion();
                    float WaterHeight = R.WaterHeight();
                    double selfZ = worldPosition.Z;
                    double UpDown = realTargetZ - selfZ;

                    double ZDist = Math.Abs(UpDown);
                    ///  Like water areas
                    bool swimming = WaterHeight > selfZ || MovementByFlight;

                    if (UpDown > 5f && CanFly)
                    {
                        targetPosition.Z = realTargetZ; ///  selfZ + 0.2f; ///  incline upward
                        if (!ClientMovement.Fly)
                        {
                            Debug("Starting to fly");
                            ClientMovement.Fly = true;
                            SendUpdate(10);
                            ///   selfZ = realTargetZ;
                            TurnToward(targetPosition);
                            ClientMovement.AtPos = true;
                            SendUpdate(10);
                            ApproachVector3D = targetPosition;
                            ///                             continue;
                        }
                    }
                    else
                    {
                        if (!MovementByFlight && !ConsumerSetFight) ClientMovement.Fly = false;
                        targetPosition.Z = selfZ;
                    }


                    ///  Reset previous Z 
                    ClientMovement.FastUp = false;
                    ClientMovement.UpPos = false;
                    ClientMovement.UpNeg = false;
                    ClientMovement.NudgeUpPos = false;
                    ClientMovement.NudgeUpNeg = false;

                    double curXYDist = Vector3d.Distance(worldPosition,
                                                         new Vector3d(targetPosition.X, targetPosition.Y, selfZ));

                    double curDist = Vector3d.Distance(worldPosition, targetPosition);
                    IsBlocked = false;
                    if (lastDistance <= curDist)
                    {
                        if (HasPrim && Prim.Velocity == Vector3.Zero)
                        {
                            var span = currentDateTime.Subtract(lastDateTime);
                            IsBlocked = true;
                        }
                        if (true /* || ApproachPosition != null*/)
                        {
                            // follower should pass by.. but on way point navigation would be ok
                            //TurnToward(targetPosition);
                            //ClientMovement.Stop = true;
                            //ClientMovement.FinishAnim = true;
                            ClientMovement.AtPos = false;
                            SendUpdate(1);
                            //ClientMovement.Stop = false;
                            lastDistance = curDist + 1;
                            continue;
                        }
                    }
                    lastDistance = curDist;


                    if (swimming)
                    {
                        double TargetHeight = WaterHeight;
                        if (MovementByFlight)
                        {
                            TargetHeight = realTargetZ;
                        }
                        if (!ClientMovement.Fly)
                        {
                            Debug("Starting to swim MovementByFlight=" + MovementByFlight);
                            ClientMovement.Fly = true;
                            SendUpdate(10);
                        }

                        bool nudgeUpDownMoves = true;

                        if (selfZ > TargetHeight - 0.5)
                        {
                            ///  Bob downward
                            if (nudgeUpDownMoves)
                                ClientMovement.NudgeUpNeg = true;
                            else
                                ClientMovement.UpNeg = true;
                            SendUpdate(10);
                            /// 
                            ///   continue; /// to keep going up
                        }
                        else
                        {
                            ///   nudge = !nudge;
                            if (selfZ < TargetHeight - 2.5)
                            {
                                ///  Bob upward
                                if (nudgeUpDownMoves)
                                    ClientMovement.NudgeUpPos = true;
                                else
                                    ClientMovement.UpPos = true;
                                SendUpdate(10);
                                ///    continue; /// to keep going up
                            }
                        }
                        targetPosition.Z = TargetHeight - 0.25f;
                    }

                    /// if ()/// ClientMovement.Fly = swimming;///  todo ||  GetPathStore().IsFlyZone(SimPathStore.GlobalToLocal(worldPosition));

                    if (swimming)
                    {
                        if (!ClientMovement.Fly)
                        {
                            Debug("Starting to swim again");
                            ClientMovement.Fly = true;
                        }
                        ///  Reset previous Z 
                        ClientMovement.FastUp = false;
                        ClientMovement.UpPos = false;
                        ClientMovement.UpNeg = false;
                        ClientMovement.NudgeUpPos = false;
                        ClientMovement.NudgeUpNeg = false;
                        SendUpdate(10);
                    }

                    /// //  Little Jumps
                    if (ZDist * 2 > curDist)
                    {
                        if (!ClientMovement.Fly)
                        {
                            if (UpDown > 0)
                            {
                                ClientMovement.NudgeUpPos = true;
                                SendUpdate(10);
                                ClientMovement.NudgeUpPos = false;
                            }
                        }
                    }
                    /// else
                    /// {
                    ///     if (ClientMovement.Fly)
                    ///     {
                    ///         ClientMovement.NudgeUpPos = false;
                    ///         ///  ClientSelf.Fly(false);
                    ///         ClientMovement.Fly = false;
                    ///     }
                    /// }


                    if (curDist < ApproachDistance)
                    {
                        ResetMoveContols();
                        //ClientMovement.FinishAnim = true;
                        ClientMovement.Stop = true;
                        SendUpdate(1);
                        TurnToward(targetPosition);
                        continue;
                    }

                    bool useNudging = UseNudging || ApproachDistance > 0.8;

                    ///  getting close though
                    if (useNudging && (curXYDist < (ApproachDistance + 3) || curDist < (ApproachDistance + 3)))
                    {
                        ClientMovement.AtPos = false;
                        ClientMovement.NudgeAtPos = true;
                        SendUpdate(10);
                        ClientMovement.NudgeAtPos = false;
                        SendUpdate(1);
                        //stopNext = true;
                        continue;
                    }

                    if (ZDist > curXYDist + 2)
                    {
                        // avoid circling while not changing ones altitude!
                       if (IsFlying) continue;
                    }

                    ///  Far away
                    ControlFlags agentControls = ((ControlFlags) ClientMovement.AgentControls & (
                                                                                                    (ControlFlags.AGENT_CONTROL_AWAY |
                                                                                                     ControlFlags.AGENT_CONTROL_FLY |
                                                                                                     ControlFlags.AGENT_CONTROL_MOUSELOOK |
                                                                                                     ControlFlags.AGENT_CONTROL_UP_NEG)))
                                                 | ControlFlags.AGENT_CONTROL_AT_POS;

                    Client.Self.Movement.TurnToward(GetLocalTo(targetPosition), false);
                    Client.Self.Movement.SendManualUpdate(agentControls, Client.Self.Movement.Camera.Position,
                        Client.Self.Movement.Camera.AtAxis, Client.Self.Movement.Camera.LeftAxis, Client.Self.Movement.Camera.UpAxis,
                        Client.Self.Movement.BodyRotation, Client.Self.Movement.HeadRotation, Client.Self.Movement.Camera.Far, AgentFlags.None,
                        AgentState.None, true);
                    // ClientMovement.UpdateInterval = 0;
                    Thread.Sleep(MyRandom.Next(25, 90));
                    continue;
                }
                catch (Exception e)
                {
                    Debug("" + e);
                }
            }
        }

        private bool CogPush(Vector3 goffset)
        {
            //Client.DisplayNotificationInChat("gloffset = " + goffset);
            goffset = SimPosition + goffset;
            ChatCogPrim(string.Format("push {0:0.0},{1:0.0},{2:0.0},{3:0.0},{4:0}", goffset.X, goffset.Y, goffset.Z, 1, 0));
            foreach (SimObject o in Children)
            {
                if (o.Matches("CogPusher"))
                {
                    return true;
                }
            }
            return false;
        }


        [ConfigSetting]
        public static bool MoveUseTeleportFallback = true;
        public static bool GotoUseTeleportFallback = !MoveUseTeleportFallback && false;
        static public MovementProceedure SimpleMoveToMovementProceedure = MovementProceedure.CogPusher;
        public MovementProceedure SalientMovementProceedure = MovementProceedure.AStar;
        public bool MovementByFlight
        {
            get
            {
                if (!CanFly) return false;
                return SimpleMoveToMovementProceedure == MovementProceedure.FlyTo ||
                       SalientMovementProceedure == MovementProceedure.FlyTo;
            }
        }
        /// <summary>
        ///  
        /// </summary>
        /// <param name="finalTarget"></param>
        /// <param name="maxDistance"></param>
        /// <param name="maxSeconds"></param>
        /// <returns></returns>
        public override bool SimpleMoveTo(Vector3d finalTarget, double maxDistance, float maxSeconds)
        {
            if (false)
            {
                Random MyRand = new Random();
                if (MyRand.Next(5) < 2)
                    Client.Self.LookAtEffect(ID, UUID.Zero, finalTarget, (LookAtType)MyRand.Next(11), ID);
            }
            OnlyMoveOnThisThread();
            ///TurnToward(finalTarget);
            IsBlocked = false;
            double currentDist = DistanceNoZ(finalTarget, GlobalPosition);
            if (currentDist < maxDistance) return true;
            bool adjustCourse = false;
            switch (SimpleMoveToMovementProceedure)
            {
                case MovementProceedure.Teleport:
                    bool tp = this.TeleportTo(SimRegion.GetWaypoint(finalTarget));
                    adjustCourse = false;
                    if (!tp)
                    {
                        Debug("TP failed => MoveToMovementProceedure = MovementProceedure.TurnToAndWalk;");
                        SimpleMoveToMovementProceedure = MovementProceedure.TurnToAndWalk;
                        adjustCourse = true;
                    }
                    break;
                case MovementProceedure.AStar:
                    Debug(" BAD SimpleMoveToMovementProceedure=" + SimpleMoveToMovementProceedure);
                    SimpleMoveToMovementProceedure = MovementProceedure.TurnToAndWalk;
                    return GotoTargetAStar(SimRegion.GetWaypoint(finalTarget));
                case MovementProceedure.FlyTo:
                case MovementProceedure.AutoPilot:
                case MovementProceedure.TurnToAndWalk:
                case MovementProceedure.CogPusher:
                default:
                    adjustCourse = true;
                    break;
            }
            if (adjustCourse)
            {
                lock (TrackerLoopLock)
                {
                    ApproachVector3D = finalTarget;
                    ApproachDistance = maxDistance;
                }
            }
            return WaitUntilPosSimple(finalTarget, maxDistance, maxSeconds, false);
        }


        public bool WaitUntilPosSimple(Vector3d finalTarget, double maxDistance, float maxSeconds, bool adjustCourse)
        {
            EnsureTrackerRunning();
            int blockCount = 0;
            maxDistance += 0.2;
            double currentDist = DistanceNoZ(finalTarget, GlobalPosition);
            if (currentDist < maxDistance) return true;
            bool IsKnownMoving = false;
            double lastDistance = currentDist;
            long endTick = Environment.TickCount + (int)(maxSeconds * 1000);
            while (Environment.TickCount < endTick)
            {
                currentDist = DistanceNoZ(finalTarget, GlobalPosition);
                if (Prim == null)
                {
                    Debug("Where is my body? " + ToString());
                    Thread.Sleep(100);
                    continue;
                }
                var PrimVelocity = Prim.Velocity;
                if (!IsKnownMoving)
                {
                    if (PrimVelocity != Vector3.Zero) IsKnownMoving = true;
                }
                else
                {
                    if (currentDist < maxDistance) return true;
                    if (adjustCourse)
                    {
                        if (Prim != null && PrimVelocity == Vector3.Zero && lastDistance == currentDist)
                        {
                            Write("!");
                            if (IsBlocked)
                            {
                                blockCount++;
                                if (blockCount > 3)
                                {
                                    if (MoveUseTeleportFallback)
                                    {
                                        Debug("Blocked so using TP to " + (finalTarget - GlobalPosition));
                                        var res = this.TeleportTo(finalTarget);
                                        TurnToward(finalTarget);
                                        return res;
                                    }
                                    Debug("BLOCKED!");
                                    //return true;
                                    return false;
                                }
                            }
                        }
                    }
                }
                if (currentDist > lastDistance + 0.1)
                {
                    if (adjustCourse) StopMoving();
                    ///  Console.Write("=");
                    if (currentDist < maxDistance) return true;
                    if (adjustCourse) StopMoving();
                    if (IsBlocked)
                    {
                        Write("=");
                        Debug("SLIPPING!");
                        return false;
                    }
                    return true;
                }
                if (currentDist <= maxDistance)
                {
                    Write("+");
                    ///  StopMoving();
                    return true;
                }
                Thread.Sleep(40);
                lastDistance = currentDist;
                //continue;
            }
            //if (adjustCourse) StopMoving();
            Write("-");
            return false;
        }

        private void Write(string s)
        {
            DLRConsole.DebugWrite(s);
        }

        public override bool SalientGoto(SimPosition pos)
        {
            double maxDistance = pos.GetSizeDistance() + 1;
            Debug("SalientMovementProceedure = " + SalientMovementProceedure + " for " + pos);
            switch (SalientMovementProceedure)
            {
                case MovementProceedure.Teleport:
                    return this.TeleportTo(pos);
                case MovementProceedure.AutoPilot:
                case MovementProceedure.FlyTo:
                case MovementProceedure.TurnToAndWalk:
                    return SimpleMoveTo(pos.UsePosition.GlobalPosition, pos.GetSizeDistance(), 3);

                // TODO 
                case MovementProceedure.AStar:
                    bool res = GotoTargetAStar(pos);
                    if (res) return res;
                    if (GotoUseTeleportFallback)
                    {
                        Debug("Goto sneaking in TP to " + pos);
                        res = this.TeleportTo(pos.UsePosition);
                        TurnToward(pos);
                        return res;
                    }
                    SetMoveTarget(pos, maxDistance - 1);
                    //Thread.Sleep(1000);
                    //StopMoving(true);
                    if (maxDistance > this.Distance(pos))
                    {
                        return true;
                    }
                    return false;
                default:
                    {
                        throw new UnsupportedOperationException("" + SalientMovementProceedure);
                    }
            }
        }

        public static int SendUpdateLagMultiplier = 5;
        public override void SendUpdate(int ms)
        {
            var ClientMovement = Client.Self.Movement;
            ClientMovement.FinishAnim = false;
            if (UseSimpleTurnToward)
            {
                ClientMovement.TurnToward(LastTTV);
            }
            else
            {
                ClientMovement.SendUpdate(true);
                //ClientMovement.ResetControlFlags();
            }
            int ms0 = ms * SendUpdateLagMultiplier;
            if (ms0 > 300) ms0 = 300;
            if (ms0 > 0) Thread.Sleep(ms0);
        }

        public override bool TeleportTo(SimRegion R, Vector3 local)
        {
            if (!IsControllable)
            {
                throw Error("GotoTarget !Client.Self.AgentID == Prim.ID");
            }
            SimPosition pos = R.GetWaypointOf(local);
            Vector3d global = pos.GlobalPosition;
            StopMovingReal();
            return Client.Self.Teleport(R.RegionHandle, local, local);

            //CmdResult s = Client.ExecuteCommand("teleport " + R.RegionName + "/" + local.X + "/" + local.Y + "/" + local.Z, Debug);
            //return s.Success;
            //  Client.Self.Teleport(R.RegionName, local);
        }

        public override void SetMoveTarget(SimPosition target, double maxDist)
        {
            OnlyMoveOnThisThread();
            if (target == null)
            {
                ApproachVector3D = Vector3d.Zero;
                //ApproachPosition = null;
                lastDistance = float.MaxValue;
                return;
            }
            // if (ApproachPosition != target)
            {
                lastDistance = float.MaxValue;
                SetMoveTarget(target.GlobalPosition);
                ApproachDistance = maxDist;
            }
        }

        public Thread MovementConsumer;
        public void OnlyMoveOnThisThread()
        {
            if (MovementConsumer != null)
            {
                if (MovementConsumer != Thread.CurrentThread && MovementConsumer.IsAlive)
                {
                    if (!MovementConsumer.IsBackground)
                    {
                        MovementConsumer.Abort();
                    }
                }
            }
            //throw new NotImplementedException();
            MovementConsumer = Thread.CurrentThread;
        }

        public void SetMoveTarget(Vector3d target)
        {
            OnlyMoveOnThisThread();
            lock (TrackerLoopLock)
            {
                if (target != ApproachVector3D)
                {
                    // ApproachPosition = null;
                    ApproachVector3D = target;
                    if (target != Vector3d.Zero)
                    {
                        EnsureTrackerRunning();
                    }
                    else
                    {
                        StopMoving(true);
                    }
                }
            }
        }

        private void EnsureTrackerRunning()
        {
            lock (TrackerLoopLock)
            {
                if (ApproachThread == null || !ApproachThread.IsAlive)
                {
                    WorldSystem.SetSimAvatar(this);
                    ApproachThread = new Thread(TrackerLoop);
                    ApproachThread.Name = "TrackerLoop for " + Client;
                    ApproachThread.Priority = ThreadPriority.Normal;
                    ApproachThread.IsBackground = true;
                    Client.Self.Movement.UseOnlyThreads.Add(ApproachThread);
                    ApproachThread.Start();

                    Client.Network.RegisterCallback(PacketType.AlertMessage, AlertMessageHandler);
                }
            }
        }

        private void AlertMessageHandler(object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.Packet;
            
            AlertMessagePacket alert = (AlertMessagePacket)packet;
            string message = Utils.BytesToString(alert.AlertData.Message);

            if (message.Contains("Autopilot cancel"))
            {
                InAutoPilot = false;
            }
        }

        private Thread ApproachThread; /// = new Thread(TrackerLoop);

        public override bool SetObjectRotation(Quaternion localPos)
        {
            if (!IsRoot)
            {
                Quaternion start = SimRotation;
                Quaternion offset = localPos / start;
                SimObject p = Parent;
                return p.SetObjectRotation(p.SimRotation * offset);
            }
            WorldSystem.SetObjectRotation(Prim, localPos);
            return true;
        }

        private static int InTurn = 0;

        private Quaternion LastTT = Quaternion.Identity;
        private Vector3 LastTTV = Vector3.Zero;
        private bool ConsumerSetFight;
        private bool InAutoPilot;
        private bool UseNudging = false;

        public override bool TurnToward(Vector3 target)
        {
            if (true)
            {
                Quaternion parentRot = Quaternion.Identity;
                Quaternion between = Vector3.RotationBetween(Vector3.UnitX, Vector3.Normalize(target - Client.Self.SimPosition));
                Quaternion rot = between * (Quaternion.Identity / parentRot);
                rot.Normalize();
                if (rot == LastTT)
                {
                    //return true;
                }
                LastTTV = target;
                LastTT = rot;
                if (UseSimpleTurnToward) return true;
            }

            return TurnToward00(target);
        }

        private bool TurnToward00(Vector3 target)
        {
            //my philosophy is if the body and Head roation is facing hood ornimant.. the controls right/left are applicable .. is someone screws up and makes my avatar stand on it head backwards.. thats still fine i can still locate the hood ornimant.. i just have to hit the opposite directions left/right.. there is no guesswork.. just math.. sometimes whne i want my avatar to ratote left.. and hes in a car.. the only choice i have is the buttons.. the head roation is useless.. so basically libomv declares.. if your sitting on something.. dont expect stuff to work. what bothrs me is why does it do what i want and probably anyone else who wants a bot to dirve a prim wants.. .. whats all this trouble about sitting ona  prim and gettign world position
            if (!IsControllable)
            {
                Debug("Cannot COntrol TurnToward " + target);
                return false;
            }

            AgentManager.AgentMovement ClientMovement = Client.Self.Movement;
            if (!IsDrivingVehical)
            {
                bool prev = Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK;
                int time = Client.Self.Movement.UpdateInterval;
                bool uen = Client.Self.Movement.AutoResetControls;
                if (InTurn > 0)
                {
                    //throw new InvalidActivityException("two TurnTowards?"); 
                }
                try
                {
                    Client.Settings.SEND_AGENT_UPDATES = true;
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                    Client.Self.Movement.TurnToward(target);
                    Client.Self.Movement.UpdateInterval = 250;
                    Client.Self.Movement.AutoResetControls = false;
                    InTurn++;
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                    ClientMovement.AutoResetControls = false;
                    ClientMovement.TurnToward(target);
                }
                finally
                {
                    Client.Self.Movement.UpdateInterval = time;
                    Client.Self.Movement.AutoResetControls = uen;
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = prev;
                    InTurn--;

                }
                return true;
            }
            else
            {
                return TurnTowardVehical(target);
            }
        }

        private bool TurnTowardVehical(Vector3 target)
        {
            bool changed = false;
            AgentManager.AgentMovement ClientMovement = Client.Self.Movement;
            Quaternion parentRot = Quaternion.Identity;

            Avatar Prim = theAvatar;
            if (Prim.ParentID != 0)
            {
                Primitive parent = null;
                if (_Parent != null)
                {
                    parent = _Parent.Prim;
                }
                if (parent == null)
                {
                    parent = WorldSystem.GetPrimitive(Prim.ParentID, Client.Network.CurrentSim);
                }
                if (parent == null)
                {
                    Logger.Log("Attempted TurnToward but parent prim is not in dictionary", Helpers.LogLevel.Warning,
                               Client);
                    parent = WorldSystem.GetPrimitive(Prim.ParentID, Client.Network.CurrentSim);
                }
                if (parent == null) Debug("cant get parrent ");
                else
                {
                    parentRot = parent.Rotation;
                    RequestedParent = true;
                }
            }

            {
                int tries = 1; // int.MaxValue;
                Vector3 lp = SimPosition;
                bool needsTurn = true;
                while (needsTurn && tries-- > 0)
                {

                    // ClientMovement.ResetControlFlags();
                    double ZDir = ZHeading;
                    Vector3 dif = target - lp;
                    double Wanted = (Math.Atan2(-dif.X, -dif.Y) + Math.PI); // 2Pi= N, 1/2Pi = E
                    double lr = (ZDir - Wanted)*SimPathStore.RAD2DEG;
                    while (lr > 180) lr -= 360;
                    while (lr < -180) lr += 360;
                    if (lr < -20)
                    {
                        //ClientMovement.ResetControlFlags();
                        ClientMovement.TurnRight = true;
                        ClientMovement.YawNeg = true;
                        ClientMovement.SendUpdate(true);
                        Thread.Sleep(800);
                        ClientMovement.TurnRight = false;
                        ClientMovement.YawNeg = false;
                        ClientMovement.SendUpdate(true);
                        //ClientMovement.YawNeg = true;
                        //ClientMovement.SendUpdate(true);
                        //ClientMovement.TurnRight = true;
                        // Thread.Sleep(10);
                        double az = (ZHeading*SimPathStore.RAD2DEG + 30)/SimPathStore.RAD2DEG;
                        float xmul = (float) Math.Cos(az);
                        float ymul = (float) Math.Sin(az);
                        //target = new Vector3(lp.X + xmul, lp.Y - ymul, lp.Z);
                        // Debug("Need to turn " + lr + " for " + target);
                    }
                    else if (lr > 20)
                    {
                        //ClientMovement.ResetControlFlags();
                        ClientMovement.YawPos = true;
                        ClientMovement.TurnLeft = true;
                        ClientMovement.SendUpdate(true);
                        Thread.Sleep(800);
                        ClientMovement.YawPos = false;
                        ClientMovement.TurnLeft = false;
                        ClientMovement.SendUpdate(true);
                        // ClientMovement.YawPos = true;
                        //// ClientMovement.SendUpdate(true);
                        // ClientMovement.TurnLeft = true;
                        //// Thread.Sleep(10);
                        double az = (ZHeading*SimPathStore.RAD2DEG - 30)/SimPathStore.RAD2DEG;
                        float xmul = (float) Math.Cos(az);
                        float ymul = (float) Math.Sin(az);
                        // target = new Vector3(lp.X + xmul, lp.Y - ymul, lp.Z);
                        // Debug("Need to turn " + lr + " for " + target);
                    }
                    else
                    {
                        needsTurn = false;
                    }

                    // use reverse?
                    if (lr < -170 || lr > 170)
                    {
                        if (IsDrivingVehical)
                        {
                            bool atPos = ClientMovement.AtPos;
                            bool nudgeAtPos = ClientMovement.NudgeAtPos;

                            // hit reverse for a moment
                            ClientMovement.AtPos = false;
                            ClientMovement.NudgeAtPos = false;
                            ClientMovement.AtNeg = true;
                            ClientMovement.SendUpdate(true);
                            Thread.Sleep(800);
                            ClientMovement.AtNeg = false;
                            ClientMovement.SendUpdate(true);

                            ClientMovement.AtPos = atPos;
                            ClientMovement.NudgeAtPos = nudgeAtPos;
                        }

                    }
                }

                Quaternion between = Vector3.RotationBetween(Vector3.UnitX,
                                                             Vector3.Normalize(target - SimPosition));
                Quaternion rot = between*(Quaternion.Identity/parentRot);

                Quaternion br = ClientMovement.BodyRotation;
                Quaternion hr = ClientMovement.HeadRotation;

                changed = true;
                ClientMovement.BodyRotation = rot;
                ClientMovement.HeadRotation = rot;
                ClientMovement.Camera.LookAt(SimPosition, target);

                bool prev = Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK;
                try
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
                    ClientMovement.TurnToward(target);
                    //ClientMovement.SendUpdate(true);
                }
                finally
                {
                    Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = prev;
                }
            }

            return changed;
        }

        /// <summary>
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// 
        /// </summary>
        /// <param name="someObject"></param>
        /// <returns></returns>
        public bool SitOn(SimObject someObject)
        {
            if (someObject == null) return SitOnGround();

            AgentManager ClientSelf = Client.Self;
            uint local = someObject.LocalID;

            //already sitting on 
            Avatar theAvatar = this.theAvatar;
            if (theAvatar == null) return false;
            if (theAvatar.ParentID == local || Client.Self.SittingOn == local)
            {
                Debug("todo should we stand once first? " + someObject);
                return true;
            }

            AutoResetEvent are = new AutoResetEvent(false);
            EventHandler<AvatarSitChangedEventArgs> OnSitChanged =
                (s, e) =>
                {
                    if (e.Avatar == theAvatar)
                    {
                        are.Set();
                    }
                };
            Client.Objects.AvatarSitChanged += OnSitChanged;
            try
            {
                ClientSelf.RequestSit(someObject.Prim.ID, Vector3.Zero);
                ClientSelf.Sit();
                if (!are.WaitOne(10000))
                {
                    // return false;
                }
                return local == ClientSelf.SittingOn;
            }
            finally
            {
                Client.Objects.AvatarSitChanged -= OnSitChanged;
            }
        }

        public bool SitOnGround()
        {
            //Client.Self.Movement.AutoResetControls = false;
            //Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
            //Client.Self.Movement.UpdateInterval = 0;

            AgentManager ClientSelf = Client.Self;
            AutoResetEvent are = new AutoResetEvent(false);
            Avatar theAvatar = this.theAvatar;
            if (theAvatar == null) return false;
            EventHandler<AvatarSitChangedEventArgs> OnSitChanged =
                (s, e) =>
                {
                    if (e.Avatar == theAvatar)
                    {
                        are.Set();
                    }
                };
            EventHandler<AvatarAnimationEventArgs> OnAnimsChanged =
                (s, e) =>
                {
                    if (e.AvatarID == theAvatar.ID)
                        if (SimAssetStore.IsSitAnim(GetAnimUUIDs(e.Animations)))
                        {
                            //int seq = anims[Animations.SIT_GROUND];
                            are.Set();
                        }
                };
            Client.Objects.AvatarSitChanged += OnSitChanged;
            Client.Avatars.AvatarAnimation += OnAnimsChanged;
            try
            {
                ClientSelf.SitOnGround();
                if (!are.WaitOne(10000))
                {
                    return false;
                }
                return 0 == ClientSelf.SittingOn;
            }
            finally
            {
                Client.Objects.AvatarSitChanged -= OnSitChanged;
                Client.Avatars.AvatarAnimation -= OnAnimsChanged;
            }
        }
        public SimObject StandUp()
        {
            var Client = GetGridClient();
            AgentManager ClientSelf = Client.Self;
            bool SAU = Client.Settings.SEND_AGENT_UPDATES;
            try
            {
                Client.Settings.SEND_AGENT_UPDATES = true;
                SimObject UnPhantom = null;
                AgentManager.AgentMovement ClientMovement = ClientSelf.Movement;
                uint sit = ClientSelf.SittingOn;
                if (sit != 0)
                {
                    Simulator simu = GetSimulator();
                    UnPhantom = WorldSystem.GetSimObject(WorldSystem.GetPrimitive(sit, simu), simu);
                    UnPhantom.PathFinding.MakeEnterable(this);
                }
                Client.Self.Crouch(false);
                ClientSelf.AnimationStart(Animations.STANDUP, true);
                ClientSelf.Stand();
                ClientSelf.AnimationStop(Animations.STANDUP, true);
                // StopAllAnimations();
                return UnPhantom;
            }
            finally
            {
                Client.Settings.SEND_AGENT_UPDATES = SAU;
            }
        }
    }
}