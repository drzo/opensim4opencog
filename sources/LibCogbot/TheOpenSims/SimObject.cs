using System;
using System.Collections.Generic;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;
using cogbot.Listeners;
using cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Mesher;
using PathSystem3D.Navigation;
using System.Reflection;
using MushDLR223.ScriptEngines;

namespace cogbot.TheOpenSims
{
    //TheSims-like object
    public class SimObjectImpl : SimPosition, BotMentalAspect, SimMover, SimObject, MeshableObject
    {
        private bool _confirmedObject;
        public bool ConfirmedObject
        {
            get { return _confirmedObject || _propertiesCache != null; }
            set { _confirmedObject = value; }
        }

        public ObjectMovementUpdate ObjectMovementUpdateValue;
        public SimPosition UsePosition
        {
            get
            {
                var pos = this;
                var finalDistance = pos.GetSizeDistance();

                if  (finalDistance > 6) finalDistance = 6;
                else
                    if (finalDistance < 1)
                    {
                        return this;
                    }


                Vector3 v3 = Vector3.Transform(Vector3.UnitX, Matrix4.CreateFromQuaternion(pos.SimRotation)) *
                             (float)finalDistance;
                //var vFinalLocation = pos.GlobalPosition;
                //vFinalLocation.X += v3.X;
                //vFinalLocation.Y += v3.Y;
                return new SimOffsetPosition(this, v3);
            }
        }
        public static implicit operator Primitive(SimObjectImpl m)
        {            
            Primitive p = m.Prim;
            p.GetType();
            return p;
        }

        public static implicit operator SimObjectImpl(Primitive m)
        {
            return (SimObjectImpl) WorldObjects.GridMaster.GetSimObject(m);
        }

        public float ZHeading
        {
            get
            {
                return (float) (double) WorldObjects.GetZHeading(SimRotation);
            }
        }

        public SimHeading GetHeading()
        {
            lock (HasPrimLock)
                if (!IsRegionAttached && HasPrim)
                {
                    Simulator sim = WorldSystem.GetSimulator(RegionHandle);
                    Debug("Requesting object for heading");
                    sim.Client.Objects.RequestObject(sim, LocalID);
                    EnsureParentRequested(sim);
                }
            return new SimHeading(this);
        }


        public bool CanShoot(SimPosition position)
        {
            if (!IsRegionAttached || !position.IsRegionAttached) return false;
            SimPathStore PS1 = PathStore;
            SimPathStore PS2 = position.PathStore;
            if (PS1 != PS2) return false;
            Vector3 end = position.SimPosition;
            Vector3 first = PS1.FirstOcclusion(SimPosition, end);
            return (first == end);
        }

        public void AddInfoMap(object properties, string name)
        {
            if (WorldObjects.MaintainSimObjectInfoMap)
            {
                List<NamedParam> from = WorldObjects.GetMemberValues("", properties);
                foreach (var o in from)
                {
                    AddInfoMapItem(o);
                }
            }
            if (!WorldObjects.SendSimObjectInfoMap) return;

            WorldSystem.SendOnUpdateDataAspect(this, name, null, properties);
            WorldSystem.SendNewRegionEvent(SimEventType.DATA_UPDATE, "On" + name + "Update", this);
        }

        protected Primitive.ObjectProperties _propertiesCache { get; set; }

        public BotMentalAspect GetObject(string name)
        {
            return WorldSystem.GetObject(name);
        }

        public ulong RegionHandle { get; set; }
        public UUID ID { get; set; }

        public Primitive.ObjectProperties Properties
        {
            get
            {
                if (_propertiesCache == null)
                {
                    if (!HasPrim) return null;
                    Primitive Prim = this.Prim;
                    if (Prim.Properties != null)
                    {
                        UpdateProperties(Prim.Properties);
                    }
                    else
                    {
                        WorldObjects.EnsureSelected(Prim.LocalID, GetSimulator());
                    }
                }
                return _propertiesCache;
            }
            set
            {
                UpdateProperties(value);
                lock (HasPrimLock) if (HasPrim)
                    {
                        Prim.Properties = value;
                    }
            }
        }

        public float GetCubicMeters()
        {
            Vector3 v3 = GetSimScale();
            return v3.X * v3.Y * v3.Z;
        }

        public SimObject GetGroupLeader()
        {
            if (!IsRoot) return Parent.GetGroupLeader();
            List<SimObject> e = GetNearByObjects(GetSizeDistance() + 1, false);
            e.Add(this);
            e.Sort(CompareSize);
            return e[0];
        }

        public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        #region SimMover Members

        public virtual void ThreadJump()
        {
        }

        public virtual void StopMoving()
        {
            // not really a mover
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="finalTarget"></param>
        /// <param name="maxDistance"></param>
        /// <param name="maxSeconds"></param>
        /// <returns></returns>
        public virtual bool MoveTo(Vector3d finalTarget, double maxDistance, float maxSeconds)
        {
            double currentDist = Vector3d.Distance(finalTarget, GlobalPosition);
            if (currentDist < maxDistance) return true;
            {
                SimWaypoint P = SimWaypointImpl.CreateGlobal(finalTarget);
                SetMoveTarget(P, (float)maxDistance);
            }
            for (int i = 0; i < maxSeconds; i++)
            {
                Application.DoEvents();
                currentDist = Vector3d.Distance(finalTarget, GlobalPosition);
                if (currentDist > maxDistance)
                {
                    Thread.Sleep(1000);
                    continue;
                }
                else
                {
                    // StopMoving();
                    return true;
                }
            }
            StopMoving();
            return false;
        }

        public virtual void Touch(SimObject simObject)
        {
            WorldSystem.client.Self.Touch(simObject.Prim.LocalID);
        }

        #endregion

        public SimPathStore PathStore
        {
            get
            {
                SimRegion R = GetSimRegion(); //
                if (R == null) return null;
                return R.GetPathStore(SimPosition);
            }
        }

        public virtual bool TurnToward(SimPosition targetPosition)
        {
            return TurnToward(targetPosition.GlobalPosition);
            //SendUpdate(0);
        }

        public virtual void SendUpdate(int ms)
        {
            Thread.Sleep(ms);
        }

        public virtual void SetMoveTarget(SimPosition target, double maxDistance)
        {
            //SimRegion R = target.GetSimRegion();
            //if (R != GetSimRegion())
            //{
            //    TeleportTo(R,target.GetSimPosition());
            //}
            Vector3d finalPos = target.GlobalPosition;
            Vector3d start = GlobalPosition;
            Vector3d offset = finalPos - start;
            double points = offset.Length();
            Vector3d offsetEach = offset / points;
            while (points > 1)
            {
                points -= 1;
                start += offsetEach;
                SetObjectPosition(start);
            }
            SetObjectPosition(finalPos);
        }

        /// <summary>
        /// Used to be 9 now its 4 times
        /// </summary>
        /// <param name="pos"></param>
        /// <returns></returns>
        public virtual bool GotoTarget(SimPosition pos)
        {
            if (!IsControllable)
            {
                throw Error("GotoTarget !IsControllable");
            }
            BotClient Client = WorldSystem.client;
            float maxDist = pos.GetSizeDistance();
            for (int i = 0; i < 4; i++)
            {
                bool result = FollowPathTo(pos, maxDist);
                if (result)
                {
                    try
                    {
                        SetMoveTarget(pos, maxDist);
                    }
                    finally
                    {
                        IndicateTarget(pos, false);
                    }
                    return true;
                }
            }
            IndicateTarget(pos, true);
            return false;
        }

        protected void IndicateTarget(SimPosition pos, bool tf)
        {
            //return;
            BotClient Client = WorldSystem.client;
            if (tf)
            {
                SimObject obj = pos as SimObject;
                if (obj != null)
                {
                    Client.ExecuteCommand("pointat " + obj.ID, Debug);
                }
                {
                    var vFinalLocation = pos.UsePosition.GlobalPosition;
                    Client.ExecuteCommand("pointat " + vFinalLocation.ToRawString(), Debug);
                }
            }
            else Client.ExecuteCommand("pointat", Debug);
        }
        public virtual bool IsControllable
        {
            get
            {
                if (!IsRoot) return false;
                return HasPrim; // WorldSystem.client.Network.CurrentSim == GetSimRegion().TheSimulator;
            }
        }

        public bool FollowPathTo(SimPosition globalEnd, double distance)
        {
            if (!IsControllable)
            {
                throw Error("FollowPathTo !IsControllable");
            }
            SimAbstractMover move = SimAbstractMover.CreateSimPathMover(this, globalEnd, distance);
            try
            {
                move.OnMoverStateChange += OnMoverStateChange;
                return move.Goto() == SimMoverState.COMPLETE;
            }
            finally
            {
                move.OnMoverStateChange -= OnMoverStateChange;
            }
        }

        protected virtual void OnMoverStateChange(SimMoverState obj)
        {
        }


        public virtual bool TeleportTo(SimPosition local)
        {
            if (!IsControllable)
            {
                throw Error("GotoTarget !Client.Self.AgentID == Prim.ID");
            }
            return TeleportTo(SimRegion.GetRegion(SimRegion.GetRegionHandle(local.PathStore)), local.SimPosition);
        }

        public virtual bool TeleportTo(SimRegion R, Vector3 local)
        {
            return SetObjectPosition(R.LocalToGlobal(local));
        }

        public bool SetObjectPosition(Vector3d globalPos)
        {
            Vector3d start = GlobalPosition;
            Vector3d offset = globalPos - start;
            Vector3 lPos = SimPosition;
            lPos.X += (float)offset.X;
            lPos.Y += (float)offset.Y;
            lPos.Z += (float)offset.Z;
            return SetObjectPosition(lPos);
        }

        public bool SetObjectPosition(Vector3 localPos)
        {
            if (!IsRoot)
            {
                Vector3 start = SimPosition;
                Vector3 offset = localPos - start;
                SimObject p = Parent;
                return p.SetObjectPosition(p.SimPosition + offset);
            }
            WorldSystem.SetObjectPosition(Prim, localPos);
            return true;
        }

        #region SimMover Members

        public virtual bool TurnToward(Vector3d targetPosition)
        {
            Vector3d Current = GlobalPosition;
            Vector3d diff = targetPosition - Current;
            while (diff.Length() > 10)
            {
                diff.X *= 0.75f;
                diff.Y *= 0.75f;
                diff.Z *= 0.75f;
            }
            return TurnToward(SimPosition + new Vector3((float)diff.X, (float)diff.Y, 0));
        }

        #endregion

        public virtual bool TurnToward(Vector3 target)
        {
            Quaternion parentRot = Quaternion.Identity;

            if (!IsRoot)
            {
                parentRot = Parent.SimRotation;
            }

            Quaternion between = Vector3.RotationBetween(Vector3.UnitX, Vector3.Normalize(target - SimPosition));
            Quaternion rot = between * (Quaternion.Identity / parentRot);

            SetObjectRotation(rot);
            return true;
        }

        public virtual bool SetObjectRotation(Quaternion localPos)
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

        // protected SimRegion PathStore;
        public Box3Fill OuterBox
        {
            get { return _Mesh.OuterBox; }
        }

        public uint LocalID
        {
            get
            {
                return Prim.LocalID;
            }
        }

        public uint ParentID
        {
            get { return Prim.ParentID; }
        }

        public virtual bool KilledPrim(Primitive primitive, Simulator simulator)
        {

            lock (_primRefs)
            {
               // _primRefs.Remove(primitive);
                IsKilled = _primRefs.Count < 1;
                if (ReferenceEquals(_Prim0, primitive))
                {
                    _Prim0 = null;
                    RequestedParent = false;
                    Parent = null;
                }
                return WasKilled;
            }
        }

        readonly Dictionary<object,NamedParam> _infoMap = new Dictionary<object, NamedParam>(); 
        public ICollection<NamedParam> GetInfoMap()
        {
            lock (_infoMap) return new List<NamedParam>(_infoMap.Values);
        }

        public void SetInfoMap(string key, MemberInfo type, object value)
        {
            if (!WorldObjects.MaintainSimObjectInfoMap) return;
            if (value == null) value = new NullType(this, type);
            AddInfoMapItem(new NamedParam(type, key, GetType(), value));
        }

        public void AddInfoMapItem(NamedParam ad)
        {
            lock (_infoMap)
                _infoMap[ad.Key] = ad;
        }

        internal void PollForPrim(WorldObjects worldObjects, Simulator sim)
        {
            if (sim == null)
            {
                return;
            }
            Primitive A = worldObjects.GetLibOMVHostedPrim(ID, sim, false);
            if (A != null) this.SetFirstPrim(A);
        }

        readonly private List<Primitive> _primRefs = new List<Primitive>();
        public virtual void ResetPrim(Primitive prim, BotClient bc, Simulator sim)
        {
            if (prim==null) return;
            lock (HasPrimLock)
                if (_Prim0 == null)
                {
                    SetFirstPrim(prim);
                    return;
                }

            Primitive.ObjectProperties properties = prim.Properties;
            bool updateCarriesProperties = properties != null;
            if (updateCarriesProperties) _propertiesCache = properties;
            if (prim.RegionHandle != _Prim0.RegionHandle || !Object.ReferenceEquals(prim, _Prim0))
            {
                lock (_primRefs)
                {
                    bool found = false;
                    foreach (Primitive av in _primRefs)
                    {
                        if (Object.ReferenceEquals(av, prim))
                        {
                            found = true;
                        }
                    }
                    if (!found)
                    {
                        if (prim.ID != ID)
                        {
                            DLRConsole.DebugWriteLine("ERROR: Different UUID! {0}", prim);
                        }
                        _primRefs.Add(prim);
                        //DLRConsole.WriteLine("\n Different prims {0}", prim);
                    }
                }
                lock (HasPrimLock) _Prim0 = prim;
                if (needUpdate)
                {
                    if (updateCarriesProperties) UpdateProperties(properties);
                }
                ResetRegion(prim.RegionHandle);
            }
            if (sim != null) ResetRegion(sim.Handle);
        }

        public virtual void ResetRegion(ulong regionHandle)
        {
            RegionHandle = regionHandle;
            lock (HasPrimLock) if (!HasPrim || _Prim0.RegionHandle != regionHandle)
                {
                    lock (_primRefs)
                    {
                        foreach (Primitive av in _primRefs)
                        {
                            if (av.RegionHandle == regionHandle) _Prim0 = av;
                        }
                    }
                }
        }

        /// <summary>
        /// Right now only sees if TouchName has been defined - need a relable way to see if script is defined.
        /// </summary>
        public bool IsTouchDefined
        {
            get
            {
                Primitive Prim = this.Prim;
                if (Prim != null && (Prim.Flags & PrimFlags.Touch) != 0) return true;
                if (_propertiesCache != null)
                    return !String.IsNullOrEmpty(_propertiesCache.TouchName);
                return false;
            }
        }

        /// <summary>
        /// Need a more relable way to see if script is defined.
        /// </summary>
        public bool IsSitDefined
        {
            get
            {
                Primitive Prim = this.Prim;
                if (Prim != null && Prim.ClickAction == ClickAction.Sit) return true;
                if (_propertiesCache != null)
                    return !String.IsNullOrEmpty(_propertiesCache.SitName);
                return false;
            }
        }

        public bool IsSculpted
        {
            get { return Prim != null && Prim.Sculpt != null; }
        }

        private bool _Passable;
        private bool _PassableKnown = false;

        public bool IsPassable
        {
            get
            {
                if (_PassableKnown) return _Passable;

                if (IsPhantom) return true;
                if (IsTypeOf(SimTypeSystem.PASSABLE) != null)
                {
                    IsPassable = true;
                    return true;
                }

                IsPassable = false;
                return _Passable;
                // unused for now
                if (IsRoot || true) return false;
                if (!IsRoot && IsRegionAttached) return Parent.IsPassable;
                if (Parent == null) return true;
                return Parent.IsPassable;
            }
            set
            {
                if (_PassableKnown)
                {
                    if (value && !_Passable && !WorldObjects.CanPhantomize)
                    {
                        Debug("Wont set IsPassable because WorldObjects.CanPhantomize=false");
                        return;
                    }
                }
                _PassableKnown = true;
                _Passable = value;

                if (_Mesh != null && _Mesh.IsSolid == value)
                {
                    _Mesh.IsSolid = !value;
                }
            }
        }

        public bool IsPhantom
        {
            get
            {
                if (MadePhantom) return true;
                if (IsRoot || true)
                {
                    Primitive Prim = this.Prim;
                    if (Prim == null) return true;
                    return (Prim.Flags & PrimFlags.Phantom) == PrimFlags.Phantom;
                }
                if (!IsRoot && IsRegionAttached) return Parent.IsPhantom;
                if (_Parent == null) return true;
                return Parent.IsPhantom;
            }
            set
            {
                if (IsPhantom == value) return;
                if (!WorldObjects.CanPhantomize)
                {
                    Debug("Wont set IsPhantom because WorldObjects.CanPhantomize=false");
                    return;
                }
                Primitive Prim = this.Prim;
                if (Prim==null)
                {
                    Debug("Wont set IsPhantom because Prim==null");
                    return;
                }
                if (value)
                {
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags)(Prim.Flags | PrimFlags.Phantom));
                    MadePhantom = true;
                }
                else
                {
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags)(Prim.Flags - PrimFlags.Phantom));
                    MadePhantom = false;
                }
            }
        }

        public bool IsPhysical
        {
            get
            {
                if (!IsRoot) return Parent.IsPhysical;
                return (Prim.Flags & PrimFlags.Physics) == PrimFlags.Physics;
            }
            set
            {
                if (IsPhysical == value) return;
                if (value)
                {
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags)(Prim.Flags | PrimFlags.Physics));
                    MadeNonPhysical = false;
                }
                else
                {
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags)(Prim.Flags - PrimFlags.Physics));
                    MadeNonPhysical = true;
                }
            }
        }

        public bool InventoryEmpty
        {
            get
            {
                lock (HasPrimLock) return (HasPrim && (Prim.Flags & PrimFlags.InventoryEmpty) != 0);
            }
        }

        public bool Sandbox
        {
            get
            {
                lock (HasPrimLock) return HasPrim && (Prim.Flags & PrimFlags.Sandbox) != 0;
            }
        }

        public bool Temporary
        {
            get
            {
                lock (HasPrimLock) return HasPrim && (Prim.Flags & PrimFlags.Temporary) != 0;
            }
        }

        public virtual bool Flying
        {
            get
            {
                lock (HasPrimLock) return HasPrim && (Prim.Flags & PrimFlags.Flying) != 0;
            }
            set
            {
                if (Flying != value)
                {
                    lock (HasPrimLock) if (IsControllable)
                        {
                            Prim.Flags = (Prim.Flags | PrimFlags.Flying);
                            //WorldSystem.client.Objects.SetFlags();
                        }
                }
            }
        }

        public bool AnimSource
        {
            get
            {
                lock (HasPrimLock) return HasPrim && (Prim.Flags & PrimFlags.AnimSource) != 0;
            }
        }

        public bool AllowInventoryDrop
        {
            get
            {
                lock (HasPrimLock) return HasPrim && (Prim.Flags & PrimFlags.AllowInventoryDrop) != 0;
            }
        }

        public bool IsAvatar
        {
            get
            {
                return this is SimAvatar || _Prim0 is Avatar;
            }
        }
        //Vector3d lastPos;
        //SimWaypoint swp;
        //public virtual SimWaypoint GetWaypoint()
        //{
        //    Vector3d v3 = GlobalPosition();

        //    if (swp == null || !swp.Passable)
        //    {
        //        SimRegion PathStore = GetSimRegion();
        //        swp = PathStore.CreateClosestWaypoint(v3);//, GetSizeDistance() + 1, 7, 1.0f);
        //        if (!swp.Passable)
        //        {
        //            double dist = Vector3d.Distance(v3, swp.GlobalPosition());
        //            swp.EnsureAtLeastOnePath();
        //            WorldSystem.WriteLine("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
        //        }
        //        if (lastPos != v3)
        //        {
        //            lastPos = v3;
        //            List<ISimObject> objs = GetNearByObjects(3f, false);
        //            foreach (ISimObject O in objs)
        //            {
        //                O.UpdateBlocked(PathStore);
        //            }
        //        }
        //        if (!swp.Passable)
        //        {
        //            double dist = Vector3d.Distance(v3, swp.GlobalPosition());
        //            WorldSystem.WriteLine("BAD: " + v3 + " <- " + dist + " -> " + swp + " " + this);
        //            swp = PathStore.ClosestNode(v3.X, v3.Y, v3.Y, out dist, false);//, GetSizeDistance() + 1, 7, 1.0f);
        //        }
        //    }
        //    return swp;
        //    //            return PathStore.CreateClosestWaypointBox(v3, 4f);
        //}


        public double Distance(SimPosition prim)
        {
            if (prim == null || !prim.IsRegionAttached) return 1300;
            if (!IsRegionAttached) return 1300;
            Vector3d primGlobalPosition = prim.GlobalPosition;
            Vector3d GlobalPosition;
            if (TryGetGlobalPosition(out GlobalPosition))
            {
                double d = Vector3d.Distance(GlobalPosition, primGlobalPosition);
                Vector3d use = prim.UsePosition.GlobalPosition;
                double d1 = Vector3d.Distance(use, GlobalPosition);
                return Math.Min(d, d1);
            }
            return 1200;
        }

        // the prim in Secondlife
        protected Primitive _Prim0;

        public Primitive Prim
        {
            get
            {
                lock (HasPrimLock)
                {
                    if (_Prim0 == null)
                    {
                        if (RegionHandle !=0)
                        {
                            Simulator S = WorldSystem.GetSimulator(RegionHandle);
                            var found = WorldSystem.GetLibOMVHostedPrim(ID, S, false);
                            if (found == null) return null;
                            SetFirstPrim(found);
                            return found;
                        }
                        return null;
                    }
                    if (_Prim0.RegionHandle != RegionHandle)
                    {
                        if (RegionHandle != 0)
                            ResetRegion(RegionHandle);
                    }
                    if (RegionHandle == 0)
                    {
                        RegionHandle = _Prim0.RegionHandle;
                    }
                    return _Prim0;
                }
            }
            // set { _Prim0 = value; }
        }

        //{
        //    get { return base.Prim; }
        //    set { Prim = value; }
        //}
        public SimObjectType ObjectType { get; set; }
        public WorldObjects WorldSystem;
        private bool MadeNonPhysical = false;
        private bool MadePhantom = false;
        private bool needUpdate = true;

        public bool NeedsUpdate
        {
            get
            {
                return needUpdate;
            }
        }

        protected bool WasKilled;

        public virtual bool IsKilled
        {
            // get { return WasKilled; }
            set
            {
                if (WasKilled != value) //already
                {
                    IsDebugging = true;
                    WasKilled = value;
                    var AttachedChildren0 = Children;
                    lock (AttachedChildren0)
                        foreach (SimObject C in AttachedChildren0)
                        {
                            C.IsKilled = value;
                        }
                    if (WasKilled) RemoveCollisions();
                }
            }
            get
            {
                return WasKilled;
            }
        }

        public void RemoveCollisions()
        {
            IsMeshed = false;
            if (_Mesh != null)
            {
                _Mesh.RemoveCollisions();
                _Mesh = null;
            }      
        }

        public SimObjectType IsTypeOf(SimObjectType superType)
        {
            return ObjectType.IsSubType(superType);
        }

        protected ListAsSet<SimObject> _children = new ListAsSet<SimObject>();

        public ListAsSet<SimObject> Children
        {
            get { return _children; }
        }

        public bool HasChildren
        {
            get { return _children.Count > 0; }
        }

        /// <summary>
        /// the bonus or handicap the object has compared to the defination 
        /// (more expensive chair might have more effect)
        /// </summary>
        public float scaleOnNeeds = 1.11F;

        public SimObjectImpl(UUID id, WorldObjects objectSystem, Simulator sim)
        //: base(prim.ID.ToString())
        // : base(prim, SimRegion.SceneProviderFromSimulator(sim))
        {
            ActionEventQueue = new Queue<SimObjectEvent>(MaxEventSize);
            ID = id;
            if (sim != null) RegionHandle = sim.Handle;
            WorldSystem = objectSystem;
            ObjectType = SimTypeSystem.CreateInstanceType(id.ToString());
            //_CurrentRegion = SimRegion.GetRegion(sim);
            // PathStore = GetSimRegion();
            //WorldSystem.EnsureSelected(prim.ParentID,sim);
            // Parent; // at least request it
        }

        public virtual void SetFirstPrim(Primitive prim)
        {
            lock (HasPrimLock)
                if (prim != null)
                {
                    if (ID == UUID.Zero) ID = prim.ID;
                    _Prim0 = prim;
                    if (prim.RegionHandle != 0)
                    {
                        RegionHandle = prim.RegionHandle;
                    }
                    lock (_primRefs)
                    {
                        if (!_primRefs.Contains(prim))
                        {
                            _primRefs.Add(prim);
                            AddInfoMap(prim, "Primitive");
                        }
                    }
                    if (prim.Properties != null)
                    {
                        // Properties = prim.Properties;
                        Properties = prim.Properties;
                    }

                    if (WorldObjects.MaintainSimCollisions(prim.RegionHandle) && prim.Sculpt != null)
                    {
                        WorldSystem.StartTextureDownload(prim.Sculpt.SculptTexture);
                    }
                }
        }

        protected SimObject _Parent = null; // null means unknown if we IsRoot then Parent == this;

        public virtual SimObject Parent
        {
            get
            {
                if (_Parent == null)
                {
                    if (Prim == null) return _Parent;
                    uint parentID = Prim.ParentID;
                    if (parentID == 0)
                    {
                        _Parent = this;
                    }
                    else
                    {
                        Simulator simu = GetSimulator();
                        Primitive prim = WorldSystem.GetPrimitive(parentID, simu);
                        if (prim == null)
                        {
                            // missing prim?!
                            IsDebugging = true;
                            // try to request for next time
                            EnsureParentRequested(simu);
                            return _Parent;
                        }
                        Parent = WorldSystem.GetSimObject(prim, simu);
                    }
                }
                return _Parent;
            }
            set
            {
                if (value == _Parent) return;
                SetInfoMap("Parent", GetType().GetProperty("Parent"), value);
                if (value == null)
                {
                    _Parent.Children.Remove(this);
                }
                else if (value != this)
                {
                    _isChild = true;
                    if (value.Children.AddTo(this))
                    {
                        needUpdate = true;
                        SimObjectImpl simObject = (SimObjectImpl)value;
                        simObject.needUpdate = true;
                    }
                }
                _Parent = value;
                if (_Parent !=null && _Parent.Prim != null) RequestedParent = true;
            }
        }

        public bool AddChild(SimObject simO)
        {
            SimObjectImpl simObject = (SimObjectImpl)simO;
            needUpdate = true;
            simObject._Parent = this;
            simObject.needUpdate = true;
            bool b = Children.AddTo(simObject);
            if (false) if (b)
                {
                    if (!IsTyped)
                    {
                        // borrow from child?
                        // UpdateProperties(simObject.theProperties);
                    }
                }
            return b;
        }

        public bool IsTyped
        {
            get
            {
                if (WasKilled) return false;
                return ObjectType.IsComplete;
            }
        }

        public virtual bool IsRoot
        {
            get
            {
                if (WasKilled) return false;
                var Prim = this.Prim;
                if (Prim == null || Prim.ParentID == 0) return true;
                IsParentAccruate(Prim);
                // _Parent = Parent;
                return false;
            }
        }

        public virtual string DebugInfo()
        {
            string str = ToString();
            var Prim = this.Prim;
            if (Prim == null || !HasPrim) return str;
            if (Prim.ParentID != 0)
                return Prim.ParentID + " " + str;
            return str;
        }

        public double RateIt(BotNeeds needs)
        {
            return ObjectType.RateIt(needs, GetBestUse(needs)) * scaleOnNeeds;
        }

        public IList<SimTypeUsage> GetTypeUsages()
        {
            return ObjectType.GetTypeUsages();
        }

        public List<SimObjectUsage> GetUsages()
        {
            List<SimObjectUsage> uses = new List<SimObjectUsage>();
            if (needUpdate)
            {
                UpdateProperties(_propertiesCache);
            }
            foreach (SimTypeUsage typeUse in ObjectType.GetTypeUsages())
            {
                uses.Add(new SimObjectUsage(typeUse, this));
            }
            return uses;
        }

        public List<string> GetMenu(SimAvatar avatar)
        {
            //props.Permissions = new Permissions(objectData.BaseMask, objectData.EveryoneMask, objectData.GroupMask,
            //  objectData.NextOwnerMask, objectData.OwnerMask);
            List<string> list = new List<string>();
            if (_propertiesCache != null)
            {
                //  if (theProperties.TextName != "")
                list.Add("grab");
                //   if (theProperties.SitName != "")
                list.Add("sit");
                PermissionMask mask = _propertiesCache.Permissions.EveryoneMask;
                if (Prim.OwnerID == avatar.theAvatar.ID)
                {
                    mask = _propertiesCache.Permissions.OwnerMask;
                }
                PermissionMask result = mask | _propertiesCache.Permissions.BaseMask;
                if ((result & PermissionMask.Copy) != 0)
                    list.Add("copy");
                if ((result & PermissionMask.Modify) != 0)
                    list.Add("modify");
                if ((result & PermissionMask.Move) != 0)
                    list.Add("move");
                if ((result & PermissionMask.Transfer) != 0)
                    list.Add("transfer");
                if ((result & PermissionMask.Damage) != 0)
                    list.Add("damage");
            }
            return list;
        }

        // This field is supposed to be the most recent property udate 
        private Primitive.ObjectProperties MostRecentPropertyUpdate;
        readonly object MostRecentPropertyUpdateLock = new object();
        private void UpdateProperties(Primitive.ObjectProperties objectProperties)
        {
            if (objectProperties==null)
            {
                Debug("NULL PROPS!!?!");
                return;
            }
            if (!_confirmedObject)
            {
                _confirmedObject = true;
                Debug("Now confirmed!!");
            }
            lock (MostRecentPropertyUpdateLock)
            {
                MostRecentPropertyUpdate = objectProperties;
            }
            WorldObjects.UpdateObjectData.Enqueue(UpdateProperties0);
        }

        private void UpdateProperties0()
        {
            Primitive.ObjectProperties objectProperties = null;
            lock (MostRecentPropertyUpdateLock)
            {
                if (MostRecentPropertyUpdate == null) return; // something allready did the work            
                objectProperties = MostRecentPropertyUpdate;
                MostRecentPropertyUpdate = null;
            }
            try
            {
                Primitive Prim = this.Prim;
                IsParentAccruate(Prim);
                toStringNeedsUpdate = true;
                if (objectProperties != null)
                {
                    _propertiesCache = objectProperties;
                    if (Prim != null && Prim.Properties == null) Prim.Properties = objectProperties;
                    ObjectType.SitName = objectProperties.SitName;
                    ObjectType.TouchName = objectProperties.TouchName;
                    if (needUpdate)
                    {
                        needUpdate = false;
                        //  Parent; 
                        if (_propertiesCache != null)
                        {
                            SimTypeSystem.GuessSimObjectTypes(objectProperties, this);
                        }
                        else
                        {
                            SimTypeSystem.GuessSimObjectTypes(objectProperties, this);
                        }
                    }
                    else
                    {
                        if (_propertiesCache != null)
                        {
                            SimTypeSystem.GuessSimObjectTypes(objectProperties, this);
                        }
                        else
                        {
                            SimTypeSystem.GuessSimObjectTypes(objectProperties, this);
                        }
                    }
                    _propertiesCache = objectProperties;
                    AddInfoMap(objectProperties, "ObjectProperties");
                }
            }
            catch (Exception e)
            {
                Debug("" + e);
            }
        }

        protected bool IsParentAccruate(Primitive child)
        {
            return true;
            //return _Parent != null;
            if (child == null) return false;
            if (child.ParentID == 0)
            {
                _Parent = this;
                return true;
            }
            if (_Parent != null)
            {
                if (_Parent.Prim == null || _Parent.Prim.LocalID == child.ParentID) return true;
                _Parent = null;
                Debug("Nulling parent");
                return false;
            }
            return true;
            //throw new NotImplementedException();
        }


        private void AddSimObjectTypes(Primitive.ObjectProperties properties)
        {
            ObjectType.SitName = properties.SitName;
            ObjectType.TouchName = properties.TouchName;
            SimTypeSystem.GuessSimObjectTypes(properties, this);
        }


        public virtual void UpdateObject(ObjectMovementUpdate objectUpdate, ObjectMovementUpdate objectUpdateDiff)
        {
            if (needUpdate)
            {
                if (_propertiesCache != null) UpdateProperties(_propertiesCache);
            }
            if (IsRegionAttached)
            {
                if (WorldSystem.IsWorthMeshing(this))
                {
                    UpdateOccupied();
                }
            }
            toStringNeedsUpdate = true;
        }

        private bool IsMeshing;
        public virtual bool UpdateOccupied()
        {
            try
            {
                return UpdateOccupied0();
            }
            catch (Exception e)
            {
                Debug("While updating " + e);
                return false;
            }

        }
        public virtual bool UpdateOccupied0()
        {
            if (!IsRegionAttached)
            {
                return false;
            }
            if (!IsRoot)
            {
                // dont mesh armor
                if (Parent is SimAvatar)
                {
                    return false;
                }
            }
            try
            {
                if (!IsMeshing)
                {
                    IsMeshing = true;
                    WorldSystem.SimPaths.MeshingQueue.Enqueue(UpdateOccupied1);
                }
                return wasMeshUpdated;
               
            } finally
            {
                wasMeshUpdated = false;
            }
        }

        public void UpdateOccupied1()
        {
            try
            {
                UpdateOccupied2();
            }
            catch (Exception e)
            {
                Debug("While updating " + e);
            }
            finally
            {
                IsMeshing = false;
                IsMeshed = true;
            }
        }

        public void UpdateOccupied2()
        {
            if (!IsRegionAttached)
            {
                return;
            }
            if (!IsRoot)
            {
                // dont mesh armor
                if (Parent is SimAvatar)
                {
                    return;
                }
            }

            bool updated = GetSimRegion().AddCollisions(Mesh);
            // update parent + siblings
            if (!IsRoot)
            {
                if (_Parent != null)
                {
                    if (_Parent.UpdateOccupied()) updated = true;
                }
            }
            // update children
            foreach (SimObject o in Children)
            {
                if (o.UpdateOccupied())
                {
                    updated = true;
                }
            }
            wasMeshUpdated = updated;
        }

        public void AddSuperTypes(IList<SimObjectType> listAsSet)
        {
            toStringNeedsUpdate = true;
            //SimObjectType _UNKNOWN = SimObjectType._UNKNOWN;
            foreach (SimObjectType type in listAsSet)
            {
                ObjectType.AddSuperType(type);
            }
        }

        public virtual bool RestoreEnterable(SimMover actor)
        {
            bool changed = false;
            Primitive Prim = this.Prim;
            if (Prim == null) return false;
            PrimFlags tempFlags = Prim.Flags;
            if (MadePhantom)
            {
                ((SimObject)actor).Touch(this);
                changed = true;
                IsPhantom = false;
            }
            if (!IsRoot)
            {
                SimObject P = Parent;
                if (P.Prim != this.Prim)
                    return P.RestoreEnterable(actor);
            }
            return changed;
        }

        public virtual bool MakeEnterable(SimMover actor)
        {
            if (IsTypeOf(SimTypeSystem.DOOR) != null)
            {
                if (!IsPhantom)
                {
                    ((SimObject)actor).Touch(this);
                    return true;
                }
                return false;
            }

            if (!IsRoot)
            {
                SimObject P = Parent;
                if (P.Prim != this.Prim)
                    return P.MakeEnterable(actor);
            }

            bool changed = false;
            if (!IsPhantom)
            {
                IsPhantom = true;
                ((SimObject)actor).Touch(this);
                changed = true;
                // reset automatically after 30 seconds
                new Thread(new ThreadStart(delegate()
                                               {
                                                   Thread.Sleep(30000);
                                                   IsPhantom = false;
                                               })).Start();
            }
            return changed;
        }

        private string _TOSRTING;

        public override string ToString()
        {
            if (needUpdate || _TOSRTING==null || toStringNeedsUpdate) 
            {
                Primitive Prim = null;
                lock (HasPrimLock)
                {
                    Prim = this.Prim;
                    if (!HasPrim || Prim == null) return "UNATTACHED_PRIM " + ID;
                }
                toStringNeedsUpdate = false;
                _TOSRTING = "";
                if (_propertiesCache != null)
                {
                    if (!String.IsNullOrEmpty(_propertiesCache.Name))
                        _TOSRTING += String.Format("{0} ", _propertiesCache.Name);
                    if (!String.IsNullOrEmpty(_propertiesCache.Description))
                        _TOSRTING += String.Format(" | {0} ", _propertiesCache.Description);
                }
                else
                {
                    needUpdate = true;
                    if (RegionHandle == 0) RegionHandle = Prim.RegionHandle;
                    // DLRConsole.WriteLine("Reselecting prim " + Prim);
                    Simulator sim = GetSimulator();
                    if (sim != null)
                        WorldObjects.EnsureSelected(Prim.LocalID, sim);
                }
                ID = Prim.ID;
                Primitive.ConstructionData PrimData = Prim.PrimData;
                PrimType Type = Prim.Type;

                if (PrimData.PCode == PCode.Prim)
                {
                    try
                    {
                        _TOSRTING += "" + Type;
                    }
                    catch (Exception e)
                    {
                    }
                }
                else
                {
                    try
                    {
                        _TOSRTING += "" + PrimData.PCode;
                    }
                    catch (Exception e)
                    {
                    }
                }
                _TOSRTING += String.Format(" {0} ", ID);

                if (!String.IsNullOrEmpty(Prim.Text))
                    _TOSRTING += String.Format(" | {0} ", Prim.Text);
                _TOSRTING += "(localID " + Prim.LocalID + ")";
                uint ParentId = Prim.ParentID;
                if (ParentId != 0)
                {
                    _TOSRTING += "(parent ";

                    Primitive pp = null;
                    if (_Parent != null)
                    {
                        //if (_Parent.!!HasPrim)
                        pp = _Parent.Prim;
                    }
                    else
                    {
                        if (RegionHandle != 0)
                        {
                            Simulator simu = GetSimulator();
                            pp = WorldSystem.GetPrimitive(ParentId, simu);
                        }
                    }
                    if (pp != null)
                    {
                        _TOSRTING += WorldSystem.GetPrimTypeName(pp) + " " + pp.ID.ToString().Substring(0, 8);
                    }
                    else
                    {
                        _TOSRTING += ParentId;
                    }
                    _TOSRTING += ")";
                }
                if (Children.Count > 0)
                {
                    _TOSRTING += "(childs " + Children.Count + ")";
                }
                else
                {
                    _TOSRTING += "(ch0)";
                }

                const PrimFlags AllPrimFlags = (PrimFlags)0xffffffff;
                const PrimFlags FlagsToPrintFalse =
                    PrimFlags.ObjectAnyOwner | PrimFlags.InventoryEmpty | PrimFlags.ObjectOwnerModify;
                const PrimFlags FlagsToPrintTrue = (PrimFlags)(AllPrimFlags - FlagsToPrintFalse);

                PrimFlags showTrue = (Prim.Flags & FlagsToPrintTrue);
                if (showTrue != PrimFlags.None) _TOSRTING += "(PrimFlagsTrue " + showTrue + ")";
                PrimFlags showFalse = ((~Prim.Flags) & FlagsToPrintFalse);
                if (showFalse != PrimFlags.None) _TOSRTING += "(PrimFlagsFalse " + showFalse + ")";
                if (_Mesh != null)
                    _TOSRTING += " (size " + GetSizeDistance() + ") ";
                _TOSRTING += SuperTypeString();
                if (Prim.Sound != UUID.Zero)
                    _TOSRTING += "(Audible)";
                if (IsTouchDefined)
                    _TOSRTING += "(IsTouchDefined)";
                if (IsSitDefined)
                    _TOSRTING += "(IsSitDefined)";
                string simverb = GetSimVerb();
                if (!String.IsNullOrEmpty(simverb))
                    _TOSRTING += string.Format("(SimVerb \"{0}\")", simverb);
                if (!IsPassable)
                    _TOSRTING += "(!IsPassable)";
                if (Prim.PrimData.ProfileHollow > 0f)
                    _TOSRTING += String.Format("(hollow {0:0.00})", Prim.PrimData.ProfileHollow);
                if (WasKilled) _TOSRTING += "(IsKilled)";
                _TOSRTING = _TOSRTING.Replace("  ", " ").Replace(") (", ")(");
            }
            string _TOSRTINGR = _TOSRTING;
            toStringNeedsUpdate = false;
            _TOSRTING = null;
            return _TOSRTINGR;
        }

        public string SuperTypeString()
        {
            String str = "[";
            lock (ObjectType.SuperType)
                ObjectType.SuperType.ForEach(delegate(SimObjectType item) { str += item.GetTypeName() + " "; });
            return str.TrimEnd() + "]";
        }


        protected bool RequestedParent = false;

        public bool IsRegionAttached
        {
            get
            {
                if (WasKilled) return false;
                if (!HasPrim) return false;
                Primitive Prim = null;
                lock (HasPrimLock)
                {

                    if (!HasPrim) return false;
                    Prim = _Prim0;
                    if (_Prim0.RegionHandle == 0)
                    {
                        return false;
                    }
                }
                if (IsRoot)
                {
                    return Prim != null && Prim.Position != Vector3.Zero;
                }
                if (_Parent == null)
                {
                    if (Prim.ParentID == 0)
                    {
                        Parent = this;
                        return true;
                    }
                    Simulator simu = GetSimulator();
                    if (simu == null) return false;
                    EnsureParentRequested(simu);
                    Primitive pUse = WorldSystem.GetPrimitive(Prim.ParentID, simu);
                    if (pUse == null)
                    {
                        return false;
                    }
                    if (_Parent == null)
                    {
                        if (pUse.ID != UUID.Zero)
                            Parent = WorldSystem.GetSimObject(pUse);
                        return false;
                    }
                }
                lock (HasPrimLock)
                {
                    return _Parent == this || (_Parent != null && _Parent.IsRegionAttached);
                }
            }
        }

        public virtual Simulator GetSimulator()
        {
            if (RegionHandle == 0)
            {
                return null;
            }
            return WorldSystem.GetSimulator(RegionHandle);
            //return GetSimRegion().TheSimulator;
        }


        public virtual Vector3 GetSimScale()
        {
            return Prim.Scale; // the scale is all in the prim w/o parents? 
        }


        public virtual Quaternion SimRotation
        {
            get
            {

                Primitive thisPrim = this.Prim;
                if (thisPrim==null)
                {
                    Error("GetSimRotation Prim==null: " + this);
                    return Quaternion.Identity;
                }
                Quaternion transValue = thisPrim.Rotation;
                if (!IsRegionAttached)
                {
                    WorldSystem.ReSelectObject(thisPrim);
                    if (thisPrim.ParentID == 0)
                    {
                        return transValue;
                    }
                    //WorldSystem.RequestMissingObject(Prim.LocalID, WorldSystem.GetSimulator(RegionHandle));
                    //WorldSystem.client.Objects.RequestObject(WorldSystem.GetSimulator(RegionHandle), Prim.LocalID);
                }
                if (thisPrim.ParentID != 0)
                {
                    Primitive outerPrim = GetParentPrim(thisPrim, Debug);
                    if (outerPrim == null)
                    {
                        //TODO no biggy here ?
                        Error("GetSimRotation !IsRegionAttached: " + this);
                        return transValue;
                    }
                    transValue = outerPrim.Rotation * transValue;
                    thisPrim = outerPrim;
                    //  transValue.Normalize();
                }
                return transValue;
            }
        }

        public Vector3 LastKnownSimPos;
        public virtual bool TryGetGlobalPosition(out Vector3d pos)
        {
            return TryGetGlobalPosition(out pos, null);
        }

        public void UpdatePosition(ulong handle, Vector3 pos)
        {
            RegionHandle = handle;
            SimPosition = pos;
        }

        public virtual bool TryGetGlobalPosition(out Vector3d pos, OutputDelegate Debug)
        {
            Vector3 local;
            if (TryGetSimPosition(out local, Debug))
            {
                if (RegionHandle != 0)
                {
                    pos = ToGlobal(RegionHandle, local);
                    return true;
                }
                pos = default(Vector3d);
                return false;
            }
            pos = default(Vector3d);
            return false;
        }

        public virtual bool TryGetSimPosition(out Vector3 pos)
        {
            return TryGetSimPosition(out pos, null);
        }

        public virtual bool TryGetSimPosition(out Vector3 pos, OutputDelegate Debug)
        {
            lock (HasPrimLock)
            {
                {
                    pos = this.LastKnownSimPos;
                    Primitive thisPrim = this.Prim;
                    if (!HasPrim || thisPrim == null)
                    {
                        return (default(Vector3) != pos);
                    }
                    if (thisPrim.ParentID == 0)
                    {
                        pos = thisPrim.Position;
                        return true;
                    }
                    if (RequestedParent && _Parent != null)
                    {
                        var _ParentPrim = _Parent.Prim;
                        if (_ParentPrim != null && _ParentPrim != thisPrim)
                        {
                            pos = LastKnownSimPos = GetPosAfterParent(_ParentPrim, thisPrim.Position);
                        }
                        return true;
                    }

                    Vector3 thisPos = thisPrim.Position;
                    while (thisPrim.ParentID != 0)
                    {
                        Primitive outerPrim = GetParentPrim(thisPrim, Debug);

                        if (outerPrim == null)
                        {
                            if (pos != default(Vector3)) return true;
                            if (Debug != null) Debug("Unknown parent");
                            return false;
                        }
                        if (outerPrim == thisPrim || outerPrim == this._Prim0 || RequestedParent)
                        {
                            return false;
                        }

                        if (outerPrim.Position == default(Vector3))
                        {
                            if (Debug != null) Debug("parent pos==Zero");
                            return false;
                        }
                        thisPos = GetPosAfterParent(outerPrim, thisPos);
                        thisPrim = outerPrim;
                    }
                    if (false && BadLocation(thisPos))
                    {
                        if (Debug != null) Debug("-------------------------" + this + " shouldnt be at " + thisPos);
                        //   WorldSystem.DeletePrim(thePrim);
                    }
                    LastKnownSimPos = thisPos;
                    return true;
                }
            }
        }

        public static Vector3 GetPosAfterParent(Primitive parentPrim, Vector3 thisPos)
        {
            thisPos = parentPrim.Position +
                      Vector3.Transform(thisPos, Matrix4.CreateFromQuaternion(parentPrim.Rotation));
            return thisPos;
        }

        public virtual Vector3 SimPosition
        {
            get
            {
                Vector3 pos;
                if (TryGetSimPosition(out pos, Debug))
                {
                    return pos;
                }
                lock (HasPrimLock)
                {
                    if (LastKnownSimPos != default(Vector3)) return LastKnownSimPos;
                    if (RequestedParent && _Parent != null)
                    {
                        var thisPrim = this._Prim0;
                        var _ParentPrim = _Parent.Prim;
                        if (_ParentPrim != null && _ParentPrim != thisPrim)
                        {
                            LastKnownSimPos = GetPosAfterParent(_ParentPrim, thisPrim.Position);
                        }
                        return LastKnownSimPos;
                    }
                }
                if (IsRegionAttached) throw Error("GetSimPosition !IsRegionAttached: " + this);
                return LastKnownSimPos;
            }
            set
            {
                if (false)
                {
                    Vector3 current = SimPosition;
                    if (current != value)
                    {
                        SetObjectPosition(value);
                    }
                }
                LastKnownSimPos = value;
            }
        }
        protected Primitive GetParentPrim(Primitive thisPrim, OutputDelegate Debug)
        {
            lock (HasPrimLock) return GetParentPrim0(thisPrim, Debug);
        }
        protected Primitive GetParentPrim0(Primitive thisPrim, OutputDelegate Debug)
        {
            if (RequestedParent && thisPrim == _Prim0 && _Parent != null)
            {
                return _Parent.Prim;
            }
            if (IsKilled) return null;
            if (thisPrim.ParentID == 0)
            {
                return WorldSystem.GetSimObject(thisPrim).Prim;
            }
            int requests = 10;
            Primitive outerPrim = null;
            while (outerPrim == null && requests-- > 0)
            {
                if (thisPrim == Prim && _Parent != null)
                {
                    return _Parent.Prim;
                }
                uint theLPrimParentID = thisPrim.ParentID;
                if (theLPrimParentID == 0 || requests-- < 1)
                {
                    if (!RequestedParent) if (Debug != null) Debug("Why are not we getting a parent prim?");
                    return null;
                }
                Simulator simu = GetSimulator();
                outerPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                if (outerPrim == null)
                {
                    if (IsKilled) return null;                    
                    if (!RequestedParent)
                    {
                        EnsureParentRequested(simu);
                        if (Debug != null)
                        {
                            Debug("Probing for parent");
                            Thread.Sleep(500);
                        }
                    }
                }
                requests--;
            }
            return outerPrim;
        }


        protected void EnsureParentRequested(Simulator simu)
        {
            if (!RequestedParent)
            {
                RequestedParent = true;
                if (_Parent != null) return;
                Primitive Prim = this.Prim;
                if (Prim == null)
                {
                    RequestedParent = false;
                    return;
                }
                uint theLPrimParentID = Prim.ParentID;
                if (theLPrimParentID == 0 || _Parent != null) return;
                Primitive outerPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                if (outerPrim != null && outerPrim.ID != UUID.Zero)
                {
                    Parent = WorldSystem.GetSimObject(outerPrim, simu);
                    return;
                }
                WorldObjects.EnsureRequested(simu, theLPrimParentID);
                WorldObjects.EnsureSelected(theLPrimParentID, simu);
                ParentGrabber.AddFirst(() => TaskGetParent(theLPrimParentID, simu));
            }
        }

        private void TaskGetParent(uint theLPrimParentID, Simulator simu)
        {
            if (IsKilled) return;
            if (theLPrimParentID == 0 || _Parent != null) return;

            Primitive outerPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
            if (outerPrim != null && outerPrim.ID != UUID.Zero)
            {
                Parent = WorldSystem.GetSimObject(outerPrim);
            }
            else
            {
                if (ParentGrabber.NoQueue) return;
                ParentGrabber.Enqueue(() => TaskGetParent(theLPrimParentID, simu));
                // missing parent still?!
                IsDebugging = true;
            }
        }

        protected TaskQueueHandler ParentGrabber
        {
            get { return WorldObjects.ParentGrabber; }
        }


        public bool BadLocation(Vector3 transValue)
        {
            if (transValue.Z < -2.0f) return true;
            if (transValue.X < 0.0f) return true;
            if (transValue.X > 256.0f) return true;
            if (transValue.Y < 0.0f) return true;
            if (transValue.Y > 256.0f) return true;
            return false;
        }


        public BotNeeds GetActualUpdate(string pUse)
        {
            if (needUpdate)
            {
                UpdateProperties(_propertiesCache);
            }
            return ObjectType.GetUsageActual(pUse).Magnify(scaleOnNeeds);
        }


        public SimTypeUsage GetBestUse(BotNeeds needs)
        {
            if (needUpdate)
            {
                if (_propertiesCache != null) UpdateProperties(_propertiesCache);
                else
                {

                    if (RegionHandle != 0 && _Prim0 != null)
                    {
                        WorldObjects.EnsureRequested(GetSimulator(), LocalID);
                    }
                }
            }

            IList<SimTypeUsage> all = ObjectType.GetTypeUsages();
            if (all.Count == 0) return null;
            SimTypeUsage typeUsage = all[0];
            double typeUsageRating = 0.0f;
            foreach (SimTypeUsage use in all)
            {
                double f = ObjectType.RateIt(needs, use);
                if (f > typeUsageRating)
                {
                    typeUsageRating = f;
                    typeUsage = use;
                }
            }
            return typeUsage;
        }

        //public Vector3 GetUsePosition()
        //{
        //    return GetSimRegion().GetUsePositionOf(GetSimPosition(),GetSizeDistance());
        //}

        public BotNeeds GetProposedUpdate(string pUse)
        {
            return ObjectType.GetUsagePromise(pUse).Magnify(scaleOnNeeds);
        }

        private float cachedSize = 0f;
        /// <summary>
        ///  Gets the distance a ISimAvatar may be from ISimObject to use
        /// </summary>
        /// <returns>1-255</returns>
        public virtual float GetSizeDistance()
        {
            if (IsAvatar) return 2f;
            if (cachedSize > 0) return cachedSize;
            double size = Math.Sqrt(BottemArea()) / 2;

            //            if (IsPhantom) return size;

            double fx; // = thePrim.Scale.X;
            //if (fx > size) size = fx;
            double fy; // = thePrim.Scale.Y;
            //if (fy > size) size = fy;

            foreach (SimObject obj in Children)
            {
                Primitive cp = obj.Prim;
                if (cp==null) continue;
                fx = cp.Scale.X;
                if (fx > size) size = fx;
                fy = cp.Scale.Y;
                if (fy > size) size = fy;
                double childSize = obj.GetSizeDistance();
                if (childSize > size) size = childSize;
            }
            return cachedSize = (float) size;
        }

        public virtual List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly)
        {
            if (!IsRegionAttached)
            {
                List<SimObject> objs = new List<SimObject>();
                var Parent = this.Parent;
                if (Parent != null && Parent != this)
                {
                    objs.Add(Parent);
                }
                return objs;
            }
            List<SimObject> objs2 = WorldSystem.GetNearByObjects(GlobalPosition, this, (float)maxDistance, rootOnly);
            SortByDistance(objs2);
            return objs2;
        }

        public virtual Vector3d GlobalPosition
        {
            get {
                return ToGlobal(RegionHandle, SimPosition);
            }
        }

        protected Vector3d ToGlobal(ulong regionHandle, Vector3 objectLoc)
        {
            if (regionHandle==0) Error("regionHandle = NULL");
            uint regionX = 0, regionY = 0;
            Utils.LongToUInts(regionHandle, out regionX, out regionY);
            return new Vector3d(regionX + objectLoc.X, regionY + objectLoc.Y, objectLoc.Z);
        }

        //static ListAsSet<ISimObject> CopyObjects(List<ISimObject> objects)
        //{
        //    ListAsSet<ISimObject> KnowsAboutList = new ListAsSet<ISimObject>();
        //    lock (objects) foreach (ISimObject obj in objects)
        //        {
        //            KnowsAboutList.Add(obj);
        //        }
        //    return KnowsAboutList;
        //}


        public virtual bool Matches(string name)
        {
            string toString1 = ToString();
            return SimTypeSystem.MatchString(toString1, name);
        }

        public virtual bool HasFlag(object name)
        {
            if (name == null) return PrimFlags.None == Prim.Flags;
            if (name is PrimFlags) return (Prim.Flags & (PrimFlags)name) != 0;
            return (" " + Prim.Flags.ToString().ToLower() + " ").Contains(" " + name.ToString().ToLower() + " ");
        }

        public virtual void Debug(string p, params object[] args)
        {
            string str = DLRConsole.SafeFormat(p, args) + " -'" + GetName() + "'-";
            WorldSystem.WriteLine(str);
        }

        public Exception Error(string p, params object[] args)
        {
            string str = String.Format(p, args);
            Debug(str);
            return new ArgumentException(str);
        }

        public void SortByDistance(List<SimObject> sortme)
        {
            lock (sortme) sortme.Sort(CompareDistance);
        }

        public int CompareDistance(SimObject p1, SimObject p2)
        {
            if (p1 == p2) return 0;
            return (int)(Distance(p1) - Distance(p2));
        }

        static int CompareSize(SimObject p1, SimObject p2)
        {
            return (int)(p1.GetSizeDistance() * p1.GetCubicMeters() - p2.GetSizeDistance() * p2.GetCubicMeters());
        }

        public int CompareDistance(Vector3d v1, Vector3d v2)
        {
            Vector3d rp = GlobalPosition;
            return (int)(Vector3d.Distance(rp, v1) - Vector3d.Distance(rp, v2));
        }

        public string DistanceVectorString(SimPosition obj)
        {
            if (!obj.IsRegionAttached)
            {
                Vector3 loc;
                loc = obj.SimPosition;
                SimPathStore R = obj.PathStore;
                return String.Format("unknown relative {0}/{1:0.00}/{2:0.00}/{3:0.00}",
                                     R != null ? R.RegionName : "NULL", loc.X, loc.Y, loc.Z);
            }
            if (!IsRegionAttached) return obj.DistanceVectorString(this);
            return DistanceVectorString(obj.GlobalPosition);
        }

        public string DistanceVectorString(Vector3d loc3d)
        {
            Vector3 loc = SimPathStore.GlobalToLocal(loc3d);
            SimPathStore R = SimPathStore.GetPathStore(loc3d);
            return String.Format("{0:0.00}m ", Vector3d.Distance(GlobalPosition, loc3d))
                   + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public string DistanceVectorString(Vector3 loc)
        {
            SimRegion R = GetSimRegion();
            return String.Format("{0:0.00}m ", Vector3.Distance(SimPosition, loc))
                   + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public virtual string GetName()
        {
            if (_propertiesCache != null)
            {
                String s = _propertiesCache.Name;
                if (s.Length > 8) return s;
                s += " | " + _propertiesCache.Description;
                if (s.Length > 12) return s;
            }
            lock (HasPrimLock)
                if (!HasPrim) return ToString() + " " + RegionHandle;
            return ToString();
        }

        private SimMesh _Mesh;

        public SimMesh Mesh
        {
            get
            {
                if (_Mesh == null)
                {
                    _Mesh = new SimMesh(this, Prim, PathStore);
                }
                return _Mesh;
            }
            // set { _Mesh = value; }
        }


        public static int CompareLowestZ(SimObject p1, SimObject p2)
        {
            Box3Fill b1 = p1.OuterBox;
            Box3Fill b2 = p2.OuterBox;
            if (b1 == b2) return 0;
            // One is fully above the other
            if (b1.MaxZ < b2.MinZ) return -1;
            if (b2.MaxZ < b1.MinZ) return 1;
            // One is partially above the other
            if (b1.MaxZ < b2.MaxZ) return -1;
            if (b2.MaxZ < b1.MaxZ) return 1;
            // they are the same hieght (basically) so compare bottems
            return (int)(b1.MinZ - b2.MinZ);
        }

        public float BottemArea()
        {
            if (_Mesh != null)
            {
                if (OuterBox.MaxX != float.MinValue)
                {
                    float bottemX = OuterBox.MaxX - OuterBox.MinX;
                    float bottemY = OuterBox.MaxY - OuterBox.MinY;
                    return bottemX * bottemY;
                }

            }
            if (!HasPrim) return 1;
            return Prim.Scale.X * Prim.Scale.Y;
        }

        //public SimRegion _CurrentRegion;

        public virtual SimRegion GetSimRegion()
        {
            lock (HasPrimLock)
            {
                Primitive Prim = this.Prim;
                while (RegionHandle == 0 && !ReferenceEquals(Prim, null))
                {
                    RegionHandle = Prim.RegionHandle;
                }
                if (RegionHandle == 0)
                {
                    return null;
                }
                return WorldSystem.GetRegion(RegionHandle);
            }
        }

        public Vector3d GetGlobalLeftPos(int angle, double Dist)
        {
            return SimRegion.GetGlobalLeftPos(this, angle, Dist);
        }

        #region SimPosition Members

        //public SimWaypoint GetWaypoint()
        //{
        //    return GetSimRegion().CreateClosestRegionWaypoint(GetSimPosition(),2);
        //}

        #endregion

        //public string ToMeshString()
        //{
        //    if (_Mesh != null)
        //    {
        //       return _Mesh.ToString();
        //    }
        //    return ToString();
        //}


        public bool IsInside(Vector3 L)
        {
            return Mesh.IsInside(L.X, L.Y, L.Z);
        }


        public virtual bool OpenNearbyClosedPassages()
        {
            bool changed = false;
            SimObjectType DOOR = SimTypeSystem.DOOR;
            // look for closed doors

            List<SimObject> UnEnterables = new List<SimObject>();
            foreach (SimObject O in GetNearByObjects(3, false))
            {
                if (O.UpdateOccupied()) changed = true;
                if (!O.IsPhantom)
                {
                    if (O.IsTypeOf(DOOR) != null)
                    {
                        O.MakeEnterable(this);
                        UnEnterables.Add(O);
                        continue;
                    }
                    if (O.IsSculpted)
                    {
                        O.MakeEnterable(this);
                        UnEnterables.Add(O);
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
                                   foreach (SimObject O in UnEnterables)
                                   {
                                       O.RestoreEnterable(this);
                                   }
                               }).Start();
            }
            return changed;
        }


        public virtual void AddCanBeTargetOf(int ArgN, SimObjectEvent evt)
        {
            if (ArgN == 1)
            {
                SimObjectType simTypeSystemCreateObjectUse = SimTypeSystem.CreateObjectType(evt.Verb);
                SimTypeUsage usage = simTypeSystemCreateObjectUse.CreateObjectUsage(evt.Verb);
                if (evt.EventType == SimEventType.SIT)
                {
                    usage.UseSit = true;
                }
                if (evt.EventType == SimEventType.TOUCH)
                {
                    usage.UseGrab = true;
                }
                if (evt.EventType == SimEventType.ANIM)
                {
                    usage.UseAnim = evt.Verb;
                }
                if (evt.EventType == SimEventType.EFFECT)
                {
                    //todo need to parse the EffectType
                    usage.UseGrab = true;
                    //   usage.UseAnim = evt.Verb;
                }
                ObjectType.AddSuperType(simTypeSystemCreateObjectUse);
            }
        }

        public static int MaxEventSize = 10; // Keeps only last 9 events
        public Queue<SimObjectEvent> ActionEventQueue { get; set; }
        public SimObjectEvent lastEvent = null;

        public readonly Dictionary<string, SimObjectEvent> LastEventByName = new Dictionary<string, SimObjectEvent>();


        public bool ShouldEventSource
        {
            get { return WorldSystem.UseEventSource(this); }
        }

        /// <summary>
        /// Returns false if the event has gone unsent
        /// </summary>
        /// <param name="SE"></param>
        /// <returns></returns>
        public virtual bool LogEvent(SimObjectEvent SE)
        {
            // string eventName = SE.Verb;
            object[] args1_N = SE.GetArgs();
            bool saveevent = true;
            object[] args0_N = PushFrontOfArray(ref args1_N, this);
            lock (ActionEventQueue)
            {
                int ActionEventQueueCount = ActionEventQueue.Count;
                if (ActionEventQueueCount > 0)
                {
                    if (lastEvent != null)
                    {
                        if (lastEvent.SameAs(SE))
                        {
                            saveevent = false;
                            return false;
                        }
                        //else if (false)
                        //{
                        //    SimObjectEvent newEvt = lastEvent.CombinesWith(SE);
                        //    if (newEvt != null)
                        //    {
                        //        lastEvent.EventStatus = newEvt.EventStatus;
                        //        lastEvent.Parameters = newEvt.Parameters;
                        //        saveevent = false;
                        //        SE = lastEvent;
                        //    }
                        //}
                    }
                    if (saveevent && ActionEventQueueCount >= MaxEventSize) ActionEventQueue.Dequeue();
                }
                lastEvent = SE;
                if (saveevent)
                {
                    ActionEventQueue.Enqueue(SE);
                }
                if (ShouldEventSource)
                {                    
                    WorldSystem.SendPipelineEvent(SE);
                    saveevent = true;
                } else
                {
                    saveevent = false;
                }
                LastEventByName[SE.EventName] = SE;
            }
            for (int argN = 1; argN < args0_N.Length; argN++)
            {
                object o = args0_N[argN];
                if (o is SimObject)
                {
                    SimObject newSit = (SimObject)o;
                    newSit.AddCanBeTargetOf(argN, SE);
                }
            }
            return saveevent;
        }

        public static object[] RestOfArray(object[] args, int p)
        {
            if (args == null) return null;
            int len = args.Length;
            Type t = args.GetType().GetElementType();
            int newLen = len - p;
            if (newLen <= 0)
            {
                return (object[])Array.CreateInstance(t, 0);
            }
            object[] o = (object[])Array.CreateInstance(t, newLen);
            Array.Copy(args, p, o, 0, newLen);
            return o;
        }

        public static object[] PushFrontOfArray(ref object[] args, object p)
        {
            if (args == null) return null;
            int len = args.Length;
            Type t = args.GetType().GetElementType();
            int newLen = len + 1;
            object[] o = (object[])Array.CreateInstance(t, newLen);
            Array.Copy(args, 0, o, 1, len);
            o[0] = p;
            return o;
        }


        public string GetSimVerb()
        {
            Primitive Prim = this.Prim;
            string sn;
            OpenMetaverse.Primitive.ObjectProperties PrimProperties = _propertiesCache;
            if (PrimProperties != null)
            {
                sn = PrimProperties.TouchName;
                if (!String.IsNullOrEmpty(sn)) return sn;
                sn = PrimProperties.SitName;
                if (!String.IsNullOrEmpty(sn)) return sn;
            }
            sn = ObjectType.GetTouchName();
            if (!String.IsNullOrEmpty(sn)) return sn;
            sn = ObjectType.GetSitName();
            if (!String.IsNullOrEmpty(sn)) return sn;
            return null;
        }


        public string SitName
        {
            get
            {
                string sn = null;
                if (_propertiesCache != null)
                    sn = _propertiesCache.SitName;
                if (!String.IsNullOrEmpty(sn)) return sn;
                sn = ObjectType.GetSitName();
                if (!String.IsNullOrEmpty(sn)) return sn;
                return "SitOnObject";
            }
        }

        public string TouchName
        {
            get
            {
                string sn = null;
                if (_propertiesCache != null)
                    sn = _propertiesCache.TouchName;
                if (!String.IsNullOrEmpty(sn)) return sn;
                sn = ObjectType.GetTouchName();
                if (!String.IsNullOrEmpty(sn)) return sn;
                return "TouchTheObject";
            }
        }


        private bool _isChild;

        public bool IsAttachment
        {
            get { return _Parent is SimAvatar; }
        }

        public AttachmentPoint AttachPoint
        {
            get
            {
                Primitive Prim = this.Prim;
                if (Prim == null) return AttachmentPoint.Default;
                return Prim.PrimData.AttachmentPoint;
            }
        }

        public bool IsAttachable
        {
            get
            {
                return AttachPoint != AttachmentPoint.Default;
            }
        }

        public bool IsChild
        {
            get { return _isChild || _Parent is SimAvatar; }
            set { _isChild = value; }
        }

        private readonly ListAsSet<UUID> CurrentSounds = new ListAsSet<UUID>();
        public static float SoundGainThreshold = 0.1f;

        public void OnSound(UUID soundID, float gain)
        {
            if (soundID == UUID.Zero)
            {
                if (gain < SoundGainThreshold)
                {
                    CurrentSounds.Clear();
                    if (IsDebugging) Debug("Clearing all sounds");
                }
                else
                {
                    Debug("Gain change for unknown sound: " + gain);
                }
            }
            else
            {
                if (gain < SoundGainThreshold)
                {
                    CurrentSounds.Remove(soundID);
                }
                else
                {
                    CurrentSounds.AddTo(soundID);
                }
            }
        }


        public virtual bool OnEffect(string effectType, object t, object p, float duration, UUID id)
        {
            SimObjectEvent newSimObjectEvent = new SimObjectEvent(SimEventStatus.Once, effectType, SimEventType.EFFECT, SimEventClass.REGIONAL,
                                                                    WorldObjects.ToParameter("doneBy", this),
                                                                    WorldObjects.ToParameter("objectActedOn", t),
                                                                    WorldObjects.ToParameter("eventPartiallyOccursAt", p),
                                                                    WorldObjects.ToParameter("simDuration", duration),
                                                                    WorldObjects.AsEffectID(id));
            bool noteable = LogEvent(newSimObjectEvent);
            if (!noteable)
            {
                if (WorldSystem.UseEventSource(t) || ShouldEventSource)
                {
                    WorldSystem.SendPipelineEvent(newSimObjectEvent);
                }
            }
            //todo
            // LogEvent will send noteables already if (noteable) WorldSystem.SendPipelineEvent(newSimObjectEvent);
            return noteable;
        }

        private bool IsSolidCachedKnown, IsSolidCachedTrue;

        public bool IsSolid
        {
            get
            {
                if (MadePhantom) return true; // since we "changed" it
                if (!IsSolidCachedKnown)
                {
                    IsSolidCachedKnown = true;
                    IsSolidCachedTrue = !(IsPhantom || IsTypeOf(SimTypeSystem.PASSABLE) != null);
                }
                return IsSolidCachedTrue;
            }
            set
            {
                IsSolidCachedKnown = true;
                IsSolidCachedTrue = value;
            }
        }

        private bool IsUseableCachedKnown, IsUseableCachedTrue;

        public bool IsUseable
        {
            get
            {
                if (!IsUseableCachedKnown)
                {
                    IsUseable = IsSitDefined || IsSitDefined || IsTypeOf(SimTypeSystem.USEABLE) != null;
                }
                return IsUseableCachedTrue;
            }

            set
            {
                IsUseableCachedKnown = true;
                IsUseableCachedTrue = value;
            }
        }

        virtual public bool HasPrim
        {
            get { return !ReferenceEquals(_Prim0, null); }
        }
        public readonly object HasPrimLock = new object();

        internal Color DebugColor()
        {
            if (IsUseable) return Color.Green;
            return Color.Empty;
        }

        #region SimObject Members

        readonly Dictionary<string, object> dict = new Dictionary<string, object>();
        protected bool toStringNeedsUpdate = true;
        private bool wasMeshUpdated;
        public bool IsDebugging { get; set; }

        public bool IsMeshed { get; set; }

        public object this[string s]
        {
            get
            {
                if (!dict.ContainsKey(s))
                {
                    return null;
                }
                return dict[s];
            }
            set
            {
                dict[s] = value;
            }
        }

        #endregion
    }

    public interface SimObject : SimPosition, BotMentalAspect, SimMover
    {
        object this[String s] { get; set; }
        bool AddChild(SimObject simObject);
        void AddSuperTypes(IList<SimObjectType> listAsSet);
        bool BadLocation(Vector3 transValue);
        float BottemArea();
        int CompareDistance(SimObject p1, SimObject p2);
        int CompareDistance(Vector3d v1, Vector3d v2);
        string DebugInfo();
        double Distance(SimPosition prim);
        string DistanceVectorString(Vector3 loc);
        string DistanceVectorString(Vector3d loc3d);
        //inherited from SimPosition: string DistanceVectorString(SimPosition obj);
        Exception Error(string p, params object[] args);
        bool FollowPathTo(SimPosition globalEnd, double distance);
        BotNeeds GetActualUpdate(string pUse);
        SimTypeUsage GetBestUse(BotNeeds needs);
        ListAsSet<SimObject> Children { get; }
        Vector3d GetGlobalLeftPos(int angle, double Dist);
        List<string> GetMenu(SimAvatar avatar);
        string GetName();
        BotNeeds GetProposedUpdate(string pUse);
        Vector3 GetSimScale();
        Simulator GetSimulator();
        IList<SimTypeUsage> GetTypeUsages();
        List<SimObjectUsage> GetUsages();
        bool GotoTarget(SimPosition pos);
        bool Flying { get; set; }
        bool IsInside(Vector3 L);
        bool IsKilled { set; }
        bool IsControllable { get; }
        //inherited from SimPosition: bool IsPassable { get; set; }
        bool IsPhantom { get; set; }
        bool IsPhysical { get; set; }
        bool IsAttachment { get; }
        bool IsAttachable { get; }
        AttachmentPoint AttachPoint { get; }
        bool IsChild { get; set; }
        bool IsRoot { get; }
        bool IsUseable { get; }
        bool IsSculpted { get; }
        bool IsSitDefined { get; }
        bool IsTouchDefined { get; }
        Primitive Prim { get; }
        Box3Fill OuterBox { get; }
        bool IsTyped { get; }
        SimObjectType IsTypeOf(SimObjectType superType);
        bool MakeEnterable(SimMover actor);
        bool Matches(string name);
        SimMesh Mesh { get; }
        SimObject Parent { get; set; }
        double RateIt(BotNeeds needs);
        void ResetPrim(Primitive prim, BotClient bc, Simulator sim);
        void ResetRegion(ulong regionHandle);
        bool RestoreEnterable(SimMover actor);
        void SendUpdate(int ms);
        void SetMoveTarget(SimPosition target, double maxDist);
        bool SetObjectPosition(Vector3 localPos);
        bool SetObjectPosition(Vector3d globalPos);
        bool SetObjectRotation(Quaternion localPos);
        string SitName { get; }
        SimObjectType ObjectType { get; }
        ulong RegionHandle { get; }
        void SortByDistance(List<SimObject> sortme);
        string SuperTypeString();
        bool TeleportTo(SimRegion R, Vector3 local);
        //inherited from Object string ToString();
        bool TurnToward(SimPosition targetPosition);
        bool TurnToward(Vector3 target);
        void UpdateObject(ObjectMovementUpdate objectUpdate, ObjectMovementUpdate objectUpdateDiff);

        //void UpdateProperties(Primitive.ObjectProperties props);

        bool UpdateOccupied();

        void Touch(SimObject simObjectImpl);

        // void AddPossibleAction(string textualActionName, params object[] args);

        void AddCanBeTargetOf(int argN, SimObjectEvent evt);

        bool LogEvent(SimObjectEvent evt);

        void OnSound(UUID soundID, float gain);

        bool OnEffect(string effectType, object t, object p, float duration, UUID id);

        SimObject GetGroupLeader();

        float GetCubicMeters();

        List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly);

        void RemoveCollisions();

        void SetFirstPrim(Primitive primitive);
        UUID ID { get; }
        Primitive.ObjectProperties Properties { get; set; }
        bool HasPrim { get; }
        uint LocalID { get; }
        uint ParentID { get;}
        bool IsDebugging { get; set; }
        bool ConfirmedObject { get; set; }
        bool ShouldEventSource { get; }
        bool KilledPrim(Primitive primitive, Simulator simulator);

        ICollection<NamedParam> GetInfoMap();

        bool CanShoot(SimPosition position);
        //void SetInfoMap(string key,Type type, Object value);
        SimHeading GetHeading();
        bool TryGetGlobalPosition(out Vector3d pos);
        void UpdatePosition(ulong handle, Vector3 pos);
        Queue<SimObjectEvent> ActionEventQueue { get; set; }
    }
}