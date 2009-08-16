using System;
using System.Collections.Generic;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;
using cogbot.Listeners;
using cogbot.Utilities;
using OpenMetaverse;
using PathSystem3D.Mesher;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    //TheSims-like object
    public class SimObjectImpl : SimPosition, BotMentalAspect, SimMover, SimObject, MeshableObject
    {

        
        public Primitive.ObjectProperties Properties { get; set; }
        
        public BotMentalAspect GetObject(string name)
        {
            return WorldSystem.GetObject(name);
        }

        public ulong RegionHandle { get; set; }
        public UUID ID { get; set;}

        public float GetCubicMeters()
        {
            Vector3 v3 = GetSimScale();
            return v3.X*v3.Y*v3.Z;
        }

        public SimObject GetGroupLeader()
        {
            if (!IsRoot) return Parent.GetGroupLeader();
            List<SimObject> e = GetNearByObjects(GetSizeDistance()+1, false);
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
            double currentDist = Vector3d.Distance(finalTarget, GetWorldPosition());
            if (currentDist < maxDistance) return true;
            {
                SimWaypoint P = SimWaypointImpl.CreateGlobal(finalTarget);
                SetMoveTarget(P, (float) maxDistance);
            }
            for (int i = 0; i < maxSeconds; i++)
            {
                Application.DoEvents();
                currentDist = Vector3d.Distance(finalTarget, GetWorldPosition());
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

        public SimPathStore GetPathStore()
        {
            return GetSimRegion().GetPathStore(GetSimPosition());
        }

        public virtual void TurnToward(SimPosition targetPosition)
        {
            TurnToward(targetPosition.GetWorldPosition());
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
            Vector3d finalPos = target.GetWorldPosition();
            Vector3d start = GetWorldPosition();
            Vector3d offset = finalPos - start;
            double points = offset.Length();
            Vector3d offsetEach = offset/points;
            while (points > 1)
            {
                points -= 1;
                start += offsetEach;
                SetObjectPosition(start);
            }
            SetObjectPosition(finalPos);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pos"></param>
        /// <returns></returns>
        public virtual bool GotoTarget(SimPosition pos)
        {
            if (!IsControllable)
            {
                throw Error("GotoTarget !IsControllable");
            }

            float maxDist = pos.GetSizeDistance();
            for (int i = 0; i < 8; i++)
            {
                bool result = FollowPathTo(pos, maxDist);
                if (result)
                {
                    SetMoveTarget(pos,maxDist);
                    return true;
                }
            }
            return FollowPathTo(pos, maxDist);
        }

        public virtual bool IsControllable
        {
            get
            {
                if (!IsRoot) return false;
                return true; // WorldSystem.client.Network.CurrentSim == GetSimRegion().TheSimulator;
            }
        }

        public bool FollowPathTo(SimPosition globalEnd, double distance)
        {
            if (!IsControllable)
            {
                throw Error("FollowPathTo !IsControllable");
            }
            SimAbstractMover move = SimAbstractMover.CreateSimPathMover(this, globalEnd, distance);
            return move.Goto() == SimMoverState.COMPLETE;
        }


        public void TeleportTo(SimPosition local)
        {
            if (!IsControllable)
            {
                throw Error("GotoTarget !Client.Self.AgentID == Prim.ID");
            }
            TeleportTo(SimRegion.GetRegion(local.GetPathStore().RegionName), local.GetSimPosition());
        }

        public virtual void TeleportTo(SimRegion R, Vector3 local)
        {
            SetObjectPosition(R.LocalToGlobal(local));
        }

        public bool SetObjectPosition(Vector3d globalPos)
        {
            Vector3d start = GetWorldPosition();
            Vector3d offset = globalPos - start;
            Vector3 lPos = GetSimPosition();
            lPos.X += (float) offset.X;
            lPos.Y += (float) offset.Y;
            lPos.Z += (float) offset.Z;
            return SetObjectPosition(lPos);
        }

        public bool SetObjectPosition(Vector3 localPos)
        {
            if (!IsRoot)
            {
                Vector3 start = GetSimPosition();
                Vector3 offset = localPos - start;
                SimObject p = Parent;
                return p.SetObjectPosition(p.GetSimPosition() + offset);
            }
            WorldSystem.SetObjectPosition(Prim, localPos);
            return true;
        }

        #region SimMover Members

        public void TurnToward(Vector3d targetPosition)
        {
            Vector3d Current = GetWorldPosition();
            Vector3d diff = targetPosition - Current;
            while (diff.Length() > 1)
            {
                diff.X *= 0.75f;
                diff.Y *= 0.75f;
                diff.Z *= 0.75f;
            }
            Vector3 LocalPos = new Vector3(GetSimPosition());
            LocalPos.X += (float) diff.X;
            LocalPos.Y += (float) diff.Y;
            TurnToward(LocalPos);
        }

        #endregion

        public virtual bool TurnToward(Vector3 target)
        {
            Quaternion parentRot = Quaternion.Identity;

            if (!IsRoot)
            {
                parentRot = Parent.GetSimRotation();
            }

            Quaternion between = Vector3.RotationBetween(Vector3.UnitX, Vector3.Normalize(target - GetSimPosition()));
            Quaternion rot = between*(Quaternion.Identity/parentRot);

            SetObjectRotation(rot);
            return true;
        }

        public virtual bool SetObjectRotation(Quaternion localPos)
        {
            if (!IsRoot)
            {
                Quaternion start = GetSimRotation();
                Quaternion offset = localPos / start;
                SimObject p = Parent;
                return p.SetObjectRotation(p.GetSimRotation() * offset);
            }
            WorldSystem.SetObjectRotation(Prim, localPos);
            return true;
        }

        // protected SimRegion PathStore;
        public Box3Fill OuterBox
        {
            get { return _Mesh.OuterBox; }
        }

        public virtual bool KilledPrim(Primitive primitive, Simulator simulator)
        {

            lock (primRefs)
            {
                primRefs.Remove(primitive);
                IsKilled = primRefs.Count == 0;
                if (ReferenceEquals(_Prim0, primitive))
                {
                    _Prim0 = null;
                    RequestedParent = false;
                    _Parent = null;
                }
                return WasKilled;
            }
        }

        readonly List<NamedParam> infoMap = new List<NamedParam>();
        public List<NamedParam> GetInfoMap()
        {
            return infoMap;
        }

        public void SetInfoMap(string key, Type type, object value)
        {
            if (value == null) value = new NullType(type);
            lock (infoMap)
                infoMap.Add(new NamedParam(key, type, value));
        }

        readonly private List<Primitive> primRefs = new List<Primitive>();
        public virtual void ResetPrim(Primitive prim, BotClient bc, Simulator sim)
        {
            if (_Prim0==null)
            {
                SetFirstPrim(prim);
                return;
            }
            if (prim.Properties != null) Properties = prim.Properties;
            if (prim.RegionHandle != _Prim0.RegionHandle || !Object.ReferenceEquals(prim, _Prim0))
            {
                lock (primRefs)
                {
                    bool found = false;
                    foreach (Primitive av in primRefs)
                    {
                        if (Object.ReferenceEquals(av, prim))
                        {
                            found = true;
                        }
                    }
                    if (!found)
                    {
                        if (prim.ID!=ID)
                        {
                            Console.WriteLine("\n Different UUID! {0}", prim);                            
                        }
                        primRefs.Add(prim);
                        //Console.WriteLine("\n Different prims {0}", prim);
                    }
                }
                _Prim0 = prim;
                if (needUpdate)
                {
                    UpdateProperties(prim.Properties);
                }
                ResetRegion(prim.RegionHandle);
            }
            if (sim!=null) ResetRegion(sim.Handle);
        }

        public virtual void ResetRegion(ulong regionHandle)
        {
            RegionHandle = regionHandle;
            if (ReferenceEquals(_Prim0,null) || _Prim0.RegionHandle != regionHandle)
            {
                lock (primRefs)
                {
                    foreach (Primitive av in primRefs)
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
                if ((Prim.Flags & PrimFlags.Touch) != 0) return true;
                if (Properties != null)
                    return !String.IsNullOrEmpty(Properties.TouchName);
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
                if (Prim.ClickAction == ClickAction.Sit) return true;
                if (Properties != null)
                    return !String.IsNullOrEmpty(Properties.SitName);
                return false;
            }
        }

        public bool IsSculpted
        {
            get { return Prim.Sculpt != null; }
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
                if (!IsRoot && IsRegionAttached()) return Parent.IsPassable;
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
                if (IsRoot || true) return (Prim.Flags & PrimFlags.Phantom) == PrimFlags.Phantom;
                if (!IsRoot && IsRegionAttached()) return Parent.IsPhantom;
                if (Parent == null) return true;
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
                if (value)
                {
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags) (Prim.Flags | PrimFlags.Phantom));
                    MadePhantom = true;
                }
                else
                {
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags) (Prim.Flags - PrimFlags.Phantom));
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
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags) (Prim.Flags | PrimFlags.Physics));
                    MadeNonPhysical = false;
                }
                else
                {
                    WorldSystem.SetPrimFlags(Prim, (PrimFlags) (Prim.Flags - PrimFlags.Physics));
                    MadeNonPhysical = true;
                }
            }
        }

        //Vector3d lastPos;
        //SimWaypoint swp;
        //public virtual SimWaypoint GetWaypoint()
        //{
        //    Vector3d v3 = GetWorldPosition();

        //    if (swp == null || !swp.Passable)
        //    {
        //        SimRegion PathStore = GetSimRegion();
        //        swp = PathStore.CreateClosestWaypoint(v3);//, GetSizeDistance() + 1, 7, 1.0f);
        //        if (!swp.Passable)
        //        {
        //            double dist = Vector3d.Distance(v3, swp.GetWorldPosition());
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
        //            double dist = Vector3d.Distance(v3, swp.GetWorldPosition());
        //            WorldSystem.WriteLine("BAD: " + v3 + " <- " + dist + " -> " + swp + " " + this);
        //            swp = PathStore.ClosestNode(v3.X, v3.Y, v3.Y, out dist, false);//, GetSizeDistance() + 1, 7, 1.0f);
        //        }
        //    }
        //    return swp;
        //    //            return PathStore.CreateClosestWaypointBox(v3, 4f);
        //}


        public double Distance(SimPosition prim)
        {
            if (!prim.IsRegionAttached()) return 1300;
            if (!IsRegionAttached()) return 1300;
            return Vector3d.Distance(GetWorldPosition(), prim.GetWorldPosition());
        }

        // the prim in Secondlife
        protected Primitive _Prim0;

        public Primitive Prim
        {
            get
            {
                if (Object.ReferenceEquals(_Prim0,null))
                {
                    return null;
                }
                if (_Prim0.RegionHandle != RegionHandle)
                {
                    if (RegionHandle != 0)
                        ResetRegion(RegionHandle);
                }
                if (RegionHandle==0)
                {
                   RegionHandle = _Prim0.RegionHandle;
                }
                return _Prim0;
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
        protected bool WasKilled;

        public virtual bool IsKilled
        {
            // get { return WasKilled; }
            set
            {
                if (!WasKilled) //already
                {
                    List<SimObject> AttachedChildren0 = GetChildren();
                    lock (AttachedChildren0)
                        foreach (SimObject C in AttachedChildren0)
                        {
                            C.IsKilled = true;
                        }
                    RemoveCollisions();
                }
                WasKilled = value;
            }
        }

        public void RemoveCollisions()
        {
            if (_Mesh != null)
            {
                _Mesh.RemoveCollisions();
            }
            _Mesh = null;
        }

        public SimObjectType IsTypeOf(SimObjectType superType)
        {
            return ObjectType.IsSubType(superType);
        }

        public ListAsSet<SimObject> AttachedChildren = new ListAsSet<SimObject>();

        public ListAsSet<SimObject> GetChildren()
        {
            return AttachedChildren;
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
            ID = id;
            if (sim!=null) RegionHandle = sim.Handle;
            WorldSystem = objectSystem;
            ObjectType = SimTypeSystem.CreateInstanceType(id.ToString());
            //_CurrentRegion = SimRegion.GetRegion(sim);
            // PathStore = GetSimRegion();
            //WorldSystem.EnsureSelected(prim.ParentID,sim);
            // Parent; // at least request it
        }

        public virtual void SetFirstPrim(Primitive prim)
        {
            if (prim != null)
            {
                if (ID == UUID.Zero) ID = prim.ID;
                _Prim0 = prim;
                if (prim.Properties != null) Properties = prim.Properties;
                if (prim.RegionHandle != 0)
                {
                    RegionHandle = prim.RegionHandle;
                }
                lock (primRefs)
                {
                   if (!primRefs.Contains(prim))
                   {
                       primRefs.Add(prim);
                   }
                }

                UpdateProperties(prim.Properties);

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
                    uint parent = Prim.ParentID;
                    if (parent == 0)
                    {
                        _Parent = this;
                    }
                    else
                    {
                        Simulator simu = GetSimulator();
                        Primitive prim = WorldSystem.GetPrimitive(parent, simu);
                        if (prim == null)
                        {
                            // try to request for next time
                            EnsureParentRequested(simu);
                            return _Parent;
                        }
                        _Parent = WorldSystem.GetSimObject(prim, simu);
                        _Parent.AddChild(this);
                    }
                }
                return _Parent;
            }
        }

        public bool AddChild(SimObject simO)
        {
            SimObjectImpl simObject = (SimObjectImpl) simO;
            needUpdate = true;
            simObject._Parent = this;
            simObject.needUpdate = true;
            bool b = AttachedChildren.AddTo(simObject);
            if (b)
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
                if (Prim==null || Prim.ParentID == 0) return true;
                // _Parent = Parent;
                return false;
            }
        }

        public virtual string DebugInfo()
        {
            string str = ToString();
            if (Object.ReferenceEquals(_Prim0,null)) return str;
            if (Prim.ParentID != 0)
                return Prim.ParentID + " " + str;
            return str;
        }

        public double RateIt(BotNeeds needs)
        {
            return ObjectType.RateIt(needs, GetBestUse(needs))*scaleOnNeeds;
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
                UpdateProperties(Properties);
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
            if (Properties != null)
            {
                //  if (theProperties.TextName != "")
                list.Add("grab");
                //   if (theProperties.SitName != "")
                list.Add("sit");
                PermissionMask mask = Properties.Permissions.EveryoneMask;
                if (Prim.OwnerID == avatar.theAvatar.ID)
                {
                    mask = Properties.Permissions.OwnerMask;
                }
                PermissionMask result = mask | Properties.Permissions.BaseMask;
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

        public void UpdateProperties(Primitive.ObjectProperties objectProperties)
        {
            try
            {
                Primitive Prim = this.Prim;
                _TOSRTING = null;
                if (objectProperties != null)
                {
                    Properties = objectProperties;
                    if (Prim.Properties == null) Prim.Properties = objectProperties;

                    ObjectType.SitName = objectProperties.SitName;
                    ObjectType.TouchName = objectProperties.TouchName;
                    if (needUpdate)
                    {
                        needUpdate = false;
                        //  Parent;
                        if (Properties != null)
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
                        if (Properties != null)
                        {
                            SimTypeSystem.GuessSimObjectTypes(objectProperties, this);                        
                        } else
                        {
                            SimTypeSystem.GuessSimObjectTypes(objectProperties, this);                        
                        }
                    }
                    Properties = objectProperties;
                }
            }
            catch (Exception e)
            {
                Debug("" + e);
            }
        }

        public virtual bool IsFloating
        {
            get { return !IsPhysical; }
            set { IsPhysical = !value; }
        }

        public virtual void UpdateObject(ObjectUpdate objectUpdate, ObjectUpdate objectUpdateDiff)
        {
            if (needUpdate)
            {
                UpdateProperties(Properties);
            }
            UpdateOccupied();
            _TOSRTING = null;
        }

        public virtual bool UpdateOccupied()
        {
            if (!IsRegionAttached()) return false;
            if (!IsRoot)
            {
                // dont mesh armor
                if (Parent is SimAvatar)
                {
                    return false;
                }
            }
            return GetSimRegion().AddCollisions(Mesh);
            //throw new NotImplementedException();
        }

        public void AddSuperTypes(IList<SimObjectType> listAsSet)
        {
            _TOSRTING = null;
            //SimObjectType _UNKNOWN = SimObjectType._UNKNOWN;
            foreach (SimObjectType type in listAsSet)
            {
                ObjectType.AddSuperType(type);
            }
        }

        public virtual bool RestoreEnterable(SimMover actor)
        {
            bool changed = false;
            PrimFlags tempFlags = Prim.Flags;
            if (MadePhantom)
            {
                ((SimObject) actor).Touch(this);
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
                    ((SimObject) actor).Touch(this);
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
                ((SimObject) actor).Touch(this);
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
            String _TOSRTING = null;
            if (_TOSRTING == null)
            {
                if (Object.ReferenceEquals(_Prim0,null)) return "UNATTACHED_PRIM "+ID.ToString();
                Primitive Prim = this.Prim;
                _TOSRTING = "";
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
                if (Properties != null)
                {
                    if (!String.IsNullOrEmpty(Properties.Name))
                        _TOSRTING += String.Format("{0} ", Properties.Name);
                    if (!String.IsNullOrEmpty(Properties.Description))
                        _TOSRTING += String.Format(" | {0} ", Properties.Description);
                } else
                {
                    needUpdate = true;
                    Console.WriteLine("Reselecting prim " + Prim);
                    WorldSystem.ReSelectObject(Prim);
                }
                if (!String.IsNullOrEmpty(Prim.Text))
                    _TOSRTING += String.Format(" | {0} ", Prim.Text);
                uint ParentId = Prim.ParentID;
                if (ParentId != 0)
                {
                    _TOSRTING += "(parent ";

                    Primitive pp = null;
                    if (_Parent != null)
                    {
                        //if (_Parent.!ReferenceEquals(_Prim0,null))
                        pp = _Parent.Prim;
                    }
                    else
                    {
                        if (RegionHandle == 0) RegionHandle = Prim.RegionHandle;
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
                if (AttachedChildren.Count > 0)
                {
                    _TOSRTING += "(childs " + AttachedChildren.Count + ")";
                }
                else
                {
                    _TOSRTING += "(ch0)";
                }

                const PrimFlags AllPrimFlags = (PrimFlags)0xffffffff;
                const PrimFlags FlagsToPrintFalse =
                    PrimFlags.ObjectAnyOwner | PrimFlags.InventoryEmpty | PrimFlags.ObjectOwnerModify;
                const PrimFlags FlagsToPrintTrue = (PrimFlags) (AllPrimFlags - FlagsToPrintFalse);

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
                    _TOSRTING += "(SimVerb " + simverb + ")";
                if (!IsPassable)
                    _TOSRTING += "(!IsPassable)";
                if (Prim.PrimData.ProfileHollow > 0f)
                    _TOSRTING += String.Format("(hollow {0:0.00})", Prim.PrimData.ProfileHollow);
                if (WasKilled) _TOSRTING += "(IsKilled)";
                _TOSRTING = _TOSRTING.Replace("  ", " ").Replace(") (", ")(");
            }
            return _TOSRTING;
        }

        public string SuperTypeString()
        {
            String str = "[";
            lock (ObjectType.SuperType)
                ObjectType.SuperType.ForEach(delegate(SimObjectType item) { str += item.GetTypeName() + " "; });
            return str.TrimEnd() + "]";
        }


        bool RequestedParent = false;
        public bool IsRegionAttached()
        {
            if (WasKilled) return false;
            if (ReferenceEquals(_Prim0,null)) return false;
            if (_Prim0.RegionHandle == 0)
            {
                return false;
            }
            if (IsRoot) return true;
            if (_Parent == null)
            {
                if (Prim.ParentID==0)
                {
                    _Parent = this;
                    return true;
                }
                Simulator simu = GetSimulator();
                if (simu == null) return false;
                EnsureParentRequested(simu);
                return false;
                Primitive pUse = WorldSystem.GetPrimitive(Prim.ParentID, simu);
                if (pUse == null)
                {
                    return false;
                }
                if(_Parent==null)
                {
                    if (pUse.ID != UUID.Zero)
                        _Parent = WorldSystem.GetSimObject(pUse);
                }
            }
            if (_Parent == this)
            {
                return true;
            }
            return _Parent != null && _Parent.IsRegionAttached();
        }

        public virtual Simulator GetSimulator()
        {
            return GetSimRegion().TheSimulator;
        }


        public virtual Vector3 GetSimScale()
        {
           return Prim.Scale; // the scale is all in the prim w/o parents? 
        }


        public virtual Quaternion GetSimRotation()
        {
            Quaternion transValue = Prim.Rotation;
            Primitive thisPrim = Prim;
            if (!IsRegionAttached())
            {
                WorldSystem.ReSelectObject(Prim);
                if (Prim.ParentID==0)
                {
                    return transValue;
                }
                //WorldSystem.RequestMissingObject(Prim.LocalID, WorldSystem.GetSimulator(RegionHandle));
                //WorldSystem.client.Objects.RequestObject(WorldSystem.GetSimulator(RegionHandle), Prim.LocalID);
            }
            if (thisPrim.ParentID != 0)
            {
                Primitive outerPrim = GetParentPrim(thisPrim);
                if (outerPrim == null)
                {
                    //TODO no biggy here ?
                    Error("GetSimRotation !IsRegionAttached: " + this);
                    return transValue;
                }
                transValue = outerPrim.Rotation*transValue;
                thisPrim = outerPrim;
                //  transValue.Normalize();
            }
            return transValue;
        }

        protected Vector3 LastKnownPos;
        public virtual Vector3 GetSimPosition()
        {
            if (Object.ReferenceEquals(_Prim0,null)) return LastKnownPos;
            Primitive thisPrim = Prim;
            Vector3 thisPos = thisPrim.Position;
            //if (!IsRegionAttached()) return Prim.Position; 
            //throw Error("GetWorldPosition !IsRegionAttached: " + this);
            if (thisPrim.ParentID == 0)
            {
                return LastKnownPos = thisPos;
            }
            {
                Primitive outerPrim = GetParentPrim(thisPrim);

                if (outerPrim == null)
                {
                    if (LastKnownPos!=default(Vector3))return LastKnownPos;
                    Debug("Unknown parent");
                    throw Error("GetSimRotation !IsRegionAttached: " + this);
                    return thisPrim.Position;
                }

                thisPos = outerPrim.Position +
                          Vector3.Transform(thisPos, Matrix4.CreateFromQuaternion(outerPrim.Rotation));
                thisPrim = outerPrim;
            }
            if (false && BadLocation(thisPos))
            {
                Debug("-------------------------" + this + " shouldnt be at " + thisPos);
                //   WorldSystem.DeletePrim(thePrim);
            }
            return LastKnownPos = thisPos;
        }

        protected Primitive GetParentPrim(Primitive thisPrim)
        {
            Primitive outerPrim = null;
            int requests = 10;
            while (outerPrim == null && requests-->0)
            {
                if (thisPrim == Prim && _Parent != null) return _Parent.Prim;
                uint theLPrimParentID = thisPrim.ParentID;
                if (theLPrimParentID == 0 || requests-- <1)
                {
                    Debug("Why are not we getting a parent prim?");
                    return null;
                }
                Simulator simu = GetSimulator();
                outerPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                if (outerPrim == null)
                {
                    Thread.Sleep(500);
                    Debug("Probing for parent");
                    if (!RequestedParent)
                    {
                       EnsureParentRequested(simu);
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
                //Primitive Prim = this.Prim;
                uint theLPrimParentID = Prim.ParentID;
                WorldObjects.RequestObject(simu, theLPrimParentID);
                WorldObjects.EnsureSelected(theLPrimParentID, simu);
                ParentGrabber.AddFirst(() => TaskGetParent(theLPrimParentID, simu));
            }
        }

        private static readonly TaskQueueHandler ParentGrabber = new TaskQueueHandler("ParentGrabber", 0);

        private void TaskGetParent(uint theLPrimParentID, Simulator simu)
        {
            if (theLPrimParentID == 0 || _Parent != null) return;

            Primitive outerPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
            if (outerPrim != null)
            {
                _Parent = WorldObjects.GetSimObjectFromUUID(outerPrim.ID);
            }
            else
            {
                ParentGrabber.Enqueue(() => TaskGetParent(theLPrimParentID, simu));
            }
        }


        public bool BadLocation(Vector3 transValue)
        {
            if (transValue.Z < -2.0f) return true;
            if (transValue.X < 0.0f) return true;
            if (transValue.X > 255.0f) return true;
            if (transValue.Y < 0.0f) return true;
            if (transValue.Y > 255.0f) return true;
            return false;
        }


        public BotNeeds GetActualUpdate(string pUse)
        {
            if (needUpdate)
            {
                UpdateProperties(Properties);
            }
            return ObjectType.GetUsageActual(pUse).Magnify(scaleOnNeeds);
        }


        public SimTypeUsage GetBestUse(BotNeeds needs)
        {
            if (needUpdate)
            {
                UpdateProperties(Properties);
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

        /// <summary>
        ///  Gets the distance a ISimAvatar may be from ISimObject to use
        /// </summary>
        /// <returns>1-255</returns>
        public virtual float GetSizeDistance()
        {
            double size = Math.Sqrt(BottemArea())/2;

            //            if (IsPhantom) return size;

            double fx; // = thePrim.Scale.X;
            //if (fx > size) size = fx;
            double fy; // = thePrim.Scale.Y;
            //if (fy > size) size = fy;

            foreach (SimObject obj in AttachedChildren)
            {
                Primitive cp = obj.Prim;
                fx = cp.Scale.X;
                if (fx > size) size = fx;
                fy = cp.Scale.Y;
                if (fy > size) size = fy;
                double childSize = obj.GetSizeDistance();
                if (childSize > size) size = childSize;
            }
            return (float) size;
        }

        public virtual List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly)
        {
            if (!IsRegionAttached())
            {
                List<SimObject> objs = new List<SimObject>();
                if (_Parent != null && Parent != this)
                {
                    objs.Add(Parent);
                }
                return objs;
            }
            List<SimObject> objs2 = WorldSystem.GetNearByObjects(GetWorldPosition(), this, (float) maxDistance, rootOnly);
            SortByDistance(objs2);
            return objs2;
        }

        public virtual Vector3d GetWorldPosition()
        {
            Vector3 objectLoc = GetSimPosition();
            uint regionX = 0, regionY = 0;
            Utils.LongToUInts(RegionHandle, out regionX, out regionY);
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
            if (name is PrimFlags) return (Prim.Flags & (PrimFlags) name) != 0;
            return (" " + Prim.Flags.ToString().ToLower() + " ").Contains(" " + name.ToString().ToLower() + " ");
        }

        public virtual void Debug(string p, params object[] args)
        {
            WorldSystem.WriteLine(String.Format(this + ": " + p, args));
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
            return (int) (Distance(p1) - Distance(p2));
        }

        static int CompareSize(SimObject p1, SimObject p2)
        {
            return (int)(p1.GetSizeDistance() * p1.GetCubicMeters() - p2.GetSizeDistance() * p2.GetCubicMeters());
        }

        public int CompareDistance(Vector3d v1, Vector3d v2)
        {
            Vector3d rp = GetWorldPosition();
            return (int) (Vector3d.Distance(rp, v1) - Vector3d.Distance(rp, v2));
        }

        public string DistanceVectorString(SimPosition obj)
        {
            if (!obj.IsRegionAttached())
            {
                Vector3 loc;
                loc = obj.GetSimPosition();
                SimPathStore R = obj.GetPathStore();
                return String.Format("unknown relative {0}/{1:0.00}/{2:0.00}/{3:0.00}",
                                     R.RegionName, loc.X, loc.Y, loc.Z);
            }
            return DistanceVectorString(obj.GetWorldPosition());
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
            SimRegion R = GetSimRegion();
            return String.Format("{0:0.00}m ", Vector3.Distance(GetSimPosition(), loc))
                   + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public virtual string GetName()
        {
            if (Object.ReferenceEquals(_Prim0,null)) return ToString() + " " + RegionHandle;
            Primitive Prim = this.Prim;
            if (Properties != null)
            {
                String s = Properties.Name;
                if (s.Length > 8) return s;
                s += " | " + Properties.Description;
                if (s.Length > 12) return s;
            }
            return ToString();
        }

        private SimMesh _Mesh;

        public SimMesh Mesh
        {
            get
            {
                if (_Mesh == null)
                {
                    _Mesh = new SimMesh(this, Prim, GetPathStore());
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
            return (int) (b1.MinZ - b2.MinZ);
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
            if (Object.ReferenceEquals(_Prim0,null)) return 1;
            return Prim.Scale.X * Prim.Scale.Y;
        }

        //public SimRegion _CurrentRegion;

        public virtual SimRegion GetSimRegion()
        {
            int tries = 0;
            while (RegionHandle == 0 && tries < 2)
            {
                tries++;
                RegionHandle = Prim.RegionHandle;
            }
            return WorldSystem.GetRegion(RegionHandle);
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


        public virtual void OpenNearbyClosedPassages()
        {
            SimObjectType DOOR = SimTypeSystem.DOOR;
            // look for closed doors

            List<SimObject> UnEnterables = new List<SimObject>();
            foreach (SimObject O in GetNearByObjects(3, false))
            {
                O.UpdateOccupied();
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
                new Thread(delegate()
                               {
                                   Thread.Sleep(90000); // 90 seconds
                                   foreach (SimObject O in UnEnterables)
                                   {
                                       O.RestoreEnterable(this);
                                   }
                               }).Start();
            }
        }


        public virtual void AddCanBeTargetOf(int ArgN,SimObjectEvent evt)
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
        public Queue<SimObjectEvent> ActionEventQueue = new Queue<SimObjectEvent>(MaxEventSize);
        public SimObjectEvent lastEvent = null;

        public readonly Dictionary<string, SimObjectEvent> LastEventByName = new Dictionary<string, SimObjectEvent>();

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
                    ActionEventQueue.Enqueue(WorldSystem.SendPipelineEvent(SE));
                    
                }
                LastEventByName[SE.EventName] = SE;
            }
            for (int argN = 1; argN < args0_N.Length; argN++)
            {
                object o = args0_N[argN];
                if (o is SimObject)
                {
                    SimObject newSit = (SimObject) o;
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
                return (object[]) Array.CreateInstance(t, 0);
            }
            object[] o = (object[]) Array.CreateInstance(t, newLen);
            Array.Copy(args, p, o, 0, newLen);
            return o;
        }

        public static object[] PushFrontOfArray(ref object[] args, object p)
        {
            if (args == null) return null;
            int len = args.Length;
            Type t = args.GetType().GetElementType();
            int newLen = len + 1;
            object[] o = (object[]) Array.CreateInstance(t, newLen);
            Array.Copy(args, 0, o, 1, len);
            o[0] = p;
            return o;
        }


        public string GetSimVerb()
        {
            Primitive Prim = this.Prim;
            string sn;
            OpenMetaverse.Primitive.ObjectProperties PrimProperties = Properties;
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
                if (Properties != null)
                    sn = Properties.SitName;
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
                if (Properties != null)
                    sn = Properties.TouchName;
                if (!String.IsNullOrEmpty(sn)) return sn;
                sn = ObjectType.GetTouchName();
                if (!String.IsNullOrEmpty(sn)) return sn;
                return "TouchTheObject";
            }
        }


        private bool _isAttachment;

        public bool IsAttachment
        {
            get { return _isAttachment || _Parent is SimAvatar; }
            set { _isAttachment = value; }
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
            SimObjectEvent newSimObjectEvent = new SimObjectEvent(SimEventStatus.Once, effectType, SimEventType.EFFECT,
                                                                    WorldObjects.ToParameter("doneBy", this),
                                                                    WorldObjects.ToParameter("objectActedOn", t),
                                                                    WorldObjects.ToParameter("eventPartiallyOccursAt", p),
                                                                    WorldObjects.ToParameter("simDuration", duration),
                                                                    WorldObjects.AsEffectID(id));
            bool noteable = LogEvent(newSimObjectEvent);
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

        internal Color DebugColor()
        {
            if (IsUseable) return Color.Green;
            return Color.Empty;
        }

        #region SimObject Members

        readonly Dictionary<string,object> dict = new Dictionary<string, object>();
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
        object this[String s]{get;set;}
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
        ListAsSet<SimObject> GetChildren();
        Vector3d GetGlobalLeftPos(int angle, double Dist);
        List<string> GetMenu(SimAvatar avatar);
        string GetName();
        BotNeeds GetProposedUpdate(string pUse);
        Vector3 GetSimScale();
        Simulator GetSimulator();
        IList<SimTypeUsage> GetTypeUsages();
        List<SimObjectUsage> GetUsages();
        bool GotoTarget(SimPosition pos);
        bool IsFloating { get; set; }
        bool IsInside(Vector3 L);
        bool IsKilled { set; }
        bool IsControllable { get; }
        //inherited from SimPosition: bool IsPassable { get; set; }
        bool IsPhantom { get; set; }
        bool IsPhysical { get; set; }
        bool IsAttachment { get; set; }
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
        SimObject Parent { get; }
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
        void TeleportTo(SimRegion R, Vector3 local);
        //inherited from Object string ToString();
        void TurnToward(SimPosition targetPosition);
        bool TurnToward(Vector3 target);
        void UpdateObject(ObjectUpdate objectUpdate, ObjectUpdate objectUpdateDiff);

        void UpdateProperties(Primitive.ObjectProperties props);

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
        bool KilledPrim(Primitive primitive, Simulator simulator);

        List<NamedParam> GetInfoMap();
        void SetInfoMap(string key,Type type, Object value);
    }
}