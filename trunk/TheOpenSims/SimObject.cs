using System;
using System.Collections.Generic;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;
using cogbot.Listeners;
using OpenMetaverse;
using org.opencyc.cycobject;
using PathSystem3D.Mesher;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    //TheSims-like object
    public class SimObjectImpl : SimPosition, BotMentalAspect, SimMover, cogbot.TheOpenSims.SimObject, MeshableObject
    {

        private CycFort fort;
        public CycFort GetCycFort()
        {
            if (fort == null)
            {
                fort = TextForm.Cyclifier.FindOrCreateCycFort(this);
            }
            return fort;
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
                SetMoveTarget(P);
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

        public virtual void SetMoveTarget(SimPosition target)
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
        /// 
        /// </summary>
        /// <param name="pos"></param>
        /// <returns></returns>
        public virtual bool GotoTarget(SimPosition pos)
        {
            if (!IsLocal())
            {
                throw Error("GotoTarget !IsLocal()");
            }

            for (int i = 0; i < 8; i++)
            {
                bool result = FollowPathTo(pos, pos.GetSizeDistance());
                if (result)
                {
                    SetMoveTarget(pos);
                    return result;
                }
            }
            return FollowPathTo(pos, pos.GetSizeDistance());
        }

        public virtual bool IsLocal()
        {
            if (!IsRoot) return false;
            return true;// WorldSystem.client.Network.CurrentSim == GetSimRegion().TheSimulator;
        }

        public bool FollowPathTo(SimPosition globalEnd, double distance)
        {
            if (!IsLocal())
            {
                throw Error("FollowPathTo !IsLocal()");
            }
            SimAbstractMover move = SimAbstractMover.CreateSimPathMover(this, globalEnd, distance);
            return move.Goto() == SimMoverState.COMPLETE;
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
            lPos.X += (float)offset.X;
            lPos.Y += (float)offset.Y;
            lPos.Z += (float)offset.Z;
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
            LocalPos.X += (float)diff.X;
            LocalPos.Y += (float)diff.Y;
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
            Quaternion rot = between * (Quaternion.Identity / parentRot);

            SetObjectRotation(rot);
            return true;
        }

        public bool SetObjectRotation(Quaternion localPos)
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
        public Box3Fill OuterBox {
            get { return Mesh.OuterBox; }
        }

        public virtual void ResetRegion(ulong regionHandle)
        {
            //lock (Prim)
            if (regionHandle!=0)
            {
                _CurrentRegion = SimRegion.GetRegion(regionHandle);
                //  Prim.RegionHandle = regionHandle;
              //  Debug("Changing regions " + this);
                // PathStore = GetSimRegion();
            }
        }

        /// <summary>
        /// Right now only sees if TouchName has been defined - need a relable way to see if script is defined.
        /// </summary>
        public bool IsTouchDefined
        {
            get
            {
                if (Prim.Properties != null)
                    return !String.IsNullOrEmpty(Prim.Properties.TouchName);
                if ((Prim.Flags & PrimFlags.Touch) != 0) return true;
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
                if (Prim.Properties != null)
                    return !String.IsNullOrEmpty(Prim.Properties.SitName);
                if (Prim.ClickAction == ClickAction.Sit) return true;
                return false;
            }
        }

        public bool IsSculpted
        {
            get { return Prim.Sculpt != null; }
        }
        bool _Passable;
        bool _PassableKnown = false;

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
        //            WorldSystem.output("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
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
        //            WorldSystem.output("BAD: " + v3 + " <- " + dist + " -> " + swp + " " + this);
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
        Primitive _Prim;
        public Primitive Prim
        {
            get { return _Prim; }
        }
        //{
        //    get { return base.Prim; }
        //    set { Prim = value; }
        //}
        readonly public SimObjectType ObjectType;
        public WorldObjects WorldSystem;
        bool MadeNonPhysical = false;
        bool MadePhantom = false;
        bool needUpdate = true;
        protected bool WasKilled;

        public virtual bool IsKilled
        {
            // get { return WasKilled; }
            set
            {
                if (!WasKilled)  //already
                {
                    List<SimObject> AttachedChildren = GetChildren();
                    lock (AttachedChildren) foreach (SimObject C in AttachedChildren)
                        {
                            C.IsKilled = true;
                        }
                    RemoveCollisions();
                }
                WasKilled = value;
            }
        }

        private void RemoveCollisions()
        {
            if (_Mesh != null) {
                _Mesh.RemoveCollisions();
            }
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

        public SimObjectImpl(Primitive prim, WorldObjects objectSystem, Simulator sim)
        //: base(prim.ID.ToString())
           // : base(prim, SimRegion.SceneProviderFromSimulator(sim))
        {

            _Prim = prim;
            WorldSystem = objectSystem;
            ObjectType = SimTypeSystem.CreateInstanceType(prim.ID.ToString());
            UpdateProperties(Prim.Properties);
            _CurrentRegion = SimRegion.GetRegion(sim);
           // PathStore = GetSimRegion();
            //WorldSystem.EnsureSelected(prim.ParentID,sim);
            if (WorldObjects.MaintainCollisions && Prim.Sculpt != null)
            {
                objectSystem.StartTextureDownload(Prim.Sculpt.SculptTexture);
            }
            // Parent; // at least request it
        }

        protected SimObject _Parent = null; // null means unknown if we IsRoot then Parent == this;

        public virtual SimObject Parent
        {
            get
            {
                if (_Parent == null)
                {
                    uint parent = Prim.ParentID;
                    if (parent != 0)
                    {
                        Simulator simu = GetSimulator();
                        Primitive prim = WorldSystem.GetPrimitive(parent, simu);
                        if (prim == null)
                        {
                            // try to request for next time
                            WorldSystem.EnsureSelected(parent, simu);
                            return null;
                        }
                        _Parent = WorldSystem.GetSimObject(prim, simu);
                        _Parent.AddChild(this);
                    }
                    else
                    {
                        _Parent = this;
                    }
                }
                return _Parent;
            }
        }

        public bool AddChild(SimObject simO)
        {
            SimObjectImpl simObject = (SimObjectImpl)simO;
            needUpdate = true;
            simObject._Parent = this;
            simObject.needUpdate = true;
            bool b = AttachedChildren.AddTo(simObject);
            if (b)
            {
                if (!IsTyped)
                {// borrow from child?
                    // UpdateProperties(simObject.thePrim.Properties);
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
                if (Prim.ParentID == 0) return true;
               // _Parent = Parent;
                return false;
            }
        }

        public virtual string DebugInfo()
        {
            string str = ToString();
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
                UpdateProperties(Prim.Properties);
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
            if (Prim.Properties != null)
            {
                //  if (thePrim.Properties.TextName != "")
                list.Add("grab");
                //   if (thePrim.Properties.SitName != "")
                list.Add("sit");
                PermissionMask mask = Prim.Properties.Permissions.EveryoneMask;
                if (Prim.OwnerID == avatar.theAvatar.ID) { mask = Prim.Properties.Permissions.OwnerMask; }
                PermissionMask result = mask | Prim.Properties.Permissions.BaseMask;
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
            _TOSRTING = null;
            if (objectProperties != null)
            {
                ObjectType.SitName = objectProperties.SitName;
                ObjectType.TouchName = objectProperties.TouchName;
                needUpdate = false;
            }
            try
            {
                //  Parent;
                AddSuperTypes(SimTypeSystem.GuessSimObjectTypes(objectProperties));
            }
            catch (Exception e)
            {
                Debug("" + e);
            }

        }

        public virtual bool IsFloating
        {
            get
            {
                return !IsPhysical;
            }
            set
            {
                IsPhysical = !value;
            }
        }

        public virtual void UpdateObject(ObjectUpdate objectUpdate, ObjectUpdate objectUpdateDiff)
        {
            UpdateProperties(Prim.Properties);
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
            //SimObjectType UNKNOWN = SimObjectType.UNKNOWN;
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

        string _TOSRTING;
        public override string ToString()
        {
            if (_TOSRTING == null)
            {
                _TOSRTING = "";
                UUID ID = Prim.ID;
                OpenMetaverse.Primitive.ConstructionData PrimData = Prim.PrimData;
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
                        ;
                    }
                    catch (Exception e)
                    {
                    }
                }
                _TOSRTING += " " + ID + " ";
                if (Prim.Properties != null)
                {
                    if (!String.IsNullOrEmpty(Prim.Properties.Name))
                        _TOSRTING += Prim.Properties.Name + " ";
                    if (!String.IsNullOrEmpty(Prim.Properties.Description))
                        _TOSRTING += " | " + Prim.Properties.Description + " ";
                }
                if (!String.IsNullOrEmpty(Prim.Text))
                    _TOSRTING += " | " + Prim.Text + " ";
                uint ParentId = Prim.ParentID;
                if (ParentId != 0)
                {
                    _TOSRTING += "(parent ";

                    Primitive pp = null;
                    if (_Parent != null)
                    {
                        pp = _Parent.Prim;
                    }
                    else
                    {
                        Simulator simu = GetSimulator();
                        pp = WorldSystem.GetPrimitive(ParentId, simu);
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
                if (_Mesh!=null)
                _TOSRTING += " (size " + GetSizeDistance() + ") ";
                _TOSRTING += SuperTypeString();
                if (Prim.Sound != UUID.Zero)
                    _TOSRTING += "(Audible)";
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
                ObjectType.SuperType.ForEach(delegate(SimObjectType item)
                {
                    str += item.GetTypeName() + " ";
                });
            return str.TrimEnd() + "]";
        }


        public bool IsRegionAttached()
        {
            if (WasKilled) return false;
            if (IsRoot) return true;
            if (_Parent == null)
            {
                Simulator simu = GetSimulator();
                Primitive pUse = WorldSystem.GetPrimitive(Prim.ParentID, simu);
                if (pUse == null)
                {
                    WorldSystem.EnsureSelected(Prim.ParentID, simu);
                    return false;
                }
            }
            _Parent = Parent;
            return _Parent!=null&& _Parent.IsRegionAttached();
        }

        public virtual Simulator GetSimulator()
        {
            return GetSimRegion().TheSimulator;
        }


        public virtual Vector3 GetSimScale()
        {
            if (true) return Prim.Scale; // the scale is all in the prim w/o parents?
            if (!IsRegionAttached()) throw Error("GetSimScale !IsRegionAttached: " + this);
            Primitive outerPrim = Prim;
            Vector3 transValue = outerPrim.Scale;
            while (outerPrim.ParentID != 0)
            {
                uint theLPrimParentID = outerPrim.ParentID;
                Simulator simu = GetSimulator();
                outerPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                while (outerPrim == null)
                {
                    Thread.Sleep(100);
                    outerPrim = WorldSystem.RequestMissingObject(theLPrimParentID, simu);
                }
                // maybe plus?
                transValue = transValue + outerPrim.Scale;
            }
            return transValue;
        }


        public virtual OpenMetaverse.Quaternion GetSimRotation()
        {
            //if (!IsRegionAttached()) ;// throw Error("GetSimRotation !IsRegionAttached: " + this);
            Quaternion transValue = Prim.Rotation;
            Primitive thisPrim = Prim;
            if (thisPrim.ParentID != 0)
            {
                Primitive outerPrim = GetParentPrim(thisPrim);
                transValue = outerPrim.Rotation * transValue;
                thisPrim = outerPrim;
                //  transValue.Normalize();
            }
            return transValue;
        }

        public virtual Vector3 GetSimPosition()
        {
            //if (!IsRegionAttached()) return Prim.Position; 
            //throw Error("GetWorldPosition !IsRegionAttached: " + this);
            Primitive thisPrim = Prim;
            Vector3 thisPos = thisPrim.Position;
            if (thisPrim.ParentID != 0)
            {
                Primitive outerPrim = GetParentPrim(thisPrim);

                thisPos = outerPrim.Position + Vector3.Transform(thisPos, Matrix4.CreateFromQuaternion(outerPrim.Rotation));
                thisPrim = outerPrim;
            }
            if (false && BadLocation(thisPos))
            {
                Debug("-------------------------" + this + " shouldnt be at " + thisPos);
                //   WorldSystem.DeletePrim(thePrim);
            }
            return thisPos;
        }

        private Primitive GetParentPrim(Primitive thisPrim)
        {
            Primitive outerPrim;
            if (thisPrim == Prim && _Parent != null) return _Parent.Prim;
            uint theLPrimParentID = thisPrim.ParentID;
            Simulator simu = GetSimulator();
            outerPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
            if (outerPrim == null)
            {
                if (_Parent != null) return _Parent.Prim;
                {
                    Thread.Sleep(100);
                    if (IsRegionAttached()) ;
                    outerPrim = WorldSystem.RequestMissingObject(theLPrimParentID, simu);
                }
            }
            if (thisPrim == Prim && _Parent != null) return _Parent.Prim;
            return outerPrim;
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
                UpdateProperties(Prim.Properties);
            }
            return ObjectType.GetUsageActual(pUse).Magnify(scaleOnNeeds);
        }


        public SimTypeUsage GetBestUse(BotNeeds needs)
        {
            if (needUpdate)
            {
                UpdateProperties(Prim.Properties);
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
            double size = Math.Sqrt(BottemArea()) / 2;

            //            if (IsPhantom) return size;

            double fx;// = thePrim.Scale.X;
            //if (fx > size) size = fx;
            double fy;// = thePrim.Scale.Y;
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
            return (float)size;
        }
                          
        public virtual List<SimObject> GetNearByObjects(double maxDistance, bool rootOnly)
        {
            if (!IsRegionAttached())
            {
                List<SimObject> objs = new List<SimObject>();
                _Parent = Parent;
                if (Parent != null && Parent != this)
                {
                    objs.Add(Parent);
                }
                return objs;
            }
            List<SimObject> objs2 = WorldSystem.GetNearByObjects(GetWorldPosition(), this, (float)maxDistance, rootOnly);
            SortByDistance(objs2);
            return objs2;
        }

        public virtual Vector3d GetWorldPosition()
        {
            return GetSimRegion().LocalToGlobal(GetSimPosition());
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
            return SimTypeSystem.MatchString(ToString(), name);
        }
        public virtual void Debug(string p, params object[] args)
        {
            WorldSystem.output(String.Format(Prim + ": " + p, args));
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
            return (int)(Distance(p1) - Distance(p2));
        }

        public int CompareDistance(Vector3d v1, Vector3d v2)
        {
            Vector3d rp = GetWorldPosition();
            return (int)(Vector3d.Distance(rp, v1) - Vector3d.Distance(rp, v2));
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
            if (Prim.Properties != null)
            {
                String s = Prim.Properties.Name;
                if (s.Length > 8) return s;
                s += " | " + Prim.Properties.Description;
                if (s.Length > 12) return s;
            }
            return ToString();
        }

        SimMesh _Mesh;

        public SimMesh Mesh
        {
            get
            {
                if (_Mesh == null)
                {
                    _Mesh = new SimMesh(this,Prim,GetPathStore());
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
            if (OuterBox.MaxX == float.MinValue)
            {
                return Prim.Scale.X * Prim.Scale.Y;
            }
            float bottemX = OuterBox.MaxX - OuterBox.MinX;
            float bottemY = OuterBox.MaxY - OuterBox.MinY;
            return bottemX * bottemY;
        }
        public SimRegion _CurrentRegion;
        public virtual SimRegion GetSimRegion()
        {
            //lock (Prim)
            {
                if (_CurrentRegion == null)
                {
                    _CurrentRegion = SimRegion.GetRegion(Prim.RegionHandle);
                }
                return _CurrentRegion;
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

        public void ResetPrim(Primitive prim)
        {
            if (prim != _Prim)
            {
                _Prim = prim;
                ResetRegion(prim.RegionHandle);
                Debug("two different prims {0} {1}", prim, Prim);
            }
           // throw new Exception("The method or operation is not implemented.");
        }
      
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
                if (!O.IsPhantom)
                {
                    O.UpdateOccupied();
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
                new Thread(new ThreadStart(delegate()
                {
                    Thread.Sleep(90000); // 90 seconds
                    foreach (SimObject O in UnEnterables)
                    {
                        O.RestoreEnterable(this);
                    }
                })).Start();
            }
        }        


        public virtual void AddCanBeTargetOf(string eventName, int ArgN, object[] arg0_N)
        {
            if (ArgN == 1)
                ObjectType.AddSuperType(SimTypeSystem.CreateObjectUse(eventName));
        }

        public static int MaxEventSize = 10; // Keeps only last 9 events
        public Queue<SimObjectEvent> ActionEventQueue = new Queue<SimObjectEvent>(MaxEventSize);
        public virtual void LogEvent(string eventName, params object[] args1_N)
        {
            object[] args0_N = PushFrontOfArray(ref args1_N, this);
            SimObjectEvent SE = new SimObjectEvent(eventName, args0_N, 0);
            if (ActionEventQueue.Count >= MaxEventSize) ActionEventQueue.Dequeue();
            ActionEventQueue.Enqueue(SE);

            for (int argN = 1; argN < args0_N.Length; argN++)
            {
                object o = args0_N[argN];
                if (o is SimObject)
                {
                    SimObject newSit = (SimObject)o;
                    newSit.AddCanBeTargetOf(eventName, argN, args0_N);
                }
            }
        }

        static public object[] RestOfArray(object[] args, int p)
        {
            if (args == null) return null;
            int len = args.Length;
            Type t = args.GetType().GetElementType();
            int newLen = len - p;
            if (newLen<=0)
            {
                return (object[])Array.CreateInstance(t, 0);
            }
            object[] o= (object[])Array.CreateInstance(t, newLen);
            Array.Copy(args,p,o,0,newLen);
            return o;
        }

        static public object[] PushFrontOfArray(ref object[] args, object p)
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
            string sn = Prim.Properties.TouchName;
            if (!String.IsNullOrEmpty(sn)) return sn;
            sn = ObjectType.GetTouchName();
            if (!String.IsNullOrEmpty(sn)) return sn;
            sn = Prim.Properties.SitName;
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
                if (Prim.Properties != null)
                    sn = Prim.Properties.SitName;
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
                if (Prim.Properties != null)
                    sn = Prim.Properties.TouchName;
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


        readonly ListAsSet<UUID> CurrentSounds = new ListAsSet<UUID>();
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


        public void OnEffect(string effectType, object t, object p, float duration, UUID id)
        {
            LogEvent(effectType, t, p, duration, id);
            //todo
            WorldSystem.SendNewEvent("on-effect", effectType, this, t, p, duration, id);
            //throw new NotImplementedException();
        }

        private bool IsSolidCachedKnown, IsSolidCachedTrue;
        public bool IsSolid
        {
            get
            {
                if (MadePhantom) return true;// since we "changed" it
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

        private bool IsUsableCachedKnown, IsUsableCachedTrue;
        public bool IsUsable
        {
            get
            {
                if (!IsUsableCachedKnown)
                {
                    IsUsable = IsSitDefined || IsSitDefined || IsTypeOf(SimTypeSystem.USEABLE) != null;
                }
                return IsUsableCachedTrue;
            }
            set
            {
                IsUsableCachedKnown = true;
            	IsUsableCachedTrue = value;
            }
        }
        internal System.Drawing.Color DebugColor()
        {
            if (IsUsable) return System.Drawing.Color.Green;
            return Color.Empty;
        }
    }

    public interface SimObject:SimPosition,BotMentalAspect,SimMover
    {
        bool AddChild(SimObject simObject);
        void AddSuperTypes(System.Collections.Generic.IList<SimObjectType> listAsSet);
        bool BadLocation(OpenMetaverse.Vector3 transValue);
        float BottemArea();
        int CompareDistance(SimObject p1, SimObject p2);
        int CompareDistance(OpenMetaverse.Vector3d v1, OpenMetaverse.Vector3d v2);
        string DebugInfo();
        double Distance(SimPosition prim);
        string DistanceVectorString(OpenMetaverse.Vector3 loc);
        string DistanceVectorString(OpenMetaverse.Vector3d loc3d);
        //inherited from SimPosition: string DistanceVectorString(SimPosition obj);
        Exception Error(string p, params object[] args);
        bool FollowPathTo(SimPosition globalEnd, double distance);
        BotNeeds GetActualUpdate(string pUse);
        SimTypeUsage GetBestUse(BotNeeds needs);
        ListAsSet<SimObject> GetChildren();
        OpenMetaverse.Vector3d GetGlobalLeftPos(int angle, double Dist);
        System.Collections.Generic.List<string> GetMenu(SimAvatar avatar);
        string GetName();
        BotNeeds GetProposedUpdate(string pUse);
        OpenMetaverse.Vector3 GetSimScale();
        OpenMetaverse.Simulator GetSimulator();
        System.Collections.Generic.IList<SimTypeUsage> GetTypeUsages();
        System.Collections.Generic.List<SimObjectUsage> GetUsages();
        bool GotoTarget(SimPosition pos);
        bool IsFloating { get; set; }
        bool IsInside(OpenMetaverse.Vector3 L);
        bool IsKilled { set; }
        bool IsLocal();
        //inherited from SimPosition: bool IsPassable { get; set; }
        bool IsPhantom { get; set; }
        bool IsPhysical { get; set; }
        bool IsAttachment { get; set; }
        bool IsRoot { get; }
        bool IsSculpted { get; }
        bool IsSitDefined { get; }
        bool IsTouchDefined { get; }
        Primitive Prim { get; }
        Box3Fill OuterBox { get; }
        bool IsTyped { get; }
        SimObjectType IsTypeOf(SimObjectType superType);
        bool MakeEnterable(PathSystem3D.Navigation.SimMover actor);
        bool Matches(string name);
        SimMesh Mesh { get; }
        SimObject Parent { get; }
        double RateIt(BotNeeds needs);
        void ResetPrim(OpenMetaverse.Primitive prim);
        void ResetRegion(ulong regionHandle);
        bool RestoreEnterable(PathSystem3D.Navigation.SimMover actor);
        void SendUpdate(int ms);
        void SetMoveTarget(SimPosition target);
        bool SetObjectPosition(OpenMetaverse.Vector3 localPos);
        bool SetObjectPosition(OpenMetaverse.Vector3d globalPos);
        bool SetObjectRotation(OpenMetaverse.Quaternion localPos);
        string SitName { get; }
        void SortByDistance(System.Collections.Generic.List<SimObject> sortme);
        string SuperTypeString();
        void TeleportTo(SimRegion R, OpenMetaverse.Vector3 local);
        //inherited from Object string ToString();
        void TurnToward(SimPosition targetPosition);
        bool TurnToward(OpenMetaverse.Vector3 target);
        void UpdateObject(OpenMetaverse.ObjectUpdate objectUpdate, OpenMetaverse.ObjectUpdate objectUpdateDiff);

        void UpdateProperties(Primitive.ObjectProperties props);

        bool UpdateOccupied();

        void Touch(SimObject simObjectImpl);

       // void AddPossibleAction(string textualActionName, params object[] args);

        void AddCanBeTargetOf(string textualActionName, int argN, params object[] args);

        void LogEvent(string p, params object[] args);

        void OnSound(UUID soundID, float gain);

        void OnEffect(string effectType, object t, object p, float duration, UUID id);
    }
}
