using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Threading;
using cogbot.TheOpenSims.Navigation;
using cogbot.TheOpenSims.Navigation.Debug;
using cogbot.TheOpenSims.Mesher;
using System.Drawing;
using Simian;
using System.Windows.Forms;

namespace cogbot.TheOpenSims
{

    //TheSims-like object
    public class SimObject : SimPosition, BotMentalAspect,SimMover
    {

        #region SimMover Members


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
        public virtual bool MoveTo(Vector3d finalTarget, double maxDistance, int maxSeconds)
        {
            double currentDist = Vector3d.Distance(finalTarget, GetWorldPosition());
            if (currentDist < maxDistance) return true;
            {
                SimWaypoint P = SimWaypoint.CreateGlobal(finalTarget);
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


        public virtual void TurnToward(SimPosition targetPosition)
        {
            TurnToward(targetPosition.GetWorldPosition());
            SendUpdate();
        }

        public virtual void SendUpdate()
        {         
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
                bool result = FollowPathTo(pos.GetWorldPosition(), pos.GetSizeDistance());
                if (result)
                {
                    SetMoveTarget(pos);
                    return result;
                }
            }
            return FollowPathTo(pos.GetWorldPosition(), pos.GetSizeDistance());
        }

        public virtual bool IsLocal()
        {
            if (!IsRoot()) return false;
            return WorldSystem.client.Network.CurrentSim == GetSimRegion().TheSimulator;
        }

        public bool FollowPathTo(Vector3d globalEnd, double distance)
        {
            if (!IsLocal())
            {
                throw Error("FollowPathTo !IsLocal()");
            }
            SimAbstractMover move = new SimAbstractMover(this, globalEnd, distance);
            return move.FollowPathTo(globalEnd, distance);
        }


        public virtual void TeleportTo(SimRegion R, Vector3 local)
        {
            SetObjectPosition(R.LocalToGlobal(local));
        }

        internal bool SetObjectPosition(Vector3d globalPos)
        {
            Vector3d start = GetWorldPosition();
            Vector3d offset = globalPos - start;
            Vector3 lPos = GetSimPosition();
            lPos.X += (float)offset.X;
            lPos.Y += (float)offset.Y;
            lPos.Z += (float)offset.Z;
            return SetObjectPosition(lPos);
        }

        internal bool SetObjectPosition(Vector3 localPos)
        {
            if (!IsRoot())
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
            while (diff.Length() > 2)
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

            if (!IsRoot())
            {
                parentRot = Parent.GetSimRotation();
            }

            Quaternion between = Vector3.RotationBetween(Vector3.UnitX, Vector3.Normalize(target - GetSimPosition()));
            Quaternion rot = between * (Quaternion.Identity / parentRot);

            SetObjectRotation(rot);
            return true;
        }

        internal bool SetObjectRotation(Quaternion localPos)
        {
            if (!IsRoot())
            {
                Quaternion start = GetSimRotation();
                Quaternion offset = localPos / start;
                SimObject p = Parent;
                return p.SetObjectRotation(p.GetSimRotation() * offset);
            }
            WorldSystem.SetObjectRotation(Prim, localPos);
            return true;
        }

        protected SimPathStore PathStore;
        public Box3Fill OuterBox = new Box3Fill(true);

        internal void ResetRegion(ulong regionHandle)
        {
            //lock (Prim)
            {
                _CurrentRegion = SimRegion.GetRegion(regionHandle);
                //  Prim.RegionHandle = regionHandle;
                Debug("Changing regions " + this);
                // PathStore = GetPathSystem();
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
                if (IsRoot() || true) return false;
                if (!IsRoot() && IsRegionAttached()) return Parent.IsPassable;
                if (Parent == null) return true;
                return Parent.IsPassable;
            }
            set {
                _PassableKnown = true;
                _Passable = value;
            }
        }
        public bool IsPhantom
        {
            get
            {
                if (MadePhantom) return true;
                if (IsRoot() || true) return (Prim.Flags & PrimFlags.Phantom) == PrimFlags.Phantom;
                if (!IsRoot() && IsRegionAttached()) return Parent.IsPhantom;
                if (Parent == null) return true;
                return Parent.IsPhantom;
            }
            set
            {
                if (IsPhantom == value) return;
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
                if (!IsRoot()) return Parent.IsPhysical;
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
        //        SimPathStore PathStore = GetPathSystem();
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
        //            List<SimObject> objs = GetNearByObjects(3f, false);
        //            foreach (SimObject O in objs)
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
        public Primitive Prim;
        //{
        //    get { return base.Prim; }
        //    set { Prim = value; }
        //}
        readonly public SimObjectType ObjectType;
        public WorldObjects WorldSystem;
        bool MadeNonPhysical = false;
        bool MadePhantom = false;
        bool needUpdate = true;
        bool WasKilled;

        public bool IsKilled
        {
            // get { return WasKilled; }
            set
            {
                if (!WasKilled)  //already
                {
                    WasKilled = value;
                    List<SimObject> AttachedChildren = GetChildren();
                    lock (AttachedChildren) foreach (SimObject C in AttachedChildren)
                        {
                            C.IsKilled = true;
                        }
                }
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

        public SimObject(Primitive prim, WorldObjects objectSystem, Simulator sim)
        //: base(prim.ID.ToString())
           // : base(prim, SimRegion.SceneProviderFromSimulator(sim))
        {

            Prim = prim;
            WorldSystem = objectSystem;
            ObjectType = SimTypeSystem.GetObjectType(prim.ID.ToString());
            UpdateProperties(Prim.Properties);
            _CurrentRegion = SimRegion.GetRegion(sim);
            PathStore = GetPathSystem();
            //WorldSystem.EnsureSelected(prim.ParentID,sim);
            if (Prim.Sculpt != null)
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

        public bool AddChild(SimObject simObject)
        {
            needUpdate = true;
            simObject._Parent = this;
            simObject.needUpdate = true;
            bool b = AttachedChildren.AddTo(simObject);
            if (b)
            {
                if (!IsTyped())
                {// borrow from child?
                    // UpdateProperties(simObject.thePrim.Properties);
                }
            }
            return b;

        }

        public bool IsTyped()
        {
            if (WasKilled) return false;
            return ObjectType.IsComplete();
        }

        public virtual bool IsRoot()
        {
            if (WasKilled) return false;
            if (Prim.ParentID == 0) return true;
            _Parent = Parent;
            return false;
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
            PathStore = GetPathSystem();            
            UpdateOccupied(PathStore);
            UpdateProperties(Prim.Properties);
            _TOSRTING = null;
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
                actor.Touch(this);
                changed = true;
                IsPhantom = false;
            }
            if (!IsRoot())
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
                    actor.Touch(this);
                    return true;
                }
                return false;
            }

            if (!IsRoot())
            {
                SimObject P = Parent;
                if (P.Prim != this.Prim)
                    return P.MakeEnterable(actor);
            }

            bool changed = false;
            if (!IsPhantom)
            {
                IsPhantom = true;
                actor.Touch(this);
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
                String str = Prim.ToString() + " ";
                if (Prim.Properties != null)
                {
                    if (!String.IsNullOrEmpty(Prim.Properties.Name))
                        str += Prim.Properties.Name + " ";
                    if (!String.IsNullOrEmpty(Prim.Properties.Description))
                        str += " | " + Prim.Properties.Description + " ";
                }
                if (!String.IsNullOrEmpty(Prim.Text))
                    str += " | " + Prim.Text + " ";
                uint ParentId = Prim.ParentID;
                if (ParentId != 0)
                {
                    str += "(parent ";

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
                        str += WorldSystem.GetPrimTypeName(pp) + " " + pp.ID.ToString().Substring(0, 8);
                    }
                    else
                    {
                        str += ParentId;
                    }
                    str += ")";
                }
                if (AttachedChildren.Count > 0)
                {
                    str += "(childs " + AttachedChildren.Count + ")";
                }
                else
                {
                    str += "(ch0)";
                }
                str += " (size " + GetSizeDistance() + ") ";
                str += SuperTypeString();
                if (Prim.Sound != UUID.Zero)
                    str += "(Audible)";
                if (!IsPassable)
                    str += "(!IsPassable)";
                if (Prim.PrimData.ProfileHollow > 0f)
                    str += String.Format("(hollow {0:0.00})", Prim.PrimData.ProfileHollow);
                if (WasKilled) str += "(IsKilled)";
                _TOSRTING = str.Replace("  ", " ").Replace(") (", ")(");
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
            if (IsRoot()) return true;
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
            return Parent.IsRegionAttached();
        }

        public virtual Simulator GetSimulator()
        {
            return GetSimRegion().TheSimulator;
        }


        public virtual Vector3 GetSimScale()
        {
            if (true) return Prim.Scale; // the scale is all in the prim w/o parents?
            if (!IsRegionAttached()) throw Error("GetSimScale !IsRegionAttached: " + this);
            Primitive theLPrim = Prim;
            Vector3 theLPos = theLPrim.Scale;
            while (theLPrim.ParentID != 0)
            {
                uint theLPrimParentID = theLPrim.ParentID;
                Simulator simu = GetSimulator();
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID, simu);
                }
                // maybe plus?
                theLPos = theLPos + theLPrim.Scale;
            }
            return theLPos;
        }


        public virtual OpenMetaverse.Quaternion GetSimRotation()
        {
            if (!IsRegionAttached()) throw Error("GetSimRotation !IsRegionAttached: " + this);
            Primitive theLPrim = Prim;
            Quaternion theLPos = theLPrim.Rotation;
            while (theLPrim.ParentID != 0)
            {
                uint theLPrimParentID = theLPrim.ParentID;
                Simulator simu = GetSimulator();
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID, simu);
                }
                theLPos = theLPos * theLPrim.Rotation;
                theLPos.Normalize();
            }
            return theLPos;
        }
        public virtual Vector3 GetSimPosition()
        {
            if (!IsRegionAttached()) throw Error("GetWorldPosition !IsRegionAttached: " + this);
            Primitive theLPrim = Prim;
            Vector3 theLPos = theLPrim.Position;
            while (theLPrim.ParentID != 0)
            {
                uint theLPrimParentID = theLPrim.ParentID;
                Simulator simu = GetSimulator();
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID, simu);
                }
                theLPos = theLPrim.Position + Vector3.Transform(theLPos, Matrix4.CreateFromQuaternion(theLPrim.Rotation));
            }
            if (false && BadLocation(theLPos))
            {
                Debug("-------------------------" + this + " shouldnt be at " + theLPos);
                //   WorldSystem.DeletePrim(thePrim);
            }
            return theLPos;
        }

        public bool BadLocation(Vector3 theLPos)
        {
            if (theLPos.Z < -2.0f) return true;
            if (theLPos.X < 0.0f) return true;
            if (theLPos.X > 255.0f) return true;
            if (theLPos.Y < 0.0f) return true;
            if (theLPos.Y > 255.0f) return true;
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
        ///  Gets the distance a SimAvatar may be from SimObject to use
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
        //static ListAsSet<SimObject> CopyObjects(List<SimObject> objects)
        //{
        //    ListAsSet<SimObject> KnowsAboutList = new ListAsSet<SimObject>();
        //    lock (objects) foreach (SimObject obj in objects)
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
                Vector3 loc = obj.GetSimPosition();
                SimRegion R = obj.GetSimRegion();
                return String.Format("unknown relative {0}/{1:0.00}/{2:0.00}/{3:0.00}",
                    R.RegionName, loc.X, loc.Y, loc.Z);
            }
            return DistanceVectorString(obj.GetWorldPosition());
        }

        public string DistanceVectorString(Vector3d loc3d)
        {
            Vector3 loc = SimRegion.GlobalToLocal(loc3d);
            SimRegion R = SimRegion.GetRegion(loc3d);
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
                    _Mesh = new SimMesh(this);
                }
                return _Mesh;
            }
            set { _Mesh = value; }
        }


        public string GetMeshInfo()
        {
            foreach (SimPathStore PS in SimPathStoresOccupied)
            {
                ForceUpdateOccupied(PS);
            }
            return Mesh.DebugString();

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

        internal float BottemArea()
        {
            float bottemX = OuterBox.MaxX - OuterBox.MinX;
            float bottemY = OuterBox.MaxY - OuterBox.MinY;
            return bottemX * bottemY;
        }

        List<SimPathStore> SimPathStoresOccupied = new List<SimPathStore>();

        public virtual void UpdateOccupied(SimPathStore simPathStore)
        {
            if (!IsRegionAttached())
            {
                //Debug("!IsRegionAttached");
                return;
            }
            // if (IsPassable) return;
            if (simPathStore.GetSimRegion() != GetSimRegion()) return;
            lock (SimPathStoresOccupied)
            {
                if (SimPathStoresOccupied.Contains(simPathStore)) return;
                SimPathStoresOccupied.Add(simPathStore);
            }
            try
            {
                ForceUpdateOccupied(simPathStore);
            }
            catch (Exception e)
            {
                lock (SimPathStoresOccupied)
                {
                    SimPathStoresOccupied.Remove(simPathStore);
                }
            };
        }

        Dictionary<Point, Vector2> OccupiedWPs = new Dictionary<Point, Vector2>();

        internal bool IsInside(Vector3 L)
        {
            return (Mesh.IsInside(L.X, L.Y, L.Z));
        }

        internal Vector2 GetMinMaxZ(SimWaypoint WP)
        {
            try { return OccupiedWPs[WP.Point]; }
            catch (Exception)
            {
                return new Vector2(OuterBox.MinZ, OuterBox.MaxZ);
            }
        }


        internal void RemeshObject(Box3Fill changed)
        {
            RemoveFromWaypoints(changed);
            Mesh = null;            
            UpdatePathOccupied(PathStore);
        }

        internal void RemeshObject()
        {
            Box3Fill changed = new Box3Fill(true);
            RemeshObject(changed);
            PathStore.Refresh(changed);
        }



        internal void RemoveFromWaypoints(Box3Fill changed)
        {
            lock (OccupiedWPs)
            {
                SimPathStore S = GetPathSystem();
                foreach (Point P in OccupiedWPs.Keys)
                {
                    changed.AddPoint(P.X, P.Y, 0, 0f);
                    SimWaypoint W = S.mWaypoints[P.X, P.Y];
                    if (W == null) continue;
                    W.RemoveObject(this);
                }
                OccupiedWPs.Clear();
            }
        }

        internal void SetLocated(float x, float y, float minZ, float maxZ)
        {
            Point WP = PathStore.SetObjectAt(x, y, this, minZ, maxZ).Point;

            Vector2 V2;
            if (OccupiedWPs.TryGetValue(WP, out V2))
            {
                bool b = false;
                if (minZ < V2.X)
                {
                    V2.X = minZ;
                    b = true;
                }
                if (maxZ > V2.Y)
                {
                    V2.Y = maxZ;
                    b = true;
                }
                if (!b) return;
            }
            else
            {
                V2.X = minZ;
                V2.Y = maxZ;
            }
            OccupiedWPs[WP] = V2;
        }

        static void AllTerrainMinMaxLevel(float x, float y, out double minLevel, out double maxLevel)
        {
            minLevel = double.MinValue;
            maxLevel = double.MaxValue;
        }

        internal void ForceUpdateOccupied(SimPathStore simPathStore)
        {
            if (!IsRegionAttached()) return;
            if (simPathStore.GetSimRegion() != GetSimRegion()) return;
            PathStore = simPathStore;
            if (!IsSculpted)
            {
                UpdatePathOccupied(simPathStore);
                return;
            }
            new Thread(new ThreadStart(delegate()
            {
                try
                {
                    UpdatePathOccupied(simPathStore);
                }
                catch (Exception)
                {
                    lock (SimPathStoresOccupied)
                    {
                        SimPathStoresOccupied.Remove(simPathStore);
                    }
                }
            })).Start();
        }


        private void UpdatePathOccupied(SimPathStore simPathStore)
        {
            if (simPathStore.GetSimRegion() != GetSimRegion()) return;
            PathStore = simPathStore;
            Vector3 Position = GetSimPosition();
            Mesh.SetOccupied(SetLocated, 10, 60, simPathStore.StepSize);
           // Mesh = null;
        }

        private void UpdatePathOccupiedNotWorkingNew(SimPathStore simPathStore)
        {
            if (simPathStore.GetSimRegion() != GetSimRegion()) return;
            PathStore = simPathStore;
            byte[,] mMatrix = PathStore.mMatrix;
            PathStore.NeedsUpdate = true;
            float POINTS_PER_METER = PathStore.POINTS_PER_METER;
            float StepSize = PathStore.StepSize;
            float xf = OuterBox.MinX;
            float yfi = OuterBox.MinY;
            int xs = (int)Math.Round(OuterBox.MinX * POINTS_PER_METER);
            int ys = (int)Math.Round(OuterBox.MinY * POINTS_PER_METER);
            int xe = (int)Math.Round(OuterBox.MaxX * POINTS_PER_METER);
            int ye = (int)Math.Round(OuterBox.MaxY * POINTS_PER_METER);
            Vector2 v2 = new Vector2(float.MaxValue, float.MinValue);
            for (int x = xs; x <= xe; x++)
            {
                float yf = yfi;
                for (int y = ys; y <= ye; y++)
                {
                    if (Mesh.MinMaxZ(xf, yf, ref v2))
                    {
                        SimWaypoint W = PathStore.Waypoint(x, y);
                        OccupiedWPs[W.Point] = v2;
                        W.AddOccupied(this, v2.X, v2.Y);
                        v2 = new Vector2(float.MaxValue, float.MinValue);
                        if (mMatrix[x, y] > 9)
                            if (mMatrix[x, y] < 200)
                                mMatrix[x, y] = W.GetOccupiedValue();
                    }
                    yf += StepSize;
                }
                xf += StepSize;
            }
           // Mesh = null;
        }

        public SimRegion _CurrentRegion;
        public virtual SimRegion GetSimRegion()
        {
            //lock (Prim)
            {
                if (_CurrentRegion == null)
                {
                    lock (Prim)
                    _CurrentRegion = SimRegion.GetRegion(Prim.RegionHandle);
                    PathStore = _CurrentRegion.PathStore;
                }
                return _CurrentRegion;
            }
        }

        public virtual SimPathStore GetPathSystem()
        {
            PathStore = GetSimRegion().PathStore;
            return PathStore;
        }


        public virtual void UpdateOccupied()
        {
            UpdateOccupied(GetPathSystem());
        }



        internal Vector3d GetGlobalLeftPos(int angle, double Dist)
        {
            return SimRegion.GetGlobalLeftPos(this, angle, Dist);
        }

        #region SimPosition Members


        //public SimWaypoint GetWaypoint()
        //{
        //    return GetSimRegion().CreateClosestRegionWaypoint(GetSimPosition(),2);
        //}

        #endregion


        //internal string ToMeshString()
        //{
        //    if (_Mesh != null)
        //    {
        //       return _Mesh.ToString();
        //    }
        //    return ToString();
        //}

        internal void ResetPrim(Primitive prim)
        {
            if (prim != Prim)
            {
                Prim = prim;
                ResetRegion(prim.RegionHandle);
                Debug("two differnt prims {0} {1}", prim, Prim);
            }
           // throw new Exception("The method or operation is not implemented.");
        }

        internal void RegionTaintedThis()
        {            
            WorldSystem.ReSelectObject(Prim);
        }

        internal bool SomethingBetween(Vector3 vector3, float low, float high)
        {
            return Mesh.SomethingBetween(vector3.X, vector3.Y, low, high);
        }
        internal bool SomethingMaxZ(Vector3 vector3, float low, float high, out float maxZ)
        {
            return Mesh.SomethingMaxZ(vector3.X, vector3.Y, low, high, out maxZ);
        }
    }
}
