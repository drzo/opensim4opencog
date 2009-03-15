using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Threading;
using cogbot.TheOpenSims.Navigation;
using cogbot.TheOpenSims.Navigation.Debug;
using cogbot.TheOpenSims.Mesher;

namespace cogbot.TheOpenSims
{

    //TheSims-like object
    public class SimObject : SimPosition, BotMentalAspect
    {

        public Vector2 MinMaxZ
        {
            get { return new Vector2(OuterBox.MinZ, OuterBox.MaxZ); }
        }

        public Box3Fill OuterBox
        {
            get { return Mesh.OuterBox; }
        }

        internal void ResetRegion(ulong regionHandle)
        {
            _CurrentRegion = null;
            thePrim.RegionHandle = regionHandle;
            Debug("Changing regions " + this);
        }

        /// <summary>
        /// Right now only sees if TouchName has been defined - need a relable way to see if script is defined.
        /// </summary>
        public bool IsTouchDefined
        {
            get
            {
                if (thePrim.Properties != null)
                    return !String.IsNullOrEmpty(thePrim.Properties.TouchName);
                if ((thePrim.Flags & PrimFlags.Touch) != 0) return true;
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
                if (thePrim.Properties != null)
                    return !String.IsNullOrEmpty(thePrim.Properties.SitName);
                if (thePrim.ClickAction == ClickAction.Sit) return true;
                return false;
            }
        }

        public bool IsSculpted
        {
            get { return thePrim.Sculpt != null; }
        }
        public bool IsPassable
        {
            get
            {
                if (IsPhantom) return true;
                if (IsTypeOf(SimTypeSystem.PASSABLE) != null) return true;
                if (IsRoot() || true) return false;
                if (!IsRoot() && IsRegionAttached()) return Parent.IsPassable;
                if (Parent == null) return true;
                return Parent.IsPassable;
            }
        }
        public bool IsPhantom
        {
            get
            {
                if (MadePhantom) return true;
                if (IsRoot() || true) return (thePrim.Flags & PrimFlags.Phantom) == PrimFlags.Phantom;
                if (!IsRoot() && IsRegionAttached()) return Parent.IsPhantom;
                if (Parent == null) return true;
                return Parent.IsPhantom;
            }
            set
            {
                if (IsPhantom == value) return;
                if (value)
                {
                    WorldSystem.SetPrimFlags(thePrim, (PrimFlags)(thePrim.Flags | PrimFlags.Phantom));
                    MadePhantom = true;
                }
                else
                {
                    WorldSystem.SetPrimFlags(thePrim, (PrimFlags)(thePrim.Flags - PrimFlags.Phantom));
                    MadePhantom = false;
                }

            }
        }

        public bool IsPhysical
        {
            get
            {
                if (!IsRoot()) return Parent.IsPhysical;
                return (thePrim.Flags & PrimFlags.Physics) == PrimFlags.Physics;
            }
            set
            {
                if (IsPhysical == value) return;
                if (value)
                {
                    WorldSystem.SetPrimFlags(thePrim, (PrimFlags)(thePrim.Flags | PrimFlags.Physics));
                    MadeNonPhysical = false;
                }
                else
                {
                    WorldSystem.SetPrimFlags(thePrim, (PrimFlags)(thePrim.Flags - PrimFlags.Physics));
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

        public Primitive thePrim;
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
            get { return WasKilled; }
            set
            {
                WasKilled = value;
                List<SimObject> AttachedChildren = GetChildren();
                lock (AttachedChildren) foreach (SimObject C in AttachedChildren)
                    {
                        C.IsKilled = true;
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

        public SimObject(Primitive prim, WorldObjects objectSystem, SimRegion reg)
            //: base(prim.ID.ToString())
           // :base(prim,null)
        {
            thePrim = prim;
            WorldSystem = objectSystem;
            ObjectType = SimTypeSystem.GetObjectType(prim.ID.ToString());
            UpdateProperties(thePrim.Properties);
            _CurrentRegion = reg;
            if (thePrim.Sculpt != null)
            {
                objectSystem.StartTextureDownload(thePrim.Sculpt.SculptTexture);
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
                    uint parent = thePrim.ParentID;
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
                        _Parent = WorldSystem.GetSimObject(prim,simu);
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
            if (IsKilled) return false;
            return ObjectType.IsComplete();
        }

        public virtual bool IsRoot()
        {
            if (IsKilled) return false;
            if (thePrim.ParentID == 0) return true;
            _Parent = Parent;
            return false;
        }

        public virtual string DebugInfo()
        {
            string str = ToString();
            if (thePrim.ParentID != 0)
                return thePrim.ParentID + " " + str;
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
                UpdateProperties(thePrim.Properties);
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
            if (thePrim.Properties != null)
            {
                //  if (thePrim.Properties.TextName != "")
                list.Add("grab");
                //   if (thePrim.Properties.SitName != "")
                list.Add("sit");
                PermissionMask mask = thePrim.Properties.Permissions.EveryoneMask;
                if (thePrim.OwnerID == avatar.theAvatar.ID) { mask = thePrim.Properties.Permissions.OwnerMask; }
                PermissionMask result = mask | thePrim.Properties.Permissions.BaseMask;
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
            SimPathStore PathStore = GetPathSystem();
            UpdateOccupied(PathStore);
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

        public virtual bool RestoreEnterable(SimAvatar actor)
        {
            bool changed = false;
            PrimFlags tempFlags = thePrim.Flags;
            if (MadePhantom && (tempFlags & PrimFlags.Phantom) != PrimFlags.Phantom)
            {
                WorldSystem.client.Self.Touch(thePrim.LocalID);
                tempFlags -= PrimFlags.Phantom;
                changed = true;
                MadePhantom = false;
            }
            if (MadeNonPhysical && (tempFlags & PrimFlags.Physics) == 0)
            {
                tempFlags |= PrimFlags.Physics;
                changed = true;
                MadeNonPhysical = false;
            }
            if (changed) WorldSystem.SetPrimFlags(thePrim, tempFlags);
            if (!IsRoot())
            {
                if (Parent.RestoreEnterable(actor)) return true;
            }
            return changed;
        }

        public virtual bool MakeEnterable(SimAvatar actor)
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

            bool changed = false;
            if (true)
            {
                return changed;
            }
            PrimFlags tempFlags = thePrim.Flags;
            if ((tempFlags & PrimFlags.Phantom) == 0)
            {
                WorldSystem.client.Self.Touch(thePrim.LocalID);
                tempFlags |= PrimFlags.Phantom;
                changed = true;
                MadePhantom = true;
            }
            if ((tempFlags & PrimFlags.Physics) != 0)
            {
                tempFlags -= PrimFlags.Physics;
                changed = true;
                MadeNonPhysical = true;
            }
            if (changed) WorldSystem.SetPrimFlags(thePrim, tempFlags);
            if (!IsRoot())
            {
                if (Parent.MakeEnterable(actor)) return true;
            }
            return changed;

        }

        string _TOSRTING;
        public override string ToString()
        {
            if (_TOSRTING == null)
            {
                String str = thePrim.ToString() + " ";
                if (thePrim.Properties != null)
                {
                    if (!String.IsNullOrEmpty(thePrim.Properties.Name))
                        str += thePrim.Properties.Name + " ";
                    if (!String.IsNullOrEmpty(thePrim.Properties.Description))
                        str += " | " + thePrim.Properties.Description + " ";
                }
                if (!String.IsNullOrEmpty(thePrim.Text))
                    str += " | " + thePrim.Text + " ";
                uint ParentId = thePrim.ParentID;
                if (ParentId != 0)
                {
                    str += " (parent ";

                    Primitive pp = null;
                    if (_Parent != null)
                    {
                        pp = _Parent.thePrim;
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
                    str += ") ";
                }
                if (AttachedChildren.Count > 0)
                {
                    str += " (childs " + AttachedChildren.Count + ") ";
                }
                else
                {
                    str += " (ch0) ";
                }
                str += " (size " + GetSizeDistance() + ") ";
                str += SuperTypeString();
                if (thePrim.Sound != UUID.Zero)
                    str += "(Audible)";
                if (!IsPassable)
                    str += "(!IsPassable)";
                if (IsKilled) str += "(IsKilled)";
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
            if (IsKilled) return false;
            if (IsRoot()) return true;
            if (_Parent == null)
            {
                Simulator simu = GetSimulator();
                Primitive pUse = WorldSystem.GetPrimitive(thePrim.ParentID, simu);
                if (pUse == null)
                {
                    WorldSystem.EnsureSelected(thePrim.ParentID, simu);
                    return false;
                }
            }
            return Parent.IsRegionAttached();
        }

        public Simulator GetSimulator()
        {
            return GetSimRegion().TheSimulator;
        }


        public virtual Vector3 GetSimScale()
        {
            if (true) return thePrim.Scale; // the scale is all in the prim w/o parents?
            if (!IsRegionAttached()) throw Error("GetSimScale !IsRegionAttached: " + this);
            Primitive theLPrim = thePrim;
            Vector3 theLPos = theLPrim.Scale;
            while (theLPrim.ParentID != 0)
            {
                uint theLPrimParentID = theLPrim.ParentID;
                Simulator simu = GetSimulator();
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID,simu);
                }
                // maybe plus?
                theLPos = theLPos + theLPrim.Scale;
            }
            return theLPos;
        }


        public virtual OpenMetaverse.Quaternion GetSimRotation()
        {
            if (!IsRegionAttached()) throw Error("GetSimRotation !IsRegionAttached: " + this);
            Primitive theLPrim = thePrim;
            Quaternion theLPos = theLPrim.Rotation;
            while (theLPrim.ParentID != 0)
            {
                uint theLPrimParentID = theLPrim.ParentID;
                Simulator simu = GetSimulator();
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID,simu);
                }
                theLPos = theLPos * theLPrim.Rotation;
                theLPos.Normalize();
            }
            return theLPos;
        }
        public virtual Vector3 GetSimPosition()
        {
            if (!IsRegionAttached()) throw Error("GetWorldPosition !IsRegionAttached: " + this);
            Primitive theLPrim = thePrim;
            Vector3 theLPos = theLPrim.Position;
            while (theLPrim.ParentID != 0)
            {
                uint theLPrimParentID = theLPrim.ParentID;
                Simulator simu = GetSimulator();
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID, simu);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID,simu);
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
                UpdateProperties(thePrim.Properties);
            }
            return ObjectType.GetUsageActual(pUse).Magnify(scaleOnNeeds);
        }


        public SimTypeUsage GetBestUse(BotNeeds needs)
        {
            if (needUpdate)
            {
                UpdateProperties(thePrim.Properties);
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
            double size = Math.Sqrt(BottemArea())/2;

            //            if (IsPhantom) return size;

            double fx;// = thePrim.Scale.X;
            //if (fx > size) size = fx;
            double fy;// = thePrim.Scale.Y;
            //if (fy > size) size = fy;

            foreach (SimObject obj in AttachedChildren)
            {
                Primitive cp = obj.thePrim;
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
                _Parent = Parent;
                if (Parent != null && Parent != this)
                {
                    objs.Add(Parent);
                }
                return objs;
            }
            List<SimObject> objs2 = WorldSystem.GetNearByObjects(GetWorldPosition(), this,(float) maxDistance, rootOnly);
            SortByDistance(objs2);
            return objs2;
        }

        public Vector3d GetWorldPosition()
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
            WorldSystem.output(String.Format(thePrim + ": " + p, args));
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
            return (int)(Vector3d.Distance(rp , v1) - Vector3d.Distance(rp , v2));
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
            if (thePrim.Properties != null)
            {
                String s = thePrim.Properties.Name;
                if (s.Length > 8) return s;
                s += " | " + thePrim.Properties.Description;
                if (s.Length > 12) return s;
            }
            return ToString();
        }

        public SimMesh _Mesh;

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

        private void SetLocated(float x, float y, float minZ, float maxZ)
        {
            SimPathStore PathStore = GetPathSystem();
            PathStore.SetObjectAt(x, y, this,minZ,maxZ);
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
            if (!Mesh.IsSculpted)
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
            Vector3 Position = GetSimPosition();
            Mesh.SetOccupied(SetLocated, 10, 60, simPathStore.StepSize);
        }


        public SimRegion _CurrentRegion;
        public virtual SimRegion GetSimRegion()
        {
                if (_CurrentRegion == null)
                {
                    _CurrentRegion = SimRegion.GetRegion(thePrim.RegionHandle);
                }
                return _CurrentRegion;
        }

        public virtual SimPathStore GetPathSystem()
        {
            return GetSimRegion().PathStore;
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



        internal string ToMeshString()
        {
            if (_Mesh != null)
            {
               return _Mesh.ToString();
            }
            return ToString();
        }
    }
}
