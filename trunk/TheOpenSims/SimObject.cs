using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Threading;
using cogbot.TheOpenSims.Navigation;
using cogbot.TheOpenSims.Navigation.Debug;
using cogbot.TheOpenSims.Mesher;
using Simian;

namespace cogbot.TheOpenSims
{

    //TheSims-like object
    public class SimObject : SimPosition, BotMentalAspect
    {

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

        public bool IsPassable
        {
            get
            {
                if (IsPhantom) return true;
                if (!IsRoot() && IsRegionAttached()) return Parent.IsPassable;
                if (IsTypeOf(SimTypeSystem.PASSABLE) != null) return true;
                if (IsRoot() || true) return false;
                if (Parent == null) return true;
                return Parent.IsPassable;
            }
        }
        public bool IsPhantom
        {
            get
            {
                if (MadePhantom) return true;
                if (!IsRoot() && IsRegionAttached()) return Parent.IsPhantom;
                if (IsRoot() || true) return (thePrim.Flags & PrimFlags.Phantom) == PrimFlags.Phantom;
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

        Vector3 lastPos;
        SimWaypoint swp;
        public virtual SimWaypoint GetWaypoint()
        {
            Vector3 v3 = GetUsePosition();
            if (swp == null || !swp.Passable)
            {
                SimPathStore PathStore = WorldSystem.SimPaths;
                swp = PathStore.CreateClosestWaypoint(v3);//, GetSizeDistance() + 1, 7, 1.0f);
                PathStore.EnsureFillSmallSteps(v3.X, v3.Y);
                if (!swp.Passable)
                {
                    float dist = Vector3.Distance(v3, swp.GetSimPosition());
                    swp.EnsureAtLeastOnePath();
                    WorldSystem.output("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
                }
                if (lastPos != v3)
                {
                    lastPos = v3;
                    List<SimObject> objs = GetNearByObjects(3f, false);
                    foreach (SimObject O in objs)
                    {
                        O.UpdatePaths(PathStore);
                    }
                }
                if (!swp.Passable)
                {
                    float dist = Vector3.Distance(v3, swp.GetSimPosition());
                    WorldSystem.output("BAD: " + v3 + " <- " + dist + " -> " + swp + " " + this);
                    swp = PathStore.ClosestNode(v3.X, v3.Y, v3.Y, out dist, false);//, GetSizeDistance() + 1, 7, 1.0f);
                }
            }
            return swp;
            //            return PathStore.CreateClosestWaypointBox(v3, 4f);
        }


        public float Distance(SimPosition prim)
        {
            if (!prim.IsRegionAttached()) return 1300;
            if (!IsRegionAttached()) return 1300;
            return Vector3.Distance(GetSimPosition(), prim.GetSimPosition());
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

        public SimObject(Primitive prim, WorldObjects objectSystem)
            //: base(prim.ID.ToString())
           // :base(prim,null)
        {
            thePrim = prim;
            WorldSystem = objectSystem;
            ObjectType = SimTypeSystem.GetObjectType(prim.ID.ToString());
            UpdateProperties(thePrim.Properties);
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
                        Primitive prim = WorldSystem.GetPrimitive(parent);
                        if (prim == null)
                        {
                            // try to request for next time
                            WorldSystem.EnsureSelected(parent);
                            return null;
                        }
                        _Parent = WorldSystem.GetSimObject(prim);
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

        public float RateIt(BotNeeds needs)
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

        public void UpdateObject(ObjectUpdate objectUpdate, ObjectUpdate objectUpdateDiff)
        {
            SimPathStore PathStore = WorldSystem.SimPaths;
            UpdatePaths(PathStore);
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
                    Primitive pp = WorldSystem.GetPrimitive(ParentId);
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
                Primitive pUse = WorldSystem.GetPrimitive(thePrim.ParentID);
                if (pUse == null)
                {
                    WorldSystem.EnsureSelected(thePrim.ParentID);
                    return false;
                }
            }
            return Parent.IsRegionAttached();
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
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID);
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
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID);
                }
                theLPos = theLPos * theLPrim.Rotation;
                theLPos.Normalize();
            }
            return theLPos;
        }
        public virtual Vector3 GetSimPosition()
        {
            if (!IsRegionAttached()) throw Error("GetSimPosition !IsRegionAttached: " + this);
            Primitive theLPrim = thePrim;
            Vector3 theLPos = theLPrim.Position;
            while (theLPrim.ParentID != 0)
            {
                uint theLPrimParentID = theLPrim.ParentID;
                theLPrim = WorldSystem.GetPrimitive(theLPrimParentID);
                while (theLPrim == null)
                {
                    Thread.Sleep(100);
                    theLPrim = WorldSystem.RequestMissingObject(theLPrimParentID);
                }
                theLPos = theLPrim.Position + Vector3.Transform(theLPos, Matrix4.CreateFromQuaternion(theLPrim.Rotation));
            }
            if (BadLocation(theLPos))
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
            float typeUsageRating = 0.0f;
            foreach (SimTypeUsage use in all)
            {
                float f = ObjectType.RateIt(needs, use);
                if (f > typeUsageRating)
                {
                    typeUsageRating = f;
                    typeUsage = use;
                }
            }
            return typeUsage;
        }

        public Vector3 GetUsePosition()
        {
            SimPathStore PathStore = WorldSystem.SimPaths;
            Vector3 v3 = GetSimPosition();
            byte b = PathStore.GetNodeQuality(v3);
            float useDist = GetSizeDistance();
            if (b > 0) return v3;
            for (float distance = useDist / 2; distance < useDist * 5; distance += 0.25f)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = GetLeftPos(dir, distance);
                    b = PathStore.GetNodeQuality(v3);
                    if (b > 0) return v3;
                }
            }
            Debug("Clearing area");
            for (float distance = 0; distance < useDist * 1.5; distance += 0.25f)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = GetLeftPos(dir, distance);
                    b = PathStore.GetNodeQuality(v3);
                    if (b == 0)
                    {
                        PathStore.SetPassable(v3.X, v3.Y);
                    }
                }
            }
            return GetSimPosition();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="zAngleFromFace"></param>
        /// <param name="distance"></param>
        /// <returns></returns>
        static public Vector3 GetLeftPos(SimPosition pos, int zAngleFromFace, float distance)
        {
            float RAD_TO_DEG = 57.29577951f;
            float Pi2 = (float)(Math.PI * 2.0);

            while (zAngleFromFace > 360)
            {
                zAngleFromFace -= 360;
            }
            while (zAngleFromFace < 0)
            {
                zAngleFromFace += 360;
            }

            float radAngle = zAngleFromFace / RAD_TO_DEG;


            Quaternion rot = pos.GetSimRotation();
            rot.Normalize();
            float rx, ry, rz;
            rot.GetEulerAngles(out rx, out rz, out ry);
            //if (rx != 0f || ry != 0f)
            //{
            //    Debug("180 Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            //else
            //{
            //    Debug("Current Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            float az = rz + radAngle;


            while (az < 0)
            {
                az += Pi2;
            }
            while (az > Pi2)
            {
                az -= Pi2;
            }

            float xmul = (float)Math.Cos(az);
            float ymul = (float)Math.Sin(az);
            Vector3 diff = new Vector3(xmul, ymul, 0) * distance;

            Vector3 result = pos.GetSimPosition() + diff;

            if (result.X > 254f)
            {
                result.X = 254;
            }
            else if (result.X < 1f)
            {
                result.X = 1;
            }
            if (result.Y > 254f)
            {
                result.Y = 254;
            }
            else if (result.Y < 1f)
            {
                result.Y = 1;
            }
            return result;
            /*
             * Client.Self.Movement.SendManualUpdate(AgentManager.ControlFlags.AGENT_CONTROL_AT_POS, Client.Self.Movement.Camera.Position,
                    Client.Self.Movement.Camera.AtAxis, Client.Self.Movement.Camera.LeftAxis, Client.Self.Movement.Camera.UpAxis,
                    Client.Self.Movement.BodyRotation, Client.Self.Movement.HeadRotation, Client.Self.Movement.Camera.Far, AgentFlags.None,
                    AgentState.None, true);*/
        }



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
            float size = 1;

            //            if (IsPhantom) return size;

            float fx = thePrim.Scale.X;
            if (fx > size) size = fx;
            float fy = thePrim.Scale.Y;
            if (fy > size) size = fy;

            foreach (SimObject obj in AttachedChildren)
            {
                Primitive cp = obj.thePrim;
                fx = cp.Scale.X;
                if (fx > size) size = fx;
                fy = cp.Scale.Y;
                if (fy > size) size = fy;
                float childSize = obj.GetSizeDistance();
                if (childSize > size) size = childSize;
            }
            return size;
        }

        public virtual List<SimObject> GetNearByObjects(float maxDistance, bool rootOnly)
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
            List<SimObject> objs2 = WorldSystem.GetNearByObjects(GetSimPosition(), this, maxDistance, rootOnly);
            SortByDistance(objs2);
            return objs2;
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

        public int CompareDistance(Vector3 v1, Vector3 v2)
        {
            Vector3 rp = GetSimPosition();
            return (int)(Vector3.Mag(rp - v1) - Vector3.Mag(rp - v2));
        }

        public string DistanceVectorString(SimPosition obj)
        {
            String str;
            Vector3 loc;
            if (!obj.IsRegionAttached())
            {
                str = "unknown relative ";
                loc = obj.GetUsePosition();
            }
            else
            {
                loc = obj.GetSimPosition();
                float dist = Vector3.Distance(GetSimPosition(), loc);
                if (dist == float.NaN)
                {
                    throw new InvalidCastException("NaN is not a number");
                }
                str = String.Format("{0:0.00}m ", dist);
            }
            return str + String.Format("<{0:0.00}, {1:0.00}, {2:0.00}>", loc.X, loc.Y, loc.Z);
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

        public SimMesh theMesh;

        public string GetMeshInfo()
        {
            if (theMesh == null) theMesh = new SimMesh(this);
            foreach (SimPathStore PS in SimPathStores)
            {
                ForceUpdatePaths(PS);
            }
            return theMesh.GetMeshInfo(Parent.GetSimPosition());

        }

        List<SimPathStore> SimPathStores = new List<SimPathStore>();

        public virtual void UpdatePaths(SimPathStore simPathStore)
        {
            if (!IsRegionAttached())
            {
                //Debug("!IsRegionAttached");
                return;
            }
            // if (IsPassable) return;
            lock (SimPathStores)
            {
                if (SimPathStores.Contains(simPathStore)) return;
                SimPathStores.Add(simPathStore);
            }
            ForceUpdatePaths(simPathStore);
        }


        public void SetPassable(float x, float y)
        {
            SetLocated(x, y);
            SimPathStore PathStore = WorldSystem.SimPaths;
            PathStore.SetPassable(x, y);
        }

        private void SetLocated(float x, float y)
        {
            SimPathStore PathStore = WorldSystem.SimPaths;
            PathStore.SetObjectAt(x, y, this);
        }

        public void SetBlocked(float x, float y)
        {
            SimPathStore PathStore = WorldSystem.SimPaths;
            PathStore.SetBlocked(x, y, this);
        }

        internal void ForceUpdatePaths(SimPathStore simPathStore)
        {
            if (!IsRegionAttached()) return;
            if (theMesh == null) theMesh = new SimMesh(this);
            float SimZLevel = simPathStore.SimZLevel;
            float SimZMaxLevel = simPathStore.SimZMaxLevel;
            Vector3 Position = GetSimPosition();
            //Console.WriteLine("ForceUpdatePaths: "+this);
            // Commented because doors should not "attract"
            //if (IsTypeOf(SimTypeSystem.DOOR) != null)
            //{
            //    theMesh.SetOccupied(SetPassable, SimZLevel, SimZMaxLevel, Position, simPathStore.StepSize);
            //    return;
            //}
            if (IsPassable)
            {
                theMesh.SetOccupied(SetLocated, 10, 33, Position, simPathStore.StepSize);
                return;
            }
            theMesh.SetOccupied(SetBlocked, SimZLevel, SimZMaxLevel, Position, simPathStore.StepSize);
        }

        protected SimRegion _CurrentRegion;
        public virtual SimRegion CurrentRegion
        {
            get
            {
                if (_CurrentRegion == null)
                {
                    _CurrentRegion = WorldSystem.GetRegion(thePrim.RegionHandle);
                }
                return _CurrentRegion;
            }
        }

        public SimPathStore GetPathSystem()
        {
            return CurrentRegion.PathStore;
        }

        internal virtual void UpdatePaths()
        {
            UpdatePaths(GetPathSystem());
        }

        internal Vector3 GetLeftPos(int angle, float Dist)
        {
            return GetLeftPos(this, angle, Dist);
        }
    }
}
