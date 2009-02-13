using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Threading;
using cogbot.TheOpenSims.Navigation;

namespace cogbot.TheOpenSims
{
    //TheSims-like object
    public class SimObject : BotMentalAspect, SimPosition
    {
        public bool IsPhantom
        {
            get
            {
                if (MadePhantom) return true;
                return (thePrim.Flags & PrimFlags.Phantom) == PrimFlags.Phantom;
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

        public SimWaypoint GetWaypoint()
        {
            Vector3 v3 = GetSimPosition();
            SimWaypoint swp = WorldSystem.SimPaths.CreateClosestWaypointBox(v3,GetSizeDistance()+4);
            float dist = Vector3.Distance(v3, swp.GetSimPosition());
            if (dist > GetSizeDistance())
            {
               WorldSystem.output("CreateClosestWaypoint: " +v3+ " <- " + dist + " -> " + swp + " " + this );
            }
            return swp;
//            return WorldSystem.SimPaths.CreateClosestWaypointBox(v3, 4f);
        }

        public SimRoute[] GetRouteList(SimWaypoint to, out bool IsFake)
        {
            SimWaypoint from = this.GetWaypoint();
            return WorldSystem.SimPaths.GetRoute(from, to, out IsFake);
        }


        public float Distance(SimPosition prim)
        {
            if (!prim.CanGetSimPosition()) return 1300;
            if (!CanGetSimPosition()) return 1300;
            return Vector3.Distance(GetSimPosition(), prim.GetSimPosition());
        }

        readonly public Primitive thePrim; // the prim in Secondlife
        readonly public SimObjectType ObjectType;
        public WorldObjects WorldSystem;
        bool MadeNonPhysical = false;
        bool MadePhantom = false;
        bool needUpdate = true;


        public SimObjectType IsTypeOf(SimObjectType superType)
        {
            return ObjectType.IsSubType(superType);
        }

        public ListAsSet<SimObject> AttachedChildren = new ListAsSet<SimObject>();

        public ListAsSet<SimObject> GetChildren()
        {
            if (AttachedChildren.Count == 0)
            {
            }
            return AttachedChildren;
        }

        /// <summary>
        /// the bonus or handicap the object has compared to the defination 
        /// (more expensive chair might have more effect)
        /// </summary>
        public float scaleOnNeeds = 1.11F; 

        public SimObject(Primitive prim, WorldObjects objectSystem)
            : base(prim.ID.ToString())
        {
            thePrim = prim;
            WorldSystem = objectSystem;
            ObjectType = SimTypeSystem.GetObjectType(prim.ID.ToString());
            UpdateProperties(thePrim.Properties);
           // GetParent(); // at least request it
        }

        SimObject Parent = null; // null means unknown if we IsRoot then Parent == this;

        public virtual SimObject GetParent()
        {
            if (Parent == null)
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
                    Parent = WorldSystem.GetSimObject(prim);
                    Parent.AddChild(this);
                }
                else
                {
                    Parent = this;
                }
            }
            return Parent;
        }

        public bool AddChild(SimObject simObject)
        {
            needUpdate = true;
            simObject.Parent = this;
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
            return ObjectType.IsComplete() ;
        }

        public virtual bool IsRoot()
        {
            if (thePrim.ParentID == 0) return true;
            GetParent();
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
                if (thePrim.OwnerID == avatar.theAvatar.ID ) { mask = thePrim.Properties.Permissions.OwnerMask; }
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
            if (objectProperties != null)
            {
                ObjectType.SitName = objectProperties.SitName;
                ObjectType.TouchName = objectProperties.TouchName;
                needUpdate = false;
            }
            try
            {
              //  GetParent();
                AddSuperTypes(SimTypeSystem.GuessSimObjectTypes(objectProperties));
            }
            catch (Exception e)
            {
                Debug(""+e);
            }

        }

        public virtual bool IsFloating {
            get
            {
                return !IsPhysical;
            }
            set
            {
                IsPhysical = !value;
            }
        }

        public void UpdateObject(ObjectUpdate objectUpdate)
        {
        }

        private void AddSuperTypes(IList<SimObjectType> listAsSet)
        {
            //SimObjectType UNKNOWN = SimObjectType.UNKNOWN;
            foreach (SimObjectType type in listAsSet)
            {
                ObjectType.AddSuperType(type);
            }
        }

        public virtual bool RestoreEnterable()
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
                if (GetParent().RestoreEnterable()) return true;
            }
            return changed;
        }

        public virtual bool MakeEnterable()
        {
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
                if (GetParent().MakeEnterable()) return true;
            }
            return changed;

        }

        public override string ToString()
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
                    str += WorldSystem.GetPrimTypeName(pp) + " " + pp.ID.ToString().Substring(0,8);
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
            return str.Replace("  ", " ").Replace(") (", ")(");
        }

        private string SuperTypeString()
        {
            String str = "[";
            ObjectType.SuperType.ForEach(delegate(SimObjectType item)
            {
                str += item.GetTypeName() + " ";
            });
            return str.TrimEnd() + "]";
        }

        public bool CanGetSimPosition()
        {
            if (IsRoot()) return true;
            Primitive pUse = WorldSystem.GetPrimitive(thePrim.ParentID);
            if (pUse == null)
            {
                WorldSystem.EnsureSelected(thePrim.ParentID);
                return false;
            }
            return GetParent().CanGetSimPosition();
        }

        public virtual Vector3 GetSimPosition()
        {
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
                theLPos = theLPos + theLPrim.Position;
            }
            if (theLPos.Z < -20.0f)
            {
                Debug("-------------------------" + this + " shouldnt be at " + theLPos);
                WorldSystem.DeletePrim(thePrim);
            }
            return theLPos;
        }


        public virtual Quaternion GetSimRotation()
        {
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
                theLPos = theLPos + theLPrim.Rotation;
                theLPos.Normalize();
            }
            return theLPos;        
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
            return GetSimPosition();
        }

        internal BotNeeds GetProposedUpdate(string pUse)
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
            }
            return size;
        }

        public List<SimObject> GetNearByObjects(float maxDistance, bool rootOnly)
        {
            if (!CanGetSimPosition())
            {
                List<SimObject> objs = new List<SimObject>();
                GetParent();
                if (Parent != null && Parent != this)
                {
                    objs.Add(Parent);
                }
                return objs;
            }
            List<SimObject> objs2 = GetNearByObjects(GetSimPosition(), WorldSystem, this, maxDistance, rootOnly);            
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



        internal static List<SimObject> GetNearByObjects(Vector3 here, WorldObjects WorldSystem, object thiz, float pUse, bool rootOnly)
        {
            List<SimObject> nearby = new List<SimObject>();
             foreach (SimObject obj in WorldSystem.GetAllSimObjects().CopyOf()) 
            {
                if (!(rootOnly && !obj.IsRoot() && !obj.IsTyped()))
                if (obj != thiz && obj.CanGetSimPosition() && Vector3.Distance(obj.GetSimPosition(), here) <= pUse)
                    nearby.Add(obj);
            };
            return nearby;
        }

        public virtual bool Matches(string name)
        {
            return SimTypeSystem.MatchString(ToString(),name);
        }
        public virtual void Debug(string p)
        {
            WorldSystem.output(thePrim+":"+ p);
        }

        internal void SortByDistance(List<SimObject> sortme)
        {
            sortme.Sort(CompareDistance);
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
            if (!obj.CanGetSimPosition())
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
                if (s.Length > 8) return s;
            }
            return ToString();
        }
    }
}
