using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Threading;

namespace cogbot.TheOpenSims
{
    //TheSims-like object
    public class SimObject : BotMentalAspect
    {
        readonly public Primitive thePrim; // the prim in Secondlife
        readonly public SimObjectType ObjectType;
        public WorldObjects WorldSystem;
        bool MadeNonPhysical = false;
        bool MadePhantom = false;
        bool needUpdate = true;
        Vector3 lastPos = Vector3.Zero;

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

        public SimObject(string name, Primitive prim, WorldObjects objectSystem)
            : base(name)
        {
            thePrim = prim;
            WorldSystem = objectSystem;
            ObjectType = SimObjectType.GetObjectType(prim.ID.ToString());
            UpdateProperties(thePrim.Properties);
        }

        SimObject Parent;

        public virtual SimObject GetParent()
        {
            if (Parent == null)
            {
                uint parent = thePrim.ParentID;
                if (parent != 0)
                {
                    Primitive prim = WorldSystem.GetPrimitive(parent);
                    while (prim == null)
                    {
                        prim = WorldSystem.RequestMissingObject(parent);
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
            bool b = AttachedChildren.AddTo(simObject);
            if (b) UpdateProperties(simObject.thePrim.Properties);
            return b;

        }

        public virtual bool IsRoot()
        {
            return (thePrim.ParentID == 0);
        }

        public virtual string DebugInfo()
        {
            string str = ToString();
            if (thePrim.ParentID != 0)
                return thePrim.ParentID + " " + str;
            return str;
        }

        public float RateIt(SimAvatar avatar)
        {
            return ObjectType.RateIt(avatar.CurrentNeeds, GetBestUse(avatar)) * scaleOnNeeds;
        }

        //public List<SimTypeUsage> GetTypeUsages()
        //{
        //  return ObjectType.GetTypeUsages();
        //}

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
            }
            catch (Exception e)
            {
                Console.WriteLine(""+e);
            }

            AddSuperTypes(SimObjectType.GuessSimObjectTypes(objectProperties));
        }

        public void UpdateObject(ObjectUpdate objectUpdate)
        {
        }

        private void AddSuperTypes(ListAsSet<SimObjectType> listAsSet)
        {
            SimObjectType UNKNOWN = SimObjectType.UNKNOWN;
            ListAsSet<SimObjectType> orig = ObjectType.SuperTypes;
            lock (orig)
                foreach (SimObjectType type in listAsSet)
                {
                    orig.AddTo(type);
                }            
        }

        public virtual bool RestoreEnterable()
        {
            bool changed = false;
            PrimFlags tempFlags = thePrim.Flags;
            if (MadePhantom && (tempFlags & PrimFlags.Phantom) != PrimFlags.Phantom)
            {
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
            String str = base.ToString();
            if (thePrim.Properties != null)
            {
                if (!String.IsNullOrEmpty(thePrim.Properties.Name))
                    str += thePrim.Properties.Name + " ";
                if (!String.IsNullOrEmpty(thePrim.Properties.Description))
                    str += thePrim.Properties.Description + " ";
            }
            str += "(" + AttachedChildren.Count + ")[";
            ObjectType.SuperTypes.ForEach(delegate(SimObjectType item)
            {
                str += item.ToString() + " ";
            });
            return str.Trim() + "]";
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
            //            LastPositionSent = theLPos;
            if (theLPos.Z < -20.0f)
            {
                Console.WriteLine("-------------------------" + this + " shouldnt be at " + theLPos);
                WorldSystem.DeletePrim(thePrim);
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


        public SimTypeUsage GetBestUse(SimAvatar avatar)
        {
            if (needUpdate)
            {
                UpdateProperties(thePrim.Properties);
            }

            ListAsSet<SimTypeUsage> all = ObjectType.GetTypeUsages();
            if (all.Count == 0) return null;
            SimTypeUsage typeUsage = all[0];
            float typeUsageRating = 0.0f;
            foreach (SimTypeUsage use in all)
            {
                float f = ObjectType.RateIt(avatar.CurrentNeeds, use);
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

        public ListAsSet<SimObject> GetNearByObjects(float pUse, bool rootOnly)
        {
            ListAsSet<SimObject> KnowsAboutList = new ListAsSet<SimObject>();
            List<SimObject> objects = WorldSystem.GetAllSimObjects();
            Vector3 here = GetSimPosition();
            lock (objects) foreach (SimObject obj in objects)
                {
                    if (rootOnly && !obj.IsRoot()) continue; 
                    if (obj !=this && obj.CanGetSimPosition() && Vector3.Distance(obj.GetSimPosition(), here) <= pUse)
                        KnowsAboutList.AddTo(obj);
                    
                }
            return KnowsAboutList;
        }

        public Quaternion GetSimRotation()
        {
            return thePrim.Rotation;
        }

    }
}
