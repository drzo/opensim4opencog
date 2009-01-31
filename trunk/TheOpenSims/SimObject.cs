using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;

namespace cogbot.TheOpenSims
{
    //TheSims-like object
    public class SimObject : BotMentalAspect
    {
        public Primitive thePrim; // the prim in Secondlife
        public readonly WorldObjects WorldSystem;
        Vector3 lastPos = Vector3.Zero;
        public float scaleOnNeeds = 1.11F; // the bonus or handicap the object has compared to the defination (more expensive chair might have more effect)

        List<SimObject> GetNearObjects(float dist)
        {
            List<SimObject> list = new List<SimObject>();
            return list;
        }

        public SimObject(string name, Primitive prim, WorldObjects objectSystem)
            : base(name)
        {
            thePrim = prim;
            WorldSystem = objectSystem;
            ObjectType = SimObjectType.GetObjectType(prim.ID.ToString());
            UpdateProperties(thePrim.Properties);
        }

        public string DebugInfo()
        {
            return ToString();
        }

        public float RateIt(BotNeeds againsNeeds, SimAvatar avatar)
        {
            // GetMenu(avatar);
            return ObjectType.RateIt(againsNeeds, GetDefaultUsage()) * scaleOnNeeds;
        }

        //public List<SimTypeUsage> GetTypeUsages()
        //{
        //  return ObjectType.GetTypeUsages();
        //}

        public List<SimObjectUsage> GetUsages()
        {
            List<SimObjectUsage> uses = new List<SimObjectUsage>();
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
            }
            ObjectType.SuperTypes = SimObjectType.GuessSimObjectTypes(thePrim);
        }

        public bool RestoreEnterable()
        {
            bool changed = false;
            PrimFlags original = thePrim.Flags;
            PrimFlags tempFlags = original;
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
            return changed;
        }

        public bool MakeEnterable()
        {
            bool changed = false;
            PrimFlags original = thePrim.Flags;
            PrimFlags tempFlags = original;
            if ((tempFlags & PrimFlags.Phantom) == 0)
            {
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
            return changed;
        }

        readonly public SimObjectType ObjectType;
        bool MadeNonPhysical = false;
        bool MadePhantom = false;

        public override string ToString()
        {
            String s = base.ToString();
            return s + "(" + ObjectType.ToDebugString() + ")";
        }


        public virtual Vector3 GetSimPosition()
        {
            Primitive theLPrim = thePrim;
            Vector3 theLPos = theLPrim.Position;
            while (theLPrim.ParentID != 0)
            {
                theLPrim = WorldSystem.GetPrimitive(theLPrim.ParentID);
                theLPos = theLPos + theLPrim.Position;
            }
            return theLPos;
        }

        public BotNeeds GetActualUpdate(string p)
        {
            return ObjectType.GetUsageActual(p).Magnify(scaleOnNeeds);
        }


        public SimTypeUsage GetDefaultUsage()
        {
            return ObjectType.GetDefaultUsage();
        }

        public Vector3 GetUsePosition()
        {
            return GetSimPosition();
        }

        internal BotNeeds GetProposedUpdate(string p)
        {
            return ObjectType.GetUsagePromise(p).Magnify(scaleOnNeeds);
        }

        //public SimTypeUsage FindObjectUsage(SimAvatar simAvatar)
        //{
        //    return ObjectType.FindObjectUsage(GetDefaultUsage());
        //}

        public virtual float GetSizeDistance()
        {
            float fx = thePrim.Scale.X;
            float fy = thePrim.Scale.Y;
            return (((fx > fy) ? fx : fy) * 2) + 1;
        }
    }
}
