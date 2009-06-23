using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace PathSystem3D.Navigation
{
    public class SimHeading : SimPosition
    {
        public static SimHeading UNKNOWN = new SimHeading();
        public SimHeading()
        {
        }
        public SimHeading(SimPosition pos)
        {
            late = pos;
            if (pos.IsRegionAttached())
            {
                InitFromPos(pos);
            }
        }

        public override string ToString()
        {
            
            if (reg==null && late!=null && !late.IsRegionAttached() )
            {
                return "HEADING: " + late;
            }
            return (reg==null?"?":reg.RegionName) + "/" + pos.X + "/" + pos.Y + "/" + pos.Z + "@" +
                   ZHeading * SimPathStore.RAD2DEG;
        }

        private readonly SimPosition late;
        SimPathStore reg;
        private Vector3 pos;
        Quaternion rot;

        public SimHeading(SimPathStore reg, Vector3 pos, Quaternion rot)
        {
            this.reg = reg;
            this.pos = pos;
            this.rot = rot;
        }

        public bool IsPassable
        {
            get { return true; }
            set { throw new NotImplementedException(); }
        }

        public string DistanceVectorString(SimPosition obj)
        {
            if (!obj.IsRegionAttached())
            {
                Vector3 loc = obj.GetSimPosition();
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
            SimPathStore R = reg;
            return String.Format("{0:0.00}m ", Vector3.Distance(GetSimPosition(), loc))
                   + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public Vector3 GetSimPosition()
        {
            if (reg == null && late != null)
            {
                InitFromPos(late);
            }
            return pos;
        }

        private void InitFromPos(SimPosition position)
        {
            this.reg = position.GetPathStore();
            this.pos = position.GetSimPosition();
            this.rot = position.GetSimRotation();
        }

        public float GetSizeDistance()
        {
            return 0.2f;
        }

        public bool IsRegionAttached()
        {
            if (reg == null && late != null)
            {
                InitFromPos(late);
            }
            return reg != null;
        }

        public Quaternion GetSimRotation()
        {
            if (reg == null && late != null)
            {
                InitFromPos(late);
            }
            return rot;
        }

        public Vector3d GetWorldPosition()
        {
            return GetPathStore().LocalToGlobal(GetSimPosition());
        }

        public SimPathStore GetPathStore()
        {
            if (reg == null && late != null)
            {
                InitFromPos(late);
            }
            return reg;
        }

        public float ZHeading
        {
            get
            {
                Vector3 v3 = Vector3.Transform(Vector3.UnitX, Matrix4.CreateFromQuaternion(GetSimRotation()));
                return (float)(Math.Atan2(-v3.X, -v3.Y) + Math.PI); // 2Pi= N, 1/2Pi = E
            }
        }
    }
}
