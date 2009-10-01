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
            if (pos.IsRegionAttached)
            {
                InitFromPos(pos);
            }
        }

        public override string ToString()
        {
            
            if (reg==null && late!=null && !late.IsRegionAttached )
            {
                return "HEADING: " + late;
            }
            return (reg==null?"?":reg.RegionName) + "/" + pos.X + "/" + pos.Y + "/" + pos.Z + "@" +
                   ZHeading * SimPathStore.RAD2DEG;
        }

        private SimPosition late;
        private Vector3 lateV3;
        SimPathStore reg;
        private Vector3 pos;
        Quaternion rot;

        public SimHeading(SimPathStore reg, Vector3 pos, Quaternion rot)
        {
            this.reg = reg;
            this.pos = pos;
            this.rot = rot;
        }

        public SimHeading(SimPosition pos, Vector3 vector3)
        {
            late = pos;
            lateV3 = vector3;
            if (pos.IsRegionAttached)
            {
                InitFromPos(pos);
            }
        }

        public bool IsPassable
        {
            get { return true; }
            set { throw new NotImplementedException(); }
        }

        public string DistanceVectorString(SimPosition obj)
        {
            if (!obj.IsRegionAttached)
            {
                Vector3 loc = obj.SimPosition;
                SimPathStore R = obj.PathStore;
                return String.Format("unknown relative {0}/{1:0.00}/{2:0.00}/{3:0.00}",
                                     R.RegionName, loc.X, loc.Y, loc.Z);
            }
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
            SimPathStore R = reg;
            return String.Format("{0:0.00}m ", Vector3.Distance(SimPosition, loc))
                   + String.Format("{0}/{1:0.00}/{2:0.00}/{3:0.00}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public Vector3 SimPosition
        {
            get
            {
                if (reg == null && late != null)
                {
                    InitFromPos(late);
                }
                return pos;
            }
        }

        private void InitFromPos(SimPosition position)
        {
            this.reg = position.PathStore;
            this.pos = position.SimPosition + lateV3;
            this.rot = position.SimRotation;
            late = null;
            lateV3 = Vector3.Zero;
        }

        public float GetSizeDistance()
        {
            return 0.2f;
        }

        public bool IsRegionAttached
        {
            get
            {
                if (reg == null && late != null)
                {
                    if (late.IsRegionAttached)
                        InitFromPos(late);
                }
                return reg != null;
            }
        }

        public Quaternion SimRotation
        {
            get
            {
                if (reg == null && late != null)
                {
                    InitFromPos(late);
                }
                return rot;
            }
        }

        public Vector3d GlobalPosition
        {
            get { return PathStore.LocalToGlobal(SimPosition); }
        }

        public SimPathStore PathStore
        {
            get
            {
                if (reg == null && late != null)
                {
                    InitFromPos(late);
                }
                return reg;
            }
        }

        public float ZHeading
        {
            get
            {
                Vector3 v3 = Vector3.Transform(Vector3.UnitX, Matrix4.CreateFromQuaternion(SimRotation));
                return (float)(Math.Atan2(-v3.X, -v3.Y) + Math.PI); // 2Pi= N, 1/2Pi = E
            }
        }

        public SimPosition GetRoot()
        {
            return late;
        }
        public Vector3 GetOffset()
        {
            return lateV3;
        }
    }
}
