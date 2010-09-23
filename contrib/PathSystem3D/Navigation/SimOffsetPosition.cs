using System;
using OpenMetaverse;

namespace PathSystem3D.Navigation
{
    public class SimOffsetPosition : SimPosition
    {
        public SimPosition UsePosition
        {
            get
            {
                return this;
            }
        }

        public override string ToString()
        {
            var reg = PathStore;
            if (reg == null && _late != null && !_late.IsRegionAttached)
            {
                return "HEADING: " + _late;
            }
            var pos = this.SimPosition;
            return (reg == null ? "?" : reg.RegionName) + "/" + pos.X + "/" + pos.Y + "/" + pos.Z + "@" +
                   ZHeading * SimPathStore.RAD2DEG;
        }

        private readonly SimPosition _late;
        private readonly Vector3 _lateV3;

        public SimOffsetPosition(SimPosition pos, Vector3 vector3)
        {
            _late = pos;
            _lateV3 = vector3;
        }

        public bool IsPassable
        {
            get { return true; }
            set { _late.IsPassable = value; }
        }

        public string DistanceVectorString(SimPosition obj)
        {
            if (!obj.IsRegionAttached)
            {
                Vector3 loc = obj.SimPosition;
                SimPathStore R = obj.PathStore;
                return String.Format("unknown relative {0}/{1:0.0#}/{2:0.0#}/{3:0.0#}",
                                     R.RegionName, loc.X, loc.Y, loc.Z);
            }
            return DistanceVectorString(obj.GlobalPosition);
        }

        public string DistanceVectorString(Vector3d loc3d)
        {
            Vector3 loc = SimPathStore.GlobalToLocal(loc3d);
            SimPathStore R = SimPathStore.GetPathStore(loc3d);
            return String.Format("{0:0.0#}m ", Vector3d.Distance(GlobalPosition, loc3d))
                   + String.Format("{0}/{1:0.0#}/{2:0.0#}/{3:0.0#}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public string DistanceVectorString(Vector3 loc)
        {
            SimPathStore R = PathStore;
            return String.Format("{0:0.0#}m ", Vector3.Distance(SimPosition, loc))
                   + String.Format("{0}/{1:0.0#}/{2:0.0#}/{3:0.0#}", R.RegionName, loc.X, loc.Y, loc.Z);
        }

        public Vector3 SimPosition
        {
            get
            {
                return _late.SimPosition + _lateV3;
            }
        }

        public float GetSizeDistance()
        {
            return 0.2f;
        }

        public bool IsRegionAttached
        {
            get
            {
                return _late.IsRegionAttached;
            }
        }

        public Quaternion SimRotation
        {
            get
            {
                return _late.SimRotation;
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
                return _late.PathStore;
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
            return _late;
        }
        public Vector3 GetOffset()
        {
            return _lateV3;
        }
    }
}