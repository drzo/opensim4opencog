using System;
using System.Collections.Generic;
using OpenMetaverse;
using PathSystem3D.Mesher;

namespace PathSystem3D.Navigation
{
    public class RuntimeMesh : MeshedObject
    {
        IComparable ID;
        static SimPathStore system;

        public RuntimeMesh(IComparable id, Box3Fill outer, IList<CollisionObject> inners, SimPathStore paths)
            : base(outer, inners, paths)
        {
            ID = id;
        }

        public override bool Update(SimPosition simObject)
        {
            return true;
        }

        public override void RemeshObject(Box3Fill changed)
        {
            return;
        }

        public override bool IsRegionAttached()
        {
            return true;
        }

        public override bool IsSolid
        {
            get { return system.IsSolidPredicate(ID); }
            set
            {
            }
        }

        public override void AddPos(Vector3 offset)
        {
            throw new NotImplementedException();
        }
    }
}
