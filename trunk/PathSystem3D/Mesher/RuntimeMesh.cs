using System;
using System.Collections.Generic;
using PathSystem3D.Mesher;

namespace PathSystem3D.Navigation
{
    public class RuntimeMesh : MeshedObject
    {
        IComparable ID;
        static SimPathStore system;

        public RuntimeMesh(IComparable id, Box3Fill outer, IList<Box3Fill> inners, SimPathStore paths)
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

        public override bool IsPassable
        {
            get { return system.IsPassablePredicate(ID); }
            set { }
        }
    }
}
