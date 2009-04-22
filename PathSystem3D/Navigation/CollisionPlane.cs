using System;


namespace PathSystem3D.Navigation
{
    /// <summary>
    /// this is the 2D object for pathfinders like A*
    /// </summary>
    public class CollisionPlane
    {

        private FloatRange _Range = FloatRange.ALL;

        public FloatRange Range
        {
            get {
                if (_Range == FloatRange.ALL)
                {
                    _Range = new FloatRange(MinZ, MaxZ);
                }
                return _Range; }
            set { _Range = value; }
        }
        static int TotalCollisionPlanes = 0;
        internal SimPathStore PathStore;
        int MaxXPt, MaxYPt;
        private bool _UsePotentialFields = true;

        public bool UsePotentialFields
        {
            get { return _UsePotentialFields; }
            set
            {
                if (!_UsePotentialFields == value)
                {
                    _UsePotentialFields = value;
                    NeedsUpdate = true;
                }
            }
        }
        private bool _AdjacentBlocking = true;

        public bool AdjacentBlocking
        {
            get { return _AdjacentBlocking; }
            set
            {
                if (!_AdjacentBlocking == value)
                {
                    _AdjacentBlocking = value;
                    NeedsUpdate = true;
                }
            }
        }

        public CollisionPlane(int xsize0, int ysize0, float minZ, float maxZ, SimPathStore pathStore)
        {
            TotalCollisionPlanes++;
            PathStore = pathStore;
            MaxXPt = xsize0 - 1;
            MaxYPt = ysize0 - 1;
            MinZ = minZ;
            MaxZ = maxZ;
        }

        public float MinZ { get; private set; }
        public float ZLevel
        {
            //get { return _ZLevelMin; }
            set
            {
                if (value > MaxZ)
                {
                    //   Console.WriteLine("Resizing {0} < {1} ", this, value);
                    NeedsUpdate = true;
                    MaxZ = value;
                }
                else if (value < MinZ)
                {
                    //   Console.WriteLine("Resizing {0} > {1} > ", this, value);
                    NeedsUpdate = true;
                    MinZ = value;
                }
                if ((MaxZ - MinZ) > 5)
                {
                    throw new ArgumentException(String.Format("Resizing {0} > {1} > ", this, value));
                }
            }
        }

        internal bool Accepts(float Z)
        {
            if (Z < MaxZ)
            {
                if (Z > MinZ) return true;
                if (MaxZ - Z > 3) return false;
            }
            else
                if (Z > MaxZ)
                {
                    if (Z - MinZ > 3) return false;
                }
            return true;
        }

        public bool NeedsUpdate { get; set; }
        public override string ToString()
        {
            return MinZ + "-" + MaxZ;
        }
        byte[,] _BM;

        public float MaxZ { get; private set; }

        public byte[,] ByteMatrix
        {
            get
            {
                if (_BM == null)
                {
                    NeedsUpdate = true;
                    _BM = new byte[MaxXPt+1, MaxYPt+1];
                    for (int y = MaxYPt; y >= 0; y--)
                        for (int x = MaxXPt; x >= 0; x--)
                            _BM[x, y] = SimPathStore.INITIALLY;
                }
                return _BM;
            }

        }
        public byte this[int x, int y]
        {
            get { return ByteMatrix[x, y]; }
            set { ByteMatrix[x, y] = value; }
        }

        public void EnsureUpToDate()
        {
            if (!NeedsUpdate) return;
            UpdateCollisionPlane(this, UsePotentialFields, AdjacentBlocking);
        }


        public byte DefaultCollisionValue(float gl, byte b)
        {
         //   return SimPathStore.INITIALLY;
            gl -= 0.001f;
            float MinZLevel = MinZ;
            float MaxZLevel = MaxZ;
            if (MaxZLevel <= gl)
            {
                return SimPathStore.BLOCKED;
            }
            else if (MinZLevel < gl)
                return SimPathStore.MAYBE_BLOCKED;
            else if (gl + 20 < MaxZLevel) // needs passable
                 return SimPathStore.MAYBE_BLOCKED;
            else if (b > 64) // needs passable
                return SimPathStore.INITIALLY;
            return b;
        }

        internal void UpdateCollisionPlane(CollisionPlane CP, bool usePotentialFieleds, bool cutNarrows)
        {
            lock (CP)
            {
                float StepSize = PathStore.StepSize;
                CollisionIndex[,] MeshIndex = PathStore.MeshIndex;
                NeedsUpdate = false;
                byte[,] ToMatrix = ByteMatrix;
                byte[,] FromMatrix = CP.ByteMatrix;
                // In case its not this
                if (FromMatrix != ToMatrix)
                {
                    CP.EnsureUpToDate();
                    CopyFromMatrix(FromMatrix);
                }

                PathStore.BakeTerrain();
                float MinZLevel = CP.MinZ - 1;
                float MaxZLevel = CP.MaxZ + StepSize;
                
                PathStore.TaintMatrix();
                Console.WriteLine("\nStart UpdateMatrix: {0} for {1}", PathStore, CP);
                float fy = 256f;
                for (int y = MaxYPt; y >= 0; y--)
                {
                    fy = fy - StepSize;
                    float fx = 256f;
                    for (int x = MaxXPt; x >= 0; x--)
                    {
                        fx = fx - StepSize;
                        byte b = ToMatrix[x, y];
                        if (b != SimPathStore.STICKY_PASSABLE)
                        {
                            CollisionIndex W = MeshIndex[x, y];
                            if (W != null)
                                W.UpdateMatrix(this, MinZLevel);
                            else
                            {
                                ToMatrix[x, y] = CP.DefaultCollisionValue(PathStore.GetGroundLevel(fx, fy), b);
                            }
                        }
                    }
                }
                if (usePotentialFieleds) AddFieldEffects(FromMatrix, ToMatrix, SimPathStore.BLOCKED, 1, 1);
                if (usePotentialFieleds) AddFieldEffects(FromMatrix, ToMatrix, SimPathStore.MAYBE_BLOCKED, 5, 3);
                if (cutNarrows) AddAdjacentBlocking(ToMatrix);
                Console.WriteLine("\nEnd UpdateMatrix: {0} for {1}", PathStore, CP);
            }
        }

        private void CopyFromMatrix(byte[,] FromMatrix)
        {
            throw new NotImplementedException();
        }

        private void AddFieldEffects(byte[,] from, byte[,] to, byte fronteer, int iterations, byte step)
        {
            int xsizem1 = MaxXPt - 1;
            while (iterations-- > 0)
            {
                byte self = (byte)(fronteer - step);
                for (int y = MaxYPt - 1; y > 0; y--)
                {
                    for (int x = xsizem1; x > 0; x--)
                    {
                        byte b = from[x, y];
                        if (b > 2 && b < fronteer)
                            if (SurroundingBlocked0(x, y, fronteer, from) > 1)
                                to[x, y] = self;
                    }
                }
                fronteer -= step;
            }
        }

        private void AddAdjacentBlocking(byte[,] to)
        {
            byte[,] from =(byte[,]) to.Clone();
            int xsizem1 = MaxXPt - 1;
            for (int y = MaxYPt - 1; y > 0; y--)
            {
                for (int x = xsizem1; x > 0; x--)
                {
                    byte b = from[x, y];
                    if (b == SimPathStore.MAYBE_BLOCKED)
                        if (SurroundingBlocked0(x, y, SimPathStore.BLOCKED, from) > 2)
                            to[x, y] = SimPathStore.BLOCKED;                        
                }
            }
        }

        public bool SurroundingBlocked(int PX, int PY, byte[,] ZMatrix)
        {
            if (PX < 1 || PY < 1 || PX > MaxXPt - 1 || PY > MaxYPt - 1) return false;
            return SurroundingBlocked0(PX, PY, SimPathStore.BLOCKED, ZMatrix) > 0;
        }

        /// <summary>
        ///  Private due to unchecked index
        /// </summary>
        /// <param name="PX"></param>
        /// <param name="PY"></param>
        /// <returns></returns>
        private static int SurroundingBlocked0(int PX, int PY, byte someValue, byte[,] ZMatrix)
        {
            int found = 0;
            byte O;

            O = ZMatrix[PX, PY + 1];
            if (O == someValue) found++;

            O = ZMatrix[PX + 1, PY + 1];
            if (O == someValue) found++;

            O = ZMatrix[PX + 1, PY];
            if (O == someValue) found++;

            O = ZMatrix[PX + 1, PY - 1];
            if (O == someValue) found++;

            O = ZMatrix[PX, PY - 1];
            if (O == someValue) found++;

            O = ZMatrix[PX - 1, PY - 1];
            if (O == someValue) found++;

            O = ZMatrix[PX - 1, PY];
            if (O == someValue) found++;

            O = ZMatrix[PX - 1, PY + 1];
            if (O == someValue) found++;

            return found;
        }
    }
}
