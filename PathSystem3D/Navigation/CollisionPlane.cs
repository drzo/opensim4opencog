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

                
        float[,] _GroundPlane;
        public float[,] GroundPlane
        {
            get
            {
                if (_GroundPlane == null)
                {
                    NeedsUpdate = true;
                    _GroundPlane = new float[MaxXPt + 1, MaxYPt + 1];
                    for (int y = MaxYPt; y >= 0; y--)
                        for (int x = MaxXPt; x >= 0; x--)
                            _GroundPlane[x, y] = MinZ - 0.5f;
                }
                return _GroundPlane;
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
            PathStore.TaintMatrix();
            PathStore.BakeTerrain();
            RenderGroundPlane();
            lock (CP)
            {
                float StepSize = PathStore.StepSize;
                CollisionIndex[,] MeshIndex = PathStore.MeshIndex;
                NeedsUpdate = false;
                byte[,] ToMatrix = ByteMatrix;
                float[,] GP = GroundPlane;
     
                float MinZLevel = CP.MinZ - 1;
                float MaxZLevel = CP.MaxZ + StepSize;

                Console.WriteLine("\nStart UpdateCollisionPlane: {0} for {1}", PathStore, CP);
                float fy = 256f;
                for (int y = MaxYPt-1; y > 0; y--)
                {
                    fy = fy - StepSize;
                    float fx = 256f;
                    for (int x = MaxXPt - 1; x > 0; x--)
                    {
                        byte b = ToMatrix[x, y];
                        float ZLevel = GP[x, y];
                        int bumps = NeighborBump(x, y, MinZLevel, MaxZLevel, ZLevel, 0.56f, GP);                                               
                        if (bumps > 2)
                            ToMatrix[x, y] = SimPathStore.BLOCKED;
                        else if (bumps > 1)
                            ToMatrix[x, y] = SimPathStore.MAYBE_BLOCKED;
                        else if (bumps > 0)
                            ToMatrix[x, y] = SimPathStore.MAYBE_BLOCKED;
                        else
                            if (b != SimPathStore.STICKY_PASSABLE)
                            {
                                CollisionIndex W = MeshIndex[x, y];
                                if (W != null)
                                {
                                    ToMatrix[x, y] = W.GetOccupiedValue(MinZLevel, MaxZLevel);
                                }
                                else
                                {
                                    ToMatrix[x, y] = SimPathStore.INITIALLY;
                                }
                            }
                    }
                }
                if (usePotentialFieleds) AddFieldEffects(ToMatrix, ToMatrix, SimPathStore.BLOCKED, 1, 1);
                if (usePotentialFieleds) AddFieldEffects(ToMatrix, ToMatrix, SimPathStore.MAYBE_BLOCKED, 5, 3);
                if (cutNarrows) AddAdjacentBlocking(ToMatrix);
                Console.WriteLine("\nEnd UpdateCollisionPlane: {0} for {1}", PathStore, CP);
            }
        }

        internal void UpdateCollisionPlaneOld(CollisionPlane CP, bool usePotentialFieleds, bool cutNarrows)
        {
            RenderGroundPlane();
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

                float[,] GP = GroundPlane;
                PathStore.BakeTerrain();
                // float MinZLevel = CP.MinZ - 1;
                //float MaxZLevel = CP.MaxZ + StepSize;

                PathStore.TaintMatrix();
                Console.WriteLine("\nStart UpdateMatrix: {0} for {1}", PathStore, CP);
                float fy = 256f;
                for (int y = MaxYPt; y >= 0; y--)
                {
                    fy = fy - StepSize;
                    float fx = 256f;
                    for (int x = MaxXPt; x >= 0; x--)
                    {
                        float ZLevel = GP[x, y];
                        fx = fx - StepSize;
                        byte b = ToMatrix[x, y];
                        if (b != SimPathStore.STICKY_PASSABLE)
                        {

                            CollisionIndex W = MeshIndex[x, y];
                            if (W != null)
                            {
                                ZLevel = W.UpdateMatrix(this, ZLevel, ZLevel - 0.5f, ZLevel + 1.7f, GP);
                                GP[x, y] = ZLevel;
                            }
                            else
                            {
                                ZLevel = PathStore.GetGroundLevel(fx, fy);
                                ToMatrix[x, y] = CP.DefaultCollisionValue(ZLevel, b);
                                GP[x, y] = ZLevel;
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

        static bool DiffLessThan(float A, float B, float D)
        {
            return Math.Abs(A - B) <= D;
        }

        public static byte NeighborBump(int PX, int PY, float low, float high, float original, float mostDiff, float[,] GP)
        {
            float O;

            byte found = 0;
            O = NeighborLevel(low, high, PX, PY + 1, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            O = NeighborLevel(low, high, PX + 1, PY + 1, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            O = NeighborLevel(low, high, PX + 1, PY, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            O = NeighborLevel(low, high, PX + 1, PY - 1, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            O = NeighborLevel(low, high, PX, PY - 1, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            O = NeighborLevel(low, high, PX - 1, PY - 1, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            O = NeighborLevel(low, high, PX - 1, PY, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            O = NeighborLevel(low, high, PX - 1, PY + 1, GP);
            if (!DiffLessThan(O, original, mostDiff)) found++;

            return found;
        }

        internal static float NeighborLevel(float low, float high, int PX, int PY, float[,] GP)
        {
            return GP[PX, PY];
            //CollisionIndex WP = PathStore.MeshIndex[PX, PY];
            //if (WP != null) return WP.GetZLevel(low, high);
            //float x = PX / PathStore.POINTS_PER_METER;
            //float y = PY / PathStore.POINTS_PER_METER;
            //float GL = GetSimRegion().GetGroundLevel(x, y);
            //float CPL = low;
            //return (GL > CPL) ? GL : CPL;

        }


        internal void RenderGroundPlane()
        {
            CollisionPlane CP = this;
            lock (CP)
            {
                float StepSize = PathStore.StepSize;
                CollisionIndex[,] MeshIndex = PathStore.MeshIndex;
                NeedsUpdate = false;
                byte[,] ToMatrix = ByteMatrix;
                byte[,] FromMatrix = CP.ByteMatrix;
                float[,] GP = GroundPlane;

                PathStore.TaintMatrix();
                Console.WriteLine("\nStart RenderPlane: {0} for {1}", PathStore, CP);
                float fy = 256f;
                for (int y = MaxYPt; y >= 0; y--)
                {
                    fy = fy - StepSize;
                    float fx = 256f;
                    for (int x = MaxXPt; x >= 0; x--)
                    {
                        float ZLevel = GP[x, y];
                        fx = fx - StepSize;
                        byte b = ToMatrix[x, y];
                        if (b != SimPathStore.STICKY_PASSABLE)
                        {

                            CollisionIndex W = MeshIndex[x, y];
                            if (W != null)
                            {
                                ZLevel = W.GetZLevel(ZLevel - 0.5f, ZLevel + 1.7f);
                                GP[x, y] = ZLevel;
                            }
                            else
                            {
                                ZLevel = PathStore.GetGroundLevel(fx, fy);
                                //ToMatrix[x, y] = CP.DefaultCollisionValue(ZLevel, b);
                                GP[x, y] = ZLevel;
                            }
                        }
                    }
                }
                Console.WriteLine("\nEnd RenderPlane: {0} for {1}", PathStore, CP);
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

        public float[,] ResizeTerrain512Interpolation(float[,] heightMap, int m_regionWidth, int m_regionHeight)
        {
            int THE512 = m_regionWidth * 5;
            int THE2 = 5;
            float[,] returnarr = new float[MaxXPt+1,MaxYPt+1];
            float[,] resultarr = new float[m_regionWidth, m_regionHeight];

            // Filling out the array into it's multi-dimentional components
            for (int y = 0; y < m_regionHeight; y++)
            {
                for (int x = 0; x < m_regionWidth; x++)
                {
                    resultarr[y, x] = heightMap[m_regionWidth, m_regionHeight];
                }
            }

            // Resize using interpolation

            // This particular way is quick but it only works on a multiple of the original

            // The idea behind this method can be described with the following diagrams
            // second pass and third pass happen in the same loop really..  just separated
            // them to show what this does.

            // First Pass
            // ResultArr:
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1
            // 1,1,1,1,1,1

            // Second Pass
            // ResultArr2:
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,
            // ,,,,,,,,,,
            // 1,,1,,1,,1,,1,,1,

            // Third pass fills in the blanks
            // ResultArr2:
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1
            // 1,1,1,1,1,1,1,1,1,1,1,1

            // X,Y = .
            // X+1,y = ^
            // X,Y+1 = *
            // X+1,Y+1 = #

            // Filling in like this;
            // .*
            // ^#
            // 1st .
            // 2nd *
            // 3rd ^
            // 4th #
            // on single loop.

            float[,] resultarr2 = new float[THE512, THE512];
            for (int y = 0; y < m_regionHeight; y++)
            {
                for (int x = 0; x < m_regionWidth; x++)
                {
                    resultarr2[y * THE2, x * THE2] = resultarr[y, x];

                    if (y < m_regionHeight)
                    {
                        if (y + 1 < m_regionHeight)
                        {
                            if (x + 1 < m_regionWidth)
                            {
                                resultarr2[(y * THE2) + 1, x * THE2] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                               resultarr[y, x + 1] + resultarr[y + 1, x + 1]) / 4);
                            }
                            else
                            {
                                resultarr2[(y * THE2) + 1, x * THE2] = ((resultarr[y, x] + resultarr[y + 1, x]) / THE2);
                            }
                        }
                        else
                        {
                            resultarr2[(y * THE2) + 1, x * THE2] = resultarr[y, x];
                        }
                    }
                    if (x < m_regionWidth)
                    {
                        if (x + 1 < m_regionWidth)
                        {
                            if (y + 1 < m_regionHeight)
                            {
                                resultarr2[y * THE2, (x * THE2) + 1] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                               resultarr[y, x + 1] + resultarr[y + 1, x + 1]) / 4);
                            }
                            else
                            {
                                resultarr2[y * THE2, (x * THE2) + 1] = ((resultarr[y, x] + resultarr[y, x + 1]) / THE2);
                            }
                        }
                        else
                        {
                            resultarr2[y * THE2, (x * THE2) + 1] = resultarr[y, x];
                        }
                    }
                    if (x < m_regionWidth && y < m_regionHeight)
                    {
                        if ((x + 1 < m_regionWidth) && (y + 1 < m_regionHeight))
                        {
                            resultarr2[(y * THE2) + 1, (x * THE2) + 1] = ((resultarr[y, x] + resultarr[y + 1, x] +
                                                                 resultarr[y, x + 1] + resultarr[y + 1, x + 1]) / 4);
                        }
                        else
                        {
                            resultarr2[(y * THE2) + 1, (x * THE2) + 1] = resultarr[y, x];
                        }
                    }
                }
            }
            //Flatten out the array
            int i = 0;
            for (int y = 0; y < THE512; y++)
            {
                for (int x = 0; x < THE512; x++)
                {
                    if (Single.IsNaN(resultarr2[y, x]) || Single.IsInfinity(resultarr2[y, x]))
                    {
                      //  Logger.Log("[PHYSICS]: Non finite heightfield element detected.  Setting it to 0", Helpers.LogLevel.Warning);
                        resultarr2[y, x] = 0;
                    }

                    if (resultarr2[y, x] <= 0)
                    {
                        returnarr[y,x] = 0.0000001f;

                    }
                    else
                        returnarr[y,x] = resultarr2[y, x];

                    i++;
                }
            }

            return returnarr;
        }
    }

}
