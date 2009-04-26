using System;


namespace PathSystem3D.Navigation
{
    /// <summary>
    /// this is the 2D object for pathfinders like A*
    /// </summary>
    public class CollisionPlane
    {

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
        public float WalkZLevel
        {
            //get { return _ZLevelMin; }
            set
            {
                if (value > MaxZ)
                {                   
                    Console.WriteLine("Resizing {0} < {1} ", this, value);
                    NeedsUpdate = true;
                    MaxZ = value;
                }
                else if (value < MinZ)
                {
                      Console.WriteLine("Resizing {0} > {1} > ", this, value);
                    NeedsUpdate = true;
                    MinZ = value;
                }
                if ((MaxZ - MinZ) > 5)
                {
                    //throw new ArgumentException(String.Format("Resizing {0} > {1} > ", this, value));
                }
            }
        }

        internal bool Accepts(float Z)
        {
            if (Z < MaxZ)
            {
                if (Z >= MinZ) return true;
                //if (MaxZ - Z > 3) return false;
            }
            return false;
        }

        public bool NeedsUpdate { get; set; }
        public delegate int NeighborPredicateDelegate(int NX,int XY);
        public int NeighborPredicate(int ix, int iy, int circleSize, NeighborPredicateDelegate param1)
        {
            int count = 0;
            int startx = ix - circleSize;
            int starty = iy - circleSize;
            int endx = ix + circleSize;
            int endy = iy + circleSize;
            if (startx < 0) startx = 0; else if (startx > MaxXPt) startx = MaxXPt;
            if (starty < 0) starty = 0; else if (starty > MaxYPt) starty = MaxYPt;
            if (endx < 0) endx = 0; else if (endx > MaxXPt) endx = MaxXPt;
            if (endy < 0) endy = 0; else if (endy > MaxYPt) endy = MaxYPt;
            for (int x = startx; x <= endx; x++)
                for (int y = starty; y <= endy; y++)
                {
                    if (y != iy && x != ix)
                        count += param1(x, y);
                }
            return count;
        }

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

                
        float[,] _HeightMap;
        public float[,] HeightMap
        {
            get
            {
                if (_HeightMap == null)
                {
                    _HeightMap =(float[,]) PathStore.GroundPlane.Clone();
                }
                return _HeightMap;
            }
        }

        //public byte this[int x, int y]
        //{
        //    get { return ByteMatrix[x, y]; }
        //    set { ByteMatrix[x, y] = value; }
        //}

        public void EnsureUpToDate()
        {
            if (!NeedsUpdate) return;
            UpdateCollisionPlane(UsePotentialFields, AdjacentBlocking);
        }


        public byte DefaultCollisionValue(int x, int y, float ZLevel, byte b, float[,] Heights, CollisionIndex[,] cI)
        {
            CollisionIndex c = cI[x, y];
            if (c!=null)
            {
               // if (c.IsPortal(this)) return SimPathStore.PASSABLE;
            }
            int bumps = NeighborBump(x, y, ZLevel, MaxZ, ZLevel, 0.60f, Heights);
            if (bumps > 0)
                return SimPathStore.BLOCKED;

            bumps = NeighborBump(x, y, ZLevel, MaxZ, ZLevel, 0.40f, Heights);
            if (bumps > 0)
                return SimPathStore.MAYBE_BLOCKED4;

            bumps = NeighborBump(x, y, ZLevel, MaxZ, ZLevel, 0.40f, Heights);
            if (bumps > 0)
                return SimPathStore.MAYBE_BLOCKED;

            float Water = PathStore.WaterHeight;
            if (DiffLessThan(Water, ZLevel, 0.1f))
            {
                return SimPathStore.WATER_Z;
            }
            if (DiffLessThan(Water, PathStore.GroundPlane[x, y], 0.1f))
            {
                return SimPathStore.WATER_G;
            }

            bumps = NeighborBump(x, y, ZLevel, MaxZ, ZLevel, 0.10f, Heights);
            if (bumps > 0)
                return SimPathStore.BLOCKED_YELLOW;

            float MaxZLevel = MaxZ;
            if (MaxZLevel <= ZLevel - 2)
            {
                return SimPathStore.TOO_HIGH;
            }
            //    else if (MinZLevel < gl)
            //      return SimPathStore.MAYBE_BLOCKED;
            if (ZLevel + 20 < MaxZLevel) // needs passable
                return SimPathStore.TOO_LOW;


            
            if (c!=null)
            {
                return c.GetOccupiedValue(ZLevel,ZLevel);
            }
            //if (PathStore.S)
            if (b > 64) // needs passable
                return SimPathStore.INITIALLY;
            return b;
        }

        internal void UpdateCollisionPlane(bool usePotentialFieleds, bool cutNarrows)
        {
            if (false)
            {
                UpdateCollisionPlaneOld(this, usePotentialFieleds, cutNarrows);
                return;
            }
            //PathStore.TaintMatrix();
            //PathStore.BakeTerrain();
            RenderHeightMap();
            {
                float MinZ = this.MinZ;
                CollisionIndex[,] MeshIndex = PathStore.MeshIndex;
                NeedsUpdate = false;
                byte[,] ToMatrix = ByteMatrix;
                float[,] Heights = HeightMap;
                float[,] GroundPlane = PathStore.GroundPlane;

                Console.WriteLine("\nStart UpdateCollisionPlane: {0} for {1}", PathStore, this);
                for (int y = MaxYPt - 1; y > 0; y--)
                {
                    for (int x = MaxXPt - 1; x > 0; x--)
                    {
                        byte b = ToMatrix[x, y];
                        //if (b != SimPathStore.STICKY_PASSABLE)
                        {
                            float ZLevel = Heights[x, y];
                            float GLevel = GroundPlane[x, y];
                            if (ZLevel < GLevel) ZLevel = GLevel;
                            if (b != SimPathStore.STICKY_PASSABLE)
                            {
                                CollisionIndex W = MeshIndex[x, y];
                                if (false && W != null)
                                {
                                    if (ZLevel < MinZ) ZLevel = MinZ;
                                    if (ZLevel > MaxZ) ZLevel = MaxZ;
                                    ToMatrix[x, y] = W.UpdateMatrix(this, ZLevel, ZLevel + 1.7f, Heights);
                                    continue;
                                }
                                else
                                {
                                    ToMatrix[x, y] = DefaultCollisionValue(x, y, ZLevel, b, Heights,MeshIndex);
                                }
                            }
                        }
                    }
                }
                if (usePotentialFieleds) AddFieldEffects(ToMatrix, ToMatrix, SimPathStore.BLOCKED, 1, 1);
                //if (usePotentialFieleds) AddFieldEffects(ToMatrix, ToMatrix, SimPathStore.MAYBE_BLOCKED, 5, 3);
                if (cutNarrows) AddAdjacentBlocking(ToMatrix,2);
                if (cutNarrows) AddAdjacentBlocking(ToMatrix,2);
                if (usePotentialFieleds) AddFieldEffects(ToMatrix, ToMatrix, SimPathStore.BLOCKED, 1, 1);
                if (usePotentialFieleds) AddFieldEffects(ToMatrix, ToMatrix, SimPathStore.MAYBE_BLOCKED, 5, 3);
                if (cutNarrows) AddAdjacentBlocking(ToMatrix,3);
                Console.WriteLine("\nEnd UpdateCollisionPlane: {0} for {1}", PathStore, this);
            }
        }

        internal void UpdateCollisionPlaneOld(CollisionPlane CP, bool usePotentialFieleds, bool cutNarrows)
        {
            RenderHeightMap();
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

                float[,] heights = HeightMap;
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
                        float locaZLevel = heights[x, y];
                        fx = fx - StepSize;
                        byte b = ToMatrix[x, y];
                        if (b != SimPathStore.STICKY_PASSABLE)
                        {

                            CollisionIndex W = MeshIndex[x, y];
                            if (W != null)
                            {
                                ToMatrix[x, y] = W.UpdateMatrix(this, locaZLevel, locaZLevel + 1.7f, heights);
                            }
                            else
                            {
                                locaZLevel = PathStore.GetGroundLevel(fx, fy);
                                ToMatrix[x, y] = CP.DefaultCollisionValue(x, y, locaZLevel, b, heights,MeshIndex);
                                heights[x, y] = locaZLevel;
                            }
                        }
                    }
                }
                if (usePotentialFieleds) AddFieldEffects(FromMatrix, ToMatrix, SimPathStore.BLOCKED, 1, 1);
                if (usePotentialFieleds) AddFieldEffects(FromMatrix, ToMatrix, SimPathStore.MAYBE_BLOCKED, 5, 3);
                if (cutNarrows) AddAdjacentBlocking(ToMatrix,2);
                Console.WriteLine("\nEnd UpdateMatrix: {0} for {1}", PathStore, CP);
            }
        }

        static bool DiffLessThan(float A, float B, float D)
        {
            return Math.Abs(A - B) <= D;
        }

        public static byte NeighborBump(int PX, int PY, float low, float high, float originZ, float mostDiff, float[,] heights)
        {
            float O;

            byte found = 0;
            O = NeighborLevel(low, high, originZ, PX, PY + 1, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            O = NeighborLevel(low, high, originZ, PX + 1, PY + 1, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            O = NeighborLevel(low, high, originZ, PX + 1, PY, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            O = NeighborLevel(low, high, originZ, PX + 1, PY - 1, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            O = NeighborLevel(low, high, originZ, PX, PY - 1, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            O = NeighborLevel(low, high, originZ, PX - 1, PY - 1, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            O = NeighborLevel(low, high, originZ, PX - 1, PY, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            O = NeighborLevel(low, high, originZ, PX - 1, PY + 1, heights);
            if (!DiffLessThan(O, originZ, mostDiff)) found++;

            return found;
        }

        internal static float NeighborLevel(float low, float high,float originZ, int PX, int PY, float[,] heights)
        {
            float n = heights[PX, PY];
          //  if (n < originZ) return originZ;
            return n;
            //CollisionIndex WP = PathStore.MeshIndex[PX, PY];
            //if (WP != null) return WP.GetZLevel(low, high);
            //float x = PX / PathStore.POINTS_PER_METER;
            //float y = PY / PathStore.POINTS_PER_METER;
            //float GL = GetSimRegion().GetGroundLevel(x, y);
            //float CPL = low;
            //return (GL > CPL) ? GL : CPL;

        }


        internal void RenderHeightMap()
        {
            _HeightMap = null;
            RenderGroundPlane();
            CollisionPlane CP = this;
            lock (CP)
            {
                CollisionIndex[,] MeshIndex = PathStore.MeshIndex;
                NeedsUpdate = false;
                byte[,] ToMatrix = ByteMatrix;
                float[,] Heights = HeightMap;
                float[,] GroundPlane = PathStore.GroundPlane;
                PathStore.TaintMatrix();
                Console.WriteLine("\nStart RenderHeightMap: {0} for {1}", PathStore, CP);
                for (int y = MaxYPt; y >= 0; y--)
                {
                    for (int x = MaxXPt; x >= 0; x--)
                    {
                        float testPlane = GroundPlane[x, y];

                        if (testPlane + 1f < MinZ)
                        {
                            testPlane = MinZ - 1;
                        }
                        Heights[x, y] = testPlane;

                        CollisionIndex W = MeshIndex[x, y];
                        if (W != null)
                        {
                            float level;
                            if (W.OpenCapsuleAt(testPlane, testPlane + 6f, CollisionIndex.CapsuleZ, out level))
                            {
                                if (level < testPlane)
                                {
                                   // level = testPlane;
                                }
                                Heights[x, y] = level;
                            } else
                                if (W.OpenCapsuleAt(testPlane, testPlane + 126f, CollisionIndex.CapsuleZ, out level))
                                {
                                    if (level < testPlane)
                                    {
                                        // level = testPlane;
                                    }
                                    Heights[x, y] = level;
                                }
                                else
                                {
                                    Heights[x, y] = W.GetZLevel(testPlane, testPlane + 6f);
                                }
                        }
                    }
                }
            }
            Console.WriteLine("\nEnd RenderHeightMap: {0} for {1}", PathStore, CP);
        }


        private void RenderGroundPlane()
        {
            float LowestCared = MinZ - 2f;
            float[,] heights = HeightMap;
            float[,] GLevels = PathStore.GroundPlane;
            Console.WriteLine("\nStart RenderGroundPlane: {0} for {1} LowestCared={2}", PathStore, this, LowestCared);
            for (int y = MaxYPt; y >= 0; y--)
            {
                for (int x = MaxXPt; x >= 0; x--)
                {
                    float ZLevel = heights[x, y];
                    float GLevel = GLevels[x, y];
                    if (ZLevel < GLevel)
                    {
                        heights[x, y] = GLevel;
                        ZLevel = GLevel;
                    }
                   // if (ZLevel < LowestCared)
                     //   heights[x, y] = LowestCared;
                }
            }
            Console.WriteLine("\nEnd RenderGroundPlane: {0} for {1}", PathStore, this);
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

        private void AddAdjacentBlocking(byte[,] to, int req)
        {
            byte[,] from =(byte[,]) to.Clone();
            int xsizem1 = MaxXPt - 1;
            for (int y = MaxYPt - 1; y > 0; y--)
            {
                for (int x = xsizem1; x > 0; x--)
                {
                    byte b = from[x, y];
                    if (b == SimPathStore.MAYBE_BLOCKED)
                        if (SurroundingBlocked0(x, y, SimPathStore.BLOCKED, from) > req)
                            to[x, y] = SimPathStore.BLOCKED;                        
                }
            }
        }

        public int SurroundingBlocked(int PX, int PY, byte[,] ZMatrix)
        {
            if (PX < 1 || PY < 1 || PX > MaxXPt - 1 || PY > MaxYPt - 1) return 0;
            return SurroundingBlocked0(PX, PY, SimPathStore.BLOCKED, ZMatrix);
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
