using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims.Mesher;
using OpenMetaverse;

namespace cogbot.TheOpenSims.Navigation
{
    /// <summary>
    /// this is the 2D object for pathfinders like A*
    /// </summary>
    public class CollisionPlane
    {

        static int TotalCollisionPlanes = 0;
        SimPathStore PathStore;
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
        private bool _AdjacentBlocking = false;

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

        public CollisionPlane(int xsize0, int ysize0, float z, SimPathStore pathStore)
        {
            TotalCollisionPlanes++;
            PathStore = pathStore;
            MaxXPt = xsize0 - 1;
            MaxYPt = ysize0 - 1;
            MinZ = z;
            MaxZ = z;
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
            return GetType().Name + "=" + MinZ + "-" + MaxZ;
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
            NeedsUpdate = false;
            UpdateCollisionPlane(this, UsePotentialFields, AdjacentBlocking);
        }


        internal void UpdateCollisionPlane(CollisionPlane CP, bool usePotentialFieleds, bool cutNarrows)
        {
            float StepSize = PathStore.StepSize;
            CollisionIndex[,] MeshIndex = PathStore.MeshIndex;
            lock (CP)
            {
                NeedsUpdate = false;
                byte[,] ToMatrix = ByteMatrix;
                byte[,] FromMatrix = CP.ByteMatrix;
                // In case its not this
                if (FromMatrix != ToMatrix)
                {
                    CP.EnsureUpToDate();
                    CopyFromMatrix(FromMatrix);
                }

                SimRegion R = PathStore.GetSimRegion();
                R.BakeTerrain();
                float MinZLevel = CP.MinZ + StepSize;
                float MaxZLevel = CP.MaxZ + StepSize;
                
                PathStore.TaintMatrix();
                Console.WriteLine("\nStart UpdateMatrix: {0} for {1}", R, CP);
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
                                W.UpdateMatrix(this);
                            else
                            {
                                float gl = R.GetGroundLevel(fx, fy);
                                if (MaxZLevel <= gl)
                                {
                                    ToMatrix[x, y] = SimPathStore.BLOCKED;
                                }
                                else if (MinZLevel < gl)
                                    ToMatrix[x, y] = SimPathStore.MAYBE_BLOCKED;
                                else if (b > 127) // needs passable
                                    ToMatrix[x, y] = SimPathStore.INITIALLY;
                            }
                        }
                    }
                }
                if (usePotentialFieleds) AddFieldEffects(ToMatrix,ByteMatrix, SimPathStore.BLOCKED, 6);
                if (cutNarrows) AddAdjacentBlocking(ToMatrix, ByteMatrix);
                Console.WriteLine("\nEnd UpdateMatrix: {0} for {1}", R, CP);
            }
        }

        private void CopyFromMatrix(byte[,] FromMatrix)
        {
            throw new NotImplementedException();
        }

        private void AddFieldEffects(byte[,] from, byte[,] to, byte fronteer, int iterations)
        {
            int xsizem1 = MaxXPt - 1;
            while (iterations-- > 0)
            {
                byte self = (byte)(fronteer - 1);
                for (int y = MaxYPt - 1; y > 0; y--)
                {
                    for (int x = xsizem1; x > 0; x--)
                    {
                        byte b = from[x, y];
                        if (b > 2 && b < self)
                            if (SurroundingBlocked0(x, y, fronteer, from) > 0)
                                to[x, y] = self;
                    }
                }
                fronteer--;
            }
        }

        private void AddAdjacentBlocking(byte[,] from,byte[,] to)
        {
            int xsizem1 = MaxXPt - 1;
            for (int y = MaxYPt; y > 0; y--)
            {
                for (int x = xsizem1; x > 0; x--)
                {
                    byte b = from[x, y];
                    if (b == SimPathStore.MAYBE_BLOCKED)
                        if (SurroundingBlocked0(x, y, SimPathStore.BLOCKED, from) > 1)
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
