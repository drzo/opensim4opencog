using System;
using System.Collections.Generic;
using System.Threading;
using PathSystem3D.Navigation;
using OpenMetaverse;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
using THIRDPARTY.PrimMesher;

namespace PathSystem3D.Mesher
{
    public class Box3Fill : IComparable<Box3Fill>, IEquatable<Box3Fill>
    {

        #region IEquatable<Box3Fill> Members

        public bool Equals(Box3Fill other)
        {
            if (other.MaxX == MaxX &&
                other.MaxY == MaxY &&
                other.MaxZ == MaxZ &&
                other.MinX == MinX &&
                other.MinY == MinY &&
                other.MinZ == MinZ) return true;
            return false;
        }

        #endregion

        public override bool Equals(object obj)
        {
            if (obj is Box3Fill)
            {
                return Equals((Box3Fill)obj);
            }
            return false;
        }

        public void AddPos(Vector3 offset)
        {
            MinX += offset.X;
            MaxX += offset.X;
            MinY += offset.Y;
            MaxY += offset.Y;
            MinZ += offset.Z;
            MaxZ += offset.Z;
        }

        public static bool operator ==(Box3Fill o1, Box3Fill o2)
        {
            if (Object.ReferenceEquals(o1, null)) return Object.ReferenceEquals(o2, null);
            if (Object.ReferenceEquals(o2, null)) return false;
            return o1.Equals(o2);
        }

        public static bool operator !=(Box3Fill o1, Box3Fill o2)
        {
            return !o1.Equals(o2);
        }


        public float MinX;// = float.MaxValue;
        public float MaxX;// = float.MinValue;
        public float MinY;// = float.MaxValue;
        public float MaxY;// = float.MinValue;
        public float MinZ;// = float.MaxValue;
        public float MaxZ;// = float.MinValue;

        public Box3Fill(Triangle t1, Triangle t2, Vector3 padXYZ)
        {
            MinX = t1.v1.X;
            MaxX = t1.v1.X;
            MinY = t1.v1.Y;
            MaxY = t1.v1.Y;
            MinZ = t1.v1.Z;
            MaxZ = t1.v1.Z;
            AddVertex(t1.v2, padXYZ);
            AddVertex(t1.v3, padXYZ);
            AddTriangle(t2, padXYZ);
        }

        /// <summary>
        /// Construct an infinately small box
        /// </summary>
        //public Box3Fill(bool b) { Reset(); }
        /// <summary>
        ///  Make the box infinatly small
        /// </summary>        
        public Box3Fill(bool b)
        {
            MinX = float.MaxValue;
            MaxX = float.MinValue;
            MinY = float.MaxValue;
            MaxY = float.MinValue;
            MinZ = float.MaxValue;
            MaxZ = float.MinValue;
        }

        public void Reset()
        {
            MinX = float.MaxValue;
            MaxX = float.MinValue;
            MinY = float.MaxValue;
            MaxY = float.MinValue;
            MinZ = float.MaxValue;
            MaxZ = float.MinValue;
        }

        //const float PadXYZ = 0.33f;// SimPathStore.StepSize*0.75f;
        //public const float PADZ = 0.1f;// SimPathStore.StepSize*0.75f;

        public override int GetHashCode()
        {

            return MinEdge.GetHashCode() ^ MaxEdge.GetHashCode();
        }

        public override string ToString()
        {
            return "(" + MinEdge + " - " + MaxEdge + ")";
        }

        internal void SetBoxOccupied(CallbackXYBox p, float detail)
        {
            for (float x = MinX; x <= MaxX; x += detail)
            {
                for (float y = MinY; y <= MaxY; y += detail)
                {
                    p(x, y, this);
                }
            }
            p(MaxX, MaxY, this);
        }

        internal void SetOccupied(CallbackXY p, SimZMinMaxLevel MinMaxZ, float detail)
        {
            // detail /= 2f;
            //float MinX = this.MinX + offset.X;
            //float MaxX = this.MaxX + offset.X;
            //float MinY = this.MinY + offset.Y;
            //float MaxY = this.MaxY + offset.Y;
            //float MinZ = this.MinZ + offset.Z;
            //float MaxZ = this.MaxZ + offset.Z;

            float SimZMinLevel, SimZMaxLevel;

            // = SimPathStore.StepSize;
            for (float x = MinX; x <= MaxX; x += detail)
            {
                for (float y = MinY; y <= MaxY; y += detail)
                {
                    MinMaxZ(x, y, out SimZMinLevel, out SimZMaxLevel);
                    if (SimZMinLevel > MaxZ || SimZMaxLevel < MinZ)
                    {
                        // this box is not between the Z levels
                        continue;
                    }
                    p(x, y, MinZ, MaxZ);
                }
            }
            /// the for/next loop probably missed this last point
            MinMaxZ(MaxX, MaxY, out SimZMinLevel, out SimZMaxLevel);
            if (SimZMinLevel > MaxZ || SimZMaxLevel < MinZ)
            {
                // this box is not between the Z levels
                return;
            }
            p(MaxX, MaxY, MinZ, MaxZ);
        }

        internal void SetOccupied(CallbackXY p, float SimZMinLevel, float SimZMaxLevel, float detail)
        {
            //float MinX = this.MinX + offset.X;
            //float MaxX = this.MaxX + offset.X;
            //float MinY = this.MinY + offset.Y;
            //float MaxY = this.MaxY + offset.Y;
            //float MinZ = this.MinZ + offset.Z;
            //float MaxZ = this.MaxZ + offset.Z;


            if (SimZMinLevel > MaxZ || SimZMaxLevel < MinZ)
            {
                // this box is not between the Z levels
                return;
            }

            // = SimPathStore.StepSize;
            for (float x = MinX; x <= MaxX; x += detail)
            {
                for (float y = MinY; y <= MaxY; y += detail)
                {
                    p(x, y, MinZ, MaxZ);
                }
            }
            /// the for/next loop probably missed this last point
            p(MaxX, MaxY, MinZ, MaxZ);
        }


        public string ToString(Vector3 offset)
        {
            string s = "(" + (Vector3)(MinEdge + offset) + " - " + (Vector3)(MaxEdge + offset) + " mass= " + Mass() + ")";
            return s;
        }

        /// <summary>
        /// Make sure box is big enough for this vertex
        /// </summary>
        /// <param name="v"></param>
        /// <returns>true if the box has grown</returns>
        internal void AddVertex(Vertex v, Vector3 padXYZ)
        {
            AddPoint(v.X, v.Y, v.Z, padXYZ);
        }

        internal void AddPoint(float x, float y, float z, Vector3 padXYZ)
        {
            // bool changed = false;
            if (x < MinX)
            {
                MinX = x - padXYZ.X;
                //  changed = true;
            }
            if (y < MinY)
            {
                MinY = y - padXYZ.Y;
                // changed = true;
            }
            if (z < MinZ)
            {
                MinZ = z - padXYZ.Z;
                //changed = true;
            }

            if (x > MaxX)
            {
                MaxX = x + padXYZ.X;
                // changed = true;
            }
            if (y > MaxY)
            {
                MaxY = y + padXYZ.Y;
                // changed = true;
            }
            if (z > MaxZ)
            {
                MaxZ = z;// +padXYZ.Z;
                //changed = true;
            }
            //return changed;
        }

        /// <summary>
        /// Add Triangle (this just pushes the size of the box outward if needed)
        /// </summary>
        /// <param name="t"></param>
        /// <returns>true if the boxsize was increased</returns>
        public void AddTriangle(Triangle t, Vector3 padXYZ)
        {
            AddVertex(t.v1, padXYZ);
            AddVertex(t.v2, padXYZ);
            AddVertex(t.v3, padXYZ);
        }

        public Vector3 MinEdge
        {
            get
            {
                return new Vector3(MinX, MinY, MinZ);
            }
        }
        public Vector3 MaxEdge
        {
            get
            {
                return new Vector3(MaxX, MaxY, MaxZ);
            }
        }

        public bool IsInsideXY(float x, float y)
        {
            if (
             (x < MinX) ||
             (y < MinY) ||
             (x > MaxX) ||
             (y > MaxY)) return false;
            return true;
        }

        public bool IsInside(float x, float y, float z)
        {
            if (
             (x < MinX) ||
             (y < MinY) ||
             (z < MinZ) ||
             (x > MaxX) ||
             (y > MaxY) ||
             (z > MaxZ)) return false;
            return true;
        }

        public float Mass()
        {
            return (MaxX - MinX) * (MaxY - MinY) * (MaxZ - MinZ);
        }

        public bool IsCompletelyInside(Box3Fill inner)
        {
            if ((inner.MaxX > MaxX) ||
             (inner.MinX < MinX) ||
             (inner.MaxY > MaxY) ||
             (inner.MinY < MinY) ||
             (inner.MaxZ > MaxZ) ||
             (inner.MinZ < MinZ)) return false;
            return true;
        }

        public static List<Box3Fill> Simplify(List<Box3Fill> simpl)
        {
            simpl.Sort(Bigger);
            List<Box3Fill> retval = new List<Box3Fill>();
            int len = simpl.Count;
            int len1 = len - 1;
            for (int i = 0; i < len; i++)
            {
                Box3Fill bi = simpl[i];
                bool foundInside = false;
                for (int ii = len1; ii > i; ii--)
                {
                    if (simpl[ii].IsCompletelyInside(bi))
                    {
                        foundInside = true;
                        break;
                    }
                }
                if (!foundInside)
                {
                    retval.Add(bi);
                }
            }
            return retval;
        }

        #region IComparable<Box3Fill> Members

        public int CompareTo(Box3Fill other)
        {
            return Bigger(this, other);
        }

        #endregion

        static int Bigger(Box3Fill b1, Box3Fill b2)
        {
            if (b1 == b2) return 0;

            if (b1.MinX > b2.MinX)
            {
                return -1;
            }
            if (b1.MinY > b2.MinY)
            {
                return -1;
            }
            if (b1.MinZ > b2.MinZ)
            {
                return -1;
            }

            if (b1.MaxX < b2.MaxX)
            {
                return -1;
            }
            if (b1.MaxY < b2.MaxY)
            {
                return -1;
            }
            if (b1.MaxZ < b2.MaxZ)
            {
                return -1;
            }

            float f1 = b1.Mass();
            float f2 = b2.Mass();
            if (f1 == f2)
            {
                return 1;
            }
            return f1 < f2 ? -1 : 1;
        }


        internal bool IsZInside(float low, float high)
        {
            if (low > MaxZ || high < MinZ) return false;
            return true;
        }

        internal void Expand(Box3Fill B)
        {
            AddPoint(B.MaxEdge);
            AddPoint(B.MinEdge);
        }

        private void AddPoint(Vector3 vector3)
        {
            AddPoint(vector3.X, vector3.Y, vector3.Z, Vector3.Zero);
        }
    }
}
