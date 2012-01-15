// Copyleft 2009 Douglas R. Miles (Daxtron Labs) - <dmiles@daxtron.com> 55%
// Copyright 2003 Eric Marchesin - <eric.marchesin@laposte.net> 45%
//
// This source file(s) may be redistributed by any means PROVIDING they
// are not sold for profit without the authors expressed written consent,
// and providing that this notice and the authors name and all copyright
// notices remain intact.
// THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED. USE IT AT YOUR OWN RISK. THE AUTHOR ACCEPTS NO
// LIABILITY FOR ANY DATA DAMAGE/LOSS THAT THIS PRODUCT MAY CAUSE.
//-----------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using HttpServer;
using PathSystem3D.Mesher;
using PathSystem3D.Navigation.Debug;
using OpenMetaverse;
using THIRDPARTY.OpenSim.Framework;
using THIRDPARTY.OpenSim.Region.Physics.Manager;
using THIRDPARTY.OpenSim.Region.Physics.Meshing;
#if COLLIDER_ODE
using NUnit.Framework;
using THIRDPARTY.OpenSim.Region.Physics.OdePlugin;
#endif

namespace PathSystem3D.Navigation
{

    public delegate void CallbackXY(float x, float y, float minZ, float maxZ);
    public delegate void CallbackXYBox(float x, float y, Box3Fill box);

    public delegate float SimZLevel(float x, float y);
    public delegate void SimZMinMaxLevel(float x, float y, out float minLevel, out float maxLevel);
    /// <summary>
    /// Graph structure. It is defined with :
    /// It is defined by a 2D matrix of CollisionIndex 
    /// It is used to perform operations on CollisionPlanes
    /// </summary>
    [Serializable]
    public class SimPathStore //: PathSystem3D.Navigation.IPathStore
    {
#if COLLIDER_ODE
        public OdePlugin odePhysics = new OdePlugin();
        public OdeScene odeScene;
#endif
        public static int DebugLevel = 0;
        public string RegionName { get; set; }
        // util
        static public void TrianglesToBoxes(IList<Triangle> tl, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            int tc = tl.Count;
            AddTrianglesV3S(tl, tc, OuterBox, padXYZ, InnerBoxes);
            // Debug(InnerBoxes.Count);
        }

        private static void AddTrianglesV1(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            int len1 = len - 1;
            OuterBox.AddTriangle(ts[len1], padXYZ);
            for (int i = 0; i < len1; i++)
            {
                Triangle t1 = ts[i];
                bool used = false;
                OuterBox.AddTriangle(t1, padXYZ);
                for (int ii = i + 1; ii < len; ii++)
                {
                    Triangle t2 = ts[ii];
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        Box3Fill B = new Box3Fill(t1, t2, padXYZ);
                        InnerBoxes.Add(B);
                        used = true;
                    }
                }
                if (!used)
                {
                    Box3Fill B = new Box3Fill(true);
                    B.AddTriangle(t1, padXYZ);
                    InnerBoxes.Add(B);
                }
            }
        }

        private static void AddTrianglesV13(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            Vertex[] unsharedV = new Vertex[6];
            Vertex[] sharedV = new Vertex[3];
            //int len1 = len - 1;
            //OuterBox.AddTriangle(ts[len1], padXYZ);
            for (int i = 0; i < len; i++)
            {
                Triangle t1 = ts[i];
                if (t1 == null) continue;
                bool used = false;
                OuterBox.AddTriangle(t1, padXYZ);
                for (int ii = i + 1; ii < len; ii++)
                {
                    Triangle t2 = ts[ii];
                    if (t2 == null) continue;
                    int shared = SharedVertexs(t1, t2,sharedV,unsharedV);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        Vertex u1 = unsharedV[0];
                        Vertex u2 = unsharedV[1];
                        Vertex s1 = sharedV[0];
                        Vertex s2 = sharedV[1];
                        float ZDiff = u1.Z - u2.Z;
                        if (ZDiff < 0) ZDiff = -ZDiff;
                        if (ZDiff < 3)
                        {
                            Box3Fill B = new Box3Fill(t1, t2, padXYZ);
                            InnerBoxes.Add(B);
                            used = true;
                        }
                        else
                        {

                        }
                    }
                }
                if (!used)
                {
                    AddThreeFour(2, t1.v1, t1.v2, t1.v3, InnerBoxes, padXYZ);
                }
            }
        }

        private static int SharedVertexs(Triangle t1, Triangle t2)
        {
            int sharedV = 0;
            if (t1.v1 == t2.v1) sharedV++;
            else
                if (t1.v1 == t2.v2) sharedV++;
                else
                    if (t1.v1 == t2.v3) sharedV++;
            if (t1.v2 == t2.v1) sharedV++;
            else
                if (t1.v2 == t2.v2) sharedV++;
                else
                    if (t1.v2 == t2.v3) sharedV++;
            if (t1.v3 == t2.v1 || t1.v3 == t2.v2 || t1.v3 == t2.v3) return sharedV + 1;
            return sharedV;
        }

        private static int SharedVertexs(Triangle t1, Triangle t2, Vertex[] shared,Vertex[] unshared)
        {
            int sharedV = 0;
            int unsharedV = 0;
            bool[] t2Shared = new bool[3];
            if (t1.v1 == t2.v1)
            {
                shared[sharedV] = t1.v1;
                t2Shared[0] = true;
                sharedV++;
            }
            else
                if (t1.v1 == t2.v2)
                {
                    shared[sharedV] = t1.v1;
                    t2Shared[1] = true;
                    sharedV++;
                }
                else
                    if (t1.v1 == t2.v3)
                    {
                        shared[sharedV] = t1.v1;
                        t2Shared[2] = true;
                        sharedV++;
                    }
                    else
                    {
                        unshared[unsharedV] = t1.v1;
                        unsharedV++;
                    }
            if (t1.v2 == t2.v1)
            {
                shared[sharedV] = t1.v2;
                t2Shared[0] = true;
                sharedV++;
            }
            else
                if (t1.v2 == t2.v2)
                {
                    shared[sharedV] = t1.v2;
                    t2Shared[1] = true;
                    sharedV++;
                }
                else
                    if (t1.v2 == t2.v3)
                    {
                        shared[sharedV] = t1.v2;
                        t2Shared[2] = true;
                        sharedV++;
                    }
                    else
                    {
                        unshared[unsharedV] = t1.v2;
                        unsharedV++;
                    }
            if (t1.v3 == t2.v1)
            {
                shared[sharedV] = t1.v3;
                t2Shared[0] = true;
                sharedV++;
            }
            else
                if (t1.v3 == t2.v2)
                {
                    shared[sharedV] = t1.v3;
                    t2Shared[1] = true;
                    sharedV++;
                }
                else
                    if (t1.v3 == t2.v3)
                    {
                        shared[sharedV] = t1.v3;
                        t2Shared[2] = true;
                        sharedV++;
                    }
                    else
                    {
                        unshared[unsharedV] = t1.v3;
                        unsharedV++;
                    }
            if (sharedV==2 && unsharedV<2)
            {
                if (unshared[0] != t2.v1)
                {
                    if (!t2Shared[0])
                    {
                        unshared[1] = t2.v1;
                        return sharedV;
                    }
                }
                if (unshared[0] != t2.v2)
                {
                    if (!t2Shared[1])
                    {
                        unshared[1] = t2.v2;
                        return sharedV;
                    }
                }
                unshared[1] = t2.v3;
                
            }
            return sharedV;
        }

        private static void AddTrianglesV2(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            foreach (Triangle t1 in ts)
            {
                Box3Fill B = new Box3Fill(true);
                OuterBox.AddTriangle(t1, padXYZ);
                B.AddTriangle(t1, padXYZ);
                InnerBoxes.Add(B);
            }
        }

        private static void AddTrianglesV3(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<CollisionObject> InnerBoxes)
        {
            int len1 = len - 2;
            OuterBox.AddTriangle(ts[len-1], padXYZ);
            for (int i = 0; i < len1; i += 2)
            {
                Triangle t1 = ts[i];
                Triangle t2 = ts[i + 1];
                OuterBox.AddTriangle(t1, padXYZ);
                OuterBox.AddTriangle(t2, padXYZ);
                Box3Fill B = new Box3Fill(t1, t2, padXYZ);
                InnerBoxes.Add(B);
                bool used = false;
                for (int ii = i + 2; ii < len; ii++)
                {
                    t2 = ts[ii];
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        B = new Box3Fill(t1, t2, padXYZ);
                        InnerBoxes.Add(B);
                        used = true;
                    }
                }
                if (!used)
                {
                    B = new Box3Fill(true);
                    B.AddTriangle(t1, padXYZ);
                    InnerBoxes.Add(B);
                }
            }
        }

        private static void AddTrianglesV3S(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            int len1 = len - 2;
            for (int i = 0; i < len1; i += 2)
            {
                Triangle t1 = ts[i];
                Triangle t2 = ts[i + 1];
                OuterBox.AddTriangle(t1, padXYZ);
                OuterBox.AddTriangle(t2, padXYZ);
                Box3Fill B = new Box3Fill(t1, t2, padXYZ);
                InnerBoxes.Add(B);
                bool used = false;
                for (int ii = i + 2; ii < len; ii++)
                {
                    t2 = ts[ii];
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        B = new Box3Fill(t1, t2, padXYZ);
                        InnerBoxes.Add(B);
                        used = true;
                    }
                }
                if (!used)
                {
                    B = new Box3Fill(true);
                    B.AddTriangle(t1, padXYZ);
                    InnerBoxes.Add(B);
                }
            }
        }


        private static void AddTrianglesV321(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            int len2 = len - 2;
            int len1 = len - 1;
            OuterBox.AddTriangle(ts[len - 1], padXYZ);
            for (int i = 0; i < len2; i += 2)
            {
                Triangle t1 = ts[i];
                if (t1 == null) continue;
                OuterBox.AddTriangle(t1, padXYZ);
                Triangle t2 = ts[i + 1];
                if (t2 != null)
                {
                    OuterBox.AddTriangle(t2, padXYZ);
                    InnerBoxes.Add(new Box3Fill(t1, t2, padXYZ));
                }
                bool used = false;
                int until = i + 1;

                for (int ii = len1; ii > until;ii-- )
                {
                    t2 = ts[ii];
                    if (t2 == null) continue;
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        InnerBoxes.Add(new Box3Fill(t1, t2, padXYZ));
                        ts[ii] = null;
                        used = true;
                    }
                }
                if (!used)
                {
                    Box3Fill B = new Box3Fill(true);
                    B.AddTriangle(t1, padXYZ);
                    InnerBoxes.Add(B);
                }
            }
        }
        private static void AddTrianglesV32(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ,
            IList<Box3Fill> InnerBoxes)
        {
            Triangle t1 = ts[0];
            Triangle t2 = ts[len - 1];
            OuterBox.AddTriangle(t1, padXYZ);
            OuterBox.AddTriangle(t2, padXYZ);
            Box3Fill B;// = new Box3Fill(t1, t2, padXYZ);
            //InnerBoxes.Add(B);

            int len2 = len - 2;
            OuterBox.AddTriangle(ts[len - 1], padXYZ);
            for (int i = 0; i < len2; i += 1)
            {
                t1 = ts[i];
                if (t1 == null) continue;
                t2 = ts[i + 1];
                if (t2 == null) continue;
                OuterBox.AddTriangle(t1, padXYZ);
                OuterBox.AddTriangle(t2, padXYZ);
                B = new Box3Fill(t1, t2, padXYZ);
                InnerBoxes.Add(B);
                bool used = false;
                for (int ii = i + 2; ii < len; ii++)
                {
                    t2 = ts[ii];
                    if (t2 == null) continue;
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        B = new Box3Fill(t1, t2, padXYZ);
                        InnerBoxes.Add(B);
                       // ts[ii] = null;
                        used = true;
                    }
                }
                if (!used)
                {
                    B = new Box3Fill(true);
                    B.AddTriangle(t1, padXYZ);
                    InnerBoxes.Add(B);
                }
            }
        }

        private static void AddTrianglesV34(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            //int len1 = len - 2;
            //OuterBox.AddTriangle(ts[len - 1], padXYZ);
            for (int i = 0; i < len; i += 1)
            {
                Triangle t1 = ts[i];
                if (t1 == null) continue;
                OuterBox.AddTriangle(t1, padXYZ);
                Box3Fill B;// = new Box3Fill(t1, t2, padXYZ);
                //InnerBoxes.Add(B);
                bool used = false;
                for (int ii = i + 1; ii < len; ii++)
                {
                    Triangle t2 = ts[ii];
                    if (t2 == null) continue;
                    int shared = SharedVertexs(t1, t2);
                    if (shared == 3) continue;
                    if (shared == 2)
                    {
                        ts[ii] = null;
                        B = new Box3Fill(t1, t2, padXYZ);
                        InnerBoxes.Add(B);
                        used = true;
                    }
                }
                if (!used)
                {
                    AddThreeFour(2, t1.v1, t1.v2, t1.v3, InnerBoxes, padXYZ);
                }
            }
        }
      
        private static void AddTrianglesV4(IEnumerable<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            foreach (Triangle t1 in ts)
            {
                OuterBox.AddTriangle(t1, padXYZ);
                Vertex v1 = t1.v1;
                Vertex v2 = t1.v2;
                Vertex v3 = t1.v3;
                AddThreeTwo(v1, v2, v3,InnerBoxes, padXYZ);
            }
        }
        private static void AddTrianglesV4S(IList<Triangle> ts, int len, Box3Fill OuterBox, Vector3 padXYZ, IList<Box3Fill> InnerBoxes)
        {
            List<Vertex> Shared;
            List<Vertex> UnShared;

            Box3Fill B;// = new Box3Fill(t1, t2, padXYZ);
            for (int i = 0; i < len; i += 1)
            {
                Triangle t1 = ts[i];
                if (t1 == null) continue;
                OuterBox.AddTriangle(t1, padXYZ);
                //InnerBoxes.Add(B);
                bool used = false;
                for (int ii = i + 1; ii < len; ii++)
                {
                    Triangle t2 = ts[ii];
                    if (t2 == null) continue;
                    List<Vertex> verts = AddedVertexs(t1, t2,out Shared,out UnShared);
                 //   if (Shared.Count == 0) continue;
                    if (Shared.Count == 2)
                    {
                       // ts[ii] = null;
                        B = new Box3Fill(t1, t2, padXYZ);
                        InnerBoxes.Add(B);
                        used = true;
                      //  break;
                    }
                }
                if (!used)
                {
                    Vertex v1 = t1.v1;
                    Vertex v2 = t1.v2;
                    Vertex v3 = t1.v3;
                    AddThreeTwo(v1, v2, v3, InnerBoxes, padXYZ);
                }
            }
        }

        private static List<Vertex> AddedVertexs(Triangle t1, Triangle t2, out List<Vertex> Shared, out List<Vertex> UnShared)
        {
            Shared = new List<Vertex>();
            UnShared = new List<Vertex>();
            List<Vertex> lsit = new List<Vertex>();
            lsit.Add(t1.v1);
            lsit.Add(t1.v2);
            lsit.Add(t1.v3);
            Vertex v = t2.v1;
            if (lsit.Remove(v))
            {
                Shared.Add(v);
            }
            else
            {
                UnShared.Add(v);
            }
            v = t2.v2;
            if (lsit.Remove(v))
            {
                Shared.Add(v);
            }
            else
            {
                UnShared.Add(v);
            }
            v = t2.v3;
            if (lsit.Remove(v))
            {
                Shared.Add(v);
            }
            else
            {
                UnShared.Add(v);
            }
            return lsit;
        }

        private static void AddFour(int count, Vertex v1, Vertex v2, Vertex v3, Vertex v4, ICollection<Box3Fill> InnerBoxes, Vector3 padXYZ)
        {
            AddThreeFour(count, v1, v2, v4, InnerBoxes, padXYZ);
            AddThreeFour(count, v2, v3, v4, InnerBoxes, padXYZ);
            AddThreeFour(count, v1, v3, v4, InnerBoxes, padXYZ);
        }

        private static void AddThree(Vertex v1, Vertex v2, Vertex v3, ICollection<Box3Fill> InnerBoxes, Vector3 padXYZ)
        {
            Box3Fill B = new Box3Fill(true);
            B.AddVertex(v1, padXYZ);
            B.AddVertex(v2, padXYZ);
            B.AddVertex(v3, padXYZ);
            InnerBoxes.Add(B);
        }

        private static void AddTwo(Vertex v1, Vertex v2, ICollection<Box3Fill> InnerBoxes, Vector3 padXYZ)
        {
            Vertex v4 = new Vertex((v1.X + v2.X) / 2, (v1.Y + v2.Y ) / 2, (v1.Z + v2.Z ) / 2);
            Box3Fill B = new Box3Fill(true);
            B.AddVertex(v1, padXYZ);
            B.AddVertex(v4, padXYZ);
            InnerBoxes.Add(B);
            B = new Box3Fill(true);
            B.AddVertex(v2, padXYZ);
            B.AddVertex(v4, padXYZ);
            InnerBoxes.Add(B);
        }

        private static void AddThreeTwo(Vertex v1, Vertex v2, Vertex v3, ICollection<Box3Fill> InnerBoxes, Vector3 padXYZ)
        {
            //Vertex v4 = new Vertex((v1.X + v2.X + v3.X)/3, (v1.Y + v2.Y + v3.Y)/3, (v1.Z + v2.Z + v3.Z)/3);
            //AddTwo(v1, v4, InnerBoxes, padXYZ);
          //  AddTwo(v2, v4, InnerBoxes, padXYZ);
            //AddTwo(v3, v4, InnerBoxes, padXYZ);
            AddTwo(v1, v2, InnerBoxes, padXYZ);
            AddTwo(v2, v3, InnerBoxes, padXYZ);
            AddTwo(v1, v3, InnerBoxes, padXYZ);
        }

        private static void AddThreeFour(int count, Vertex v1, Vertex v2, Vertex v3, ICollection<Box3Fill> InnerBoxes, Vector3 padXYZ)
        {
            if (count > 0)
                AddFour(count-1,v1, v2, v3, new Vertex((v1.X + v2.X + v3.X)/3, (v1.Y + v2.Y + v3.Y)/3, (v1.Z + v2.Z + v3.Z)/3),
                        InnerBoxes, padXYZ);
            else
                AddThree(v1, v2, v3, InnerBoxes, padXYZ);
        }

        Dictionary<IComparable, IMeshedObject> meshedObjects = new Dictionary<IComparable, IMeshedObject>();
        /// <summary>
        /// By default no boxes are passable
        /// </summary>
        public Predicate<IComparable> IsSolidPredicate = delegate(IComparable id) { return true; };

        /// <summary>
        /// The Pathstore can implement this
        /// </summary>
        public SimZLevel GroundLevelDelegate = null;//delegate(float x, float y) { return 10; };

        // setup
        void AddBoxes(IComparable id, IList<Box3Fill> boxes)
        {
            Box3Fill Outer = new Box3Fill(true);

            foreach (Box3Fill B in boxes)
            {
                Outer.Expand(B);

            }
            IMeshedObject MO = new RuntimeMesh(id, Outer, boxes, this);
            //MeshedObject simMesh = new MeshedObject(boxes);
            meshedObjects.Add(id, MO);
            AddCollisions(MO);
        }

        public bool AddCollisions(IMeshedObject MO)
        {
            return MO.UpdateOccupied(this);
        }
        public void RemoveBoxes(IComparable id)
        {
            IMeshedObject MO = meshedObjects[id];//.Remove(id);
            Box3Fill changed = new Box3Fill(true);
            MO.RemoveFromWaypoints(changed);
            RecomputeMatrix();
            meshedObjects.Remove(id);

        }

        public void SetPhysicalPredicate(Predicate<IComparable> callback)
        {
            IsSolidPredicate = callback;
        }

        public void SetGroundLevel(SimZLevel callback)
        {
            GroundLevelDelegate = callback;
#if COLLIDER_ODE
            StartGatheringTerrainFromCallback();
#endif
        }

        // Updates
        public void SetTraveled(Vector3 LastPosition, Vector3 nextPosition)
        {
            Vector3 dif = LastPosition - nextPosition;
            if (Math.Abs(dif.Z) > 0.5)
            {
                Debug("BIGZ " + LastPosition + " ->" + nextPosition + " " + this);
                return;
            }
            if (!(dif.X == 0 && dif.Y == 0))
            {
                float dist = Vector3.Distance(LastPosition, nextPosition);
                if (dist > 2) return;
                int stepsNeeded = (int)(dist * POINTS_PER_METER) + 1;
              // if (OpenMetaverse.Settings.LOG_LEVEL == OpenMetaverse.Helpers.LogLevel.Debug) 
                Debug("MakeMovement " + LastPosition + " -> " + stepsNeeded + " -> " + nextPosition + " " + this);
                Vector3 vstep = dif / stepsNeeded;
                Vector3 traveled = nextPosition;
                SetTraveled(nextPosition.X, nextPosition.Y, nextPosition.Z);
                for (int i = 0; i < stepsNeeded; i++)
                {
                    traveled = traveled + vstep;
                    if (stepsNeeded > 10) SetTraveled(traveled.X, traveled.Y, traveled.Z);
                    else SetPassable(traveled.X, traveled.Y, traveled.Z);
                }
            }
        }

        public void SetBlockedTemp(Vector3 cp, Vector3 v3, float time, byte blockLevel)
        {
            Point P1 = ToPoint(cp);
            Vector3 offset = v3 - cp;
            double ZAngle = (double)Math.Atan2(offset.X, offset.Y);
            Point Last = ToPoint(v3);
            float Dist = 0.3f;
            Vector3 b1 = v3;
            while (offset.Length() > StepSize)
            {
                offset *= 0.75f;
                Vector3 blocked = cp + offset;
                Point P2 = ToPoint(blocked);
                if (P2 != P1)
                {
                    Dist = offset.Length();
                    Last = P2;
                    b1 = blocked;
                }
            }
            float x = UNARRAY_X(Last.X);
            float y = UNARRAY_Y(Last.Y);
            Vector3 v3o = new Vector3(x, y, v3.Z);

                          
            List<ThreadStart> undo = new List<ThreadStart>();
            BlockPointTemp(v3o, undo, blockLevel);
            double A45 = 45f / RAD2DEG;
            Debug("BlockTowardsVector {0}", Vector3.Distance(cp,v3o));
            Vector3 middle = ZAngleVector(ZAngle) * Dist;
            middle += cp;
            double mdist = Vector3.Distance(middle, b1);
            if (mdist > 0.1)
            {
                Debug("Weird mdist = " + mdist);
            }
            Dist = 0.4f;
            //ZAngle -= (90 / SimPathStore.RAD2DEG);

            BlockPointTemp(ZAngleVector(ZAngle - A45 * 1.5) * Dist + cp, undo, blockLevel);
            BlockPointTemp(ZAngleVector(ZAngle - A45) * Dist + cp, undo, blockLevel);
            BlockPointTemp(ZAngleVector(ZAngle - A45 * 0.5) * Dist + cp, undo, blockLevel);

            BlockPointTemp(ZAngleVector(ZAngle) * Dist + cp, undo, blockLevel);

            BlockPointTemp(ZAngleVector(ZAngle + A45 * 0.5) * Dist + cp, undo, blockLevel);
            BlockPointTemp(ZAngleVector(ZAngle + A45) * Dist + cp, undo, blockLevel);
            BlockPointTemp(ZAngleVector(ZAngle + A45 * 1.5) * Dist + cp, undo, blockLevel);

            if (undo.Count > 0)
                new Thread(() =>
                {
                    Thread.Sleep((int)(time*1000));
                    foreach (ThreadStart u in undo)
                    {
                        u();
                    }
                }).Start();
            //Don't Run back (anymore)
            //MoveTo(cp + ZAngleVector(ZAngle - Math.PI) * 2, 1f, 2);
        }

        /// <summary>
        /// Blocks a point temporarily (one minute)
        /// However! if the point is of questionable quality it will permanently block it
        /// </summary>
        /// <param name="vector3"></param>
        public void BlockPointTemp(Vector3 vector3, List<ThreadStart> undo, byte blockLevel)
        {
            CollisionPlane CP = FindCollisionPlane(vector3.Z);
            if (CP != null)
            {
                int ix = ARRAY_X(vector3.X);
                int iy = ARRAY_Y(vector3.Y);

                byte b = CP.ByteMatrix[ix, iy];
                if (b == INITIALLY) return; //nothing to do
                if (b == BLOCKED) return; //nothing to do
                // this is for the second time around
                if (MaybeBlocked(b))
                {
                    CP.ByteMatrix[ix, iy] = BLOCKED;
                    Debug("Permanently blocking off " + vector3);
                    return;
                }
                CollisionIndex CI = CreateFirstNode(vector3.X, vector3.Y);
                CP.ByteMatrix[ix, iy] = MAYBE_BLOCKED;
                CI.SetNodeQualityTimer(CP, BLOCKED, undo);
            }
            else
                SetNodeQualityTimer(vector3, BLOCKED, 60);
        }

        private float _WaterHeight = Single.MinValue;

        public float WaterHeight
        {
            get { return _WaterHeight; }
            set { _WaterHeight = value;
#if COLLIDER_ODE
                odeScene.SetWaterLevel(value);
                StartGatheringTerrainFromCallback();
#endif
            }
        }


        public static IList<Vector3d> GetPath(CollisionPlane CP, Vector3d globalStart, Vector3d globalEnd, double endFudge, out bool OnlyStart, out bool faked)
        {
            CP.LastUsed = DateTime.Now;
            SimPathStore regStart = SimPathStore.GetPathStore(globalStart);// posStart.GetPathStore();
            SimPathStore regEnd = GetPathStore(globalEnd);
            Vector3 localStart = GlobalToLocal(globalStart);
            Vector3 localEnd = GlobalToLocal(globalEnd); 
            // Same region?
            if (regStart == regEnd)
            {
                return regStart.GetAtLeastPartial(CP,localStart, localEnd, (float)endFudge, out OnlyStart, out faked);
            }
            OnlyStart = true; // will be only a partial path
            SimPathStore nextRegion;
            Vector3 localLast = regStart.LocalOuterEdge(localStart, globalEnd, out nextRegion);
            // needs to go to edge
            IList<Vector3d> route = regStart.GetLocalPath(CP, localStart, localLast, out faked);
            // at edge so make a crossing
            Vector3 enterEdge = EnterEdge(localLast, nextRegion.GetGridLocation() - regStart.GetGridLocation());
            route.Add(nextRegion.LocalToGlobal(enterEdge));
            return route;
        }


        internal IList<Vector3d> GetAtLeastPartial(CollisionPlane CP, Vector3 localStart, Vector3 localEnd, float endFudge, out bool OnlyStart, out bool faked)
        {
            Vector3 newEnd = localEnd;
            IList<Vector3d> route = GetLocalPath(CP, localStart, newEnd, out faked);
            if (route.Count > 1)
            {
                OnlyStart = false;
                return route;
            }
            OnlyStart = true;
            Vector3 diff = localEnd - localStart;
            while (diff.Length() > 10)
            {
                diff = diff * 0.8f;
                newEnd = localStart + diff;
                route = GetLocalPath(CP,localStart, newEnd, out faked);
                if (route.Count > 1) return route;
            }
            OnlyStart = false; // Since this will be the best
            // try to move to nearby
            float step = 45 * RAD2DEG;
            for (double angle = 0; angle < PI2; angle += step)
            {
                newEnd = localEnd + ZAngleVector(angle) * endFudge;
                route = GetLocalPath(CP,localStart, newEnd, out faked);
                if (route.Count > 1) return route;
            }
            route = new List<Vector3d>();
            route.Add(LocalToGlobal(localStart));           
            SimPathStore PathStore = GetPathStore3D(localStart);
            faked = true;
            Debug("very bad fake route for " + CP);
            return route;
        }

        public  Vector3d LocalToGlobal(Vector3 objectLoc)
        {
            Vector2 V2 = GetGridLocation();
            return new Vector3d(V2.X * 256 + objectLoc.X, V2.Y * 256 + objectLoc.Y, objectLoc.Z);
        }

#if COLLIDER_ODE

        public Vector3 CreateAndDropPhysicalCube(Vector3 from)
        {
            OdeScene ps = odeScene;
            PrimitiveBaseShape newcube = PrimitiveBaseShape.CreateBox();
            PhysicsVector position = new PhysicsVector(from.X, from.Y, from.Z);
            PhysicsVector size = new PhysicsVector(0.2f, 0.2f, 1f);
            Quaternion rot = Quaternion.Identity;
            PhysicsActor prim = ps.AddPrimShape("CoolShape", newcube, position, size, rot, true);
            OdePrim oprim = (OdePrim)prim;
            OdeScene pscene = (OdeScene)ps;
            //   prim.OnCollisionUpdate;
            Assert.That(oprim.m_taintadd);
            bool falling = true;
            prim.LocalID = 5;
            //oprim.OnVelocityUpdate += delegate(PhysicsVector velocity)
            //                              {
            //                                  falling = false;
            //                              };
            oprim.OnCollisionUpdate += delegate(EventArgs args)
                                           {
                                               CollisionEventUpdate arg = (CollisionEventUpdate)args;
                                               //simhinfo 58 58 30
                                               Console.WriteLine("oprim OnCollisionUpdate " + args);
                                               falling = false;
                                           };
           
            oprim.SubscribeEvents(30000)
            ;
            
            while (falling)
            {
                ps.Simulate(0.133f);
                //Assert.That(oprim.prim_geom != (IntPtr)0);

                //Assert.That(oprim.m_targetSpace != (IntPtr)0);

                ////Assert.That(oprim.m_targetSpace == pscene.space);
                //Debug("TargetSpace: " + oprim.m_targetSpace + " - SceneMainSpace: " + pscene.space);

                //Assert.That(!oprim.m_taintadd);
                //Debug("Prim Position (" + oprim.m_localID + "): " + prim.Position.ToString());

                //// Make sure we're above the ground
                ////Assert.That(prim.Position.Z > 20f);
                ////m_log.Info("PrimCollisionScore (" + oprim.m_localID + "): " + oprim.m_collisionscore);

                //// Make sure we've got a Body
                //Assert.That(oprim.Body != (IntPtr)0);
                ////m_log.Info(
            }

            PhysicsVector primPosition = prim.Position;

            // Make sure we're not somewhere above the ground
 
            Vector3 v3 = new Vector3(primPosition.X,primPosition.Y,primPosition.Z);
            ps.RemovePrim(prim);
           // Assert.That(oprim.m_taintremove);
            ps.Simulate(0.133f); // for removal or not needed?
            // Assert.That(oprim.Body == (IntPtr)0);
            return v3;
        }
#endif
        /// <summary>
        ///  The closet usable space to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        ///         
        public Vector3 GetUsableLocalPositionOf(CollisionPlane CP, Vector3 v3, float useDist)
        {
            SimPathStore PathStore = GetPathStore3D(v3);

            int ix = ARRAY_X(v3.X);
            int iy = ARRAY_Y(v3.Y);

            byte[,] ByteMatrix = CP.ByteMatrix;
            byte b = ByteMatrix[ix,iy];
            if (b != BLOCKED) return v3;
            float[,] GP = CP.HeightMap;
            float zl = GP[ix,iy];

            List<CollisionIndex> RestoreBlocked = new List<CollisionIndex>();
            RestoreBlocked.Add(GetCollisionIndex(ix, iy));

            int count = 1+CP.NeighborPredicate(ix, iy,(int)( useDist*POINTS_PER_METER), delegate(int NX, int NY)
            {
                byte NB = ByteMatrix[NX, NY];
                if (NB == BLOCKED)
                {
                    float NZ = GP[NX, NY];
                    if (Math.Abs(NZ - zl) < 0.2)
                    {
                        CollisionIndex CI = GetCollisionIndex(NX, NY);
                      //  if (CI.IsTimerTicking) return 0;
                        RestoreBlocked.Add(CI);
                        return 1;
                    }
                }
                return 0;
            });
            if (count > 3)
            {
                Debug("Clearing small area " + v3);
                foreach (CollisionIndex CI in RestoreBlocked)
                {
                  //  CI.IsTimerTicking = true;
                    CI.SetMatrixForced(CP,MAYBE_BLOCKED);
                }
                new Thread(() =>
                {
                    Thread.Sleep(30000);
                    foreach (CollisionIndex CI in RestoreBlocked)
                    {
                       // CI.IsTimerTicking = false;
                        CI.SetMatrixForced(CP, BLOCKED);
                    }
                });
                if (PanelGUI != null) PanelGUI.Invalidate();
                return v3;
            }
            return v3;

            SimWaypoint swp = GetWaypointOf(v3);
            for (float distance = PathStore.StepSize; distance < useDist * 2; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3, CP);
                    if (b < BLOCKED) return v3;
                }
            }
            Debug("Clearing area " + swp);
            SetNodeQualityTimer(v3, MAYBE_BLOCKED, 30);
            for (float distance = PathStore.StepSize; distance < useDist * 2; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3, CP);
                    if (b == BLOCKED)
                    {
                        SetNodeQualityTimer(v3, MAYBE_BLOCKED, 30);
                    }
                }
            }
            if (PanelGUI != null) PanelGUI.Invalidate();
            return GetWaypointOf(v3).SimPosition;
        }

        public Vector3 GetUsableLocalPositionOfOLD(CollisionPlane CP, Vector3 v3, float useDist)
        {
            SimPathStore PathStore = GetPathStore3D(v3);

            byte b = PathStore.GetNodeQuality(v3, CP);
            // float useDist = GetSizeDistance();      
            if (b != BLOCKED) return v3;
            SimWaypoint swp = GetWaypointOf(v3);
            for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3, CP);
                    if (b < BLOCKED) return v3;
                }
            }
            Debug("Clearing area " + swp);
            SetNodeQualityTimer(v3, MAYBE_BLOCKED, 30);
            for (float distance = PathStore.StepSize; distance < useDist * 1.5; distance += PathStore.StepSize)
            {
                for (int dir = 0; dir < 360; dir += 15)
                {
                    v3 = GetLocalLeftPos(swp, dir, distance);
                    b = PathStore.GetNodeQuality(v3, CP);
                    if (b == BLOCKED)
                    {
                        SetNodeQualityTimer(v3, MAYBE_BLOCKED, 30);
                    }
                }
            }
            return GetWaypointOf(v3).SimPosition;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="zAngleFromFace"></param>
        /// <param name="distance"></param>
        /// <returns></returns>
        static public Vector3 GetLocalLeftPos(SimPosition pos, int zAngleFromFace, double distance)
        {
            //double RAD_TO_DEG = 57.29577951f;
            //double PI2 = (double)(Math.PI * 2.0);

            while (zAngleFromFace > 360)
            {
                zAngleFromFace -= 360;
            }
            while (zAngleFromFace < 0)
            {
                zAngleFromFace += 360;
            }

            Vector3 result = pos.SimPosition + GetXYDiffOfMovement(zAngleFromFace, pos.SimRotation, distance);


            if (result.X > 254f)
            {
                result.X = 254;
            }
            else if (result.X < 1f)
            {
                result.X = 1;
            }
            if (result.Y > 254f)
            {
                result.Y = 254;
            }
            else if (result.Y < 1f)
            {
                result.Y = 1;
            }
            return result;
            /*
             * Client.Self.Movement.SendManualUpdate(AgentManager.ControlFlags.AGENT_CONTROL_AT_POS, Client.Self.Movement.Camera.Position,
                    Client.Self.Movement.Camera.AtAxis, Client.Self.Movement.Camera.LeftAxis, Client.Self.Movement.Camera.UpAxis,
                    Client.Self.Movement.BodyRotation, Client.Self.Movement.HeadRotation, Client.Self.Movement.Camera.Far, AgentFlags.None,
                    AgentState.None, true);*/
        }

        static Vector3 GetXYDiffOfMovement(int zAngleFromFace, Quaternion rot, double distance)
        {
            double radAngle = zAngleFromFace / RAD2DEG;
            rot.Normalize();
            float rx, ry, rz;
            rot.GetEulerAngles(out rx, out rz, out ry);
            //if (rx != 0f || ry != 0f)
            //{
            //    Debug("180 Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            //else
            //{
            //    Debug("Current Eulers:  {0} {1} {2}", rx * RAD_TO_DEG, ry * RAD_TO_DEG, rz * RAD_TO_DEG);
            //}
            double az = rz + radAngle;


            while (az < 0)
            {
                az += PI2;
            }
            while (az > PI2)
            {
                az -= PI2;
            }

            float xmul = (float)Math.Cos(az);
            float ymul = (float)Math.Sin(az);
            return new Vector3(xmul, ymul, 0) * (float)distance;
        }

        public void SetNodeQualityTimer(Vector3 vector3, int value, int seconds)
        {
            SimPathStore PathStore = GetPathStore3D(vector3);
            Point P = PathStore.ToPoint(vector3);
            CollisionIndex WP = PathStore.GetCollisionIndex(P.X, P.Y);
            List<ThreadStart> undo = new List<ThreadStart>();
            foreach (CollisionPlane CP in PathStore.CollisionPlanesFor(vector3.Z))
                WP.SetNodeQualityTimer(CP, value, undo);
            if (undo.Count > 0)
                new Thread(() =>
                               {
                                   Thread.Sleep(seconds*1000);
                                   foreach (ThreadStart u in undo)
                                   {
                                       u();
                                   }
                               }).Start();
        }

        private IEnumerable<CollisionPlane> CollisionPlanesFor(float z)
        {
            List<CollisionPlane> CPS = new List<CollisionPlane>();
            lock (Matrixes)
                foreach (CollisionPlane CP in Matrixes)
                    if (CP.Accepts(z))
                        CPS.Add(CP);
            return CPS;
        }

        private IEnumerable<CollisionPlane> CollisionPlanesFor(float min,float max)
        {
            List<CollisionPlane> CPS = new List<CollisionPlane>();
            lock (Matrixes)
                foreach (CollisionPlane CP in Matrixes)
                    if (CP.Overlaps(min,max))
                        CPS.Add(CP);
            return CPS;
        }

        static Dictionary<Vector2, SimPathStore> _PathStores = new Dictionary<Vector2, SimPathStore>();

        readonly float MAXY = 256f;

        static Vector2 vC = new Vector2(0, 0), // C
               vN = new Vector2(0, 1), // N
               vNE = new Vector2(1, 1), // NE
               vE = new Vector2(1, 0),  // E
               vSE = new Vector2(1, -1), // SE
               vS = new Vector2(0, -1), // S
               vSW = new Vector2(-1, -1), // SW
               vW = new Vector2(-1, 0), // W
               vNW = new Vector2(-1, 1); // NW

        public static Vector2[] XYOf = { vC, vN, vNE, vE, vSE, vS, vSW, vW, vNW };

        public void SetPathStoreAtOffset(Vector2 v2, SimPathStore value)
        {
            RegisterPathStore((GetGridLocation() + v2), value);
        }

        public SimPathStore GetOffsetPathStore(Vector2 v2)
        {
            return GetPathStore(GetGridLocation() + v2);
        }

        private Vector2 GetGridLocation()
        {
            return RegionLocation;
        }

        static void RegisterPathStore(Vector2 h, SimPathStore value)
        {
            lock (_PathStores) if (_PathStores.ContainsKey(h))
            {
                SimPathStore OLD = _PathStores[h];
                if (OLD == null || OLD == value) return;
                throw new ArgumentException("Bad region change " + OLD + " -> " + value);
            }
            _PathStores[h] = value;
        }

        public SimPathStore N
        {
            get { return GetOffsetPathStore(vN); }
            set { SetPathStoreAtOffset(vN, value); }
        }
        public SimPathStore E
        {
            get { return GetOffsetPathStore(vE); }
            set { SetPathStoreAtOffset(vE, value); }
        }
        public SimPathStore S
        {
            get { return GetOffsetPathStore(vS); }
            set { SetPathStoreAtOffset(vS, value); }
        }
        public SimPathStore W
        {
            get { return GetOffsetPathStore(vW); }
            set { SetPathStoreAtOffset(vW, value); }
        }


        /// <summary>
        ///  The closet usable waypoint to the v3 TODO
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public SimWaypoint GetWaypointOf(Vector3 v3)
        {
            if (v3.X < 0)
            {
                Vector3 V = v3;
                V.X += 256f;
                return W.GetWaypointOf(V);
            }
            else
                if (v3.X >= MAXY)
                {
                    Vector3 V = v3;
                    V.X -= 256f;
                    return E.GetWaypointOf(V);
                }
                else
                    if (v3.Y < 0)
                    {
                        Vector3 V = v3;
                        V.Y += 256f;
                        return S.GetWaypointOf(V);
                    }
                    else
                        if (v3.Y >= MAXY)
                        {
                            Vector3 V = v3;
                            V.Y -= 256f;
                            return N.GetWaypointOf(V);
                        }
                        else
                        {
                            SimPathStore PathStore0 = GetPathStore3D(v3);
                            return SimWaypointImpl.CreateLocal(v3, PathStore0);
                        }

            SimPathStore PathStore = GetPathStore3D(v3);
            SimWaypoint swp = PathStore.CreateClosestRegionWaypoint(v3, 2);
            float dist = Vector3.Distance(v3, swp.SimPosition);
            if (!swp.IsPassable)
            {
                Debug("CreateClosestWaypoint: " + v3 + " <- " + dist + " -> " + swp + " " + this);
            }
            return swp;
        }



        public IList<Vector3d> GetLocalPath(CollisionPlane CP, Vector3 start, Vector3 end, out bool faked)
        {

            CP.EnsureUpdated();
            float Z = start.Z;
            if (!IsPassable(start, CP))
            {
                Vector3 newStart = start;
                Debug("start is not passable: " + start);
                for (int i = 0; i < 360; i += 45)
                {
                    newStart = start + GetXYDiffOfMovement(i, Quaternion.Identity, 4);
                    if (IsPassable(newStart))
                    {
                        break;
                    }
                }
                if (newStart == start)
                {
                    start = GetUsableLocalPositionOf(CP, start, 2);
                }
            }
            if (!IsPassable(end, CP))
            {
                Debug("end is not passable: " + end);
            }
            return (IList<Vector3d>)GetLocalPath0(start, GetUsableLocalPositionOf(CP,end, 2), CP, Z, out faked);
        }

        float[,] _GroundPlane;
        public float[,] GroundPlane
        {
            get
            {
                if (_GroundPlane == null)
                {

                    int MAPSPACE1 = MAPSPACE - 1; // 1270
                    _GroundPlane = new float[MAPSPACE,MAPSPACE];
                    float fy = 256f;
                    for (int y = MAPSPACE1; y >= 0; y--)
                    {
                        fy = fy - StepSize;
                        float fx = 256f;
                        for (int x = MAPSPACE1; x >= 0; x--)
                        {
                            fx = fx - StepSize;
                            _GroundPlane[x, y] = GetGroundLevel(fx, fy);
                        }
                    }


#if COLLIDER_ODE
                    if (GroundLevel512 == null)
#endif
                    {
                        MAPSPACE1--;

                        // smooth it
                        for (int i = (int) POINTS_PER_METER; i > 0; i--)
                        {
                            fy = 256f;
                            for (int y = MAPSPACE1; y > 1; y--)
                            {
                                fy = fy - StepSize;
                                float fx = 256f;
                                for (int x = MAPSPACE1; x > 1; x--)
                                {
                                    fx = fx - StepSize;
                                    _GroundPlane[x, y] =
                                        (_GroundPlane[x, y]*2 +

                                         _GroundPlane[x + 1, y] +
                                         _GroundPlane[x, y + 1] +
                                         _GroundPlane[x + 1, y + 1] +

                                         _GroundPlane[x - 1, y] +
                                         _GroundPlane[x, y - 1] +
                                         _GroundPlane[x - 1, y - 1] +

                                         _GroundPlane[x - 1, y + 1] +
                                         _GroundPlane[x + 1, y - 1])/10;

                                }
                            }
                        }
                    }
                }
                return _GroundPlane;
            }
        }


#if COLLIDER_ODE
        private object StartedBakingTerrainLock = new Object();
        private Thread PutTerrainInSceneThread;

        private void StartGatheringTerrainFromCallback()
        {
            lock (StartedBakingTerrainLock)
            {
                if (PutTerrainInSceneThread != null)
                    return;
                if (GroundLevelDelegate == null)
                    return;
                PutTerrainInSceneThread = new Thread(new ThreadStart(PutTerrainInScene));
                PutTerrainInSceneThread.Name = String.Format("PutTerrainInSceneThread {0}", RegionName);
                PutTerrainInSceneThread.Start();
            }
        }

        public float[,] GroundLevel512;
      
        public void PutTerrainInScene()
        {
            float[] hts =
                new float[256 * 256];
            Thread.Sleep(15000);
            for (int x = 0; x < 256; x++)
                for (int y = 0; y < 256; y++)
                    hts[y * 256 + x] = GetGroundLevel(x, y);


            odeScene.SetTerrain(hts);
            GroundLevel512 = odeScene.GroundLevel512;

            _GroundPlane = null;
        }
#endif

        public float GetGroundLevel(float x, float y)
        {
            if (GroundLevelDelegate == null) return 10f;

#if COLLIDER_ODE

            if (GroundLevel512!=null)
            {
                int ix = (int)(x * 2);
                int iy = (int)(y * 2);
                return GroundLevel512[ix, iy];
            }
#endif
            return GroundLevelDelegate(x,y);
        }

        public void BlockRange(float x, float y, float sx, float sy, float z)
        {
            SimPathStore PathStore = GetPathStore3D(new Vector3(x, y, z));
            if (PathStore == null) return;
            float StepSize = PathStore.StepSize;
            sx += x;
            sy += y;
            float loopY = sy;
            while (sx >= x)
            {
                while (sy >= y)
                {
                    PathStore.SetBlocked(sx, sy, z, null);
                    sy -= StepSize;
                }
                sy = loopY;
                sx -= StepSize;
            }
        }

        public Vector3 ZAngleVector(double ZAngle)
        {
            while (ZAngle < 0)
            {
                ZAngle += PI2;
            }
            while (ZAngle > PI2)
            {
                ZAngle -= PI2;
            }
            return new Vector3((float)Math.Sin(ZAngle), (float)Math.Cos(ZAngle), 0);
        }

        public static Vector3 EnterEdge(Vector3 localLast, Vector2 dir)
        {
            if (Math.Abs(dir.X) > Math.Abs(dir.Y))
            {
                dir.Y = 0;
            }
            else  // avoid diagonals
            {
                dir.X = 0;
            }

            Vector3 exitEdge = new Vector3(localLast);
            if (dir.X != 0)
            {
                exitEdge.X = 256f - exitEdge.X;
            }
            if (dir.Y != 0)
            {
                exitEdge.Y = 256f - exitEdge.Y;
            }
            return exitEdge;
        }


        private /*i*/ float _XY256;

        public float XY256
        {
            get { return _XY256; }
        }
        private /*i*/ float _Max256;

        public float Max256
        {
            get { return _Max256; }
        }
        private /*i*/ Box3Fill _OuterBounds;

        public Box3Fill OuterBounds
        {
            get { return _OuterBounds; }
        }

        private int _AddedCount = 0;
        public int AddedCount
        {
            get { return _AddedCount; }
            set { _AddedCount = value; }
        }

        private CollisionIndex[,] _MeshIndex;

        public CollisionIndex[,] MeshIndex
        {
            get
            {
                if (_MeshIndex == null)
                    _MeshIndex = new CollisionIndex[MAPSPACE, MAPSPACE];
                return _MeshIndex;
            }
        }

        public override string ToString()
        {
            return String.Format("{0}: {1}", GetType().Name,RegionName);// +" Level=" + SimZAverage;
        }

        public const byte BLOCKED = 255;
        public const byte MAYBE_BLOCKED = 254;
        public const byte BLOCK_PURPLE = 253;
        public const byte BLOCKED_YELLOW = 200;
        public const byte BLOCK_ORANGE = 203;
        public const byte BLOCKED_AIR = 206;
        public const byte TOO_HIGH = 208;
        public const byte TOO_LOW = 209;
        public const byte WATER_G = 210;
        public const byte WATER_Z = 211;
        public const byte BRIDGY = 44;
        public const byte INITIALLY = 20;
        public const byte PASSABLE = 1;
        public const byte STICKY_PASSABLE = 0;
        readonly public float POINTS_PER_METER = 5f;
        readonly private float LargeScale = 1f;//StepSize;//0.2f;

        public static float PI2 = (float)(Math.PI * 2f);
        public static float RAD2DEG = 360f / PI2;

        readonly public float StepSize;// = 1f / POINTS_PER_METER;
        readonly public int MAPSPACE;// = XY256 * ((int)POINTS_PER_METER);

        /// <summary>
        ///  Todo needs a smarter way to grab the right layers.. 
        ///    The CollisionPlane[] is pretty much a hack
        /// </summary>
        /// <param name="Z"></param>
        /// <returns></returns>
        public CollisionPlane GetCollisionPlane(float Z)
        {
            lock (Matrixes) return GetCollisionPlane0(Z);
        }
        internal CollisionPlane GetCollisionPlane0(float Z)
        {
            if (Matrixes.Count > 0)
            {                
                foreach (CollisionPlane P in Matrixes)
                {
                    if (P.Accepts(Z))
                    {                        
                        P.LastUsed = DateTime.Now;
                        return P;
                    }
                }
            }
            return CreateMoverPlane(Z);
        }


        internal CollisionPlane FindCollisionPlane(float Z)
        {
            if (Matrixes.Count > 0)
            {
                lock (Matrixes) foreach (CollisionPlane P in Matrixes)
                {
                    if (P.Accepts(Z))
                    {
                        return P;
                    }
                }
            }
            return null;
        }
        public CollisionPlane RemoveCollisionPlane(float Z)
        {
            if (Matrixes.Count > 0)
            {
                lock (Matrixes) foreach (CollisionPlane P in Matrixes)
                    {
                        if (P.Accepts(Z))
                        {
                            Matrixes.Remove(P);
                            return P;
                        }
                    }
            }
            return null;
        }
        public void RemoveAllCollisionPlanes()
        {
            lock (Matrixes) Matrixes.Clear();
        }
      //  float CollisionPlaneHeights = 3.0f;
        readonly internal IList<CollisionPlane> Matrixes = new List<CollisionPlane>();
        public byte[,] GetByteMatrix(float Z)
        {
            return GetCollisionPlane(Z).ByteMatrix;
        }

        //readonly string RegionName;
        //readonly byte[,] paths = PathFinding.PathFinderDemo.Instance


        public float UNARRAY_X(int p)
        {
            return p / POINTS_PER_METER + Start.X;
        }
        public float UNARRAY_Y(int p)
        {
            return p / POINTS_PER_METER + Start.Y;
        }

        public int ARRAY_X(float x)
        {
            x -= Start.X;
            if (x < StepSize) return 0;
            double i = Math.Round(x * POINTS_PER_METER, 1);
            int ii = (int)i;
            if ((double)i != ii)
            {
                //throw new ArgumentException("ARRAY_IDX " + i + "!=" + ii);
            }
            if (ii < 0) return 0;
            if (ii >= MAPSPACE) ii = MAPSPACE - 1;
            return ii;
        }

        public int ARRAY_Y(float x)
        {
            x -= Start.Y;
            if (x < StepSize) return 0;
            double i = Math.Round(x * POINTS_PER_METER, 1);
            int ii = (int)i;
            if ((double)i != ii)
            {
                //throw new ArgumentException("ARRAY_IDX " + i + "!=" + ii);
            }
            if (ii < 0) return 0;
            if (ii >= MAPSPACE) ii = MAPSPACE - 1;
            return ii;
        }

        readonly Color[] lastColour = new Color[256];//(Color.Black);

        public Color GetColor(CollisionPlane CP, int x, int y , byte[,] matrix)
        {
            byte p = matrix[x, y];
            switch (p)
            {
                case STICKY_PASSABLE:
                    return OccupiedColor(CP, Color.Blue, MeshIndex[x, y]);
                case PASSABLE:
                    return OccupiedColor(CP, Color.Green, MeshIndex[x, y]);
                case BLOCKED:
                    return OccupiedColor(CP, Color.Olive, MeshIndex[x, y]);
                case MAYBE_BLOCKED:
                    return OccupiedColor(CP, Color.Pink, MeshIndex[x, y]);
                case BLOCKED_YELLOW:
                    return OccupiedColor(CP, Color.Yellow, MeshIndex[x, y]);
                case BLOCK_ORANGE:
                    return OccupiedColor(CP, Color.Orange, MeshIndex[x, y]);
                case BLOCK_PURPLE:
                    return OccupiedColor(CP, Color.Orchid, MeshIndex[x, y]);
                case WATER_G:
                    return OccupiedColor(CP, Color.Blue, MeshIndex[x, y]);
                case WATER_Z:
                    return OccupiedColor(CP, Color.CornflowerBlue, MeshIndex[x, y]);
                case TOO_LOW:
                    return OccupiedColor(CP, Color.Tomato, MeshIndex[x, y]);
                case TOO_HIGH:
                    return OccupiedColor(CP, Color.Firebrick, MeshIndex[x, y]);
                case BRIDGY:
                    return OccupiedColor(CP, Color.DarkTurquoise, MeshIndex[x, y]);
            }
            Color sb = lastColour[p];
            if (sb == Color.Empty)
            {
                int colorIndex = 240 - ((int)(Math.Log10(p) * 127));
                colorIndex = colorIndex < Byte.MinValue ? Byte.MinValue : colorIndex > Byte.MaxValue ? Byte.MaxValue : colorIndex;
                sb = Color.FromArgb(Byte.MaxValue, colorIndex, colorIndex, colorIndex);
                lastColour[p] = sb;
            }
            return sb;
        }

        private static Color OccupiedColor(CollisionPlane CP, Color c, CollisionIndex cIndex)
        {
            //return c;
            if (cIndex != null)
            {
                Color cAdd = cIndex.DebugColor(CP);
                int dense = cIndex.OccupiedCount;
                int A = 240 - 10 * dense;
                if (A < 0) A = 20;
                if (cAdd!=Color.Empty)
                {
                    c = cAdd; 
                }
                return Color.FromArgb(A, c.R, c.G, c.B);
            }
            return c;
        }


        public byte GetNodeQuality(Vector3 v3, CollisionPlane CP)
        {
            return CP.ByteMatrix[ARRAY_X((v3.X)), ARRAY_Y((v3.Y))];
        }

        public void SetNodeQuality(Vector3 v3, byte v, CollisionPlane CP)
        {
            CP.ByteMatrix[ARRAY_X((v3.X)), ARRAY_Y((v3.Y))] = v;
        }

        /// <summary>
        /// Will not changed blocked points - if needed use SetPassable
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        public void SetTraveled(float x, float y, float z)
        {
            return;
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            foreach (CollisionPlane CP in CollisionPlanesFor(z))
            {
                byte[,] ByteMatrix = CP.ByteMatrix;
                ///Debug("SetBlocked: {0} {1}", x, y);
                byte b = ByteMatrix[ix, iy];
                switch (b)
                {
                    case BLOCKED:
                        {
                           // ByteMatrix[ix, iy] = MAYBE_BLOCKED;
                            continue;
                        }
                    case MAYBE_BLOCKED:
                        continue;
                    case STICKY_PASSABLE:
                        continue;
                    case PASSABLE:
                        continue;
                    case INITIALLY:
                        continue;
                }
                if (b < 3)
                {
                    continue;
                }
                ByteMatrix[ix, iy] = (byte)(b / 2);
            }
        }

        public CollisionIndex SetObjectAt(float x, float y, IMeshedObject simObject, float minZ, float maxZ)
        {
            int ix = ARRAY_X((x));
            int iy = ARRAY_Y((y));

            CollisionIndex W = GetCollisionIndex(ix, iy);
            if (W.AddOccupied(simObject,minZ, maxZ))
            {
               // NeedsUpdate = true;
            }
            return W;
        }

        public void SetPassable(float x, float y, float z)
        {
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            foreach (CollisionPlane CP in CollisionPlanesFor(z))
            {
                byte[,] ByteMatrix = CP.ByteMatrix;
                ///Debug("SetBlocked: {0} {1}", x, y);
                if (ByteMatrix[ix, iy] > 100)
                {
                    continue;
                }
                ByteMatrix[ix, iy] = STICKY_PASSABLE;           
            }
        }

        //static IList<ISimObject> NOOBJECTS = new List<ISimObject>();
        //private IEnumerable<ISimObject> ObjectsAt(float x, float y)
        //{
        //    x = RangeCheck(x); y = RangeCheck(y);
        //    ///Debug("SetBlocked: {0} {1}", x, y);
        //    int ix = ARRAY_IDX(x);
        //    int iy = ARRAY_IDX(y);
        //    CollisionIndex P = mWaypoints[ix, iy];
        //    if (P == null)
        //    {
        //        return NOOBJECTS;
        //    }
        //    return P.OccupiedListObject;
        //}



        public void SetBlocked(float x, float y, float z, IMeshedObject blocker)
        {
            if (blocker != null) SetObjectAt(x, y, blocker,z,z);
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            foreach (CollisionPlane CP in CollisionPlanesFor(z))
            {
                byte[,] ByteMatrix = CP.ByteMatrix;
                ///Debug("SetBlocked: {0} {1}", x, y);
                // if was set Passable dont re-block
                if (ByteMatrix[ix, iy] == STICKY_PASSABLE)
                {
                    //  return;
                }
                ByteMatrix[ix, iy] = BLOCKED;
            }
            //   SetBubbleBlock(ix, iy, blocker);
        }

        //private void SetBubbleBlock(int ix, int iy, ISimObject blocker)
        //{
        //    int TWO = 2;// (int)Math.Round(POINTS_PER_METER / 2);
        //    int ONE = 1;// (int)Math.Round(POINTS_PER_METER / 2);
        //    if (ix > TWO && iy > TWO && ix < MAPSPACE - TWO && iy < MAPSPACE - TWO)
        //    {
        //        for (int x = ix - ONE; x < ix + TWO; x++)
        //        {
        //            for (int y = iy - ONE; y < iy + TWO; y++)
        //            {
        //                if (x != ix || y != iy)
        //                {
        //                    if (ZMatrix(Z)[x, y] != 0)
        //                    {
        //                        if (blocker != null)
        //                        {
        //                            CollisionIndex W = Waypoint(x, y);
        //                            if (W.AddShadow(blocker) > 1)
        //                            {
        //                                ZMatrix(Z)[x, y] = 0;
        //                            }
        //                            else
        //                            {
        //                                int newV = ZMatrix(Z)[x, y] * 2;
        //                                if (newV > 200) newV = 200;
        //                                ZMatrix(Z)[x, y] = (byte)newV;
        //                            }
        //                        }
        //                    }
        //                }
        //            }
        //        }
        //    }
        //}

        public SimPathStore GetPathStore()
        {
            return this;
        }

        public readonly Vector2 RegionLocation;
        readonly Vector3d GlobalStart;
        readonly Vector3d GlobalEnd;
        readonly Vector3 Start;
        readonly Vector3 Size;

#if COLLIDER_ODE  
        readonly static Meshmerizer meshMerizer = new Meshmerizer();
#endif
        /// <summary>
        /// Constructor.
        /// </summary>
        public SimPathStore(String simName, Vector2 pos, Vector3d globalPos, Vector3 endTop)
        {
            RegisterPathStore(pos, this);
            RegionName = simName;
            RegionLocation = pos;
            Start = Vector3.Zero;
            GlobalStart = globalPos;
            GlobalEnd = globalPos;
            Size = endTop;
            _XY256 = Size.X;
            _OuterBounds = new Box3Fill(true);
            OuterBounds.AddPoint(Start.X, Start.Y, Start.Z, Vector3.Zero);
            OuterBounds.AddPoint(endTop.X, endTop.Y, endTop.Z, Vector3.Zero);
            //TheSimZMinMaxLevel = new SimZMinMaxLevel(MinMaxLevel);
            StepSize = 1f/POINTS_PER_METER;
            _Max256 = XY256 - StepSize;
            MAPSPACE = (int) XY256*((int) POINTS_PER_METER);
            RegisterHttp();
            if (Size.X != Size.Y) throw new Exception("X and Y must be the same for " + this);
#if COLLIDER_ODE            
            odeScene = (OdeScene) odePhysics.GetScene(RegionName);
            odeScene.Initialise(meshMerizer, null);
            float[] _heightmap = new float[256*256];
            for (int i = 0; i < (256*256); i++)
            {
                _heightmap[i] = 21f;
            }
            odeScene.SetTerrain(_heightmap);
#endif
            //CreateDefaultRoutes();
            //  CurrentPlane = new CollisionPlane(MAPSPACE,MAPSPACE,0);
        }

        //private float RangeCheckX(float PtY)
        //{
        //    PtY -= Start.X;
        //    if (PtY > Max256)
        //    {
        //        return Max256;
        //    }
        //    else if (PtY < 0)
        //    {
        //        return 0f;
        //    }
        //    else
        //    {
        //        return (float)(Math.Round(PtY * POINTS_PER_METER, 0) / POINTS_PER_METER);
        //    }
        //}
        //private float RangeCheckY(float PtY)
        //{
        //    PtY -= Start.Y;
        //    if (PtY > Max256)
        //    {
        //        return Max256;
        //    }
        //    else if (PtY < 0)
        //    {
        //        return 0f;
        //    }
        //    else
        //    {
        //        return (float)(Math.Round(PtY * POINTS_PER_METER, 0) / POINTS_PER_METER);
        //    }
        //}

        public CollisionIndex FindNode(float x, float y)
        {
            return MeshIndex[ARRAY_X((x)), ARRAY_Y((y))];
        }

        public CollisionIndex CreateFirstNode(float x, float y)
        {
            int ix = ARRAY_X(x);
            int iy = ARRAY_Y(y);
            return GetCollisionIndex(ix, iy);
        }

        public CollisionIndex GetCollisionIndexNoSideEffect(int ix, int iy)
        {
            lock (MeshIndex)
            {
                CollisionIndex wp = MeshIndex[ix, iy];
                return wp;
            }
        }

        public CollisionIndex GetCollisionIndex(int ix, int iy)
        {
            lock (MeshIndex)
            {
                CollisionIndex wp = MeshIndex[ix, iy];
                if (wp == null)
                {
                    float x = UNARRAY_X(ix);
                    float y = UNARRAY_Y(iy);
                    wp = CollisionIndex.CreateCollisionIndex(x, y, this);
                    //if (mWaypoints[ix, iy] != wp)
                    //{
                    //    Debug("diff wapoints in same space {0} != {1}", mWaypoints[ix, iy], wp);
                    //}
                }
                return wp;
            }
        }
         

        public IList<SimRoute> GetSimplifedRoute(IList<SimRoute> v3s)
        {
            List<SimRoute> vectors = new List<SimRoute>();
            List<SimRoute> skipped = new List<SimRoute>();
            float ZAngle = Single.NaN;
            bool ZAngleValid = false;
            Vector3d currentV3 = v3s[0].StartNode.Position;
            vectors.Add(v3s[0]);
            for (int Current = 0; Current < v3s.Count; Current++)
            {
                Vector3d compare = v3s[Current].EndNode.Position;
                Vector3d dif = compare - currentV3;
                float NewZAngle = ComparableAngle(Math.Atan2(dif.X, dif.Y));
                if (!ZAngleValid)
                {
                    ZAngleValid = true;
                }
                else
                {
                    float adif = Math.Abs(NewZAngle - ZAngle);
                    if (adif * RAD2DEG > 10)
                    {
                        if (skipped.Count == 0)
                        {
                            vectors.Add(v3s[Current]);
                        }
                        else
                        {
                            SimRoute R = new SimRouteMulti(skipped.ToArray());
                            R.ReWeight(0.9f);
                            AddArc(R);
                            skipped.Clear();
                            vectors.Add(R);
                            skipped.Add(v3s[Current]);
                        }
                    }
                    else
                    {
                        skipped.Add(v3s[Current]);
                    }
                }
                ZAngle = NewZAngle;
                currentV3 = compare;
            }

            return vectors;
        }


        static public IList<Vector3d> GetSimplifedRoute(Vector3d currentV3In, IList<Vector3d> v3s, int MaxTurnDegrees, float MaxDist)
        {
            if (v3s.Count < 3) return v3s;
            Vector3d currentV3 = currentV3In;
            IList<Vector3d> vectors = new List<Vector3d>();
            float MaxTurnRadians = MaxTurnDegrees / RAD2DEG;
            float ZAngle = Single.NaN;
            bool ZAngleValid = false;
            int Max = v3s.Count - 1;
            for (int Current = 0; Current < Max; Current++)
            {
                bool UsePoint = false;
                Vector3d compare = v3s[Current];
                Vector3d dif = compare - currentV3;
                float NewZAngle = ComparableAngle(Math.Atan2(dif.Y, dif.X));
                if (!ZAngleValid)
                {
                    ZAngleValid = true;
                    ZAngle = NewZAngle;
                    // UsePoint = true;
                }
                else
                {
                    float adif = Math.Abs(NewZAngle - ZAngle);
                    if (adif > MaxTurnRadians)
                        UsePoint = true;
                    else
                        if (DistanceNoZ(currentV3, compare) > MaxDist)
                            UsePoint = true;
                }
                if (UsePoint)
                {
                    vectors.Add(compare);
                    currentV3 = compare;
                    ZAngle = NewZAngle;
                }
            }
            // add last
            vectors.Add(v3s[v3s.Count - 1]);
            return vectors;
        }


        private static float ComparableAngle(double p)
        {
            while (p < 0)
            {
                p += PI2;
            }
            while (p > PI2)
            {
                p -= PI2;
            }
            return (float)p + PI2;
        }


        static public IList<SimWaypoint> RouteListToPoints(IList<SimRoute> routeToSimplify)
        {
            IList<SimWaypoint> points = new List<SimWaypoint>();
            SimWaypoint Last = null;
            foreach (SimRoute move in routeToSimplify)
            {
                if (move.StartNode != Last)
                {
                    Last = move.StartNode;
                    points.Add(Last);
                }
                if (move.EndNode != Last)
                {
                    Last = move.EndNode;
                    points.Add(Last);
                }
            }
            return points;
        }
        
        public bool SendPathImage(IHttpRequest request, IHttpResponse response)
        {
            var pq = request.Uri.PathAndQuery;
            if (pq.StartsWith("/cogpath/path."))
            {         
                Bitmap imageToSave = null;
                imageToSave = imageToSave ?? new Bitmap(1280, 1280);
                ImageFormat bmp = ImageFormat.Gif;
                if (pq.Contains("bmp")) bmp = ImageFormat.Bmp;
                if (pq.Contains("jpg")) bmp = ImageFormat.Jpeg;
                if (pq.Contains("tif")) bmp = ImageFormat.Tiff;
                            
                var CurrentPlane = NewestMatrix();
                if (CurrentPlane != null)
                {
                    var Matrix = CurrentPlane.ByteMatrix;
                    for (int x = 0; x < 1280; x++)
                    {
                        int yy = 1280;
                        for (int y = 0; y < 1280; y++)
                        {
                            yy--;
                            Color sb = GetColor(CurrentPlane, x, y, Matrix);
                            imageToSave.SetPixel(x, yy, sb);
                        }
                    }
                }

                MemoryStream ms = new MemoryStream();
                imageToSave.Save(ms, bmp);
                response.ContentType = "image/" + bmp.ToString().ToLower();
                response.Body = ms;

                return true;
            }
            return false;
        }

        private void RegisterHttp()
        {
            MushDLR223.Utilities.ClientManagerHttpServer.OverrideHandlerList.Add(SendPathImage);
        }

        bool PunishChangeDirection;
        private IList<Vector3d> GetLocalPath0(Vector3 start, Vector3 end, CollisionPlane CP, float Z, out bool faked)
        {
            PathFinderDemo panel = PanelGUI;
            PunishChangeDirection = !PunishChangeDirection;    //toggle each time
            if (!PunishChangeDirection)
            {

            }
            if (panel != null)
            {
                panel.PnlGUI.CurrentPlane = CP;
            }
            Point S = ToPoint(start);
            Point E = ToPoint(end);
            IList<PathFinderNode> pfn = null;

            faked = false;
            try
            {
                var bm = CP.ByteMatrix;
                CP.SetSurroundings(S.X, S.Y, 2, bm, SimPathStore.PASSABLE);
                CP.SetSurroundings(E.X, E.Y, 2, bm, SimPathStore.PASSABLE);
                PathFinderFasting pff = new PathFinderFasting(bm);
                if (panel != null) panel.SetStartEnd(S, E);
                // pff.Diagonals = false;
                //pff.ReopenCloseNodes = true;
                pff.SearchLimit = 100000000;     // took off 2 0s
                pff.PunishChangeDirection = PunishChangeDirection;
                pfn = pff.FindPath(S, E);
                if (pfn == null)
                {
                    faked = true;
                    pfn = pff.FindPathFallback(S, E);
                }
                pff = null;
            }
            catch (Exception e)
            {
                Debug("Cant do route! " + e + " on " + CP);
            }
            if (pfn == null || pfn.Count == 0)
            {
                Debug("Cant do pfn on " + CP);
                IList<Vector3d> temp = new List<Vector3d>();
                temp.Add(GetPathStore().LocalToGlobal(end));
                faked = true;
                return temp;
            }
            if (panel != null) panel.ShowPath(pfn);
            List<Vector3d> r = (List<Vector3d>)PathfinderNodesToV3s(pfn, Z);
            r.Reverse();
          //  faked = false;
            return r;
        }

        public bool IsPassable(Vector3 end, CollisionPlane CP)
        {
            double Dist;
            if (GetNodeQuality(end,CP) == BLOCKED) return false;
            if (true) return true;
            SimWaypoint W = ClosestRegionNode(end.X, end.Y, end.Z, out Dist, true);
            return W.IsPassable;
        }



        private IList<Vector3d> PathfinderNodesToV3s(IList<PathFinderNode> pfn, float Z)
        {
            Vector3d V = GetGlobalCorner();
            IList<Vector3d> v3s = new List<Vector3d>();
            foreach (PathFinderNode P in pfn)
            {
                float x = UNARRAY_X(P.X);
                float y = UNARRAY_Y(P.Y);
                Vector3d v3 = new Vector3d(x + V.X, y + V.Y, Z);
                v3s.Add(v3);
            }
            return v3s;// GetSimplifedRoute(v3s[0], v3s, 5, 4f);
        }

        private Vector3d GetGlobalCorner()
        {
            return new Vector3d(GetPathStore().LocalToGlobal(new Vector3(0, 0, 0)));
        }

        public Point ToPoint(Vector3 start)
        {
            return new Point(ARRAY_X((start.X)), ARRAY_Y((start.Y)));
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="point"></param>
        /// <param name="rotation"></param>
        private void Update(UUID agentID, Vector3 point, Quaternion rotation)
        {
            SetTraveled(point.X, point.Y, point.Z);
        }


        internal void UpdateFromImage(Image image)
        {
            if (image == null) return;
            Bitmap edges = new Bitmap((Image)image.Clone());
            new DisplayImage("Edges", edges).Activate();
            Debug("START Edge detection " + image);
            Bitmap e = EdgeDetection(edges, 34f, delegate(int x, int y)
            {
                edges.SetPixel(x, y, Color.Yellow);
                SetBlocked((float)x, (float)y, 0f, null);
            });
            Debug("END Edge detection");
            new DisplayImage("Clone", e).Activate();
        }


        private delegate void EdgeAction(int x, int y);

        private static Bitmap EdgeDetection(Bitmap Image, double Threshold, EdgeAction EdgeColor)
        {
            System.Drawing.Bitmap TempBitmap = Image;
            System.Drawing.Bitmap NewBitmap = new System.Drawing.Bitmap(TempBitmap.Width, TempBitmap.Height);
            System.Drawing.Graphics NewGraphics = Graphics.FromImage(NewBitmap);
            NewGraphics.DrawImage(TempBitmap, new System.Drawing.Rectangle(0, 0, TempBitmap.Width, TempBitmap.Height), new System.Drawing.Rectangle(0, 0, TempBitmap.Width, TempBitmap.Height), GraphicsUnit.Pixel);
            NewGraphics.Dispose();
            for (int x = 0; x < NewBitmap.Width; ++x)
            {
                for (int y = 0; y < NewBitmap.Height; ++y)
                {
                    bool EdgeSet = false;
                    Color CurrentColor = NewBitmap.GetPixel(x, y);
                    if (y < NewBitmap.Height - 1 && x < NewBitmap.Width - 1)
                    {
                        Color TempColor = NewBitmap.GetPixel(x + 1, y + 1);
                        if (Math.Sqrt(((CurrentColor.R - TempColor.R) * (CurrentColor.R - TempColor.R)) +
                            ((CurrentColor.G - TempColor.G) * (CurrentColor.G - TempColor.G)) +
                            ((CurrentColor.B - TempColor.B) * (CurrentColor.B - TempColor.B))) > Threshold)
                        {
                            EdgeColor(x, y);
                        }
                        EdgeSet = true;
                    }
                    if (y < NewBitmap.Height - 1 && !EdgeSet)
                    {
                        Color TempColor = NewBitmap.GetPixel(x, y + 1);
                        if (Math.Sqrt(((CurrentColor.R - TempColor.R) * (CurrentColor.R - TempColor.R)) +
                            ((CurrentColor.G - TempColor.G) * (CurrentColor.G - TempColor.G)) +
                            ((CurrentColor.B - TempColor.B) * (CurrentColor.B - TempColor.B))) > Threshold)
                        {
                            EdgeColor(x, y);
                            // NewBitmap.SetPixel(x, y, EdgeColor);
                        }
                        EdgeSet = true;
                    }
                    if (x < NewBitmap.Width - 1 && !EdgeSet)
                    {
                        Color TempColor = NewBitmap.GetPixel(x + 1, y);
                        if (Math.Sqrt(((CurrentColor.R - TempColor.R) * (CurrentColor.R - TempColor.R)) +
                            ((CurrentColor.G - TempColor.G) * (CurrentColor.G - TempColor.G)) +
                            ((CurrentColor.B - TempColor.B) * (CurrentColor.B - TempColor.B))) > Threshold)
                        {
                            EdgeColor(x, y);
                            //NewBitmap.SetPixel(x, y, EdgeColor);
                        }
                        EdgeSet = true;
                    }
                }
            }
            return NewBitmap;
        }
        public static void Debug(string format, params object[] arg)
        {
            if (DebugLevel > 0) 
                Console.WriteLine(String.Format("[SimPathStore] {0}", format), arg);
        }


        public void TaintMatrix()
        {
            CollisionIndex[,] MeshIndex = this.MeshIndex;
            //lock (mWaypoints)
            for (int x = 0; x < MAPSPACE; x++)
            {
                for (int y = 0; y < MAPSPACE; y++)
                {
                    CollisionIndex W = MeshIndex[x, y];
                    if (W != null)
                        W.TaintMatrix();
                }
            }
        }

        public SimRoute InternArc(SimWaypoint StartNode, SimWaypoint EndNode, double Weight)
        {
            return SimGlobalRoutes.Instance.InternArc(StartNode, EndNode, Weight);
        }

        public SimWaypoint CreateClosestRegionWaypoint(Vector3 v3, double maxDist)
        {
            Vector3d v3d = GetPathStore().LocalToGlobal(v3);
            return CreateClosestWaypoint(v3d, maxDist);
        }

        public SimWaypoint CreateClosestWaypoint(Vector3d v3d, double maxDist)
        {
            double Dist;
            SimWaypoint W = SimGlobalRoutes.Instance.ClosestNode(v3d.X, v3d.Y, v3d.Z, out Dist, true);
            if (Dist > maxDist)
            {
                SimWaypoint V3 = SimGlobalRoutes.Instance.CreateClosestWaypoint(v3d);
                return V3;
            }
            return W;
        }
        public SimRoute Intern2Arc(SimWaypoint StartNode, SimWaypoint EndNode, double Weight)
        {
            return SimGlobalRoutes.Instance.Intern2Arc(StartNode, EndNode, Weight);
        }

        private SimWaypoint ClosestRegionNode(float x, float y, float z, out double Dist, bool IgnorePassable)
        {
            Vector3d v3d = GetPathStore().LocalToGlobal(new Vector3(x, y, z));
            return SimGlobalRoutes.Instance.ClosestNode(v3d.X, v3d.Y, v3d.Z, out Dist, IgnorePassable);
        }

        public void Clear()
        {
            SimGlobalRoutes.Instance.Clear();
        }

        private void AddArc(SimRoute R)
        {
            SimGlobalRoutes.Instance.AddArc(R);
        }

        Dictionary<UUID, MoverTracking> LaskKnownPos = new Dictionary<UUID, MoverTracking>();

        public void UpdateTraveled(UUID uUID, Vector3 after, Quaternion rot)
        {
            return;
            lock (LaskKnownPos) if (!LaskKnownPos.ContainsKey(uUID))
                {
                    LaskKnownPos[uUID] = new MoverTracking(after, rot, this);
                    return;
                }
            LaskKnownPos[uUID].Update(after, rot);
        }



        public void Refresh(Box3Fill changed, float BumpConstraint)
        {
            foreach (CollisionPlane CP in CollisionPlanesFor(changed.MinZ, changed.MaxZ))
                CP.Refresh(changed, BumpConstraint);
        }

        internal PathFinderDemo PanelGUI;
        public SimMover LastSimMover;

        public void ShowDebugger()
        {
            if (PanelGUI == null)
            {                
                (new Thread(() =>
                                {
                                    try
                                    {
                                        PanelGUI = new PathFinderDemo(this);
                                        PanelGUI.CollisionPlaneListUpdate();
                                        PanelGUI.Closing += new CancelEventHandler((sender, e) => PanelGUI = null);
                                        Application.EnableVisualStyles();
                                        PanelGUI.Show();
                                        Application.Run(PanelGUI);
                                    }
                                    catch (Exception e)
                                    {
                                        Debug("" + e);
                                        PanelGUI = null;
                                    }

                                }) {Name = string.Format("PathFinder Form for {0}", this.RegionName), IsBackground = true}).Start();
            }
            else
            {
                PanelGUI.CollisionPlaneListUpdate();
                PanelGUI.Show();
            }

        }

        public Vector3 FirstOcclusion(Vector3 start, Vector3 end)
        {
            Vector3 oclusion = start;
            Vector3 direction = end - start;
            float dist = direction.Length();
            while (direction.Length()>0.2)
            {
                direction *= 0.9f;               
            }
            float mag = direction.Length();
            float traveled = 0f;
            while (traveled<dist)
            {
                traveled += mag;
                oclusion += direction;
                if (!IsPassable(oclusion)) return oclusion;
            }
            return end;
        }
        public Vector3 LocalOuterEdge(Vector3 startLocal, Vector3d globalEnd, out SimPathStore nextRegion)
        {
            SimPathStore rother = GetPathStore(globalEnd);
            Vector3 vother = GlobalToLocal(globalEnd);//.GetSimPosition();
            if (rother == this)
            {
                nextRegion = this;
                return vother;
            }

            Vector2 VD = rother.GetGridLocation() - GetGridLocation();

            Vector2 SD = new Vector2(Math.Sign(VD.X), Math.Sign(VD.Y));
            float x = 128 + SD.X * 127;
            if (x == 128)
            {
                x = (startLocal.X + vother.X) / 2;
            }
            float y = 128 + SD.Y * 127;
            if (y == 128)
            {
                y = (startLocal.Y + vother.Y) / 2;
            }
            nextRegion = GetPathStore(GetGridLocation() + SD);
            return new Vector3(x, y, vother.Z);
        }


        public static SimPathStore GetPathStore(Vector3d pos)
        {
            if (pos.X < 0 || pos.Y < 0)
            {
                throw new ArgumentException("GlobalToWaypoint? " + pos);
            }
            if (pos.X < 256 || pos.Y < 256)
            {
                Debug("GlobalToWaypoint? " + pos);
            }
            return GetPathStore(new Vector2(Round256(pos.X)/256, Round256(pos.Y)/256));
        }

        public static SimPathStore GetPathStore(Vector2 loc)
        {
            lock (_PathStores)
            {
                SimPathStore PS;
                if (_PathStores.TryGetValue(loc,out PS)) {
                    return PS;
                }
                PS = new SimPathStore(loc.ToString(), loc, new Vector3d(loc.X * 256, loc.Y * 256, 0), new Vector3(256, 256, Single.MaxValue));
                return PS;
            }
        }

        public static Vector3 GlobalToLocal(Vector3d pos)
        {
            if (pos.X < 0 || pos.Y < 0)
            {
                throw new ArgumentException("GlobalToWaypoint? " + pos);
            }
            if (pos.X < 256 || pos.Y < 256)
            {
                Debug("GlobalToWaypoint? " + pos);
            }
            return new Vector3((float)pos.X - Round256(pos.X), (float)pos.Y - Round256(pos.Y), (float)pos.Z);
        }

        public static uint Round256(double global)
        {
            return ((uint)global / 256) * 256;
        }

        public SimPathStore GetPathStore3D(Vector3 vv3)
        {
            return this;
        }

        public static bool OutOfRegion(Vector3 v3)
        {
            if (v3.X < 0 || v3.X >= 256f)
                return true;
            if (v3.Y < 0 || v3.Y >= 256f)
                return true;
            if (v3 == Vector3.Zero) return true;
            return false;
        }

        public bool IsPassable(Vector3 next)
        {
            if (OutOfRegion(next)) return false;
            SimPathStore PathStore = GetPathStore3D(next);
            CollisionPlane CP = PathStore.GetCollisionPlane(next.Z);
            return PathStore.IsPassable(next, CP);
        }


        public CollisionIndex GetCollisionIndexAt(Vector3 V)
        {
            throw new NotImplementedException();
        }

        internal CollisionPlane CreateMoverPlane(float Z)
        {
            CollisionPlane found = new CollisionPlane(MAPSPACE, MAPSPACE, Z, this);
            Console.WriteLine("Created matrix[{0}] {1} for {2}", Z, found, this);
            DropOldMatrixes(6);
            lock (Matrixes) Matrixes.Add(found);
            if (PanelGUI != null) (new Thread(()=>     
                PanelGUI.OnNewCollisionPlane(found))).Start();
            return found;
        }

        public static bool Special(byte b)
        {
            switch (b)
            {
                case STICKY_PASSABLE:
                case PASSABLE:
                case BRIDGY:
                case BLOCKED:
                case MAYBE_BLOCKED:
                case BLOCKED_YELLOW:
                case BLOCK_PURPLE:
                case BLOCK_ORANGE:
                case WATER_G:
                case WATER_Z:
                case TOO_LOW:
                case TOO_HIGH:
                    return true;
                default:
                    return false;
            }
        }

        public static bool MaybeBlocked(byte b)
        {
            switch (b)
            {
                case MAYBE_BLOCKED:
                case BLOCKED:
                case BLOCKED_YELLOW:
                case BLOCK_PURPLE:
                case BLOCK_ORANGE:
                case WATER_G:
                case WATER_Z:
                case TOO_LOW:
                case TOO_HIGH:
                    return true;
                default:
                    return false;
            }
        }

        internal void TaintCollisionPlanes(Box3Fill OuterBox)
        {
            foreach (CollisionPlane list in Matrixes)
            {
                if (OuterBox.IsZInside(list.MinZ,list.MaxZ))
                {
                    list.HeightMapNeedsUpdate = true;
                }
            }
        }

        public bool IsUnderWater(Vector3d vector3)
        {
            return vector3.Z <= WaterHeight;
        }

        //public bool IsFlyZone(Vector3 vector3)
        //{
        //    return GetCollisionPlane(vector3.Z).IsFlyZone(ARRAY_X(vector3.X),ARRAY_Y(vector3.Y));
        //}

        public void RecomputeMatrix()
        {
            foreach (CollisionPlane list in Matrixes)
            {
                list.HeightMapNeedsUpdate = true;
                list.EnsureUpdated();
            }
        }
        public void DropOldMatrixes(int keep)
        {
            int drop = Matrixes.Count - keep;
            while (drop-- > 0)
            {
                CollisionPlane oldest = null;
                foreach (CollisionPlane list in Matrixes)
                {
                    if (oldest == null)
                    {
                        oldest = list;
                        continue;
                    }
                    if (oldest.LastUsed > list.LastUsed)
                    {
                        oldest = list;
                        continue;
                    }
                }
                Matrixes.Remove(oldest);
            }
        }

        public CollisionPlane NewestMatrix()
        {
            CollisionPlane newest = null;
            foreach (CollisionPlane list in Matrixes)
            {
                if (newest == null)
                {
                    newest = list;
                    continue;
                }
                if (newest.LastUsed < list.LastUsed)
                {
                    newest = list;
                    continue;
                }
            }
            return newest;
        }

        internal void Refresh(Box3Fill changed)
        {
           Refresh(changed,CollisionIndex.MaxBump);
        }

        public static double DistanceNoZ(Vector3d target, Vector3d position)
        {
            target.Z = position.Z;
            return Vector3d.Distance(target, position);
        }

    }

    public class MoverTracking
    {
        protected double MovedAllot = 3.0f;
        Vector3 LastPosition;
        Quaternion LastRotation;
        SimPathStore Store;
        public MoverTracking(Vector3 firstP, Quaternion firtsR, SimPathStore store)
        {
            LastPosition = firstP;
            Store = store;
            LastRotation = firtsR;
        }

        public void Update(Vector3 nextPosition, Quaternion rotation)
        {
            double dist = Vector3.Distance(LastPosition, nextPosition);
            if (dist > 1)
            {
                MakeMovement(nextPosition);
            }
            else
                if (RotationDiffernt(rotation, LastRotation))
                {
                    MakeMovement(nextPosition);
                    LastRotation = rotation;
                }
        }

        private void MakeMovement(Vector3 nextPosition)
        {
            float dist = Vector3.Distance(LastPosition, nextPosition);
            if (dist > Store.StepSize)
            {
                Store.SetTraveled(LastPosition, nextPosition);
                LastPosition = nextPosition;
            }
        }

        static bool RotationDiffernt(Quaternion rotation, Quaternion LastRotation)
        {
            Quaternion diff = rotation - LastRotation;
            return (diff.Length() > 0.2);
        }
    }
}
