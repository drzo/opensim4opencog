// Copyleft 2009 Douglas R. Miles (Daxtron Labs) - <dmiles@daxtron.com>
// Copyright 2003 Eric Marchesin - <eric.marchesin@laposte.net>
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
using System.Text;
using OpenMetaverse;
using cogbot.Listeners;
using System.Collections;
using System.IO;
using System.Threading;
using cogbot.TheOpenSims.Navigation.Debug;
using System.Runtime.Serialization.Formatters.Binary;
using System.Drawing;

namespace cogbot.TheOpenSims.Navigation
{
/// <summary>
	/// Graph structure. It is defined with :
	/// It is defined with both a list of nodes and a list of arcs.
	/// </summary>
    [Serializable]
    public class SimPathStore
    {
        public static readonly float POINTS_PER_METER = 10f;
        public static readonly float StepSize = 1f / POINTS_PER_METER;
        public static readonly float LargeScale = 1f;//StepSize;//0.2f;
        public static SimPathStore Instance = new SimPathStore("millspec.serz");
        IList<SimWaypoint> SimWaypoints;
        IList<SimRoute> SimRoutes;
        public float SimZLevel = 22;
        readonly SimWaypoint[,] saved = new SimWaypoint[(int)256 * (int)POINTS_PER_METER, (int)256 * (int)POINTS_PER_METER];


        /// <summary>
        /// Constructor.
        /// </summary>
        private SimPathStore(String simName)
        {
            Instance = this;
            RegionFileName = simName;
            SimWaypoints = new List<SimWaypoint>();
            SimRoutes = new List<SimRoute>();
            CreateDefaultRoutes();
            LoadFromFile();
        }

        private float RangeCheck(float PtY)
        {
            if (PtY > 255)
            {
                PtY = 255f;
            }
            else if (PtY < 0)
            {
                PtY = 0;
            }
            else
            {
                PtY = (float)(Math.Round(PtY * POINTS_PER_METER, 0) / POINTS_PER_METER);
            }
            return PtY;
        }

        ///// <summary>
        ///// Adds a grid of LargeScale
        ///// </summary>
        ///// <param name="LargeScale"></param>
        //public void CreateDefaultRoutes(float LargeScale)
        //{
        //    Debug("START CreateDefaultRoutes StepSize={0}", LargeScale);
        //    float MX = 255f - LargeScale - StepSize;
        //    float MY = 255f - LargeScale - StepSize;
        //    float Weight = 0.05f;
        //    for (float x = StepSize; x < MX; x += LargeScale)
        //    {
        //        for (float y = StepSize; y < MY; y += LargeScale)
        //        {
        //            /*

        //             Draws two-way Routes StepSize meters appart

        //                     * - *  
        //                     |
        //                     * 
                     
        //            */
        //            SimWaypoint C, S, E;
        //            C = InternNode(x, y);
        //            S = InternNode(x, y + LargeScale);
        //            E = InternNode(x + LargeScale, y);
        //            AddInitalArcs(C, S, Weight); //dirrection  |
        //            AddInitalArcs(C, E, Weight); //dirrection  -
        //        }
        //    }
        //    Debug("END CreateDefaultRoutes StepSize={0}", LargeScale);
        //}

        public void CreateDefaultRoutes()
        {
            Debug("START CreateDefaultRoutes StepSize={0}", LargeScale);
            float MX = 255f - LargeScale - StepSize;
            float MY = 255f - LargeScale - StepSize;
            float Weight = 0.05f;
            for (float x = 0; x < MX; x += LargeScale)
            {
                for (float y = 0; y < MY; y += LargeScale)
                {
                    /*

                     Draws two-way Routes StepSize meters appart

                             * - *  
                             |
                             * 
                     
                    */
                    SimWaypoint C, S, E;
                    C = CreateFirstNode(x, y);
                    S = CreateFirstNode(x, y + LargeScale);
                    E = CreateFirstNode(x + LargeScale, y);
                    AddInitalArcsNoCheck(C, S, Weight); //dirrection  |
                    AddInitalArcsNoCheck(C, E, Weight); //dirrection  -
                }
            }
            Debug("END CreateDefaultRoutes StepSize={0}", LargeScale);
        }

        public SimWaypoint CreateFirstNode(float x, float y)
        {
            x = RangeCheck(x);
            y = RangeCheck(y);
            int ix = INT(x);
            int iy = INT(y);
            SimWaypoint wp = saved[ix, iy];
            if (wp == null)
            {
                wp = SimWaypoint.Create(x, y, SimZLevel);
                //lock (SimWaypoints)
                SimWaypoints.Add(wp);
                saved[ix, iy] = wp;
            }
            return wp;
        }


        ///// <summary>
        /////  This Fills a LargeScale area with StepSize
        ///// </summary>
        ///// <param name="xx"></param>
        ///// <param name="yy"></param>
        ///// <returns></returns>
        //public SimWaypoint InternNodeFill(float xx, float yy)
        //{
        //    if (StepSize >= LargeScale) return CreateFirstNode(xx, yy);
        //    float MX = xx + LargeScale;
        //    float MY = yy + LargeScale;
        //    float Weight = 1f;
        //    for (float x = xx; x < MX; x += StepSize)
        //    {
        //        for (float y = yy; y < MY; y += StepSize)
        //        {
        //            SimWaypoint C = InternNode(x, y);
        //            SimWaypoint S = InternNode(x, y + StepSize);
        //            SimWaypoint E = InternNode(x + StepSize, y);
        //            /*

        //             Draws two-way Routes StepSize meters appart

        //                     * - *  
        //                     |
        //                     * 
                      
                     
        //            */
        //            AddInitalArcs(C, S, Weight); //dirrection  |
        //            AddInitalArcs(C, E, Weight); //dirrection  -
        //        }
        //    }
        //    return InternNode(xx, yy);
        //}

        ///// <summary>
        /////  Create 4 StepSizes around the Node
        ///// </summary>
        ///// <param name="x"></param>
        ///// <param name="y"></param>
        ///// <returns></returns>
        //public SimWaypoint InternNode(float x, float y)
        //{
        //    SimWaypoint C = CreateFirstNode(x, y);
        //    if (StepSize >= LargeScale) return C;
        //    float Weight = 1f;
        //    if (C.ArcCount() < 2)
        //    {
        //        SimWaypoint S = CreateFirstNode(x, y + StepSize);
        //        SimWaypoint E = CreateFirstNode(x + StepSize, y);
        //        SimWaypoint N = CreateFirstNode(x, y - StepSize);
        //        SimWaypoint W = CreateFirstNode(x - StepSize, y);
        //        //SimWaypoint sw11 = InternNode(x + StepSize, y + StepSize);
        //        /*

        //         Draws two-way Routes StepSize meters appart

        //                 * 
        //                 | 
        //             * - * - *  
        //                 |    
        //                 *   
        //        */
        //        AddInitalArcs(C, N, Weight); //dirrection  |
        //        AddInitalArcs(C, W, Weight); //dirrection  -
        //        AddInitalArcs(C, S, Weight); //dirrection  |
        //        AddInitalArcs(C, E, Weight); //dirrection  -
        //    }
        //    return C;
        //}

        private int INT(double x)
        {
            if (x == 0.0) return 0;
            double i = Math.Round(x * POINTS_PER_METER, 1);
            int ii = (int)i;
            if ((double)i != ii)
            {
                throw new ArgumentException("INT " + i + "!="+ii);
            }
            return ii;
        }

        private void AddInitalArcsNoCheck(SimWaypoint s, SimWaypoint e, float Weight)
        {
            if (s == e) return;
            SimRoute se, es;
            {
                se = new SimRoute(s, e);
                se.Weight = Weight;
                SimRoutes.Add(se);
            }
            {
                es = new SimRoute(e, s);
                SimRoutes.Add(es);
                es.Weight = Weight;
            }
            {
                es._Reverse = se;
                se._Reverse = es;
            }
        }

        internal void AddInitalArcs(SimWaypoint s, SimWaypoint e, float Weight)
        {
            if (s == e) return;
            SimRoute se=null,es=null;
            if (!s.GoesTo(e))
            {
                se = new SimRoute(s, e);
                se.Weight = Weight;
                SimRoutes.Add(se);
            }
            if (!e.GoesTo(s))
            {
                es  = new SimRoute(e, s);
                SimRoutes.Add(es);
                es.Weight = Weight;
            }
            if (es != null && se != null)
            {
                es._Reverse = se;
                se._Reverse = es;
            }
        }


        public SimRoute InternArc(SimWaypoint s, SimWaypoint e, float Weight)
        {
            if (s == e) throw new ArgumentException("s and e the same!" + s);
            SimRoute NewArc = FindArc(s, e);
            if (NewArc == null)
            {
                NewArc = new SimRoute(s, e);
                AddArc(NewArc);

            }
            NewArc.ReWeight(Weight);
            return NewArc;
        }

        public SimRoute Intern2Arc(SimWaypoint s, SimWaypoint e, float Weight)
        {
            InternArc(e, s, Weight);
            Debug("Intern2Arc: " + s + " <-> " + e);
            return InternArc(s, e, Weight);
        }

        private SimRoute FindArc(SimWaypoint s, SimWaypoint e)
        {
            lock (SimRoutes) for (int i = SimRoutes.Count; i != 0; )
                {
                    SimRoute sr = SimRoutes[--i];
                    if (sr.IsSame(s, e))
                    {
                        return sr;
                    }

                }
            return null;
        }

        public SimRoute[] GetRoute(SimWaypoint StartNode, SimWaypoint EndNode, out bool IsFake)
        {
            SimMovement AS = new SimMovement(this);
            AS.Initialize(StartNode, EndNode);
            while (AS.NextStep()) { }
            if (AS.PathFound)
            {
                // Full Path
                IsFake = false;
                return AS.PathByArcs;
            }
            // Partial Path
            IsFake = true;

            //int Nb = AS._LeafToGoBackUp.NbArcsVisited;
            //SimRoute[] Path = new SimRoute[Nb];
            //Track Cur = _LeafToGoBackUp;
            //for (int i = Nb - 1; i >= 0; i--, Cur = Cur.Queue)
            //    Path[i] = Cur.Queue.EndNode.ArcGoingTo(Cur.EndNode);
            //return Path;

            //AS.Open.Length, AS.Closed.Length, AS.StepCounter
            SimRoute[] PathByArcs = AS.PathByArcs;
            if (PathByArcs == null || PathByArcs.Length == 0)
            {
                return FakeRoute(StartNode, EndNode);
            }
            List<SimRoute> list = new List<SimRoute>();
            list.AddRange(PathByArcs);
            SimRoute LastArc = PathByArcs[PathByArcs.Length - 1];
            list.AddRange(FakeRoute(LastArc.EndNode, EndNode));
            return list.ToArray();
        }

        private SimRoute[] FakeRoute(SimWaypoint StartNode, SimWaypoint EndNode)
        {
            SimRoute[] route = new SimRoute[1];//
            SimRoute NewArc = Intern2Arc(StartNode, EndNode, 1.2f);
            //NewArc.Passable = true;
            route[0] = NewArc;
            return route;
        }

        /// <summary>
        /// Gets the List interface of the nodes in the graph.
        /// </summary>
        public IList<SimWaypoint> Nodes { get { return SimWaypoints; } }

        /// <summary>
        /// Gets the List interface of the arcs in the graph.
        /// </summary>
        public IList<SimRoute> Arcs { get { return SimRoutes; } }

        /// <summary>
        /// Empties the graph.
        /// </summary>
        public void Clear()
        {
            SimWaypoints.Clear();
            SimRoutes.Clear();
        }

        /// <summary>
        /// Directly Adds a node to the graph.
        /// </summary>
        /// <param name="NewNode">The node to add.</param>
        /// <returns>'true' if it has actually been added / 'false' if the node is null or if it is already in the graph.</returns>
        public bool AddNode(SimWaypoint NewNode)
        {
            if (NewNode == null || SimWaypoints.Contains(NewNode)) return false;
            lock (SimWaypoints) SimWaypoints.Add(NewNode);
            return true;
        }

        /// <summary>
        /// Creates a node, adds to the graph and returns its reference.
        /// </summary>
        /// <param name="x">X coordinate.</param>
        /// <param name="y">Y coordinate.</param>
        /// <param name="z">Z coordinate.</param>
        /// <returns>The reference of the new node / null if the node is already in the graph.</returns>
        public SimWaypoint AddNode(float x, float y, float z)
        {
            SimWaypoint NewNode = SimWaypoint.Create(x, y, z);
            return AddNode(NewNode) ? NewNode : null;
        }

        /// <summary>
        /// Directly Adds an arc to the graph.
        /// </summary>
        /// <exception cref="ArgumentException">Cannot add an arc if one of its extremity nodes does not belong to the graph.</exception>
        /// <param name="NewArc">The arc to add.</param>
        /// <returns>'true' if it has actually been added / 'false' if the arc is null or if it is already in the graph.</returns>
        public bool AddArc(SimRoute NewArc)
        {
            if (NewArc == null || SimRoutes.Contains(NewArc)) return false;
            if (!SimWaypoints.Contains(NewArc.StartNode) || !SimWaypoints.Contains(NewArc.EndNode))
                throw new ArgumentException("Cannot add an arc if one of its extremity nodes does not belong to the graph.");
            lock (SimRoutes) SimRoutes.Add(NewArc);
            return true;
        }

        /// <summary>
        /// Creates an arc between two nodes that are already registered in the graph, adds it to the graph and returns its reference.
        /// </summary>
        /// <exception cref="ArgumentException">Cannot add an arc if one of its extremity nodes does not belong to the graph.</exception>
        /// <param name="StartNode">Start node for the arc.</param>
        /// <param name="EndNode">End node for the arc.</param>
        /// <param name="Weight">Weight for the arc.</param>
        /// <returns>The reference of the new arc / null if the arc is already in the graph.</returns>
        public SimRoute AddArc(SimWaypoint StartNode, SimWaypoint EndNode, float Weight)
        {
            SimRoute NewArc = FindArc(StartNode, EndNode);
            if (NewArc == null)
            {
                NewArc = new SimRoute(StartNode, EndNode);
                NewArc.Weight = Weight;
                return AddArc(NewArc) ? NewArc : null;
            }
            else
            {
                NewArc.Weight = Weight;
                return AddArc(NewArc) ? NewArc : null;
            }
            ///SimRoute NewArc = new SimRoute(StartNode, EndNode);
        }

        /// <summary>
        /// Adds the two opposite arcs between both specified nodes to the graph.
        /// </summary>
        /// <exception cref="ArgumentException">Cannot add an arc if one of its extremity nodes does not belong to the graph.</exception>
        /// <param name="Node1"></param>
        /// <param name="Node2"></param>
        /// <param name="Weight"></param>
        public void Add2Arcs(SimWaypoint Node1, SimWaypoint Node2, float Weight)
        {
            AddArc(Node1, Node2, Weight);
            AddArc(Node2, Node1, Weight);
        }

        /// <summary>
        /// Removes a node from the graph as well as the linked arcs.
        /// </summary>
        /// <param name="NodeToRemove">The node to remove.</param>
        /// <returns>'true' if succeeded / 'false' otherwise.</returns>
        public bool RemoveNode(SimWaypoint NodeToRemove)
        {
            if (NodeToRemove == null) return false;
            try
            {
                foreach (SimRoute A in NodeToRemove.IncomingArcs)
                {
                    lock (A.StartNode.OutgoingArcs) A.StartNode.OutgoingArcs.Remove(A);
                    lock (SimRoutes) SimRoutes.Remove(A);
                }
                foreach (SimRoute A in NodeToRemove.OutgoingArcs)
                {
                    lock (A.EndNode.IncomingArcs) A.EndNode.IncomingArcs.Remove(A);
                    lock (SimRoutes) SimRoutes.Remove(A);
                }
                lock (SimWaypoints) SimWaypoints.Remove(NodeToRemove);
            }
            catch { return false; }
            return true;
        }

        /// <summary>
        /// Removes a node from the graph as well as the linked arcs.
        /// </summary>
        /// <param name="ArcToRemove">The arc to remove.</param>
        /// <returns>'true' if succeeded / 'false' otherwise.</returns>
        public bool RemoveArc(SimRoute ArcToRemove)
        {
            if (ArcToRemove == null) return false;
            try
            {
                lock (SimRoutes) SimRoutes.Remove(ArcToRemove);
                lock (ArcToRemove.StartNode.OutgoingArcs) ArcToRemove.StartNode.OutgoingArcs.Remove(ArcToRemove);
                lock (ArcToRemove.StartNode.IncomingArcs) ArcToRemove.EndNode.IncomingArcs.Remove(ArcToRemove);
            }
            catch { return false; }
            return true;
        }

        /// <summary>
        /// Determines the bounding box of the entire graph.
        /// </summary>
        /// <exception cref="InvalidOperationException">Impossible to determine the bounding box for this graph.</exception>
        /// <param name="MinPoint">The point of minimal coordinates for the box.</param>
        /// <param name="MaxPoint">The point of maximal coordinates for the box.</param>
        public void BoundingBox(out float[] MinPoint, out float[] MaxPoint)
        {
            try
            {
                SimWaypoint.BoundingBox(Nodes, out MinPoint, out MaxPoint);
            }
            catch (ArgumentException e)
            { throw new InvalidOperationException("Impossible to determine the bounding box for this graph.\n", e); }
        }

        /// <summary>
        /// This function will find the closest node from a geographical position in space.
        /// </summary>
        /// <param name="PtX">X coordinate of the point from which you want the closest node.</param>
        /// <param name="PtY">Y coordinate of the point from which you want the closest node.</param>
        /// <param name="PtZ">Z coordinate of the point from which you want the closest node.</param>
        /// <param name="Distance">The distance to the closest node.</param>
        /// <param name="IgnorePassableProperty">if 'false', then nodes whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The closest node that has been found.</returns>
        public SimWaypoint ClosestNode(float PtX, float PtY, float PtZ, out float Distance, bool IgnorePassableProperty)
        {

            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            PtZ = SimZLevel;

            SimWaypoint NodeMin = null;
            float DistanceMin = -1;
            Vector3 P = new Vector3(PtX, PtY, PtZ);

            lock (SimWaypoints) foreach (SimWaypoint N in SimWaypoints)
                {
                    if (IgnorePassableProperty && N.Passable == false) continue;
                    float DistanceTemp = N.Distance(P);
                    if (DistanceMin == -1 || DistanceMin > DistanceTemp)
                    {
                        DistanceMin = DistanceTemp;
                        NodeMin = N;
                    }
                }
            Distance = DistanceMin;
            return NodeMin;
        }

        /// <summary>
        /// This function will find the nodes from a geographical position in space.
        /// </summary>
        /// <param name="P">Waypoint node.</param>
        /// <param name="MinDistance">The min distance to the node.</param>
        /// <param name="MaxDistance">The max distance to the node.</param>
        /// <param name="IgnorePassableProperty">if 'false', then nodes whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The nodes that has been found.</returns>
        public IList<SimWaypoint> ClosestNodes(float PtX, float PtY, float PtZ, float DistanceMin, float DistanceMax, bool IgnorePassableProperty)
        {
            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            PtZ = SimZLevel;
            Vector3 P = new Vector3(PtX, PtY, PtZ);
            List<SimWaypoint> waypoints = new List<SimWaypoint>();
            lock (SimWaypoints) foreach (SimWaypoint N in SimWaypoints)
                {
                    //if (P == N) continue;
                    if (IgnorePassableProperty && N.Passable == false) continue;
                    float Distance = N.Distance(P);
                    if (Distance < DistanceMin || DistanceMax < Distance) continue;
                    waypoints.Add(N);
                }
            return waypoints;
        }

        /// <summary>
        /// This function will find the routes from a geographical position in space.
        /// </summary>
        /// <param name="P">Waypoint route.</param>
        /// <param name="MinDistance">The min distance to the route.</param>
        /// <param name="MaxDistance">The max distance to the route.</param>
        /// <param name="IgnorePassableProperty">if 'false', then routes whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The routes that has been found.</returns>
        public IList<SimRoute> ClosestArcs(float PtX, float PtY, float PtZ, float DistanceMin, float DistanceMax, bool IgnorePassableProperty)
        {
            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            PtZ = SimZLevel;
            Vector3 P = new Vector3(PtX, PtY, PtZ);
            List<SimRoute> waypoints = new List<SimRoute>();
            lock (SimWaypoints) foreach (SimRoute N in SimRoutes)
                {
                    //if (P == N) continue;
                    if (IgnorePassableProperty && N.Passable == false) continue;
                    float Distance = N.Distance(P);
                    if (Distance < DistanceMin || DistanceMax < Distance) continue;
                    waypoints.Add(N);
                }
            return waypoints;
        }


        /// <summary>
        /// This function will find the closest arc from a geographical position in space using projection.
        /// </summary>
        /// <param name="PtX">X coordinate of the point from which you want the closest arc.</param>
        /// <param name="PtY">Y coordinate of the point from which you want the closest arc.</param>
        /// <param name="PtZ">Z coordinate of the point from which you want the closest arc.</param>
        /// <param name="Distance">The distance to the closest arc.</param>
        /// <param name="IgnorePassableProperty">if 'false', then arcs whose property Passable is set to false will not be taken into account.</param>
        /// <returns>The closest arc that has been found.</returns>
        public SimRoute ClosestArc(float PtX, float PtY, float PtZ, out float Distance, bool IgnorePassableProperty)
        {
            PtX = RangeCheck(PtX);
            PtY = RangeCheck(PtY);
            PtZ = SimZLevel;

            SimRoute ArcMin = null;
            float DistanceMin = -1;
            Vector3 P = new Vector3(PtX, PtY, PtZ);
            lock (SimRoutes) foreach (SimRoute A in SimRoutes)
                {
                    if (IgnorePassableProperty && A.Passable == false) continue;
                    float DistanceTemp = A.Distance(P);//s, Projection);
                    if (DistanceMin == -1 || DistanceMin > DistanceTemp)
                    {
                        DistanceMin = DistanceTemp;
                        ArcMin = A;
                    }
                }
            Distance = DistanceMin;
            return ArcMin;
        }

        Dictionary<uint, PrimTracker> TrackedAgents = new Dictionary<uint, PrimTracker>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="agentID"></param>
        /// <param name="point"></param>
        /// <param name="rotation"></param>
        public void Update(uint agentID, Vector3 point, Quaternion rotation)
        {
            // return;
            if (!TrackedAgents.ContainsKey(agentID))
            {
                TrackedAgents[agentID] = new PrimTracker(SimWaypoint.Create(point), rotation, this);
            }
            else
            {
                PrimTracker tracker = TrackedAgents[agentID];
                tracker.Update(point, rotation);
            }
        }

        readonly string RegionFileName;

        //public SimRoute GetBeginToEndMovement(Vector3 begin, Vector3 end, float maxDist)
        //{
        //    SimRoute b = GetBeginMovement(begin, maxDist);
        //    SimRoute e = GetEndMovement(end, maxDist);
        //    if (b == null || e == null) return null;
        //    SimRoute m = new SimRoute(b.EndNode, e.StartNode);
        //    List<SimRoute> list = new List<SimRoute>();
        //    list.Add(b);
        //    list.Add(m);
        //    list.Add(e);
        //    return new SimRouteMovement(list);
        //}

        //public SimRoute GetBeginMovement(Vector3 vect, float maxDist)
        //{
        //    SimRoute bestBegin = null;
        //    float distToBegin = maxDist;
        //    foreach (SimRoute move in SimRoutes)
        //    {
        //        float close = Vector3.Distance(vect, move.StartNode.GetSimPosition());
        //        if (close < distToBegin)
        //        {
        //            bestBegin = move;
        //            distToBegin = close;
        //        }
        //        else
        //        {
        //            if (!move.IsOneDirrection)
        //            {
        //                close = Vector3.Distance(vect, move.EndNode.GetSimPosition());
        //                if (close < distToBegin)
        //                {
        //                    bestBegin = move.Reverse();
        //                    distToBegin = close;
        //                }
        //            }
        //        }
        //    }
        //    return bestBegin;
        //}

        //public SimRoute GetEndMovement(Vector3 vect, float maxDist)
        //{
        //    SimRoute bestEnd = null;
        //    float distToEnd = maxDist;
        //    foreach (SimRoute move in SimRoutes)
        //    {
        //        float close = Vector3.Distance(vect, move.EndNode.GetSimPosition());
        //        if (close < distToEnd)
        //        {
        //            bestEnd = move;
        //            distToEnd = close;
        //        }
        //        else
        //        {
        //            if (!move.IsOneDirrection)
        //            {
        //                close = Vector3.Distance(vect, move.StartNode.GetSimPosition());
        //                if (close < distToEnd)
        //                {
        //                    bestEnd = move.Reverse();
        //                    distToEnd = close;
        //                }
        //            }
        //        }
        //    }
        //    return bestEnd;
        //}

        //public SimPathStore(string regionFileName)
        //{
        //    RegionFileName = regionFileName;
        //    LoadFromFile();
        //}
        /// <summary>
        /// 
        /// </summary>
        void SaveToFile()
        {
            FileInfo save = new FileInfo(RegionFileName);
            if (save.Exists)
            {
                save.Delete();
            }
            SaveFile(RegionFileName);
            //FileStream stream = save.Open(FileMode.OpenOrCreate, FileAccess.Write);
            //StreamWriter sw = new StreamWriter(stream);
            //foreach (SimRoute sm in SimRoutes)
            //{
            //    sw.WriteLine(sm.ToFileString());
            //}
            //sw.Close();
        }
        /// <summary>
        /// 
        /// </summary>
        void LoadFromFile()
        {
            FileInfo read = new FileInfo(RegionFileName);
            if (!read.Exists)
            {
                Logger.Log("Not loading file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Info);
                return;
            }
            //FileStream stream = read.Open(FileMode.Open, FileAccess.Read);
            //TextReader tr = new StreamReader(stream);
            //while (tr.Peek() != -1)
            //{
            //    string s = tr.ReadLine();
            //    LoadFromLine(s);
            //}
            //tr.Close();
            LoadFile(RegionFileName);
        }

        //private void LoadFromLine(string s)
        //{
        //    SimRoute sm = new SimRoute(s);
        //    AddArc(sm);
        //}


        //internal static void EnsureKnown(SimWaypoint wp)
        //{
        //    if (Instance.SimWaypoints.Contains(wp)) return;
        //    Instance.SimWaypointsAdd(wp);
        //}
        /// <summary>
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        internal bool SaveFile(string filename)
        {
            FileStream StreamWrite = (new FileInfo(filename)).OpenWrite();
            if (StreamWrite != null)
            {
                BinaryFormatter BinaryWrite = new BinaryFormatter();
                BinaryWrite.Serialize(StreamWrite, this);
                StreamWrite.Close();
                Logger.Log("Success saving file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Info);
                return true;
            }
            Logger.Log("Error saving file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Error);
            return false;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="pathName"></param>
        /// <returns></returns>
        internal bool LoadFile(string pathName)
        {
            Stream StreamRead = (new FileInfo(pathName)).OpenRead();
            if (StreamRead != null)
            {
                BinaryFormatter BinaryRead = new BinaryFormatter();
                SimPathStore G = (SimPathStore)BinaryRead.Deserialize(StreamRead);
                StreamRead.Close();
                Clear();
                //   RegionFileName = pathName;
                SimRoutes = G.SimRoutes;
                SimWaypoints = G.SimWaypoints;
                Logger.Log("Loaded file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Info);
                return true;
            }
            Logger.Log("Error loading file " + RegionFileName, OpenMetaverse.Helpers.LogLevel.Error);
            return false;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="v3"></param>
        /// <returns></returns>
        public SimWaypoint CreateClosestWaypoint(Vector3 v3)
        {
            float Dist;
            SimWaypoint Closest = ClosestNode(v3.X, v3.Y, SimZLevel, out Dist, false);
            SimWaypoint V3Waypoint;
            if (Dist < StepSize)
            {
                V3Waypoint = Closest;
            }
            else
            {
                V3Waypoint = CreateFirstNode(v3.X, v3.Y);
            }
            if (Closest != V3Waypoint)
            {
                IList<SimWaypoint> more = ClosestNodes(v3.X, v3.Y, SimZLevel, StepSize, LargeScale + StepSize, false);
                foreach (SimWaypoint P in more)
                {
                    if (P != V3Waypoint)
                        Intern2Arc(P, V3Waypoint, 1f);
                }
                Intern2Arc(Closest, V3Waypoint, 1f);
            }
             //V3Waypoint.EnsureAtLeastOnePath()
            return V3Waypoint;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="v3"></param>
        /// <param name="radius"></param>
        /// <param name="numPoints"></param>
        /// <param name="Weight"></param>
        /// <returns></returns>
        public SimWaypoint CreateClosestWaypointBox(Vector3 v3, float radius, int numPoints, float Weight)
        {
            SimWaypoint node = SimWaypoint.Create(v3);
            double radiansStep = Math.PI * 2 / numPoints;
            SimWaypoint Last = node;
            Dictionary<SimWaypoint, List<SimWaypoint>> newWaypoints = new Dictionary<SimWaypoint, List<SimWaypoint>>();
            for (int Step = 0; Step < numPoints; Step++)
            {
                double ThisAngle = Step * radiansStep;
                Vector3 vectNew = (new Vector3((float)Math.Cos(ThisAngle), (float)Math.Sin(ThisAngle), 0) * radius) + v3;
                SimWaypoint nodeNew = SimWaypoint.Create(v3);
                List<SimWaypoint> closeNodes = new List<SimWaypoint>();
                newWaypoints[nodeNew] = closeNodes;
                float Dist;
                closeNodes.Add(node);
                closeNodes.Add(Last);
                closeNodes.Add(ClosestNode(vectNew.X, vectNew.Y, vectNew.Z, out Dist, false));
                Last = nodeNew;

            }
            foreach (SimWaypoint P in newWaypoints.Keys)
            {
                AddNode(P);
                foreach (SimWaypoint V in newWaypoints[P])
                {
                    if (V == null) continue;
                    AddNode(V);
                    if (P != V) Intern2Arc(P, V, Weight);
                }
            }
            return node;
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
                SetPointBlocked((float)x, (float)y);
            });
            Debug("END Edge detection");
            new DisplayImage("Clone", e).Activate();
        }


        internal void UpdateFromObject(SimObject obj)
        {
            //int SimZLevelWalk = SimZLevel + 1;
            if (!obj.CanGetSimPosition()) return;
            if (obj.IsPassable) return;
            ICollection<Vector3> points = obj.GetOccupied((float)SimZLevel, (float)SimZLevel + 1f);
            if (obj.IsPassable)
            {
                foreach (Vector3 point in points)
                {
                    {
                        //    SetPointPassable(point.X, point.Y);
                    }
                }
            }
            else
            {
                foreach (Vector3 point in points)
                {
                    {
                        SetPointBlocked(point.X, point.Y);
                    }
                }
            }
        }

        public void SetPointPassable(float x, float y)
        {
            float Dist;
            SimWaypoint waypoint = ClosestNode(x, y, SimZLevel, out Dist, false);
            if (Dist < 1f)
            {
                waypoint.EnsureAtLeastOnePath();

            }
        }

        public void SetPointBlocked(float x, float y)
        {
            //float Dist;
            //SimRoute route = ClosestArc((double)x, (double)y, SimZLevel, out Dist, false);
            //if (route.Passable) //no need to search
            //    if (route.OnRoute(new Vector2((double)x, (double)y)))
            //    {
            //        Debug("Edge at " + route + " for " + x + " " + y);
            //        route.Passable = false;
            //        route.Reverse().Passable = false;
            //        route.ReWeight(3f);
            //    }

            Vector3 P = new Vector3(x, y, SimZLevel);
            lock (SimRoutes) foreach (SimRoute A in SimRoutes)
                {
                    if (A.Passable)
                        if (A.OnRoute(P))
                        {
                            SimRoute R = A.Reverse();
                            A.Passable = false;
                            Debug("SetPointBlocked: {0}  the {1}",P, R);
                            R.Passable = false;
                        }
                }
        }


        //                                    NewBitmap.SetPixel(x, y, EdgeColor);
        public delegate void EdgeAction(int x, int y);

        public static Bitmap EdgeDetection(Bitmap Image, double Threshold, EdgeAction EdgeColor)
        {
            System.Drawing.Bitmap TempBitmap = Image;
            System.Drawing.Bitmap NewBitmap = new System.Drawing.Bitmap(TempBitmap.Width, TempBitmap.Height);
            System.Drawing.Graphics NewGraphics = System.Drawing.Graphics.FromImage(NewBitmap);
            NewGraphics.DrawImage(TempBitmap, new System.Drawing.Rectangle(0, 0, TempBitmap.Width, TempBitmap.Height), new System.Drawing.Rectangle(0, 0, TempBitmap.Width, TempBitmap.Height), System.Drawing.GraphicsUnit.Pixel);
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
            Console.WriteLine("[SimPathStore] "+format, arg);
        }
    }

    public class PrimTracker
    {
        protected float MovedAllot = SimPathStore.StepSize;
        Vector3 WayPoint;
        Quaternion Orientation;
        SimPathStore Store;
        public PrimTracker(SimPosition firstP, Quaternion firtsR, SimPathStore store)
        {
            WayPoint = firstP.GetSimPosition();
            Store = store;
            Orientation = firtsR;
        }

        public void Update(Vector3 point, Quaternion rotation)
        {
            float dist = Vector3.Distance(WayPoint, point);
            if (dist > MovedAllot)
            {
                MakeMovement(point);
            }
            else
                if (RotationDiffernt(rotation, Orientation))
                {
                    MakeMovement(point);
                    Orientation = rotation;
                }
        }


        public void MakeMovement(Vector3 point)
        {
            if (Vector3.Distance(WayPoint, point) > MovedAllot / 3)
            {
                Console.WriteLine("WAYPOINT " + WayPoint + " -> " + point);
                SimWaypoint tieIn1 = Store.CreateClosestWaypoint(point);
                SimWaypoint tieIn2 = Store.CreateClosestWaypoint(WayPoint);
                if (tieIn1 == tieIn2) return;
                Store.Intern2Arc(tieIn1, tieIn2, 0.01f); //Cheap
                WayPoint = point;
            }
        }

        static bool RotationDiffernt(Quaternion rotation, Quaternion Orientation)
        {
            Quaternion diff = rotation - Orientation;
            return (diff.Length() > 0.2);
        }
    }
}
