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
using System.Threading;
using System.Windows.Forms;

namespace PathSystem3D.Navigation
{

    /// <summary>
    /// Class to search the best path between two nodes on a graph.
    /// </summary>
    public class SimMovement
    {
        /// <summary>
        /// A heuristic is a function that associates a value with a node to gauge it considering the node to reach.
        /// </summary>
        public delegate double Heuristic(SimWaypoint NodeToEvaluate, SimWaypoint TargetNode);

        SimGlobalRoutes _Graph;
        SortableList _Open, _Closed;
        Track _LeafToGoBackUp;
        int _NbIterations = -1;

        SortableList.Equality SameNodesReached = new SortableList.Equality(Track.SameEndNode);

        /// <summary>
        /// Heuristic based on the euclidian distance : Sqrt(Dx²+Dy²+Dz²)
        /// </summary>
        public static Heuristic EuclidianHeuristic
        { get { return new Heuristic(SimWaypointImpl.EuclidianDistance); } }

        /// <summary>
        /// Heuristic based on the maximum distance : Max(|Dx|, |Dy|, |Dz|)
        /// </summary>
        public static Heuristic MaxAlongAxisHeuristic
        { get { return new Heuristic(SimWaypointImpl.MaxDistanceAlongAxis); } }

        /// <summary>
        /// Heuristic based on the manhattan distance : |Dx|+|Dy|+|Dz|
        /// </summary>
        public static Heuristic ManhattanHeuristic
        { get { return new Heuristic(SimWaypointImpl.ManhattanDistance); } }

        /// <summary>
        /// Gets/Sets the heuristic that AStar will use.
        /// It must be homogeneous to arc's cost.
        /// </summary>
        public Heuristic ChoosenHeuristic
        {
            get { return Track.ChoosenHeuristic; }
            set { Track.ChoosenHeuristic = value; }
        }

        /// <summary>
        /// This value must belong to [0; 1] and it determines the influence of the heuristic on the algorithm.
        /// If this influence value is set to 0, then the search will behave in accordance with the Dijkstra algorithm.
        /// If this value is set to 1, then the cost to come to the current node will not be used whereas only the heuristic will be taken into account.
        /// </summary>
        /// <exception cref="ArgumentException">Value must belong to [0;1].</exception>
        public double DijkstraHeuristicBalance
        {
            get { return Track.DijkstraHeuristicBalance; }
            set
            {
                if (value < 0 || value > 1) throw new ArgumentException("DijkstraHeuristicBalance value must belong to [0;1].");
                Track.DijkstraHeuristicBalance = value;
            }
        }

        /// <summary>
        /// AStar Constructor.
        /// </summary>
        /// <param name="G">The graph on which AStar will perform the search.</param>
        public SimMovement(SimGlobalRoutes G)
        {
            _Graph = G;
            _Open = new SortableList();
            _Closed = new SortableList();
            ChoosenHeuristic = EuclidianHeuristic;
            DijkstraHeuristicBalance = 0.5f;
        }

        /// <summary>
        /// Searches for the best path to reach the specified EndNode from the specified StartNode.
        /// </summary>
        /// <exception cref="ArgumentNullException">StartNode and EndNode cannot be null.</exception>
        /// <param name="StartNode">The node from which the path must start.</param>
        /// <param name="EndNode">The node to which the path must end.</param>
        /// <returns>'true' if succeeded / 'false' if failed.</returns>
        public bool SearchPath(SimWaypoint StartNode, SimWaypoint EndNode)
        {
            lock (_Graph)
            {
                Initialize(StartNode, EndNode);
                while (NextStep()) { }
                return PathFound;
            }
        }

        /// <summary>
        /// Use for debug in 'step by step' mode only.
        /// Returns all the tracks found in the 'Open' list of the algorithm at a given time.
        /// A track is a list of the nodes visited to come to the current node.
        /// </summary>
        public SimWaypoint[][] Open
        {
            get
            {
                SimWaypoint[][] NodesList = new SimWaypoint[_Open.Count][];
                for (int i = 0; i < _Open.Count; i++) NodesList[i] = GoBackUpNodes((Track)_Open[i]);
                return NodesList;
            }
        }

        /// <summary>
        /// Use for debug in a 'step by step' mode only.
        /// Returns all the tracks found in the 'Closed' list of the algorithm at a given time.
        /// A track is a list of the nodes visited to come to the current node.
        /// </summary>
        public SimWaypoint[][] Closed
        {
            get
            {
                SimWaypoint[][] NodesList = new SimWaypoint[_Closed.Count][];
                for (int i = 0; i < _Closed.Count; i++) NodesList[i] = GoBackUpNodes((Track)_Closed[i]);
                return NodesList;
            }
        }

        /// <summary>
        /// Use for a 'step by step' search only. This method is alternate to SearchPath.
        /// Initializes AStar before performing search steps manually with NextStep.
        /// </summary>
        /// <exception cref="ArgumentNullException">StartNode and EndNode cannot be null.</exception>
        /// <param name="StartNode">The node from which the path must start.</param>
        /// <param name="EndNode">The node to which the path must end.</param>
        public void Initialize(SimWaypoint StartNode, SimWaypoint EndNode)
        {
            if (StartNode == null || EndNode == null) throw new ArgumentNullException();
            _Closed.Clear();
            _Open.Clear();
            Track.Target = EndNode;
            _Open.Add(new Track(StartNode));
            _NbIterations = 0;
            _LeafToGoBackUp = null;
        }

        /// <summary>
        /// Use for a 'step by step' search only. This method is alternate to SearchPath.
        /// The algorithm must have been initialize before.
        /// </summary>
        /// <exception cref="InvalidOperationException">You must initialize AStar before using NextStep().</exception>
        /// <returns>'true' unless the search ended.</returns>
        public bool NextStep()
        {
            if (!Initialized) throw new InvalidOperationException("You must initialize AStar before launching the algorithm.");
            if (_Open.Count == 0) return false;
            _NbIterations++;

            int IndexMin = _Open.IndexOfMin();
            Track BestTrack = (Track)_Open[IndexMin];
            _Open.RemoveAt(IndexMin);
            if (BestTrack.Succeed)
            {
                _LeafToGoBackUp = BestTrack;
                _Open.Clear();
            }
            else
            {
                Propagate(BestTrack);
                _Closed.Add(BestTrack);
            }
            return _Open.Count > 0;
        }

        private void Propagate(Track TrackToPropagate)
        {
            foreach (SimRoute A in TrackToPropagate.EndNode.OutgoingArcs)
            {
                if (A.Passable && A.EndNode.IsPassable)
                {
                    Track Successor = new Track(TrackToPropagate, A);
                    int PosNF = _Closed.IndexOf(Successor, SameNodesReached);
                    int PosNO = _Open.IndexOf(Successor, SameNodesReached);
                    if (PosNF > 0 && Successor.Cost >= ((Track)_Closed[PosNF]).Cost) continue;
                    if (PosNO > 0 && Successor.Cost >= ((Track)_Open[PosNO]).Cost) continue;
                    if (PosNF > 0) _Closed.RemoveAt(PosNF);
                    if (PosNO > 0) _Open.RemoveAt(PosNO);
                    _Open.Add(Successor);
                }
            }
        }

        /// <summary>
        /// To know if the search has been initialized.
        /// </summary>
        public bool Initialized { get { return _NbIterations >= 0; } }

        /// <summary>
        /// To know if the search has been started.
        /// </summary>
        public bool SearchStarted { get { return _NbIterations > 0; } }

        /// <summary>
        /// To know if the search has ended.
        /// </summary>
        public bool SearchEnded { get { return SearchStarted && _Open.Count == 0; } }

        /// <summary>
        /// To know if a path has been found.
        /// </summary>
        public bool PathFound { get { return _LeafToGoBackUp != null; } }

        /// <summary>
        /// Use for a 'step by step' search only.
        /// Gets the number of the current step.
        /// -1 if the search has not been initialized.
        /// 0 if it has not been started.
        /// </summary>
        public int StepCounter { get { return _NbIterations; } }

        private void CheckSearchHasEnded()
        {
            if (!SearchEnded) throw new InvalidOperationException("You cannot get a result unless the search has ended.");
        }

        /// <summary>
        /// Returns information on the result.
        /// </summary>
        /// <param name="NbArcsOfPath">The number of arcs in the result path / -1 if no result.</param>
        /// <param name="CostOfPath">The cost of the result path / -1 if no result.</param>
        /// <returns>'true' if the search succeeded / 'false' if it failed.</returns>
        public bool ResultInformation(out int NbArcsOfPath, out double CostOfPath)
        {
            CheckSearchHasEnded();
            if (!PathFound)
            {
                NbArcsOfPath = -1;
                CostOfPath = -1;
                return false;
            }
            else
            {
                NbArcsOfPath = _LeafToGoBackUp.NbArcsVisited;
                CostOfPath = _LeafToGoBackUp.Cost;
                return true;
            }
        }

        /// <summary>
        /// Gets the array of nodes representing the found path.
        /// </summary>
        /// <exception cref="InvalidOperationException">You cannot get a result unless the search has ended.</exception>
        public SimWaypoint[] PathByNodes
        {
            get
            {
                CheckSearchHasEnded();
                if (!PathFound) return null;
                return GoBackUpNodes(_LeafToGoBackUp);
            }
        }

        private SimWaypoint[] GoBackUpNodes(Track T)
        {
            int Nb = T.NbArcsVisited;
            SimWaypoint[] Path = new SimWaypoint[Nb + 1];
            for (int i = Nb; i >= 0; i--, T = T.Queue)
                Path[i] = T.EndNode;
            return Path;
        }

        /// <summary>
        /// Gets the array of arcs representing the found path.
        /// </summary>
        /// <exception cref="InvalidOperationException">You cannot get a result unless the search has ended.</exception>
        public SimRoute[] PathByArcs
        {
            get
            {
                CheckSearchHasEnded();
                if (!PathFound) return null;
                int Nb = _LeafToGoBackUp.NbArcsVisited;
                SimRoute[] Path = new SimRoute[Nb];
                Track Cur = _LeafToGoBackUp;
                for (int i = Nb - 1; i >= 0; i--, Cur = Cur.Queue)
                    Path[i] = Cur.Queue.EndNode.ArcGoingTo(Cur.EndNode);
                return Path;
            }
        }

        /// <summary>
        /// Gets the array of points representing the found path.
        /// </summary>
        /// <exception cref="InvalidOperationException">You cannot get a result unless the search has ended.</exception>
        public Vector3d[] PathByCoordinates
        {
            get
            {
                CheckSearchHasEnded();
                if (!PathFound) return null;
                int Nb = _LeafToGoBackUp.NbArcsVisited;
                Vector3d[] Path = new Vector3d[Nb + 1];
                Track Cur = _LeafToGoBackUp;
                for (int i = Nb; i >= 0; i--, Cur = Cur.Queue)
                    Path[i] = Cur.EndNode.Position;
                return Path;
            }
        }
    }

    /// <summary>
    /// A track is a succession of nodes which have been visited.
    /// Thus when it leads to the target node, it is easy to return the result path.
    /// These objects are contained in Open and Closed lists.
    /// </summary>
    internal class Track : IComparable
    {
        private static SimWaypoint _Target = null;
        private static double _Coeff = 0.5f;
        private static PathSystem3D.Navigation.SimMovement.Heuristic _ChoosenHeuristic = SimMovement.EuclidianHeuristic;

        public static SimWaypoint Target { set { _Target = value; } get { return _Target; } }

        public SimWaypoint EndNode;
        public Track Queue;

        public static double DijkstraHeuristicBalance
        {
            get { return _Coeff; }
            set
            {
                if (value < 0 || value > 1) throw new ArgumentException(
  @"The coefficient which balances the respective influences of Dijkstra and the Heuristic must belong to [0; 1].
-> 0 will minimize the number of nodes explored but will not take the real cost into account.
-> 0.5 will minimize the cost without developing more nodes than necessary.
-> 1 will only consider the real cost without estimating the remaining cost.");
                _Coeff = value;
            }
        }

        public static PathSystem3D.Navigation.SimMovement.Heuristic ChoosenHeuristic
        {
            set { _ChoosenHeuristic = value; }
            get { return _ChoosenHeuristic; }
        }

        private int _NbArcsVisited;
        public int NbArcsVisited { get { return _NbArcsVisited; } }

        private double _Cost;
        public double Cost { get { return _Cost; } }

        virtual public double Evaluation
        {
            get
            {
                return _Coeff * _Cost + (1 - _Coeff) * _ChoosenHeuristic(EndNode, _Target);
            }
        }

        public bool Succeed { get { return EndNode == _Target; } }

        public Track(SimWaypoint GraphNode)
        {
            if (_Target == null) throw new InvalidOperationException("You must specify a target Node for the Track class.");
            _Cost = 0;
            _NbArcsVisited = 0;
            Queue = null;
            EndNode = GraphNode;
        }

        public Track(Track PreviousTrack, SimRoute Transition)
        {
            if (_Target == null) throw new InvalidOperationException("You must specify a target Node for the Track class.");
            Queue = PreviousTrack;
            _Cost = Queue.Cost + Transition.Cost;
            _NbArcsVisited = Queue._NbArcsVisited + 1;
            EndNode = Transition.EndNode;
        }

        public int CompareTo(object Objet)
        {
            Track OtherTrack = (Track)Objet;
            return Evaluation.CompareTo(OtherTrack.Evaluation);
        }

        public static bool SameEndNode(object O1, object O2)
        {
            Track P1 = O1 as Track;
            Track P2 = O2 as Track;
            if (P1 == null || P2 == null) throw new ArgumentException("Objects must be of 'Track' type.");
            return P1.EndNode == P2.EndNode;
        }
    }
}
