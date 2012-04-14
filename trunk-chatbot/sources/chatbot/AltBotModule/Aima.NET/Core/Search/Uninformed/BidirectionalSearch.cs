using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Uninformed
{
    using System.Collections.ObjectModel;
    using System.Diagnostics;

    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 90.
    /// Bidirectional search.
    /// 
    /// Note: Based on the description of this algorithm i.e. 'Bidirectional search
    /// is implemented by replacing the goal test with a check to see whether the frontiers
    /// of the two searches intersect;', it is possible for the searches to pass each other's frontiers by,
    /// in particular if the problem is not fully reversible (i.e. unidirectional links on a graph), and
    /// could instead intersect at the explored Set.
    /// </summary>
    public class BidirectionalSearch : ISearch 
    {
        public enum SearchOutcome {
            PathFoundFromOriginalProblem, PathFoundFromReverseProblem, PathFoundBetweenProblems, PathNotFound
        };

        protected Metrics Metrics;

        private SearchOutcome searchOutcome = SearchOutcome.PathNotFound;

        private static readonly String NodesExpanded = "nodesExpanded";

        private const String QueueSize = "queueSize";

        private const String MaxQueueSize = "maxQueueSize";

        private const String PathCost = "pathCost";

        public BidirectionalSearch() {
            this.Metrics = new Metrics();
        }

        //TODO: split this method to multiple methods.
        public IList<IAction> Search(Problem p) 
        {

            Debug.Assert(p is IBidirectionalProblem);

            searchOutcome = SearchOutcome.PathNotFound;

            ClearInstrumentation();

            var op = ((IBidirectionalProblem) p).GetOriginalProblem();
            var rp = ((IBidirectionalProblem) p).GetReverseProblem();

            var opFrontier = new CachedStateQueue<Node>();
            var rpFrontier = new CachedStateQueue<Node>();

            var ogs = new GraphSearch();
            var rgs = new GraphSearch();
            // Ensure the instrumentation for these
            // are cleared down as their values
            // are used in calculating the overall
            // bidirectional Metrics.
            ogs.ClearInstrumentation();
            rgs.ClearInstrumentation();

            var opNode = new Node(op.InitialState);
            var rpNode = new Node(rp.InitialState);
            opFrontier.Insert(opNode);
            rpFrontier.Insert(rpNode);

            this.SetQueueSize(opFrontier.Size() + rpFrontier.Size());
            this.SetNodesExpanded(ogs.GetNodesExpanded() + rgs.GetNodesExpanded());

            while (!(opFrontier.IsEmpty() && rpFrontier.IsEmpty()))
            {
                // Determine the nodes to work with and expand their fringes
                // in preparation for testing whether or not the two
                // searches meet or one or other is at the GOAL.
                if (!opFrontier.IsEmpty())
                {
                    opNode = opFrontier.Pop();
                    opFrontier.AddAll(ogs.GetResultingNodesToAddToFrontier(opNode, op));
                }
                else
                {
                    opNode = null;
                }
                if (!rpFrontier.IsEmpty())
                {
                    rpNode = rpFrontier.Pop();
                    rpFrontier.AddAll(rgs.GetResultingNodesToAddToFrontier(rpNode, rp));
                }
                else
                {
                    rpNode = null;
                }

                this.SetQueueSize(opFrontier.Size() + rpFrontier.Size());
                this.SetNodesExpanded(ogs.GetNodesExpanded() + rgs.GetNodesExpanded());

                //
                // First Check if either frontier contains the other's state
                if (null != opNode && null != rpNode)
                {
                    Node popNode = null;
                    Node prpNode = null;
                    if (opFrontier.ContainsNodeBasedOn(rpNode.State))
                    {
                        popNode = opFrontier.GetNodeBasedOn(rpNode.State);
                        prpNode = rpNode;
                    }
                    else if (rpFrontier.ContainsNodeBasedOn(opNode.State))
                    {
                        popNode = opNode;
                        prpNode = rpFrontier.GetNodeBasedOn(opNode.State);
                        // Need to also check whether or not the nodes that
                        // have been taken off the frontier actually represent the
                        // same state, otherwise there are instances whereby
                        // the searches can pass each other by
                    }
                    else if (opNode.State.Equals(rpNode.State))
                    {
                        popNode = opNode;
                        prpNode = rpNode;
                    }
                    if (null != popNode && null != prpNode)
                    {
                        IList<IAction> actions = this.RetrieveActions(op, rp, popNode, prpNode);
                        // It may be the case that it is not in fact possible to
                        // traverse from the original node to the goal node based on
                        // the reverse path (i.e. unidirectional links: e.g.
                        // InitialState(A)<->C<-Goal(B) )
                        if (null != actions)
                        {
                            return actions;
                        }
                    }
                }

                //
                // Check if the original problem is at the GOAL state
                if (null != opNode && SearchUtils.IsGoalState(op, opNode))
                {
                    // No need to check return value for null here
                    // as an action path discovered from the goal
                    // is guaranteed to exist
                    return this.RetrieveActions(op, rp, opNode, null);
                }
                //
                // Check if the reverse problem is at the GOAL state
                if (null != rpNode && SearchUtils.IsGoalState(rp, rpNode))
                {
                    IList<IAction> actions = this.RetrieveActions(op, rp, null, rpNode);
                    // It may be the case that it is not in fact possible to
                    // traverse from the original node to the goal node based on
                    // the reverse path (i.e. unidirectional links: e.g.
                    // InitialState(A)<-Goal(B) )
                    if (null != actions)
                    {
                        return actions;
                    }
                }
            }

            // Empty IList can indicate already at Goal
            // or unable to find valid Set of actions
            return new List<IAction>();
        }

        public SearchOutcome GetSearchOutcome() {
            return searchOutcome;
        }

        public Metrics GetMetrics() {
            return Metrics;
        }

        public void ClearInstrumentation() {
            Metrics.Set(NodesExpanded, 0);
            Metrics.Set(QueueSize, 0);
            Metrics.Set(MaxQueueSize, 0);
            Metrics.Set(PathCost, 0.0);
        }

        public int GetNodesExpanded() {
            return Metrics.GetInt(NodesExpanded);
        }

        public void SetNodesExpanded(int nodesExpanded) {
            Metrics.Set(NodesExpanded, nodesExpanded);
        }

        public int GetQueueSize() {
            return Metrics.GetInt(QueueSize);
        }

        public void SetQueueSize(int queueSize) {
            Metrics.Set(QueueSize, queueSize);
            int maxQSize = Metrics.GetInt(MaxQueueSize);
            if (queueSize > maxQSize) {
                Metrics.Set(MaxQueueSize, queueSize);
            }
        }

        public int GetMaxQueueSize() {
            return Metrics.GetInt(MaxQueueSize);
        }

        public double GetPathCost() {
            return Metrics.GetDouble(PathCost);
        }

        public void SetPathCost(double pathCost) {
            Metrics.Set(PathCost, pathCost);
        }

        private IList<IAction> RetrieveActions(Problem op, Problem rp, Node originalPath, Node reversePath) 
        {
            IList<IAction> actions = new List<IAction>();

            if (null == reversePath) {
                // This is the simple case whereby the path has been found
                // from the original problem first
                this.SetPathCost(originalPath.PathCost);
                searchOutcome = SearchOutcome.PathFoundFromOriginalProblem;
                actions = SearchUtils.ActionsFromNodes(originalPath.GetPathFromRoot());
            } else
            {
                var nodePath = new List<Node>();
                object originalState = null;
                if (null != originalPath)
                {
                    nodePath.AddRange(originalPath.GetPathFromRoot());
                    originalState = originalPath.State;
                }
                // Only append the reverse path if it is not the
                // GOAL state from the original problem (if you don't
                // you could end up appending a partial reverse path
                // that looks back on its initial state)
                if (!SearchUtils.IsGoalState(op, reversePath))
                {
                    IList<Node> rpath = reversePath.GetPathFromRoot();
                    for (int i = rpath.Count - 1; i >= 0; i--)
                    {
                        // Ensure do not include the node from the reverse path
                        // that is the one that potentially overlaps with the
                        // original path (i.e. if started in goal state or where
                        // they meet in the middle).
                        if (!rpath[i].State.Equals(originalState))
                        {
                            nodePath.Add(rpath[i]);
                        }
                    }
                }

                if (!canTraversePathFromOriginalProblem(op, nodePath, actions))
                {
                    // This is where it is possible to get to the initial state
                    // from the goal state (i.e. reverse path) but not the other way
                    // round, null returned to indicate an invalid path found from
                    // the reverse problem
                    return null;
                }

                if (null == originalPath)
                {
                    searchOutcome = SearchOutcome.PathFoundFromReverseProblem;
                }
                else
                {
                    // Need to ensure that where the original and reverse paths
                    // overlap, as they can link based on their fringes, that
                    // the reverse path is actually capable of connecting to
                    // the previous node in the original path (if not root).
                    if (this.CanConnectToOriginalFromReverse(rp, originalPath, reversePath))
                    {
                        searchOutcome = SearchOutcome.PathFoundBetweenProblems;
                    }
                    else
                    {
                        searchOutcome = SearchOutcome.PathFoundFromOriginalProblem;
                    }
                }
            }

            return actions;
        }

        private bool canTraversePathFromOriginalProblem(Problem op,
                IList<Node> path, IList<IAction> actions) {
            bool rVal = true;
            double pc = 0.0;

            for (int i = 0; i < (path.Count - 1); i++) {
                object currentState = path[i].State;
                object nextState = path[i + 1].State;
                bool found = false;
                foreach (IAction a in op.ActionsFunction.Actions(currentState)) {
                    object isNext = op.ResultFunction.Result(currentState, a);
                    if (nextState.Equals(isNext)) {
                        found = true;
                        pc += op.StepCostFunction.C(currentState, a, nextState);
                        actions.Add(a);
                        break;
                    }
                }

                if (!found) {
                    rVal = false;
                    break;
                }
            }

            this.SetPathCost(true == rVal ? pc : 0.0);

            return rVal;
        }

        private bool CanConnectToOriginalFromReverse(Problem rp,Node originalPath, Node reversePath) 
        {
            var canConnect = true;

            // Only need to test if not already at root
            if (!originalPath.IsRootNode()) {
                canConnect = rp.ActionsFunction.Actions(reversePath.State)
                    .Select(a => rp.ResultFunction.Result(reversePath.State, a))
                    .Contains(originalPath.Parent.State);
            }

            return canConnect;
        }
    }

    class CachedStateQueue<T> : FIFOQueue<T>
        where T : Node
    {
        private IDictionary<object, Node> cachedState = new Dictionary<object, Node>();

        public CachedStateQueue()
        {
        }

        public CachedStateQueue(ICollection<T> c) : base(c)
        {
        }

        public bool ContainsNodeBasedOn(object state) 
        {
            return cachedState.ContainsKey(state);
        }

        public Node GetNodeBasedOn(object state) 
        {
            return cachedState[state];
        }

        public override T Pop() 
        {
            T popped = base.Pop();
            cachedState.Remove(popped.State);
            return popped;
        }

        // Note: This is called by FIFOQueue.insert()->LinkedList.offer();
        public new void Add(T element) 
        {
            base.Add(element);
            cachedState[element.State] = element;
        }

        public void AddAll(ICollection<T> c) 
        {
            foreach (T element in c) 
            {
                cachedState[element.State] = element;
            }
            this.AddRange(c);
        }
    }

}
