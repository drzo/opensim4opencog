using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.7, page 77. 
    /// <code>
    /// function GRAPH-SEARCH(problem) returns a solution, or failure
    ///   initialize the frontier using the initial state of problem
    ///   initialize the explored set to be empty
    ///   loop do
    ///     if the frontier is empty then return failure
    ///     choose a leaf node and remove it from the frontier
    ///     if the node contains a goal state then return the corresponding solution
    ///     Add the node to the explored set
    ///     expand the chosen node, adding the resulting nodes to the frontier
    ///       only if not in the frontier or explored set
    /// </code> 
    /// Figure 3.7 An informal description of the general graph-search algorithm.
    /// </summary>
    public class GraphSearch : QueueSearch 
    {

        private ISet<object> explored = new HashedSet<object>();
        private IDictionary<object, Node> frontierState = new Dictionary<object, Node>();
        private IComparer<Node> replaceFrontierNodeAtStateCostFunction = null;
        private IList<Node> addToFrontier = new List<Node>();

        public IComparer<Node> ReplaceFrontierNodeAtStateCostFunction
        {
            get
            {
                return this.replaceFrontierNodeAtStateCostFunction;
            }
            set
            {
                this.replaceFrontierNodeAtStateCostFunction = value;
            }
        }

        // Need to override search() method so that I can re-initialize
        // the explored set should multiple calls to search be made.
        public override IList<IAction> Search(Problem problem, IQueue<Node> frontier) 
        {
            // initialize the explored set to be empty
            explored.Clear();
            frontierState.Clear();
            return base.Search(problem, frontier);
        }

        public override Node PopNodeFromFrontier() {
            Node toRemove = base.PopNodeFromFrontier();
            frontierState.Remove(toRemove.State);
            return toRemove;
        }

        public override bool RemoveNodeFromFrontier(Node toRemove) {
            bool removed = base.RemoveNodeFromFrontier(toRemove);
            if (removed) {
                frontierState.Remove(toRemove.State);
            }
            return removed;
        }

        public override IList<Node> GetResultingNodesToAddToFrontier(Node nodeToExpand,Problem problem) 
        {
            addToFrontier.Clear();
            // Add the node to the explored set
            explored.Add(nodeToExpand.State);
            // expand the chosen node, adding the resulting nodes to the frontier
            foreach (Node cfn in this.ExpandNode(nodeToExpand, problem))
            {
                Node frontierNode = frontierState[cfn.State];
                bool yesAddToFrontier = false;
                // only if not in the frontier or explored set
                if (null == frontierNode && !explored.Contains(cfn.State))
                {
                    yesAddToFrontier = true;
                }
                else if (null != frontierNode && null != this.ReplaceFrontierNodeAtStateCostFunction &&
                         this.ReplaceFrontierNodeAtStateCostFunction.Compare(cfn, frontierNode) < 0)
                {
                    // child.STATE is in frontier with higher cost
                    // replace that frontier node with child
                    yesAddToFrontier = true;
                    // Want to replace the current frontier node with the child
                    // node therefore mark the child to be added and remove the
                    // current fontierNode
                    this.RemoveNodeFromFrontier(frontierNode);
                    // Ensure removed from Add to frontier as well
                    // as 1 or more may reach the same state at the same time
                    addToFrontier.Remove(frontierNode);
                }

                if (yesAddToFrontier)
                {
                    addToFrontier.Add(cfn);
                    frontierState[cfn.State] = cfn;
                }
            }

            return addToFrontier;
        }
    }
}
