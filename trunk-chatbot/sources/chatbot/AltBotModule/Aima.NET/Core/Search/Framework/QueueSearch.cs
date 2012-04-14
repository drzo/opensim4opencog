using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;
    using Aima.Core.Util;
    using Aima.Core.Util.Datastructure;

    public abstract class QueueSearch : NodeExpander 
    {
        public static readonly string MetricQueueSize = "queueSize";

        public static readonly string MetricMaxQueueSize = "maxQueueSize";

        public static readonly string MetricPathCost = "pathCost";

        private IQueue<Node> frontier = null;
        private bool checkGoalBeforeAddingToFrontier = false;

        public bool IsFailure(IList<IAction> result) {
            return result.Count == 0;
        }

        /// <summary>
        /// </summary>
        /// <param name="problem"></param>
        /// <param name="localFrontier"></param>
        /// <returns>if goal found, the list of actions to the Goal. If already at the goal 
        /// you will receive a IList with a single NoOp IAction in it. If fail to find the Goal, 
        /// an empty list will be returned to indicate that the search failed.</returns>
        public virtual IList<IAction> Search(Problem problem, IQueue<Node> localFrontier) {
            this.frontier = localFrontier;

            ClearInstrumentation();
            // initialize the frontier using the initial state of the problem
            var root = new Node(problem.InitialState);
            if (this.IsCheckGoalBeforeAddingToFrontier()) {
                if (SearchUtils.IsGoalState(problem, root)) {
                    return SearchUtils.ActionsFromNodes(root.GetPathFromRoot());
                }
            }
            localFrontier.Push(root);
            this.SetQueueSize(localFrontier.Size());
            while (!localFrontier.IsEmpty() && !CancelableThread.CurrIsCanceled()) 
            {
                // choose a leaf node and remove it from the frontier
                Node nodeToExpand = this.PopNodeFromFrontier();
                this.SetQueueSize(localFrontier.Size());
                // Only need to check the nodeToExpand if have not already
                // checked before adding to the frontier
                if (!this.IsCheckGoalBeforeAddingToFrontier()) 
                {
                    // if the node contains a goal state then return the
                    // corresponding solution
                    if (SearchUtils.IsGoalState(problem, nodeToExpand)) {
                        this.SetPathCost(nodeToExpand.PathCost);
                        return SearchUtils.ActionsFromNodes(nodeToExpand.GetPathFromRoot());
                    }
                }
                // expand the chosen node, adding the resulting nodes to the
                // frontier
                foreach (Node fn in this.GetResultingNodesToAddToFrontier(nodeToExpand, problem))
                {
                    if (this.IsCheckGoalBeforeAddingToFrontier()) 
                    {
                        if (SearchUtils.IsGoalState(problem, fn)) 
                        {
                            return SearchUtils.ActionsFromNodes(fn.GetPathFromRoot());
                        }
                    }
                    localFrontier.Push(fn);
                }
                this.SetQueueSize(localFrontier.Size());
            }
            // if the frontier is empty then return failure
            return this.Failure();
        }

        public bool IsCheckGoalBeforeAddingToFrontier() 
        {
            return checkGoalBeforeAddingToFrontier;
        }

        public void SetCheckGoalBeforeAddingToFrontier(bool chkGoalBeforeAddingToFrontier) 
        {
            this.checkGoalBeforeAddingToFrontier = chkGoalBeforeAddingToFrontier;
        }

        public virtual Node PopNodeFromFrontier()
        {
            return frontier.Pop();
        }

        public virtual bool RemoveNodeFromFrontier(Node toRemove) 
        {
            return frontier.Remove(toRemove);
        }

        public abstract IList<Node> GetResultingNodesToAddToFrontier(Node nodeToExpand, Problem p);

        public override void ClearInstrumentation()
        {
            base.ClearInstrumentation();
            this.Metrics.Set(MetricQueueSize, 0);
            this.Metrics.Set(MetricMaxQueueSize, 0);
            this.Metrics.Set(MetricPathCost, 0);
        }

        public int GetQueueSize() {
            return this.Metrics.GetInt("queueSize");
        }

        public void SetQueueSize(int queueSize) 
        {

            this.Metrics.Set(MetricQueueSize, queueSize);
            int maxQSize = this.Metrics.GetInt(MetricMaxQueueSize);
            if (queueSize > maxQSize) {
                this.Metrics.Set(MetricMaxQueueSize, queueSize);
            }
        }

        public int GetMaxQueueSize() {
            return this.Metrics.GetInt(MetricMaxQueueSize);
        }

        public double GetPathCost() {
            return this.Metrics.GetDouble(MetricPathCost);
        }

        public void SetPathCost(double pathCost) {
            this.Metrics.Set(MetricPathCost, pathCost);
        }

        private IList<IAction> Failure() 
        {
            return new List<IAction>();
        }
    }
}
