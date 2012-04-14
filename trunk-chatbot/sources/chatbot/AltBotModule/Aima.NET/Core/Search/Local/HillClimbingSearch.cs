using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Local
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;
    using Aima.Core.Util;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.2, page 122.
    /// 
    /// <code><![CDATA[
    /// function HILL-CLIMBING(problem) returns a state that is a local maximum
    ///                    
    ///   current <- MAKE-NODE(problem.INITIAL-STATE)
    ///   loop do
    ///     neighbor <- a highest-valued successor of current
    ///     if neighbor.VALUE <= current.VALUE then return current.STATE
    ///     current <- neighbor
    /// ]]></code>
    /// Figure 4.2 The hill-climbing search algorithm, which is the most basic local search technique. 
    /// At each step the current node is replaced by the best neighbor; in this version, that means 
    /// the neighbor with the highest VALUE, but if a heuristic cost estimate h is used, we would find 
    /// the neighbor with the lowest h.
    /// </summary>
    public class HillClimbingSearch : NodeExpander , ISearch 
    {

        public enum SearchOutcome {
            Failure, SolutionFound
        };

        private IHeuristicFunction hf;

        private SearchOutcome outcome = SearchOutcome.Failure;

        private object lastState;

        public HillClimbingSearch(IHeuristicFunction hf) 
        {
            this.hf = hf;
        }

        // function HILL-CLIMBING(problem) returns a state that is a local maximum
        public IList<IAction> Search(Problem p) 
        {
            ClearInstrumentation();
            outcome = SearchOutcome.Failure;
            lastState = null;
            // current <- MAKE-NODE(problem.INITIAL-STATE)
            Node current = new Node(p.InitialState);
            Node neighbor = null;
            // loop do
            while (!CancelableThread.CurrIsCanceled()) {
                IList<Node> children = ExpandNode(current, p);
                // neighbor <- a highest-valued successor of current
                neighbor = this.GetHighestValuedNodeFrom(children, p);

                // if neighbor.VALUE <= current.VALUE then return current.STATE
                if ((neighbor == null) || (this.GetValue(neighbor) <= this.GetValue(current))) {
                    if (SearchUtils.IsGoalState(p, current)) {
                        outcome = SearchOutcome.SolutionFound;
                    }
                    lastState = current.State;
                    return SearchUtils.ActionsFromNodes(current.GetPathFromRoot());
                }
                // current <- neighbor
                current = neighbor;
            }
            return new List<IAction>();
        }

        public Metrics GetMetrics()
        {
            return this.Metrics;
        }

        public SearchOutcome GetOutcome() {
            return outcome;
        }

        public object GetLastSearchState() {
            return lastState;
        }

        private Node GetHighestValuedNodeFrom(IList<Node> children, Problem p) {
            double highestValue = double.NegativeInfinity;
            Node nodeWithHighestValue = null;
            foreach (var child in children)
            {
                var value = this.GetValue(child);
                if (value > highestValue)
                {
                    highestValue = value;
                    nodeWithHighestValue = child;
                }
            }
            return nodeWithHighestValue;
        }

        private double GetValue(Node n) 
        {
            // assumption greater heuristic value =>
            // HIGHER on hill; 0 == goal state;
            return -1 * hf.H(n.State);
        }
    }
}
