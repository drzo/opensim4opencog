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
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.5, page 126.
    /// 
    /// <code><![CDATA[
    /// function SIMULATED-ANNEALING(problem, schedule) returns a solution state
    ///                    
    ///   current <- MAKE-NODE(problem.INITIAL-STATE)
    ///   for t = 1 to INFINITY do
    ///     T <- schedule(t)
    ///     if T = 0 then return current
    ///     next <- a randomly selected successor of current
    ///     /\E <- next.VALUE - current.value
    ///     if /\E > 0 then current <- next
    ///     else current <- next only with probability e^(/\E/T)
    /// ]]></code>
    /// Figure 4.5 The simulated annealing search algorithm, a version of
    /// stochastic hill climbing where some downhill moves are allowed. Downhill
    /// moves are accepted readily early in the annealing schedule and then less
    /// often as time goes on. The schedule input determines the value of
    /// the temperature T as a function of time.
    /// </summary>
    public class SimulatedAnnealingSearch : NodeExpander , ISearch 
    {

        public enum SearchOutcome {
            Failure, SolutionFound
        };

        private readonly IHeuristicFunction hf;
        private readonly Scheduler scheduler;

        private SearchOutcome outcome = SearchOutcome.Failure;

        private object lastState;

        public SimulatedAnnealingSearch(IHeuristicFunction hf) {
            this.hf = hf;
            this.scheduler = new Scheduler();
        }

        public SimulatedAnnealingSearch(IHeuristicFunction hf, Scheduler scheduler) {
            this.hf = hf;
            this.scheduler = scheduler;
        }

        // function SIMULATED-ANNEALING(problem, schedule) returns a solution state
        public IList<IAction> Search(Problem p) 
        {
            ClearInstrumentation();
            outcome = SearchOutcome.Failure;
            lastState = null;
            // current <- MAKE-NODE(problem.INITIAL-STATE)
            Node current = new Node(p.InitialState);
            Node next = null;
            IList<IAction> ret = new List<IAction>();
            // for t = 1 to INFINITY do
            int timeStep = 0;
            while (!CancelableThread.CurrIsCanceled()) {
                // temperature <- schedule(t)
                double temperature = scheduler.GetTemp(timeStep);
                timeStep++;
                // if temperature = 0 then return current
                if (temperature == 0.0) {
                    if (SearchUtils.IsGoalState(p, current)) {
                        outcome = SearchOutcome.SolutionFound;
                    }
                    ret = SearchUtils.ActionsFromNodes(current.GetPathFromRoot());
                    lastState = current.State;
                    break;
                }

                IList<Node> children = ExpandNode(current, p);
                if (children.Count > 0) 
                {
                    // next <- a randomly selected successor of current
                    next = Util.SelectRandomlyFromList(children);
                    // /\E <- next.VALUE - current.value
                    double deltaE = this.GetValue(p, next) - this.GetValue(p, current);

                    if (this.ShouldAccept(temperature, deltaE)) 
                    {
                        current = next;
                    }
                }
            }

            return ret;
        }

        public Metrics GetMetrics()
        {
            return this.Metrics;
        }

        public double ProbabilityOfAcceptance(double temperature, double deltaE) {
            return Math.Exp(deltaE / temperature);
        }

        public SearchOutcome GetOutcome() {
            return outcome;
        }

        public object GetLastSearchState() {
            return lastState;
        }

        // if /\E > 0 then current <- next
        // else current <- next only with probability e^(/\E/T)
        private bool ShouldAccept(double temperature, double deltaE) 
        {
            return (deltaE > 0.0)
                    || (new Random().NextDouble() <= this.ProbabilityOfAcceptance(temperature, deltaE));
        }

        private double GetValue(Problem p, Node n) 
        {
            // assumption greater heuristic value =>
            // HIGHER on hill; 0 == goal state;
            // SA deals with gardient DESCENT
            return -1 * hf.H(n.State);
        }
    }
}
