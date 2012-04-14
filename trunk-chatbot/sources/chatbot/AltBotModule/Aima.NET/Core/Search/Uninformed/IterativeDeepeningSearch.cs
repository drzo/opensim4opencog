using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Uninformed
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.18, page 89.
    /// 
    /// <code><![CDATA[
    /// function ITERATIVE-DEEPENING-SEARCH(problem) returns a solution, or failure
    ///   for depth = 0 to infinity  do
    ///     result <- DEPTH-LIMITED-SEARCH(problem, depth)
    ///     if result != cutoff then return result
    /// ]]></code>
    /// Figure 3.18 The iterative deepening search algorithm, which repeatedly
    /// applies depth-limited search with increasing limits. It terminates when a
    /// solution is found or if the depth- limited search returns failure, meaning
    /// that no solution exists.
    /// </summary>
    public class IterativeDeepeningSearch : NodeExpander, ISearch 
    {
        public static readonly string PathCost = "pathCost";

        // Not infinity, but will do, :-)
        private const int Infinity = int.MaxValue;

        private readonly Metrics iterationMetrics;

        public IterativeDeepeningSearch() {
            iterationMetrics = new Metrics();
            iterationMetrics.Set(MetricNodesExpanded, 0);
            iterationMetrics.Set(PathCost, 0);
        }

        // function ITERATIVE-DEEPENING-SEARCH(problem) returns a solution, or
        // failure
        public IList<IAction> Search(Problem p) 
        {
            iterationMetrics.Set(MetricNodesExpanded, 0);
            iterationMetrics.Set(PathCost, 0);
            // for depth = 0 to infinity do
            for (int i = 0; i <= Infinity; i++) {
                // result <- DEPTH-LIMITED-SEARCH(problem, depth)
                DepthLimitedSearch dls = new DepthLimitedSearch(i);
                IList<IAction> result = dls.Search(p);
                iterationMetrics.Set(MetricNodesExpanded, 
                    iterationMetrics.GetInt(MetricNodesExpanded) + dls.GetMetrics().GetInt(MetricNodesExpanded));
                // if result != cutoff then return result
                if (!dls.IsCutOff(result)) {
                    iterationMetrics.Set(PathCost, dls.GetPathCost());
                    return result;
                }
            }
            return this.Failure();
        }

        public Metrics GetMetrics() 
        {
            return iterationMetrics;
        }

        private IList<IAction> Failure() {
            return new List<IAction>();
        }
    }
}
