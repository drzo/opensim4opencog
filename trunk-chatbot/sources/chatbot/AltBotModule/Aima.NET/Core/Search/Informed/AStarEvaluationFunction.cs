using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Informed
{
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 93.
    /// 
    /// The most widely known form of best-first search is called A* search (pronounced "A-star
    /// Search"). It evaluates nodes by combining g(n), the cost to reach the node, and h(n), the cost
    /// to get from the node to the goal:<br />
    /// <code>
    ///        f(n) = g(n) + h(n).
    /// </code>
    /// </summary>
    public class AStarEvaluationFunction : IEvaluationFunction 
    {

        private PathCostFunction gf = new PathCostFunction();
        private IHeuristicFunction hf = null;

        public AStarEvaluationFunction(IHeuristicFunction hf) 
        {
            this.hf = hf;
        }

        public double F(Node n) 
        {
            // f(n) = g(n) + h(n)
            return gf.G(n) + hf.H(n.State);
        }
}

}
