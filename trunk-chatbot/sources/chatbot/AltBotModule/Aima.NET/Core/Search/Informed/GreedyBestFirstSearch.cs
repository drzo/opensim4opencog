using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Informed
{
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 92.
    /// 
    /// Greedy best-first search tries to expand the node that is closest to the goal,
    /// on the grounds that this is likely to lead to a solution quickly. Thus, it evaluates
    /// nodes by using just the heuristic function; that is, f(n) = h(n)
    /// </summary>
    public class GreedyBestFirstSearch : BestFirstSearch 
    {

        public GreedyBestFirstSearch(QueueSearch search, IHeuristicFunction hf): base(search, new GreedyBestFirstEvaluationFunction(hf))
        {
        }
    }
}
