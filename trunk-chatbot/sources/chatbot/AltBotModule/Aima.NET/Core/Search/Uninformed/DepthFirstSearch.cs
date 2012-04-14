using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Uninformed
{
    using Aima.Core.Agent;
    using Aima.Core.Search.Framework;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 85.
    /// 
    /// Depth-first search always expands the deepest node in the current frontier of the search tree.
    /// 
    /// Note: Supports both Tree and Graph based versions by assigning an instance
    /// of TreeSearch or GraphSearch to its constructor.
    /// </summary>
    public class DepthFirstSearch : ISearch 
    {

        QueueSearch search;

        public DepthFirstSearch(QueueSearch search) 
        {
            this.search = search;
        }

        public IList<IAction> Search(Problem p) {
            return search.Search(p, new LIFOQueue<Node>());
        }

        public Metrics GetMetrics() {
            return search.Metrics;
        }
    }
}
