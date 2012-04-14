using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    using Aima.Core.Agent;
    using Aima.Core.Util.Datastructure;

    public abstract class PrioritySearch : ISearch 
    {
        protected QueueSearch search;

        public Metrics GetMetrics()
        {
            return this.search.Metrics;
        }

        public IList<IAction> Search(Problem p) 
        {
            return this.search.Search(p, new PriorityQueue<Node>(5, this.GetComparer()));
        }

        protected abstract IComparer<Node> GetComparer();
    }
}
