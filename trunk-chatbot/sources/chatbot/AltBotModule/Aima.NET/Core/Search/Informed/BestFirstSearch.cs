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
    /// Best-first search is an instance of the general TREE-SEARCH or GRAPH-SEARCH algorithm 
    /// in which a node is selected for expansion based on an evaluation function, f(n). The 
    /// evaluation function is construed as a cost estimate, so the node with the lowest evaluation 
    /// is expanded first. The implementation of best-first graph search is identical to that for 
    /// uniform-cost search (Figure 3.14), except for the use of f instead of g to order the 
    /// priority queue.
    /// </summary>
    public class BestFirstSearch : PrioritySearch 
    {

        private readonly IEvaluationFunction evaluationFunction;

        public BestFirstSearch(QueueSearch search, IEvaluationFunction ef) 
        {
            this.search = search;
            evaluationFunction = ef;
        }

        protected override IComparer<Node> GetComparer()
        {
            IComparer<Node> f = new BestFirstComparer(evaluationFunction);
            
            if (this.search is GraphSearch) 
            {
                ((GraphSearch) this.search).ReplaceFrontierNodeAtStateCostFunction = f;
            }

            return f;
        }
        class BestFirstComparer : Comparer<Node>
        {
            private IEvaluationFunction evaluationFunction;

            public BestFirstComparer(IEvaluationFunction evalF)
            {
                evaluationFunction = evalF;
            }
            public override int Compare(Node x, Node y)
            {
                double f1 = evaluationFunction.F(x);
                double f2 = evaluationFunction.F(y);

                return f1.CompareTo(f2);
            }
        }
    }

}
