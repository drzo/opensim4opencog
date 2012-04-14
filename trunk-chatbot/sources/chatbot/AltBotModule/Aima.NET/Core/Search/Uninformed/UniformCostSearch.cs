using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Uninformed
{
    using Aima.Core.Search.Framework;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.14, page 84. 
    /// <code><![CDATA[
    /// function UNIFORM-COST-SEARCH(problem) returns a solution, or failure
    ///   node <- a node with STATE = problem.INITIAL-STATE, PATH-COST = 0
    ///   frontier <- a priority queue ordered by PATH-COST, with node as the only element
    ///   explored <- an empty set
    ///   loop do
    ///      if EMPTY?(frontier) then return failure
    ///      node <- POP(frontier) // chooses the lowest-cost node in frontier
    ///      if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
    ///      add node.STATE to explored
    ///      for each action in problem.ACTIONS(node.STATE) do
    ///          child <- CHILD-NODE(problem, node, action)
    ///          if child.STATE is not in explored or frontier then
    ///             frontier <- INSERT(child, frontier)
    ///          else if child.STATE is in frontier with higher PATH-COST then
    ///             replace that frontier node with child
    /// ]]></code> 
    /// Figure 3.14 Uniform-cost search on a graph. The algorithm is identical to the general
    /// graph search algorithm in Figure 3.7, except for the use of a priority queue and the addition
    /// of an extra check in case a shorter path to a frontier state is discovered. The data structure
    /// for frontier needs to support efficient membership testing, so it should combine the capabilities
    /// of a priority queue and a hash table.
    /// </summary>
    public class UniformCostSearch : PrioritySearch
    {
        private static readonly IComparer<Node> G = new UniformCostSearchComparer();
        

        public UniformCostSearch() : this(new GraphSearch()) 
        {
        }

        public UniformCostSearch(QueueSearch search) 
        {
            this.search = search;
            if (search is GraphSearch) 
            {
                ((GraphSearch) search).ReplaceFrontierNodeAtStateCostFunction = G;
            }
        }

        protected override IComparer<Node> GetComparer() 
        {
            return G;
        }

        private class UniformCostSearchComparer : IComparer<Node>
        {
            public int Compare(Node node1, Node node2) 
            {
                return (node1.PathCost.CompareTo(node2.PathCost));
            }
        }
    }
}
