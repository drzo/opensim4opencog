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
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.11, page 82.
    /// <code><![CDATA[
    /// function BREADTH-FIRST-SEARCH(problem) returns a solution, or failure
    ///   node <- a node with STATE = problem.INITIAL-STATE, PATH-COST=0
    ///   if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
    ///   frontier <- a FIFO queue with node as the only element
    ///   explored <- an empty set
    ///   loop do
    ///      if EMPTY?(frontier) then return failure
    ///      node <- POP(frontier) // chooses the shallowest node in frontier
    ///      add node.STATE to explored
    ///      for each action in problem.ACTIONS(node.STATE) do
    ///          child <- CHILD-NODE(problem, node, action)
    ///          if child.STATE is not in explored or frontier then
    ///              if problem.GOAL-TEST(child.STATE) then return SOLUTION(child)
    ///              frontier <- INSERT(child, frontier)
    /// ]]></code> 
    /// Figure 3.11 Breadth-first search on a graph.
    /// Note: Supports both Tree and Graph based versions by assigning an instance
    /// of TreeSearch or GraphSearch to its constructor.
    /// </summary>
    public class BreadthFirstSearch : ISearch 
    {

        private readonly QueueSearch search;

        public BreadthFirstSearch() : this(new GraphSearch()) {
        }

        public BreadthFirstSearch(QueueSearch search) {
            // Goal test is to be applied to each node when it is generated
            // rather than when it is selected for expansion.
            search.SetCheckGoalBeforeAddingToFrontier(true);
            this.search = search;
        }

        public IList<IAction> Search(Problem p) {
            return this.search.Search(p, new FIFOQueue<Node>());
        }

        public Metrics GetMetrics() 
        {
            return this.search.Metrics;
        }
    }
}
