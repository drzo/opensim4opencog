using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.7, page 77.
    /// <code>
    /// function TREE-SEARCH(problem) returns a solution, or failure
    ///   initialize the frontier using the initial state of the problem
    ///   loop do
    ///     if the frontier is empty then return failure
    ///     choose a leaf node and remove it from the frontier
    ///     if the node contains a goal state then return the corresponding solution
    ///     expand the chosen node, adding the resulting nodes to the frontier
    /// </code>
    /// Figure 3.7 An informal description of the general tree-search algorithm.
    /// </summary>
    public class TreeSearch : QueueSearch 
    {

        public override IList<Node> GetResultingNodesToAddToFrontier(Node nodeToExpand, Problem problem) 
        {
            // expand the chosen node, adding the resulting nodes to the frontier
            return ExpandNode(nodeToExpand, problem);
        }
    }
}
