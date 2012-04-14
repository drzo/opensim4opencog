using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 92.<para/>
    /// a heuristic function, denoted H(n):<br />
    ///   H(n) = estimated cost of the cheapest path from the state at node n to a goal state.<para/>
    /// Notice that H(n) takes a node as input, but, unlike g(n) it depends only on the state at that node.
    /// </summary>
    public interface IHeuristicFunction
    {
        double H(object state);
    }
}
