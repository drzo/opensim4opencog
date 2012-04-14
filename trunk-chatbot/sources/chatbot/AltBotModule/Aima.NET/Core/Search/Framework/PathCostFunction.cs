using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Framework
{
    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): page 78.
    /// </summary>
    public class PathCostFunction
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="n"></param>
        /// <returns>the cost, traditionally denoted by g(n), of the path from the
        /// initial state to the node, as indicated by the parent pointers.</returns>
        public double G(Node n)
        {
            return n.PathCost;
        }
    }
}
