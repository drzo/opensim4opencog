using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Compares two nodes based on their position within a tree
    /// </summary>
    public class NodeHierarchyPositionComparer : IComparer<TreeBankNode>
    {

        /// <summary>
        /// Compares two nodes based on their position within a tree. Returns -1 if the first node is a strict ancestor of the second, 
        /// 1 if the second node is a strict ancestor of the first, and 0 otherwise. Here, "strict" implies that the nodes are not
        /// equal. Both nodes must come from the same tree.
        /// </summary>
        /// <param name="x">First node</param>
        /// <param name="y">Second node</param>
        /// <returns>-1 if the first node is an ancestor of the second, 1 if the second node is an ancestor of the first, and 
        /// 0 otherwise.</returns>
        public int Compare(TreeBankNode x, TreeBankNode y)
        {
            if (x.Root != y.Root)
                throw new Exception("Nodes come from different trees");

            if (x == y)
                return 0;

            if (x.IsAncestorOf(y))
                return -1;
            else if (y.IsAncestorOf(x))
                return 1;
            else
                return 0;
        }
    }
}
