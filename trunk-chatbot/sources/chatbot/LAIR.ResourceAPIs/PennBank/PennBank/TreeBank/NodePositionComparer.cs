using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Compares TreeBankNodes using their article/sentence/leaf positions
    /// </summary>
    public class NodePositionComparer : IComparer<TreeBankNode>
    {
        private bool _throwExceptionOnNodeOverlap;

        /// <summary>
        /// Gets or sets whether or not to throw an exception when compared nodes overlap
        /// </summary>
        public bool ThrowExceptionOnNodeOverlap
        {
            get { return _throwExceptionOnNodeOverlap; }
            set { _throwExceptionOnNodeOverlap = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="throwExceptionOnNodeOverlap">Whether or not to throw an exception when compared nodes overlap</param>
        public NodePositionComparer(bool throwExceptionOnNodeOverlap)
        {
            _throwExceptionOnNodeOverlap = throwExceptionOnNodeOverlap;
        }

        /// <summary>
        /// Compares the position of two TreeBankNodes, first by article, then by sentence, and last by leaf position. Both 
        /// nodes must come from the same MRG file. -1 if x ends before y begins, 1 if x begins after y ends. Otherwise, the 
        /// nodes overlap and either (1) zero will be returned if ThrowExceptionOnNodeOverlap is false or (2) an exception will
        /// be thrown.
        /// </summary>
        /// <param name="x">First node</param>
        /// <param name="y">Second node</param>
        /// <returns>-1 if x ends before y begins, 1 if x begins after y ends. Otherwise, the nodes overlap and either (1) zero will be returned
        /// if ThrowExceptionOnNodeOverlap is false or (2) an exception will be thrown.</returns>
        public int Compare(TreeBankNode x, TreeBankNode y)
        {
            if (x == null || y == null)
                throw new NullReferenceException("Cannot pass null nodes to comparison");

            if (x.MrgFile != y.MrgFile)
                throw new Exception("Compared nodes must reside in same MRG file");

            // compare sentence numbers
            if (x.SentenceNumber < y.SentenceNumber)
                return -1;
            else if (x.SentenceNumber > y.SentenceNumber)
                return 1;
            // sentence numbers are equal, compare leaf positions
            else if (x.LastLeaf.LeafNumber < y.FirstLeaf.LeafNumber)
                return -1;
            else if (x.FirstLeaf.LeafNumber > y.LastLeaf.LeafNumber)
                return 1;
            // the nodes overlap in the same sentence
            else
                if (_throwExceptionOnNodeOverlap)
                    throw new Exception("Overlapping nodes");
                else
                    return 0;
        }

        /// <summary>
        /// Checks if a node comes between two end points. The linear order of the end points does not matter.
        /// </summary>
        /// <param name="node">Node to check</param>
        /// <param name="endPoint1">First end point</param>
        /// <param name="endPoint2">Second end point</param>
        /// <returns>True if node is between two end points, false otherwise.</returns>
        public bool NodeIsBetween(TreeBankNode node, TreeBankNode endPoint1, TreeBankNode endPoint2)
        {
            int cmp1 = Compare(node, endPoint1);
            if (cmp1 == 0)
                return false;

            int cmp2 = Compare(node, endPoint2);
            if (cmp2 == 0)
                return false;

            return cmp1 + cmp2 == 0;  // draw it out...it makes sense!
        }
    }
}
