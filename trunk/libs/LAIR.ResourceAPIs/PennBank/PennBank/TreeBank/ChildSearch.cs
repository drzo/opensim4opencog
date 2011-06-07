using System;
using System.Collections.Generic;
using System.Text;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Simple rules for locating children of a node using a linear search on their syntactic categories
    /// </summary>
    public class ChildSearch
    {
        #region static members
        /// <summary>
        /// Search directions
        /// </summary>
        public enum SearchDirection
        {
            /// <summary>
            /// Searches child nodes from left to right
            /// </summary>
            LeftToRight,

            /// <summary>
            /// Searches child nodes from right to left
            /// </summary>
            RightToLeft
        };
        #endregion

        private SearchDirection _direction;
        private List<string> _searchList;

        /// <summary>
        /// Gets or sets the search direction
        /// </summary>
        public SearchDirection Direction
        {
            get { return _direction; }
            set { _direction = value; }
        }

        /// <summary>
        /// Gets or sets the search list
        /// </summary>
        public List<string> SearchList
        {
            get { return _searchList; }
            set { _searchList = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="direction">Search direction</param>
        /// <param name="searchList">Prioritized search list</param>
        public ChildSearch(SearchDirection direction, List<string> searchList)
        {
            _direction = direction;
            _searchList = searchList;
        }

        /// <summary>
        /// Runs the current search for the first match of the current rule among the children of a node
        /// </summary>
        /// <param name="node">Node whose children should be searched</param>
        /// <param name="excludeNodes">Nodes to exclude from the search</param>
        /// <returns>First child that matches the current rule</returns>
        public TreeBankNode Run(TreeBankNode node, Set<TreeBankNode> excludeNodes)
        {
            // get list of children to search, reversing the order if we're searching from right to left
            List<TreeBankNode> children = new List<TreeBankNode>();
            for (int i = 0; i < node.ChildCount; ++i)
                children.Add(node.GetChild(i));

            if (_direction == SearchDirection.RightToLeft)
                children.Reverse();

            // search for each category in the search list
            foreach (string cat in _searchList)
            {
                TreeBankEngine.SyntacticCategory searchCat = TreeBankEngine.GetSyntacticCategory(cat);
                foreach (TreeBankNode child in children)
                    if (child.Category == searchCat)
                        if (excludeNodes == null || !excludeNodes.Contains(child))
                            return child;
            }

            return null;
        }
    }
}
