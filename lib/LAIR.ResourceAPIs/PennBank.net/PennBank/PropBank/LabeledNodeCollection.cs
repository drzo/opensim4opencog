using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Collection for nodes labeled in the style of PropBank and NomBank
    /// </summary>
    public abstract class LabeledNodeCollection
    {
        private Set<TreeBankNode> _singleNodes;          // each node represents a non-split label...multiple nodes are coreferential
        private Set<List<TreeBankNode>> _splitNodes;     // each inner list holds the nodes that make up a single split node
        private NodePositionComparer _positionComparer;  // used to sort split argument lists

        /// <summary>
        /// Gets the set of single coreferential nodes. WARNING:  don't add nodes to the returned collection. Instead call the 
        /// appropriate function in this class.
        /// </summary>
        public Set<TreeBankNode> SingleNodes
        {
            get { return _singleNodes; }
        }

        /// <summary>
        /// Gets the set of split nodes.  WARNING:  don't add nodes to the returned collection. Instead call the 
        /// appropriate function in this class.
        /// </summary>
        public Set<List<TreeBankNode>> SplitNodes
        {
            get { return _splitNodes; }
        }

        /// <summary>
        /// Gets location label (in the style of PropBank) for the nodes in this collection
        /// </summary>
        public virtual string NodeLocations
        {
            get
            {
                StringBuilder locations = new StringBuilder();

                // add single nodes first
                bool prependAsterisk = false;
                foreach (TreeBankNode singleNode in _singleNodes)
                {
                    locations.Append((prependAsterisk ? "*" : "") + singleNode.Location);
                    prependAsterisk = true;
                }

                // add split nodes
                foreach (List<TreeBankNode> splitNode in _splitNodes)
                {
                    // build split location label
                    StringBuilder splitLabel = new StringBuilder();
                    bool prependComma = false;
                    foreach (TreeBankNode node in splitNode)
                    {
                        splitLabel.Append((prependComma ? "," : "") + node.Location);
                        prependComma = true;
                    }

                    // add to coreference chain
                    locations.Append((prependAsterisk ? "*" : "") + splitLabel);
                    prependAsterisk = true;
                }

                return locations.ToString();
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        protected LabeledNodeCollection()
        {
            _singleNodes = new Set<TreeBankNode>();
            _splitNodes = new Set<List<TreeBankNode>>();
            _positionComparer = new NodePositionComparer(false);
        }

        /// <summary>
        /// Adds a single node to this collection
        /// </summary>
        /// <param name="singleNode">Single node to add</param>
        public virtual void AddSingleNode(TreeBankNode singleNode)
        {
            _singleNodes.Add(singleNode);
        }

        /// <summary>
        /// Gets whether or not a single node is contained in this collection
        /// </summary>
        /// <param name="singleNode">Single node to check for</param>
        /// <returns>True if node is contained and false otherwise</returns>
        public virtual bool ContainsSingleNode(TreeBankNode singleNode)
        {
            return _singleNodes.Contains(singleNode);
        }

        /// <summary>
        /// Removes a single node from this collection
        /// </summary>
        /// <param name="singleNode">Single node to remove</param>
        public virtual void RemoveSingleNode(TreeBankNode singleNode)
        {
            _singleNodes.Remove(singleNode);
        }

        /// <summary>
        /// Adds a split node to this collection
        /// </summary>
        /// <param name="splitNode">Split node to add</param>
        public virtual void AddSplitNode(List<TreeBankNode> splitNode)
        {
            if (splitNode.Count <= 1)
                throw new Exception("Split nodes must contain more than one node");

            // make sure there aren't any duplicates in the list...set constructor will throw exception if there are
            new Set<TreeBankNode>(splitNode);

            // make sure nodes in split node are sorted
            splitNode.Sort(_positionComparer);

            // check if we alredy have the given split node
            foreach (List<TreeBankNode> existingSplitNode in _splitNodes)
            {
                // check if the current split node is identical to the given one
                bool equal = true;
                if (existingSplitNode.Count != splitNode.Count)
                    equal = false;
                else
                    foreach (TreeBankNode node in existingSplitNode)
                        if (!splitNode.Contains(node))
                        {
                            equal = false;
                            break;
                        }

                if (equal)
                    throw new Exception("Attempted to add a duplicate split node to collection");
            }

            // make sure all components are from the same sentence
            foreach (TreeBankNode node in splitNode)
                if (!node.InSameSentenceAs(splitNode[0], false))
                    throw new Exception("All components of a split node must come from the same sentence");

            // add to collection
            _splitNodes.Add(splitNode);
        }

        /// <summary>
        /// Gets all nodes in this collection
        /// </summary>
        /// <returns>All nodes in this collection</returns>
        public Set<TreeBankNode> GetNodes()
        {
            Set<TreeBankNode> nodes = new Set<TreeBankNode>();
            foreach (List<TreeBankNode> splitNode in _splitNodes)
                nodes.AddRange(splitNode);

            nodes.AddRange(_singleNodes);

            return nodes;
        }

        /// <summary>
        /// Gets nodes within this collection
        /// </summary>
        /// <param name="includeNullElementNodes">Whether or not to include null-element nodes</param>
        /// <param name="includeSplitNodes">Whether or not to include nodes from a split node</param>
        /// <param name="headSplitNodesOnly">If including split nodes, this specifies whether or not to only include the head node
        /// of the split node. The head node is defined as the node that is the semantic head of the LCA of all nodes
        /// in the split node.</param>
        /// <param name="includeSingleNodes">Whether or not to include single nodes</param>
        /// <param name="excludeSingleNodesWhenMultiple">Whether or not to exclude single nodes if there are more than one</param>
        /// <returns>Set of nodes</returns>
        public Set<TreeBankNode> GetNodes(bool includeNullElementNodes,
                                          bool includeSplitNodes,
                                          bool headSplitNodesOnly,
                                          bool includeSingleNodes,
                                          bool excludeSingleNodesWhenMultiple)
        {
            if (!includeSplitNodes && headSplitNodesOnly)
                throw new Exception("Inconsistent parameters. Cannot restrict split nodes to head nodes if we're not including split nodes to begin with.");

            if (!includeSingleNodes && excludeSingleNodesWhenMultiple)
                throw new Exception("Inconsistent parameters. Cannot restrict single nodes when node including single nodes to begin with.");

            Set<TreeBankNode> nodes = new Set<TreeBankNode>();

            // add split nodes
            if (includeSplitNodes)
                foreach (List<TreeBankNode> splitNode in _splitNodes)
                {
                    // track LCA of split node if requested
                    TreeBankNode lca = null;
                    foreach (TreeBankNode node in splitNode)
                        if (includeNullElementNodes || !node.IsNullElement)
                            // track the LCA of all split nodes if we're including only head split nodes
                            if (headSplitNodesOnly)
                            {
                                if (lca == null)
                                    lca = node;
                                else
                                    lca = lca.GetLowestCommonAncestor(node);
                            }
                            // otherwise, simply add the node to the set
                            else
                                nodes.Add(node);

                    // add the head of the LCA node if it's covered by the split node
                    if (lca != null)
                    {
                        TreeBankNode lcaHead = lca.SemanticHeadToken;
                        if (includeNullElementNodes || !lcaHead.IsNullElement)
                            foreach (TreeBankNode node in splitNode)
                                if (node.IsAncestorOf(lcaHead))
                                {
                                    nodes.Add(lcaHead);
                                    break;
                                }
                    }
                }

            // add single nodes
            if (includeSingleNodes)
                if (!excludeSingleNodesWhenMultiple || _singleNodes.Count == 1)
                    foreach (TreeBankNode node in _singleNodes)
                        if (includeNullElementNodes || !node.IsNullElement)
                            nodes.Add(node);

            return nodes;
        }

        /// <summary>
        /// Gets location label
        /// </summary>
        /// <returns>Location label</returns>
        public override string ToString()
        {
            return NodeLocations;
        }

        /// <summary>
        /// Gets whether or not the token span of any node (single or split) in this collection is covered exactly by the given node's token span
        /// </summary>
        /// <param name="node">Given node</param>
        /// <returns>True if any token span is covered, false otherwise.</returns>
        public bool AnyCoversSameTokenSpanAs(TreeBankNode node)
        {
            // check single nodes
            foreach (TreeBankNode singleNode in _singleNodes)
                if (singleNode.CoversSameTokensAs(node))
                    return true;

            // pre-fetch start and end token numbers
            int givenFirstTokenNumber = node.FirstToken.TokenNumber;
            int givenLastTokenNumber = node.LastToken.TokenNumber;

            // check each split node
            foreach (List<TreeBankNode> splitNode in _splitNodes)
                // split node must be in same sentence as given node
                if (splitNode[0].InSameSentenceAs(node, true))
                {
                    // get start and end token numbers of split node
                    int firstTokenNumber = splitNode[0].FirstToken.TokenNumber;
                    int lastTokenNumber = splitNode[splitNode.Count - 1].LastToken.TokenNumber;

                    // compare spans
                    if (firstTokenNumber == givenFirstTokenNumber && lastTokenNumber == givenLastTokenNumber)
                        return true;
                }

            return false;
        }

        /// <summary>
        /// Gets a copy of this collection
        /// </summary>
        /// <returns>Copy of this collection</returns>
        public abstract LabeledNodeCollection Copy();
    }
}
