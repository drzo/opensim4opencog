using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.IO;

using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Represents argument trees in the PropBank resource
    /// </summary>
    public class PropBankNode : TreeBankNode
    {
        #region static members
        /// <summary>
        /// Options used when bracketing a PropBank tree
        /// </summary>
        public enum BracketedOutputOptions
        {
            /// <summary>
            /// Don't include features on argument (Arg0-Arg5) nodes. Arguments rarely have features attached to them, but it does happen.
            /// </summary>
            IgnoreArgumentFeatures,

            /// <summary>
            /// Don't include label probabilities
            /// </summary>
            IgnoreBracketProbabilities,

            /// <summary>
            /// Whether or not to include predicate frames
            /// </summary>
            IncludePredicateFrame
        }

        /// <summary>
        /// Constructs a PropBankNode from a TreeBankNode
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode to construct a PropBankNode from</param>
        /// <param name="parent">Parent node</param>
        /// <returns>Constructed node</returns>
        private static TreeBankNode PropBankChildConstructor(TreeBankNode treeBankNode, TreeBankNode parent)
        {
            if (!(parent is PropBankNode))
                throw new Exception("Must have a PropBankNode");

            return new PropBankNode(treeBankNode, parent as PropBankNode);
        }
        #endregion

        private PropBankNodeLabel _label;
        private VerbInfo _information;
        List<PropBankLabeledNodeCollection> _labeledNodeCollections;

        /// <summary>
        /// Gets the role set for this predicate tree. Beware:  some entries in PropBank don't have a role set yet (indicated by XX
        /// in the prop.txt file. This will return null for such entries. Only valid for root nodes.
        /// </summary>
        public RoleSet RoleSet
        {
            get
            {
                if (!IsRoot)
                    throw new Exception("Non-root node");

                // make sure a role set was defined
                if (_information.RoleSetId == -1)
                    return null;

                return _information.VerbFrame.GetRoleSet(_information.RoleSetId);
            }
        }

        /// <summary>
        /// Gets the label associated with this node
        /// </summary>
        public PropBankNodeLabel Label
        {
            get { return _label; }
        }

        /// <summary>
        /// Gets the labeled node collections for the current tree
        /// </summary>
        public List<PropBankLabeledNodeCollection> LabeledNodeCollections
        {
            get
            {
                if (IsRoot)
                    return _labeledNodeCollections;
                else
                    return ((PropBankNode)Root).LabeledNodeCollections;
            }
        }

        /// <summary>
        /// Gets or sets the information for the tree containing this node
        /// </summary>
        public VerbInfo Information
        {
            get
            {
                if (IsRoot)
                    return _information;
                else
                    return ((PropBankNode)Root).Information;
            }
            set
            {
                if (IsRoot)
                    _information = value;
                else
                    ((PropBankNode)Root).Information = value;
            }
        }

        /// <summary>
        /// Gets the labeled node locations as used in the PropBank props.txt file
        /// </summary>
        public string LabeledNodeLocations
        {
            get
            {
                StringBuilder locations = new StringBuilder();

                bool prependSpace = false;
                foreach (PropBankLabeledNodeCollection nodeCollection in LabeledNodeCollections)
                {
                    locations.Append((prependSpace ? " " : "") + nodeCollection);
                    prependSpace = true;
                }

                return locations.ToString();
            }
        }

        /// <summary>
        /// Gets predicate nodes. Only valid for root nodes
        /// </summary>
        /// <returns></returns>
        public Set<PropBankNode> PredicateNodes
        {
            get
            {
                if (!IsRoot)
                    throw new Exception("Current node is not a root node");

                Set<PropBankNode> predicateNodes = new Set<PropBankNode>();

                foreach (PropBankLabeledNodeCollection nodeCollection in _labeledNodeCollections)
                    if (nodeCollection.Label.IsPredicate)
                        predicateNodes.AddRange(nodeCollection.GetNodes().Cast<PropBankNode>());

                return predicateNodes;
            }
        }

        /// <summary>
        /// Constructor. WARNING:  this will accept an instance of any class derived from TreeBankNode (e.g., PropBankNode), but 
        /// will return a PropBankNode with only TreeBankNode members instantiated.
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode from which to construct this PropBankNode</param>
        public PropBankNode(TreeBankNode treeBankNode)
            : this(treeBankNode, null)
        {
            if (!treeBankNode.IsRoot)
                throw new Exception("Can only create PropBankNodes from root TreeBankNodes");
        }

        /// <summary>
        /// Constructor. WARNING:  this will accept an instance of any class derived from TreeBankNode (e.g., PropBankNode), but 
        /// will return a PropBankNode with only TreeBankNode members instantiated.
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode from which to construct this PropBankNode</param>
        /// <param name="parent">Parent of this PropBank node</param>
        protected PropBankNode(TreeBankNode treeBankNode, PropBankNode parent)
            : base(treeBankNode, parent, new TreeBankNodeConstructor(PropBankChildConstructor))
        {
            _label = null;
            _information = null;
            _labeledNodeCollections = new List<PropBankLabeledNodeCollection>();
        }

        /// <summary>
        /// Sets the label on this node
        /// </summary>
        /// <param name="label">Label to use</param>
        /// <param name="syncWithRootNodeCollection">Whether or not to add this node to the corresponding node collection on the root. The
        /// root collections are shortcut collections that allow quick searching for particular node types. Thus, the collections need to
        /// remain synchronized with the labels that are applied to the nodes. Passing true here will perform this synchronization. If you
        /// will do the synchronization on your own later, pass false.</param>
        public void SetLabel(PropBankNodeLabel label, bool syncWithRootNodeCollection)
        {
            // first remove current node from previous node collection if it is present
            if (_label != null)
            {
                PropBankLabeledNodeCollection nodes = GetLabeledNodeCollection(label, false);
                if (nodes != null && nodes.ContainsSingleNode(this))
                    nodes.RemoveSingleNode(this);
            }

            _label = label;

            // add node to root's collection if needed
            if (syncWithRootNodeCollection)
                GetLabeledNodeCollection(_label, true).AddSingleNode(this);
        }

        /// <summary>
        /// Gets the set of roles not filled. Numbers in the set correspond to the argument positions in the frame file for the
        /// current verbal predicate. Only valid for root nodes.
        /// </summary>
        /// <param name="considerNullElementNodes">Whether or not to consider null-element nodes when checking whether a role is filled</param>
        public Set<int> GetUnfilledRoles(bool considerNullElementNodes)
        {
            if (!IsRoot)
                throw new Exception("Not valid for non-root nodes");

            Set<int> unfilledRoles = new Set<int>();

            /* check for a node that fills each role in the set. some annotations in propbank don't 
             * have a role set specified, so we can't determine which roles are unfilled.*/
            if (RoleSet != null)
                foreach (Role role in RoleSet)
                {
                    bool filled = false;
                    PropBankNodeLabel.NodeType argType = (PropBankNodeLabel.NodeType)Enum.Parse(typeof(PropBankNodeLabel.NodeType), "Arg" + role.Number);
                    foreach (PropBankNode node in GetDescendants(argType))
                        if (!node.IsNullElement || considerNullElementNodes)
                        {
                            filled = true;
                            break;
                        }

                    if (!filled)
                        unfilledRoles.Add(role.Number);
                }

            return unfilledRoles;
        }

        /// <summary>
        /// Gets descendant nodes by their type
        /// </summary>
        /// <param name="type">Type of node to get</param>
        /// <returns>Descendant nodes</returns>
        public List<PropBankNode> GetDescendants(PropBankNodeLabel.NodeType type)
        {
            List<PropBankNode> nodes = new List<PropBankNode>();

            foreach (PropBankNode n in Descendants)
                if (n.Label != null && n.Label.Type == type)
                    nodes.Add(n);

            return nodes;
        }

        /// <summary>
        /// Gets descendant nodes by their feature
        /// </summary>
        /// <param name="feature">Feature of nodes to get</param>
        /// <returns>Nodes with given feature</returns>
        public List<PropBankNode> GetDescendants(PropBankNodeLabel.NodeFeature feature)
        {
            List<PropBankNode> nodes = new List<PropBankNode>();

            foreach (PropBankNode n in Descendants)
                if (n.Label.Feature == feature)
                    nodes.Add(n);

            return nodes;
        }

        /// <summary>
        /// Gets bracketed text for this node
        /// </summary>
        /// <param name="options">Bracketing options</param>
        public string GetBracketedText(params BracketedOutputOptions[] options)
        {
            // get set of options
            Set<BracketedOutputOptions> optionsSet = new Set<BracketedOutputOptions>();
            if (options != null)
                optionsSet.AddRange(options);

            StringBuilder text = new StringBuilder();

            bool bracketed = false;
            bool prependSpace = false;

            // add label if we have one
            if (_label != null)
            {
                text.Append("[" + _label.ToString(true));

                // append predicate sense if needed
                if (_label.IsPredicate && optionsSet.Contains(BracketedOutputOptions.IncludePredicateFrame))
                    text.Append(Information.RoleSetId);

                // add label probability if needed
                if (!optionsSet.Contains(BracketedOutputOptions.IgnoreBracketProbabilities))
                    text.Append(" " + _label.Confidence);

                bracketed = prependSpace = true;
            }

            // check for leaf
            if (IsLeaf)
                text.Append((prependSpace ? " " : "") + SurfaceText);
            // add children
            else
            {
                IEnumerator<TreeBankNode> children = Children;
                while (children.MoveNext())
                {
                    PropBankNode child = children.Current as PropBankNode;
                    if (!child.IsNullElement)
                    {
                        text.Append((prependSpace ? " " : "") + child.GetBracketedText(options));
                        prependSpace = true;
                    }
                }
            }

            string bracketedText = text.ToString() + (bracketed ? "]" : "");

            return bracketedText;
        }

        /// <summary>
        /// Gets argument nodes. Only valid for root nodes.
        /// </summary>
        /// <param name="includeNullElementNodes">Whether or not to include null-element argument nodes</param>
        /// <param name="includeSplitArguments">Whether or not to include split arguments</param>
        /// <param name="headSplitArgumentNodesOnly">If including split nodes, this specifies whether or not to only include the head node
        /// of the split argument. The head node is defined as the node containing the semantic head of the LCA of all nodes
        /// in the split argument.</param>
        /// <param name="includeSingleNodeArguments">Whether or not to include single nodes</param>
        /// <param name="excludeSingleNodeArgumentsWhenMultiple">Whether or not to exclude single nodes if there are more than one</param>
        /// <returns>List of argument nodes</returns>
        public Set<PropBankNode> GetArgumentNodes(bool includeNullElementNodes,
                                                  bool includeSplitArguments,
                                                  bool headSplitArgumentNodesOnly,
                                                  bool includeSingleNodeArguments,
                                                  bool excludeSingleNodeArgumentsWhenMultiple)
        {
            if (!IsRoot)
                throw new Exception("Current node is not a root node");

            Set<PropBankNode> argumentNodes = new Set<PropBankNode>(false);

            // check each argument node list
            foreach (PropBankLabeledNodeCollection nodeCollection in _labeledNodeCollections)
                if (nodeCollection.Label.IsArgument)
                    argumentNodes.AddRange(nodeCollection.GetNodes(includeNullElementNodes, includeSplitArguments, headSplitArgumentNodesOnly, includeSingleNodeArguments, excludeSingleNodeArgumentsWhenMultiple).Cast<PropBankNode>());

            return argumentNodes;
        }

        /// <summary>
        /// Gets modifier nodes. Only valid for root nodes.
        /// </summary>
        /// <param name="includeNullElementNodes">Whether or not to include null-element modifier nodes</param>
        /// <param name="includeSplitModifiers">Whether or not to include split modifiers</param>
        /// <param name="headSplitModifierNodesOnly">If including split nodes, this specifies whether or not to only include the head node
        /// of the split modifier. The head node is defined as the node containing the semantic head of the LCA of all nodes
        /// in the split modifier.</param>
        /// <param name="includeSingleNodeModifiers">Whether or not to include single nodes</param>
        /// <param name="excludeSingleNodeModifiersWhenMultiple">Whether or not to exclude single nodes if there are more than one</param>
        /// <returns>List of modifier nodes</returns>
        public Set<PropBankNode> GetModifierNodes(bool includeNullElementNodes,
                                                  bool includeSplitModifiers,
                                                  bool headSplitModifierNodesOnly,
                                                  bool includeSingleNodeModifiers,
                                                  bool excludeSingleNodeModifiersWhenMultiple)
        {
            if (!IsRoot)
                throw new Exception("Current node is not a root node");

            Set<PropBankNode> modifierNodes = new Set<PropBankNode>(false);

            // check each modifier node list
            foreach (PropBankLabeledNodeCollection nodeCollection in _labeledNodeCollections)
                if (nodeCollection.Label.IsModifier)
                    modifierNodes.AddRange(nodeCollection.GetNodes(includeNullElementNodes, includeSplitModifiers, headSplitModifierNodesOnly, includeSingleNodeModifiers, excludeSingleNodeModifiersWhenMultiple).Cast<PropBankNode>());

            return modifierNodes;
        }

        /// <summary>
        /// Compares arguments
        /// </summary>
        public class ArgumentComparer : IComparer<PropBankNode>
        {
            /// <summary>
            /// Compares two nodes based on their argument types
            /// </summary>
            /// <param name="x">First node</param>
            /// <param name="y">Second node</param>
            /// <returns>1 if x's argument types comes after y's argument type, 0 if they are the same, and -1 otherwise</returns>
            public int Compare(PropBankNode x, PropBankNode y)
            {
                return x.Label.Type.CompareTo(y.Label.Type);
            }
        }

        /// <summary>
        /// Marks argument nodes from the current node in the corresponding parse from a different TreeBank. This is used when
        /// transferring PropBank annotations to parse trees other than those distributed in the TreeBank (e.g., those produced
        /// by an automatic syntactic parser).
        /// </summary>
        /// <param name="treeBankEngine">Initialized TreeBank engine from which to pull the parse tree to mark PropBank arguments within</param>
        /// <returns>PropBank node, or null if all arguments couldn't be minimally transferred to the other parse tree. An argument
        /// is minimally transferred if the corresponding node in the other parse tree subsumes precisely the same text as the node in the
        /// current parse tree. Sometimes this is not possible due to parse errors.</returns>
        public PropBankNode MarkArgumentNodesIn(TreeBankEngine treeBankEngine)
        {
            if (!IsRoot)
                throw new Exception("Attempted to transform non-root node");

            // get mrg file in other tree bank
            string treeBankMrgFile = treeBankEngine.GetFullMrgPath(MrgFile.Substring(MrgFile.LastIndexOf(Path.DirectorySeparatorChar) + 1));

            // need a PropBank root to mark arguments within
            PropBankNode pbRoot = new PropBankNode(treeBankEngine.GetParseTree(treeBankMrgFile, SentenceNumber));

            // make sure we got the right sentence
            if (pbRoot.SurfaceText != SurfaceText)
                throw new Exception("Failed to convert root to Charniak-parsed version");

            // Add information to root. Ignore leaf number and argument info for now - we'll set them at the end.
            treeBankMrgFile = treeBankMrgFile.Substring(treeBankEngine.MrgPath.Length);
            VerbInfo pbInfo = Information;
            pbRoot.Information = new VerbInfo(pbInfo.Verb, treeBankMrgFile, pbInfo.SentenceNumber,
                                              -1, pbInfo.Tagger, pbInfo.RoleSetId,
                                              pbInfo.VForm, pbInfo.VTense, pbInfo.VAspect,
                                              pbInfo.VPerson, pbInfo.VVoice, "");

            // transfer all argument node lists
            foreach (PropBankLabeledNodeCollection nodeCollection in LabeledNodeCollections)
            {
                // new node collection
                PropBankLabeledNodeCollection otherNodeCollection = new PropBankLabeledNodeCollection(new PropBankNodeLabel(nodeCollection.Label.Type, nodeCollection.Label.Feature, nodeCollection.Label.Confidence));

                // get single nodes
                foreach (PropBankNode singleNode in nodeCollection.SingleNodes)
                    if (!singleNode.IsNullElement)
                    {
                        // get argument node from other parse tree
                        PropBankNode otherArgNode = (PropBankNode)pbRoot.GetMinimallySubsumingNode(singleNode.FirstToken, singleNode.LastToken);
                        if (otherArgNode == null)
                            return null;

                        otherNodeCollection.AddSingleNode(otherArgNode);
                    }

                // get split arguments
                foreach (List<TreeBankNode> splitArg in nodeCollection.SplitNodes)
                {
                    List<TreeBankNode> otherSplitArg = new List<TreeBankNode>();

                    // get each node in the split argument
                    foreach (PropBankNode splitArgNode in splitArg)
                        if (!splitArgNode.IsNullElement)
                        {
                            // get split node in other tree
                            PropBankNode otherSplitArgNode = (PropBankNode)pbRoot.GetMinimallySubsumingNode(splitArgNode.FirstToken, splitArgNode.LastToken);
                            if (otherSplitArgNode == null)
                                return null;

                            otherSplitArg.Add(otherSplitArgNode);
                        }

                    // if only one node of the split arg was non-null, at that node as a single
                    if (otherSplitArg.Count == 1)
                        otherNodeCollection.AddSingleNode(otherSplitArg.First());
                    // otherwise, add the split arg normally
                    else if (otherSplitArg.Count >= 2)
                        otherNodeCollection.AddSplitNode(otherSplitArg);
                }

                // add coref list if we found non-null nodes
                if (otherNodeCollection.SingleNodes.Count > 0 || otherNodeCollection.SplitNodes.Count > 0)
                    pbRoot.LabeledNodeCollections.Add(otherNodeCollection);
            }

            // return null if we didn't find any argument node lists with non-null nodes
            if (pbRoot.LabeledNodeCollections.Count == 0)
                return null;

            // set leaf number and argument information
            pbRoot.Information.LeafNumber = pbRoot.PredicateNodes.First().Leaves[0].LeafNumber;
            pbRoot.Information.LabeledNodeLocations = pbRoot.LabeledNodeLocations;

            return pbRoot;
        }

        /// <summary>
        /// Gets argument-bracketed text for this node
        /// </summary>
        /// <returns>Argument-bracketed text for this node</returns>
        public override string ToString()
        {
            return GetBracketedText();
        }

        /// <summary>
        /// Gets whether or not the current tree has the same argument labeling as another tree
        /// </summary>
        /// <param name="otherTree">Tree to compare the current one to</param>
        /// <param name="ignoreNullNodes">Whether or not to ignore null nodes</param>
        /// <param name="argumentTypesToCheck">Argument types to check</param>
        /// <returns>True if argument labelings are the same, false otherwise</returns>
        public bool HasSameLabelingAs(PropBankNode otherTree, bool ignoreNullNodes, params PropBankNodeLabel.NodeType[] argumentTypesToCheck)
        {
            if (!IsRoot)
                throw new Exception("Non-root node");

            if (!otherTree.IsRoot)
                throw new Exception("Non-root node");

            // check all node types
            foreach (PropBankNodeLabel.NodeType nodeType in argumentTypesToCheck)
            {
                // get list of nodes in the current tree of the current type
                List<PropBankNode> nodes1 = GetDescendants(nodeType);
                if (ignoreNullNodes)
                    for (int i = 0; i < nodes1.Count; )
                        if (nodes1[i].IsNullElement)
                            nodes1.RemoveAt(i);
                        else
                            ++i;

                // get list of nodes in the current tree of the current type
                List<PropBankNode> nodes2 = otherTree.GetDescendants(nodeType);
                if (ignoreNullNodes)
                    for (int i = 0; i < nodes2.Count; )
                        if (nodes2[i].IsNullElement)
                            nodes2.RemoveAt(i);
                        else
                            ++i;

                if (nodes1.Count != nodes2.Count)
                    return false;

                // check current nodes against the other nodes
                foreach (PropBankNode node1 in nodes1)
                {
                    bool matched = false;
                    foreach (PropBankNode node2 in nodes2)
                        if (node1.CoversSameTokensAs(node2))
                        {
                            matched = true;
                            break;
                        }

                    if (!matched)
                        return false;
                }

                // check the other nodes against the current ones
                foreach (PropBankNode node2 in nodes2)
                {
                    bool matched = false;
                    foreach (PropBankNode node1 in nodes1)
                        if (node2.CoversSameTokensAs(node1))
                        {
                            matched = true;
                            break;
                        }

                    if (!matched)
                        return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Tests this node
        /// </summary>
        public override void Test()
        {
            if (IsRoot)
            {
                RoleSet roleSet = RoleSet;
                if (roleSet != null)
                    foreach (Role role in roleSet)
                        if (roleSet.Get(role.Number).Description != role.Description)
                            throw new Exception();
            }

            IEnumerator<TreeBankNode> children = Children;
            while (children.MoveNext())
                children.Current.Test();
        }

        /// <summary>
        /// Gets labeled node collection
        /// </summary>
        /// <param name="typeFeature">Type-feature combination of node collection to get</param>
        /// <param name="createIfMissing">Whether or not to create and return a new node collection if none exists</param>
        /// <param name="confidence">Confidence of newly created node collection</param>
        /// <returns>Labeled node collection</returns>
        public PropBankLabeledNodeCollection GetLabeledNodeCollection(string typeFeature, bool createIfMissing, float confidence)
        {
            return GetLabeledNodeCollection(new PropBankNodeLabel(typeFeature, confidence), createIfMissing);
        }

        /// <summary>
        /// Gets labeled node collection
        /// </summary>
        /// <param name="label">Label of node collection to get</param>
        /// <param name="createIfMissing">Whether or not to create and return a new node collection if none exists</param>
        /// <returns>Labeled node collection</returns>
        public PropBankLabeledNodeCollection GetLabeledNodeCollection(PropBankNodeLabel label, bool createIfMissing)
        {
            // look for an existing collection with the given label
            foreach (PropBankLabeledNodeCollection nodeCollection in LabeledNodeCollections)
                if (nodeCollection.Label == label)
                    return nodeCollection;

            if (!createIfMissing)
                return null;

            // return new collection
            PropBankLabeledNodeCollection newCollection = new PropBankLabeledNodeCollection(label);
            LabeledNodeCollections.Add(newCollection);

            return newCollection;
        }
    }
}
