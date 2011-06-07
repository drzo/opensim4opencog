using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Linq;

using LAIR.ResourceAPIs.NomBank.NomLex;
using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.MachineLearning;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Represents trees in the NomBank resource
    /// </summary>
    public class NomBankNode : TreeBankNode
    {
        #region static members
        /// <summary>
        /// Options used when bracketing a NomBank tree
        /// </summary>
        public enum BracketedOutputOptions
        {
            /// <summary>
            /// Don't include features on argument (Arg0-Arg9) nodes. Arguments rarely have features attached to them, but it does happen.
            /// </summary>
            IgnoreArgumentFeatures,

            /// <summary>
            /// Don't include hyphen indexes
            /// </summary>
            IgnoreHyphenIndexes,

            /// <summary>
            /// Don't include label probabilities
            /// </summary>
            IgnoreBracketProbabilities,

            /// <summary>
            /// Don't bracket support verbs. If a support verb is also an argument/modifier, it will be bracketed with node types
            /// other than support verb.
            /// </summary>
            IgnoreSupportVerbs,

            /// <summary>
            /// Whether or not to include predicate frames
            /// </summary>
            IncludePredicateFrame
        }

        /// <summary>
        /// Constructs a NomBankNode from a TreeBankNode
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode to construct a NomBankNode from</param>
        /// <param name="parent">Parent node</param>
        /// <returns>Constructed node</returns>
        private static TreeBankNode NomBankChildConstructor(TreeBankNode treeBankNode, TreeBankNode parent)
        {
            if (!(parent is NomBankNode))
                throw new Exception("Must have a NomBankNode");

            return new NomBankNode(treeBankNode, parent as NomBankNode);
        }
        #endregion

        private Set<NomBankNodeLabel> _labels;                               // stored on all nodes
        private NounInfo _information;                                       // stored on root node
        private List<NomBankLabeledNodeCollection> _labeledNodeCollections;  // stored on root node
        private List<NomLexEntry> _nomLexEntries;                            // stored on predicate node

        /// <summary>
        /// Gets the role set for this predicate tree. Only valid for root nodes.
        /// </summary>
        public RoleSet RoleSet
        {
            get { return _information.Frame.GetRoleSet(_information.RoleSetId); }
        }

        /// <summary>
        /// Gets the labels on this node
        /// </summary>
        public IEnumerable<NomBankNodeLabel> Labels
        {
            get { return _labels; }
        }

        /// <summary>
        /// Gets the number of labels on this node
        /// </summary>
        public int LabelCount
        {
            get { return _labels.Count; }
        }

        /// <summary>
        /// Gets all argument (Arg0-Arg9) labels on node
        /// </summary>
        public NomBankNodeLabel[] ArgumentLabels
        {
            get
            {
                NomBankNodeLabel[] labels = new NomBankNodeLabel[ArgumentLabelCount];
                int currIndex = 0;
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsArgument)
                        labels[currIndex++] = label;

                if (currIndex != labels.Length)
                    throw new Exception("Argument label count mismatch");

                return labels;
            }
        }

        /// <summary>
        /// Gets number of argument (Arg0-Arg9) labels on node
        /// </summary>
        public int ArgumentLabelCount
        {
            get
            {
                int count = 0;
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsArgument)
                        ++count;

                return count;
            }
        }

        /// <summary>
        /// Gets the argument indexes of this node. Most of the time, this will return a list with a single argument;
        /// however, some nodes have multiple argument labels (e.g., hyphenated nodes) and so will have multiple indexes.
        /// </summary>
        public Set<int> ArgumentIndexes
        {
            get
            {
                Set<int> indexes = new Set<int>();
                foreach (NomBankNodeLabel argLabel in ArgumentLabels)
                    indexes.Add(int.Parse(argLabel.Type.ToString().Substring(3)));

                return indexes;
            }
        }

        /// <summary>
        /// Gets all modifier (ArgM) labels on this node
        /// </summary>
        public NomBankNodeLabel[] ModifierLabels
        {
            get
            {
                NomBankNodeLabel[] labels = new NomBankNodeLabel[ModifierLabelCount];
                int currIndex = 0;
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsModifier)
                        labels[currIndex++] = label;

                if (currIndex != labels.Length)
                    throw new Exception("Modifier label count mismatch");

                return labels;
            }
        }

        /// <summary>
        /// Gets number of modifier (ArgM) labels on this node
        /// </summary>
        public int ModifierLabelCount
        {
            get
            {
                int count = 0;
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsModifier)
                        ++count;

                return count;
            }
        }

        /// <summary>
        /// Gets or sets the NomLex entries for this node (only valid for predicate nodes)
        /// </summary>
        public List<NomLexEntry> NomLexEntries
        {
            get
            {
                if (!IsPredicate)
                    throw new Exception("Only valid for predicate nodes");

                return _nomLexEntries;
            }
            set
            {
                if (!IsPredicate)
                    throw new Exception("Only valid for predicate nodes");

                _nomLexEntries = value;
            }
        }

        /// <summary>
        /// Gets the labeled node collections for the tree containing this node
        /// </summary>
        public List<NomBankLabeledNodeCollection> LabeledNodeCollections
        {
            get
            {
                if (IsRoot)
                    return _labeledNodeCollections;
                else
                    return (Root as NomBankNode).LabeledNodeCollections;
            }
        }

        /// <summary>
        /// Gets the labeled node locations as used in the NomBank props file for the tree containing this node
        /// </summary>
        public string LabeledNodeLocations
        {
            get
            {
                StringBuilder locations = new StringBuilder();

                bool prependSpace = false;
                foreach (NomBankLabeledNodeCollection nodeCollection in LabeledNodeCollections)
                {
                    locations.Append((prependSpace ? " " : "") + nodeCollection);
                    prependSpace = true;
                }

                return locations.ToString();
            }
        }

        /// <summary>
        /// Gets or sets the information for the tree containing this node
        /// </summary>
        public NounInfo Information
        {
            get
            {
                if (IsRoot)
                    return _information;
                else
                    return (Root as NomBankNode).Information;
            }
            set
            {
                if (IsRoot)
                    _information = value;
                else
                    (Root as NomBankNode).Information = value;
            }
        }

        /// <summary>
        /// Gets whether or not this node is an argument (Arg0-Arg9)
        /// </summary>
        public bool IsArgument
        {
            get
            {
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsArgument)
                        return true;

                return false;
            }
        }

        /// <summary>
        /// Gets whether or not this node is a modifier
        /// </summary>
        public bool IsModifier
        {
            get
            {
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsModifier)
                        return true;

                return false;
            }
        }

        /// <summary>
        /// Gets whether or not this node is a predicate node
        /// </summary>
        public bool IsPredicate
        {
            get
            {
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsPredicate)
                        return true;

                return false;
            }
        }

        /// <summary>
        /// Gets whether or not this node is a support verb
        /// </summary>
        public bool IsSupportVerb
        {
            get
            {
                foreach (NomBankNodeLabel label in _labels)
                    if (label.IsSupportVerb)
                        return true;

                return false;
            }
        }

        /// <summary>
        /// Gets whether or not this node has one or more labels associated with it
        /// </summary>
        public bool IsLabeled
        {
            get { return _labels.Count > 0; }
        }

        /// <summary>
        /// Gets predicate the predicate node. Only valid for root nodes.
        /// </summary>
        /// <returns></returns>
        public NomBankNode PredicateNode
        {
            get
            {
                if (!IsRoot)
                    throw new Exception("Current node is not a root node");

                NomBankNode predicateNode = null;

                // check each node collection for a predicate node
                foreach (NomBankLabeledNodeCollection nodeCollection in _labeledNodeCollections)
                    if (nodeCollection.Label.IsPredicate)
                    {
                        Set<TreeBankNode> predicateNodes = nodeCollection.GetNodes();
                        if (predicateNodes.Count != 1)
                            throw new Exception("Expected a single predicate node");

                        if (predicateNode == null)
                            predicateNode = predicateNodes.First() as NomBankNode;
                        else
                            throw new Exception("Multiple predicate nodes");
                    }

                return predicateNode;
            }
        }

        /// <summary>
        /// Constructor. WARNING:  this will accept an instance of any class derived from TreeBankNode (e.g., NomBankNode), but 
        /// will return a NomBankNode with only TreeBankNode members instantiated. Co-index IDs and referents are lost.
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode from which to construct this NomBankNode</param>
        public NomBankNode(TreeBankNode treeBankNode)
            : this(treeBankNode, null)
        {
            if (!treeBankNode.IsRoot)
                throw new Exception("Can only create NomBankNodes from root TreeBankNodes");
        }

        /// <summary>
        /// Constructor. WARNING:  this will accept an instance of any class derived from TreeBankNode (e.g., NomBankNode), but 
        /// will return a NomBankNode with only TreeBankNode members instantiated. Co-index IDs and referents are lost.
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode from which to construct this NomBankNode</param>
        /// <param name="parent">Parent of this NomBankNode node</param>
        protected NomBankNode(TreeBankNode treeBankNode, NomBankNode parent)
            : base(treeBankNode, parent, new TreeBankNodeConstructor(NomBankChildConstructor))
        {
            _labels = new Set<NomBankNodeLabel>();
            _labeledNodeCollections = new List<NomBankLabeledNodeCollection>();
        }

        /// <summary>
        /// Gets whether or not this predicate tree has a full role set (i.e., all core argument positions 
        /// are filled). Only valid for root nodes.
        /// </summary>
        /// <param name="considerNullElementNodes">Whether or not to consider null-element nodes when checking whether a role is filled</param>
        public bool HasFullRoleSet(bool considerNullElementNodes)
        {
            return GetUnfilledRoles(considerNullElementNodes).Count == 0;
        }

        /// <summary>
        /// Gets the set of roles not filled. Numbers in the set correspond to the argument positions in the frame file for the
        /// current nominal predicate. Only valid for root nodes.
        /// </summary>
        /// <param name="considerNullElementNodes">Whether or not to consider null-element nodes when checking whether a role is filled</param>
        public Set<int> GetUnfilledRoles(bool considerNullElementNodes)
        {
            if (!IsRoot)
                throw new Exception("Not valid for non-root nodes");

            // check for a node that fills each role in the set
            Set<int> unfilledRoles = new Set<int>();
            foreach (Role role in RoleSet)
            {
                bool filled = false;
                NomBankNodeLabel.NodeType argType = (NomBankNodeLabel.NodeType)Enum.Parse(typeof(NomBankNodeLabel.NodeType), "Arg" + role.Number);
                foreach (NomBankNode node in GetDescendants(argType))
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
        /// <returns>List of nodes of given type</returns>
        public List<NomBankNode> GetDescendants(NomBankNodeLabel.NodeType type)
        {
            List<NomBankNode> nodes = new List<NomBankNode>();

            foreach (NomBankNode n in Descendants)
                if (n.HasType(type))
                    nodes.Add(n);

            return nodes;
        }

        /// <summary>
        /// Gets descendant nodes by their feature
        /// </summary>
        /// <param name="feature">Feature of nodes to get</param>
        /// <returns>Nodes with given feature</returns>
        public List<NomBankNode> GetDescendants(NomBankNodeLabel.NodeFeature feature)
        {
            List<NomBankNode> nodes = new List<NomBankNode>();

            foreach (NomBankNode n in Descendants)
                if (n.HasFeature(feature))
                    nodes.Add(n);

            return nodes;
        }

        /// <summary>
        /// Gets descendant nodes by label
        /// </summary>
        /// <param name="label">Label of nodes to get</param>
        /// <returns>List of nodes matching label</returns>
        public List<NomBankNode> GetDescendants(NomBankNodeLabel label)
        {
            List<NomBankNode> nodes = new List<NomBankNode>();

            foreach (NomBankNode n in Descendants)
                if (n.Labels.Contains(label))
                    nodes.Add(n);

            return nodes;
        }

        /// <summary>
        /// Gets bracketed text for this node
        /// </summary>
        public string GetBracketedText()
        {
            return GetBracketedText(null);
        }

        /// <summary>
        /// Gets bracketed text for this node
        /// </summary>
        /// <param name="options">Bracketing options</param>
        /// <returns>Bracketed text</returns>
        public string GetBracketedText(params BracketedOutputOptions[] options)
        {
            // get set of options
            Set<BracketedOutputOptions> optionsSet = new Set<BracketedOutputOptions>();
            if (options != null)
                optionsSet.AddRange(options);

            // bracketed text builder
            StringBuilder text = new StringBuilder();

            // whether or not we have bracketed this node
            bool bracketed = false;

            // whether or not we should prepend a space before adding any more text
            bool prependSpace = false;

            // get labels to bracket for this node
            List<NomBankNodeLabel> labelsToBracket = new List<NomBankNodeLabel>();
            bool dontBracketSupportVerbs = optionsSet.Contains(BracketedOutputOptions.IgnoreSupportVerbs);
            foreach (NomBankNodeLabel label in _labels)
                if (dontBracketSupportVerbs && label.Type == NomBankNodeLabel.NodeType.Support)
                    continue;
                else
                    labelsToBracket.Add(label);

            // add bracketing with labels
            if (labelsToBracket.Count > 0)
            {
                text.Append("[");

                // multiple labels are separated by a slash
                bool prependSlash = false;
                foreach (NomBankNodeLabel label in labelsToBracket)
                {
                    if (prependSlash)
                        text.Append("/");

                    // get label string
                    text.Append(label.ToString(!optionsSet.Contains(BracketedOutputOptions.IgnoreArgumentFeatures),
                                              !optionsSet.Contains(BracketedOutputOptions.IgnoreHyphenIndexes)));

                    // add sense information to predicate labels
                    if (label.IsPredicate && optionsSet.Contains(BracketedOutputOptions.IncludePredicateFrame))
                        text.Append("." + Information.RoleSetId);

                    // add confidence score
                    if (!optionsSet.Contains(BracketedOutputOptions.IgnoreBracketProbabilities))
                        text.Append(" " + label.Confidence);

                    prependSlash = true;
                }

                bracketed = prependSpace = true;
            }

            // check for leaf
            if (IsLeaf)
                text.Append((prependSpace ? " " : "") + SurfaceText);
            // add bracketed text for each child
            else
            {
                IEnumerator<TreeBankNode> childEnum = Children;
                while (childEnum.MoveNext())
                {
                    NomBankNode child = childEnum.Current as NomBankNode;

                    if (!child.IsNullElement)
                    {
                        text.Append((prependSpace ? " " : "") + child.GetBracketedText(options));
                        prependSpace = true;
                    }
                }
            }

            // add closing bracket if we started one above
            if (bracketed)
                text.Append("]");

            return text.ToString();
        }

        /// <summary>
        /// Gets whether or not this node has a particular label
        /// </summary>
        /// <param name="label">Label to check for</param>
        /// <returns>True if label is present and false otherwise</returns>
        public bool HasLabel(NomBankNodeLabel label)
        {
            return _labels.Contains(label);
        }

        /// <summary>
        /// Gets whether or not this node is of the given type
        /// </summary>
        /// <param name="type">Type to check for</param>
        /// <returns>True if node is of given type and false otherwise</returns>
        public bool HasType(NomBankNodeLabel.NodeType type)
        {
            foreach (NomBankNodeLabel label in _labels)
                if (label.Type == type)
                    return true;

            return false;
        }

        /// <summary>
        /// Gets whether or not this node has a given feature
        /// </summary>
        /// <param name="feature">Feature to check for</param>
        /// <returns>True if node has feature, false otherwise</returns>
        public bool HasFeature(NomBankNodeLabel.NodeFeature feature)
        {
            foreach (NomBankNodeLabel label in _labels)
                if (label.Feature == feature)
                    return true;

            return false;
        }

        /// <summary>
        /// Gets labels matching given type
        /// </summary>
        /// <param name="type">Type of label to get</param>
        /// <returns>Set of labels</returns>
        public Set<NomBankNodeLabel> GetLabels(NomBankNodeLabel.NodeType type)
        {
            Set<NomBankNodeLabel> labels = new Set<NomBankNodeLabel>();
            foreach (NomBankNodeLabel label in _labels)
                if (label.Type == type)
                    labels.Add(label);

            return labels;
        }

        /// <summary>
        /// Gets label on the current node that matches the given label
        /// </summary>
        /// <param name="label">Label to match</param>
        /// <returns>Matching label</returns>
        public NomBankNodeLabel GetLabel(NomBankNodeLabel label)
        {
            // check each label
            foreach (NomBankNodeLabel current in _labels)
                if (current == label)
                    return current;

            throw new Exception("Given label not found");
        }

        /// <summary>
        /// Tries to get a label on this node that matches a given label
        /// </summary>
        /// <param name="label">Label to find</param>
        /// <param name="matchingLabel">Matching label, or null if none was found</param>
        /// <returns>Whether or not label was found</returns>
        public bool TryGetLabel(NomBankNodeLabel label, out NomBankNodeLabel matchingLabel)
        {
            matchingLabel = null;

            if (!HasLabel(label))
                return false;

            matchingLabel = GetLabel(label);

            return true;
        }

        /// <summary>
        /// Adds a label to this node
        /// </summary>
        /// <param name="label">Label to add</param>
        /// <param name="syncWithRootNodeCollection">Whether or not to add this node to the corresponding node collection on the root. The
        /// root collections are shortcut collections that allow quick searching for particular node types. Thus, the collections need to
        /// remain synchronized with the labels that are applied to the nodes. Passing true here will perform this synchronization. If you
        /// will do the synchronization on your own later, pass false.</param>
        public void AddLabel(NomBankNodeLabel label, bool syncWithRootNodeCollection)
        {
            _labels.Add(label);

            // add node to root's collection if needed
            if (syncWithRootNodeCollection)
                GetLabeledNodeCollection(label, true).AddSingleNode(this);
        }

        /// <summary>
        /// Removes a label from this node
        /// </summary>
        /// <param name="label">Label to remove</param>
        public void RemoveLabel(NomBankNodeLabel label)
        {
            _labels.Remove(label);

            // remove from root node collection if it is present
            NomBankLabeledNodeCollection nodes = GetLabeledNodeCollection(label, false);
            if (nodes != null && nodes.ContainsSingleNode(this))
                nodes.RemoveSingleNode(this);
        }

        /// <summary>
        /// Removes labels of a given type
        /// </summary>
        /// <param name="type">Type of label to remove</param>
        public void RemoveLabels(NomBankNodeLabel.NodeType type)
        {
            List<NomBankNodeLabel> toRemove = new List<NomBankNodeLabel>();
            foreach (NomBankNodeLabel label in _labels)
                if (label.Type == type)
                    toRemove.Add(label);

            foreach (NomBankNodeLabel label in toRemove)
                RemoveLabel(label);
        }

        /// <summary>
        /// Remove all but the best label
        /// </summary>
        public void KeepBestLabel()
        {
            // get best label confidence
            float bestConfidence = float.MinValue;
            foreach (NomBankNodeLabel label in _labels)
                if (label.Confidence > bestConfidence)
                    bestConfidence = label.Confidence;

            // get labels to remove
            List<NomBankNodeLabel> toRemove = new List<NomBankNodeLabel>();
            foreach (NomBankNodeLabel label in _labels)
                if (label.Confidence < bestConfidence)
                    toRemove.Add(label);

            // remove labels
            foreach (NomBankNodeLabel label in toRemove)
                RemoveLabel(label);

            if (_labels.Count > 1)
                throw new Exception("Multiple labels left on node");
        }

        /// <summary>
        /// Gets labeled node collection
        /// </summary>
        /// <param name="typeFeature">Type-feature combination of node collection to get</param>
        /// <param name="createIfMissing">Whether or not to create and return a new node collection if none exists</param>
        /// <param name="confidence">Confidence of newly created node collection</param>
        /// <returns>Labeled node collection</returns>
        public NomBankLabeledNodeCollection GetLabeledNodeCollection(string typeFeature, bool createIfMissing, float confidence)
        {
            return GetLabeledNodeCollection(new NomBankNodeLabel(typeFeature, confidence), createIfMissing);
        }

        /// <summary>
        /// Gets labeled node collection
        /// </summary>
        /// <param name="label">Label of node collection to get</param>
        /// <param name="createIfMissing">Whether or not to create and return a new node collection if none exists</param>
        /// <returns>Labeled node collection</returns>
        public NomBankLabeledNodeCollection GetLabeledNodeCollection(NomBankNodeLabel label, bool createIfMissing)
        {
            // look for an existing collection with the given label
            foreach (NomBankLabeledNodeCollection nodeCollection in LabeledNodeCollections)
                if (nodeCollection.Label == label)
                    return nodeCollection;

            if (!createIfMissing)
                return null;

            // return new collection
            NomBankLabeledNodeCollection newCollection = new NomBankLabeledNodeCollection(label);
            LabeledNodeCollections.Add(newCollection);

            return newCollection;
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
        public Set<NomBankNode> GetArgumentNodes(bool includeNullElementNodes,
                                                 bool includeSplitArguments,
                                                 bool headSplitArgumentNodesOnly,
                                                 bool includeSingleNodeArguments,
                                                 bool excludeSingleNodeArgumentsWhenMultiple)
        {
            if (!IsRoot)
                throw new Exception("Current node is not a root node");

            Set<NomBankNode> argNodes = new Set<NomBankNode>(false);

            // check each argument node list
            foreach (NomBankLabeledNodeCollection nodeCollection in _labeledNodeCollections)
                if (nodeCollection.Label.IsArgument)
                    argNodes.AddRange(nodeCollection.GetNodes(includeNullElementNodes, includeSplitArguments, headSplitArgumentNodesOnly, includeSingleNodeArguments, excludeSingleNodeArgumentsWhenMultiple).Cast<NomBankNode>());

            return argNodes;
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
        public Set<NomBankNode> GetModifierNodes(bool includeNullElementNodes,
                                                 bool includeSplitModifiers,
                                                 bool headSplitModifierNodesOnly,    
                                                 bool includeSingleNodeModifiers,
                                                 bool excludeSingleNodeModifiersWhenMultiple)        
        {
            if (!IsRoot)
                throw new Exception("Current node is not a root node");

            Set<NomBankNode> modifierNodes = new Set<NomBankNode>(false);

            // check each modifier node list
            foreach (NomBankLabeledNodeCollection nodeCollection in _labeledNodeCollections)
                if (nodeCollection.Label.IsModifier)
                    modifierNodes.AddRange(nodeCollection.GetNodes(includeNullElementNodes, includeSplitModifiers, headSplitModifierNodesOnly, includeSingleNodeModifiers, excludeSingleNodeModifiersWhenMultiple).Cast<NomBankNode>());

            return modifierNodes;
        }

        /// <summary>
        /// Gets support verb nodes. Only valid for root nodes.
        /// </summary>
        /// <param name="includeNullElementNodes">Whether or not to include null-element support verb nodes</param>
        /// <returns>Set of support verb nodes</returns>
        public Set<NomBankNode> GetSupportVerbNodes(bool includeNullElementNodes)
        {
            if (!IsRoot)
                throw new Exception("Current node is not a root node");

            Set<NomBankNode> supportVerbNodes = new Set<NomBankNode>(false);

            // check each support verb node list
            foreach (NomBankLabeledNodeCollection nodeCollection in _labeledNodeCollections)
                if (nodeCollection.Label.IsSupportVerb)
                    supportVerbNodes.AddRange(nodeCollection.GetNodes(includeNullElementNodes, true, false, true, false).Cast<NomBankNode>());  // support verbs are never actually split nor are they coreferential...just include everything

            return supportVerbNodes;
        }

        /// <summary>
        /// Marks argument nodes from the current node in the corresponding parse from a different TreeBank. This is used when
        /// transferring NomBank annotations to parse trees other than those distributed in the TreeBank (e.g., those produced
        /// by an automatic syntactic parser).
        /// </summary>
        /// <param name="treeBankEngine">Initialized TreeBank engine from which to pull the parse tree to mark NomBank arguments within</param>
        /// <returns>NomBank node, or null if all arguments couldn't be minimally transferred to the other parse tree. An argument
        /// is minimally transferred if the corresponding node in the other parse tree subsumes precisely the same text as the node
        /// in the current parse tree. Sometimes this is not possible due to parse errors.</returns>
        public NomBankNode MarkArgumentNodesIn(TreeBankEngine treeBankEngine)
        {
            // make sure we're marking arguments using a root node
            if (!IsRoot)
                throw new Exception("Must pass root node");

            // get mrg file in other tree bank
            string treeBankMrgFile = treeBankEngine.GetFullMrgPath(MrgFile);

            // need a NomBank root to mark arguments within
            NomBankNode nbRoot = new NomBankNode(treeBankEngine.GetParseTree(treeBankMrgFile, SentenceNumber));

            // make sure we got the right sentence
            if (nbRoot.SurfaceText != SurfaceText)
                throw new Exception("Failed to get same parse tree");

            // Add information to root. Ignore leaf number and argument information - we'll set them at the end.
            treeBankMrgFile = treeBankMrgFile.Substring(treeBankEngine.MrgPath.Length).Trim(Path.DirectorySeparatorChar);
            NounInfo currInfo = Information;
            nbRoot.Information = new NounInfo(currInfo.Noun, treeBankMrgFile, currInfo.SentenceNumber, -1, currInfo.RoleSetId, "");

            // transfer all argument node lists
            foreach (NomBankLabeledNodeCollection corefList in LabeledNodeCollections)
            {
                // new node list
                NomBankLabeledNodeCollection otherCorefList = new NomBankLabeledNodeCollection(corefList.Label.Copy());

                // get single nodes
                foreach (NomBankNode singleNode in corefList.SingleNodes)
                    if (!singleNode.IsNullElement)
                    {
                        // get argument node from other parse tree
                        NomBankNode otherArgNode = nbRoot.GetMinimallySubsumingNode(singleNode.FirstToken, singleNode.LastToken) as NomBankNode;
                        if (otherArgNode == null)
                            return null;

                        otherCorefList.AddSingleNode(otherArgNode);
                    }

                // get split arguments
                foreach (List<TreeBankNode> splitNode in corefList.SplitNodes)
                {
                    List<TreeBankNode> otherSplitArg = new List<TreeBankNode>();

                    // get each node in the split argument
                    foreach (NomBankNode node in splitNode)
                        if (!node.IsNullElement)
                        {
                            // get split node in other tree
                            NomBankNode otherSplitArgNode = nbRoot.GetMinimallySubsumingNode(node.FirstToken, node.LastToken) as NomBankNode;
                            if (otherSplitArgNode == null)
                                return null;

                            otherSplitArg.Add(otherSplitArgNode);
                        }

                    // if only one node of the split arg was non-null, at that node as a single
                    if (otherSplitArg.Count == 1)
                        otherCorefList.AddSingleNode(otherSplitArg.First());
                    // otherwise, add the split arg normally
                    else if (otherSplitArg.Count >= 2)
                        otherCorefList.AddSplitNode(otherSplitArg);
                }

                // make sure all hyphen indexes were applied
                if (otherCorefList.Label.HyphenIndexes.Count != otherCorefList.AppliedIndexes.Count)
                    throw new Exception("Not all hyphen indexes were applied");

                // add coref list if we found non-null nodes
                if (otherCorefList.SingleNodes.Count > 0 || otherCorefList.SplitNodes.Count > 0)
                    nbRoot.LabeledNodeCollections.Add(otherCorefList);
            }

            // return null if we didn't find any argument node lists with non-null nodes
            if (nbRoot.LabeledNodeCollections.Count == 0)
                return null;

            // set leaf number and argument locations in the information object
            nbRoot.Information.LeafNumber = nbRoot.PredicateNode.Leaves[0].LeafNumber;
            nbRoot.Information.LabeledNodeLocations = nbRoot.LabeledNodeLocations;

            return nbRoot;
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
        /// <param name="labelsToCheck">Node labels to check</param>
        /// <returns>True if argument labelings are the same, false otherwise</returns>
        public bool HasSameLabelingAs(NomBankNode otherTree, bool ignoreNullNodes, NomBankNodeLabel[] labelsToCheck)
        {
            if (!IsRoot)
                throw new Exception("Non-root node");

            if (!otherTree.IsRoot)
                throw new Exception("Non-root node");

            // check all node labels
            foreach (NomBankNodeLabel label in labelsToCheck)
            {
                List<NomBankNode> nodes1 = GetDescendants(label);
                if (ignoreNullNodes)
                    for (int i = 0; i < nodes1.Count; )
                        if (nodes1[i].IsNullElement)
                            nodes1.RemoveAt(i);
                        else
                            ++i;

                List<NomBankNode> nodes2 = otherTree.GetDescendants(label);
                if (ignoreNullNodes)
                    for (int i = 0; i < nodes2.Count; )
                        if (nodes2[i].IsNullElement)
                            nodes2.RemoveAt(i);
                        else
                            ++i;

                if (nodes1.Count != nodes2.Count)
                    return false;

                // check current nodes against the other nodes
                foreach (NomBankNode node1 in nodes1)
                {
                    bool matched = false;
                    foreach (NomBankNode node2 in nodes2)
                        if (node1.CoversSameTokensAs(node2))
                        {
                            matched = true;
                            break;
                        }

                    if (!matched)
                        return false;
                }

                // check the other nodes against the current ones
                foreach (NomBankNode node2 in nodes2)
                {
                    bool matched = false;
                    foreach (NomBankNode node1 in nodes1)
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
        /// Converts argument indexes for the current NomBank tree to their PropBank version
        /// </summary>
        /// <param name="argumentIndexes">Argument indexes to convert</param>
        public void ConvertArgumentIndexesToPropBank(Set<int> argumentIndexes)
        {
            // get current role set
            RoleSet roleSet = RoleSet;

            // check each index
            Set<int> indexesToAdd = null;
            Set<int> indexesToRemove = null;
            Role role;
            foreach (int index in argumentIndexes)
                if (roleSet.TryGet(index, out role) && role.Source != -1)
                {
                    // add the source index
                    if (indexesToAdd == null)
                        indexesToAdd = new Set<int>(false);  // might have duplicates

                    indexesToAdd.Add(role.Source);

                    // remove the current index
                    if (indexesToRemove == null)
                        indexesToRemove = new Set<int>();

                    indexesToRemove.Add(index);
                }

            // remove converted indexes first
            if (indexesToRemove != null)
                foreach (int indexToRemove in indexesToRemove)
                    argumentIndexes.Remove(indexToRemove);

            // add verb-sourced indexes
            if (indexesToAdd != null)
                foreach (int indexToAdd in indexesToAdd)
                    if (!argumentIndexes.Contains(indexToAdd))
                        argumentIndexes.Add(indexToAdd);
        }

        /// <summary>
        /// Converts argument index for the current NomBank tree to its PropBank version
        /// </summary>
        /// <param name="argumentIndex">Argument index to convert</param>
        /// <returns>Converted argument index</returns>
        public int ConvertArgumentIndexToPropBank(int argumentIndex)
        {
            Set<int> indexes = new Set<int>(new int[] { argumentIndex });
            ConvertArgumentIndexesToPropBank(indexes);
            return indexes.First();
        }

        /// <summary>
        /// Copies particular labels from the current node to corresponding nodes in another tree
        /// </summary>
        /// <param name="n">Tree to which to copy labels</param>
        /// <param name="labels">Labels to copy</param>
        public void CopyLabelsTo(NomBankNode n, params NomBankNodeLabel[] labels)
        {
            foreach (NomBankNodeLabel label in labels)
                foreach (NomBankNode fromNode in GetDescendants(label))
                {
                    int startToken = fromNode.FirstToken.TokenNumber;
                    int endToken = fromNode.LastToken.TokenNumber;
                    NomBankNode toNode = n.GetLowestCommonAncestorOfTokens(startToken, endToken) as NomBankNode;
                    toNode.AddLabel(label.Copy(), true);
                }
        }

        /// <summary>
        /// Tests the current node
        /// </summary>
        /// <param name="nomBankEngine">NomBankEngine to use in testing</param>
        public void Test(NomBankEngine nomBankEngine)
        {
            // check predicate node
            if (IsPredicate && !nomBankEngine.TokenIsMarkable(MrgFile, SentenceNumber, TokenNumber))
                throw new Exception("Invalid markable token entry");

            // check support verbs
            if (IsSupportVerb && !nomBankEngine.TokenIsSupportVerb(MrgFile, SentenceNumber, TokenNumber))
                throw new Exception("Support verb mismatch");

            // check morphological variant
            if (IsRoot && !nomBankEngine.ContainsMorphologicalVariantOf(Information.Noun))
                throw new Exception("Noun does not exist in section");

            // test children nodes
            IEnumerator<TreeBankNode> childEnum = Children;
            while (childEnum.MoveNext())
                (childEnum.Current as NomBankNode).Test(nomBankEngine);

            RoleSet roleSet = (Root as NomBankNode).RoleSet;
            foreach (Role role in roleSet)
                if (roleSet.Get(role.Number).Description != role.Description)
                    throw new Exception();
        }
    }
}
