using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;
using System.Collections;

using LAIR.MachineLearning;
using LAIR.Collections.Generic;
using LAIR.GraphViz;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Represents a node in a Penn TreeBank parse tree
    /// </summary>
    public class TreeBankNode : ClassifiableEntity, IGraphVizible
    {
        #region static members
        /// <summary>
        /// Siblings, either left or right
        /// </summary>
        private enum SiblingSide
        {
            /// <summary>
            /// Left siblings
            /// </summary>
            Left,

            /// <summary>
            /// Right siblings
            /// </summary>
            Right
        }

        /// <summary>
        /// Delegate for functions that construct TreeBankNodes from other TreeBankNodes
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode from which to construct another TreeBankNode</param>
        /// <param name="parent">Parent of constructed node</param>
        /// <returns>Resulting TreeBankNode</returns>
        public delegate TreeBankNode TreeBankNodeConstructor(TreeBankNode treeBankNode, TreeBankNode parent);

        private static Regex _whitespaceRE;
        private static Set<string> _anaphoricWords;
        private static Set<string> _nonAnaphoricWords;
        private static Set<TreeBankEngine.SyntacticCategory> _anaphoricCategories;
        private static Dictionary<string, string> _untokenizationReplacements;
        private static Dictionary<TreeBankEngine.SyntacticCategory, SiblingSide> _semanticHeadCategoryReplacement;
        private static Set<string> _beVerbs;

        /// <summary>
        /// Static constructor
        /// </summary>
        static TreeBankNode()
        {
            _whitespaceRE = new Regex(@"\s");

            // category replacements for finding the semantic head (as opposed to the syntactic head)
            _semanticHeadCategoryReplacement = new Dictionary<TreeBankEngine.SyntacticCategory, SiblingSide>();
            _semanticHeadCategoryReplacement.Add(TreeBankEngine.SyntacticCategory.Preposition, SiblingSide.Right);
            _semanticHeadCategoryReplacement.Add(TreeBankEngine.SyntacticCategory.To, SiblingSide.Right);
            _semanticHeadCategoryReplacement.Add(TreeBankEngine.SyntacticCategory.Possessive, SiblingSide.Left);
            _semanticHeadCategoryReplacement.Add(TreeBankEngine.SyntacticCategory.Determiner, SiblingSide.Right);
            _semanticHeadCategoryReplacement.Add(TreeBankEngine.SyntacticCategory.PreDeterminer, SiblingSide.Right);

            // anaphoric words
            _anaphoricWords = new Set<string>(new string[]{
                "all", "another", "any", "both", "each", "each other", "either", "everybody", "everyone", "everything", "few", "he",
                "her", "hers", "herself", "him", "himself", "his", "i", "it", "its", "itself", "little", "many", "me", "mine", "more",
                "most", "much", "my", "myself", "neither", "none", "nothing", "one", "one another", "other", "others", "our", "ours", 
                "ourselves", "several", "she", "some", "that", "their", "theirs", "them", "themselves", "there", "these", "they", "this", 
                "those", "us", "we", "what", "which", "whichever", "who", "whoever", "whom", "whomever", "whose", "you", "your", "yours", 
                "yourself", "yourselves"});

            // words that are subsumed under the anaphoric categories (below) but should never be treated as anaphoric
            _nonAnaphoricWords = new Set<string>(new string[]{
                "", "a", "the", "half"});

            // categories of potentially anaphoric words
            _anaphoricCategories = new Set<TreeBankEngine.SyntacticCategory>(new TreeBankEngine.SyntacticCategory[]{
                TreeBankEngine.SyntacticCategory.PersonalPronoun, TreeBankEngine.SyntacticCategory.PossessivePronoun,
                TreeBankEngine.SyntacticCategory.Determiner, TreeBankEngine.SyntacticCategory.PreDeterminer,
                TreeBankEngine.SyntacticCategory.WhPronoun, TreeBankEngine.SyntacticCategory.WhPronounPossessive,
                TreeBankEngine.SyntacticCategory.WhDeterminer, TreeBankEngine.SyntacticCategory.WhNounPhrase});

            // add untokenization replacement strings...this is an approximation that attempts to recover original tokenization
            _untokenizationReplacements = new Dictionary<string, string>();
            _untokenizationReplacements.Add(" !", "!");
            _untokenizationReplacements.Add("$ ", "$");
            _untokenizationReplacements.Add(" %", "%");
            _untokenizationReplacements.Add(" *", "*");
            _untokenizationReplacements.Add("( ", "(");
            _untokenizationReplacements.Add(" )", ")");
            _untokenizationReplacements.Add("[ ", "[");
            _untokenizationReplacements.Add(" ]", "]");
            _untokenizationReplacements.Add("{ ", "{");
            _untokenizationReplacements.Add(" }", "}");
            _untokenizationReplacements.Add(@" \ ", @"\");
            _untokenizationReplacements.Add(" :", ":");
            _untokenizationReplacements.Add(" ;", ";");
            _untokenizationReplacements.Add(" '", "'");  // takes care of the end double-quote '' and the plural possessive (e.g., Sams ')
            _untokenizationReplacements.Add("`` ", "``");
            _untokenizationReplacements.Add(" ,", ",");
            _untokenizationReplacements.Add(" .", ".");
            _untokenizationReplacements.Add(" / ", "/");

            // contractions
            _untokenizationReplacements.Add(" 's", "'s");
            _untokenizationReplacements.Add(" n't", "n't");

            // braces
            _untokenizationReplacements.Add("-LRB-", "(");
            _untokenizationReplacements.Add("-RRB-", ")");
            _untokenizationReplacements.Add("-LSB-", "[");
            _untokenizationReplacements.Add("-RSB-", "]");
            _untokenizationReplacements.Add("-LCB-", "{");
            _untokenizationReplacements.Add("-RCB-", "}");

            // be verbs
            _beVerbs = new Set<string>(new string[] { "am", "are", "is", "was", "were", "been", "being", "become", "became" });
        }

        /// <summary>
        /// Concatenates the text subsumed by a list of nodes
        /// </summary>
        /// <param name="nodes">Nodes to concatenate</param>
        /// <returns>Text</returns>
        public static string Concatenate(IEnumerable<TreeBankNode> nodes)
        {
            StringBuilder text = new StringBuilder();
            bool prependSpace = false;
            foreach (TreeBankNode node in nodes)
            {
                text.Append((prependSpace ? " " : "") + node.SurfaceText);
                prependSpace = true;
            }

            return text.ToString();
        }

        /// <summary>
        /// Gets the lowest common ancestor of a list of nodes
        /// </summary>
        /// <param name="nodes">Nodes to get LCA for</param>
        /// <returns>LCA of given nodes</returns>
        public static TreeBankNode GetLowestCommonAncestor(IEnumerable<TreeBankNode> nodes)
        {
            TreeBankNode lca = null;
            foreach (TreeBankNode node in nodes)
                if (lca == null)
                    lca = node;
                else
                    lca = lca.GetLowestCommonAncestor(node);

            return lca;
        }

        /// <summary>
        /// Gets lists of contiguous nodes from a collection of nodes. Nodes in each list are sorted by linear position.
        /// </summary>
        /// <param name="nodes">Nodes to process</param>
        /// <returns>Lists of contiguous nodes</returns>
        public static List<List<TreeBankNode>> GetContiguousNodes(IEnumerable<TreeBankNode> nodes)
        {
            // copy list of nodes so we don't affect caller
            List<TreeBankNode> nodesCopy = new List<TreeBankNode>(nodes);

            List<List<TreeBankNode>> contiguousNodeLists = new List<List<TreeBankNode>>();
            NodePositionComparer positionComparer = new NodePositionComparer(false);

            // while nodes remain (we might have multiple contiguous lists)
            while (nodesCopy.Count > 0)
            {
                // grab the first node
                List<TreeBankNode> contiguousNodes = new List<TreeBankNode>();
                contiguousNodes.Add(nodesCopy[0]);
                nodesCopy.RemoveAt(0);

                // transitively add all adjacent nodes until we cannot add more
                while (true)
                {
                    // find nodes adjacent to the current set of contiguous nodes
                    Set<TreeBankNode> adjacentNodes = new Set<TreeBankNode>();
                    foreach (TreeBankNode node in nodesCopy)
                        if (contiguousNodes.Contains(node))
                            throw new Exception("Duplicate node in list");
                        // check if node is adjacent to (on either side of) any nodes that we already have
                        else if (contiguousNodes.Where(contiguousNode => node.InSameSentenceAs(contiguousNode, true) &&
                                                                         System.Math.Abs(node.TokenDistanceFrom(contiguousNode)) == 1).Count() > 0)
                            adjacentNodes.Add(node);

                    // if we didn't find any, the current list of adjacent nodes is maximal
                    if (adjacentNodes.Count == 0)
                        break;

                    // add adjacent nodes
                    foreach (TreeBankNode adjacentNode in adjacentNodes)
                    {
                        contiguousNodes.Add(adjacentNode);
                        nodesCopy.Remove(adjacentNode);
                    }
                }

                // sort contiguous nodes and add to list
                contiguousNodes.Sort(positionComparer);
                contiguousNodeLists.Add(contiguousNodes);
            }

            return contiguousNodeLists;
        }

        /// <summary>
        /// Checks whether two lists of nodes cover the same token span
        /// </summary>
        /// <param name="nodes1">First list of nodes</param>
        /// <param name="nodes2">Second list of nodes</param>
        /// <returns>True if lists cover the same token span and false otherwise</returns>
        public static bool NodesCoverSameTokens(List<TreeBankNode> nodes1, List<TreeBankNode> nodes2)
        {
            // make sure nodes come from the same sentence
            for (int i = 1; i < nodes1.Count; ++i)
                if (nodes1[1].SentenceNumber != nodes1[i - 1].SentenceNumber)
                    throw new Exception("Nodes must come from same sentence");

            // make sure nodes come from the same sentence and spans are contiguous
            for (int i = 1; i < nodes2.Count; ++i)
                if (nodes2[1].SentenceNumber != nodes2[i - 1].SentenceNumber)
                    throw new Exception("Nodes must come from same sentence");

            // make sure the proposed spans come from the same sentence (nodes in the same list come from the same sentence)
            if (nodes1[0].SentenceNumber != nodes2[0].SentenceNumber)
                return false;
            
            // get tokens
            Set<int> tokens1 = new Set<int>();
            foreach (TreeBankNode node in nodes1)
                tokens1.AddRange(node.Tokens.Select(t => t.TokenNumber));

            Set<int> tokens2 = new Set<int>();
            foreach (TreeBankNode node in nodes2)
                tokens2.AddRange(node.Tokens.Select(t => t.TokenNumber));

            // compare
            return tokens1.Equals(tokens2);
        }
        #endregion

        private TreeBankNode _root;
        private TreeBankNode _parent;
        private TreeBankNodeList _children;
        private string _surfaceText;
        private List<TreeBankEngine.GrammaticalFunction> _functions;
        private TreeBankEngine.SyntacticCategory _category;
        private List<TreeBankEngine.SyntacticCategory> _ambiguousWithCategories;
        private int _coIndexID;
        private TreeBankNode _coIndexReferent;
        private string _mrgFile;
        private int _sentenceNumber;
        private TreeBankNode _head;
        private int _leafNumber;
        private int _tokenNumber;
        private int _tokenStartCharacter;
        private TreeBankNodeList _leafNodes;
        private List<int> _nullLeafNumbers;
        private int _firstLeafNumber;
        private int _lastLeafNumber;
        private int _totalNodesInTree;

        /// <summary>
        /// Gets the subject head token node of this parse. This property can be called from any node, but it assumes that
        /// the head token of the current node is something about which can reasonably be thought to take a subject, i.e.,
        /// verbs (main, auxiliary, etc.).
        /// </summary>
        public TreeBankNode SubjectHeadToken
        {
            get
            {
                // get head
                TreeBankNode headToken = HeadToken;

                // no subject if the head is first token
                if (headToken.TokenNumber == 0)
                    return null;

                // get head of tokens that precede the head we just got
                TreeBankNode subjectHeadToken = _root.GetLowestCommonAncestorOfTokens(0, headToken.TokenNumber - 1).HeadToken;
                if (subjectHeadToken == headToken)
                {
                    subjectHeadToken = null;

                    // starting before the first head of the root node, search from right-to-left for the first NP and use its head token
                    for (int i = Head.ChildIndex - 1; i >= 0 && subjectHeadToken == null; --i)
                        if (_children[i].Category == TreeBankEngine.SyntacticCategory.NounPhrase)
                            subjectHeadToken = _children[i].HeadToken;
                }

                return subjectHeadToken;
            }
        }

        /// <summary>
        /// Gets an enumerator over the current node's children nodes
        /// </summary>
        public IEnumerator<TreeBankNode> Children
        {
            get { return _children.GetEnumerator(); }
        }

        /// <summary>
        /// Gets whether or not the current tree is drawn from a TreeBank. If false, the tree was constructed in some other way.
        /// </summary>
        public bool IsTreeBanked
        {
            get
            {
                if (IsRoot)
                    return _mrgFile != null && _sentenceNumber >= 0;
                else
                    return _root.IsTreeBanked;
            }
        }

        /// <summary>
        /// Gets the total number of nodes in the tree
        /// </summary>
        public int TotalNodesInTree
        {
            get { return _totalNodesInTree; }
        }

        /// <summary>
        /// Gets or sets the number of the first leaf subsumed by this node
        /// </summary>
        public int FirstLeafNumber
        {
            get
            {
                if (_firstLeafNumber < 0)
                    throw new Exception("Invalid first leaf number");

                return _firstLeafNumber;
            }
            set
            {
                if (_firstLeafNumber >= 0)
                    throw new Exception("First leaf number has already been set");
                else if (value < 0)
                    throw new Exception("Invalid first leaf number");

                _firstLeafNumber = value;
            }
        }

        /// <summary>
        /// Gets or sets the number of the last leaf subsumed by this node
        /// </summary>
        public int LastLeafNumber
        {
            get
            {
                if (_lastLeafNumber < 0)
                    throw new Exception("Invalid last leaf number");

                return _lastLeafNumber;
            }
            set
            {
                if (_lastLeafNumber >= 0)
                    throw new Exception("Last leaf number has already been set");
                else if (value < 0)
                    throw new Exception("Invalid last leaf number");

                _lastLeafNumber = value;
            }
        }

        /// <summary>
        /// Gets or sets the MRG file in which the root of this parse tree is contained
        /// </summary>
        public string MrgFile
        {
            get
            {
                if (IsRoot)
                    return _mrgFile;
                else
                    return Root.MrgFile;
            }
            set
            {
                if (!IsRoot)
                    throw new Exception("Cannot set MRG file for non-root nodes");

                if (_mrgFile != null)
                    throw new Exception("MRG file has already been set");

                if (!File.Exists(value))
                    throw new FileNotFoundException("Invalid MRG file");

                _mrgFile = value;
            }
        }

        /// <summary>
        /// Gets or sets the sentence number within the MRG file that this tree is drawn from
        /// </summary>
        public int SentenceNumber
        {
            get
            {
                if (IsRoot)
                    return _sentenceNumber;
                else
                    return Root.SentenceNumber;
            }
            set
            {
                if (!IsRoot)
                    throw new Exception("Cannot set sentence number for non-root nodes");

                if (_sentenceNumber != -1)
                    throw new Exception("Sentence number has already been set");

                if (value < 0)
                    throw new ArgumentOutOfRangeException("Invalid sentence number");

                _sentenceNumber = value;
            }
        }

        /// <summary>
        /// Gets all descendants of this node. A node is a descendant of this node if it is at or below this node in the tree.
        /// </summary>
        public TreeBankNode[] Descendants
        {
            get
            {
                TreeBankNode[] descendants = new TreeBankNode[TotalNodesInTree];
                descendants[0] = this;

                int added = 1;
                foreach (TreeBankNode child in _children)
                    foreach (TreeBankNode descendant in child.Descendants)
                        descendants[added++] = descendant;

                if (added != TotalNodesInTree)
                    throw new Exception("Descendant count mismatch");

                return descendants;
            }
        }

        /// <summary>
        /// Gets all ancestors of this node. A node is an ancestor of this node if (1) it is the parent of this node (or is this node) 
        /// or (2) it is the parent of an ancestor node.
        /// </summary>
        public TreeBankNode[] Ancestors
        {
            get
            {
                int totalAncestors = Depth + 1;
                TreeBankNode[] ancestors = new TreeBankNode[totalAncestors];
                ancestors[0] = this;

                int added = 1;
                TreeBankNode parent = this;
                while ((parent = parent.Parent) != null)
                    ancestors[added++] = parent;

                if (added != totalAncestors)
                    throw new Exception("Ancestor count mismatch");

                return ancestors;
            }
        }

        /// <summary>
        /// Gets direct relatives of this node. A node is a direct relative of this node if it is an ancestor or descendant of this node.
        /// </summary>
        public TreeBankNode[] DirectRelatives
        {
            get
            {
                TreeBankNode[] ancestors = Ancestors;
                TreeBankNode[] descendants = Descendants;
                int numRelatives = ancestors.Length + descendants.Length - 1;  // the current node is counted in both lists...count it once
                TreeBankNode[] relatives = new TreeBankNode[numRelatives];

                int added = 0;
                foreach (TreeBankNode ancestor in ancestors)
                    if (ancestor != this)
                        relatives[added++] = ancestor;

                // add this node
                relatives[added++] = this;

                // add descendants
                foreach (TreeBankNode descendant in descendants)
                    if (descendant != this)
                        relatives[added++] = descendant;

                if (added != relatives.Length)
                    throw new Exception("Relative count mismatch");

                return relatives;
            }
        }

        /// <summary>
        /// Gets a list of passive verbs subsumed by this node
        /// </summary>
        public TreeBankNodeList PassiveVerbs
        {
            get
            {
                TreeBankNodeList passives = new TreeBankNodeList();
                if (IsNullElement)
                    return passives;

                int lastTokenNumber = LastToken.TokenNumber;

                // our rule for passiveness:  form of BE followed by a past participle
                TreeBankNodeList verbs = GetNodesBySyntacticCategory(TreeBankEngine.VerbPartsOfSpeech);

                // check verbs
                foreach (TreeBankNode verb in verbs)
                {
                    string text = verb.SurfaceText.ToLower();
                    if (!_beVerbs.Contains(text))
                        continue;

                    int nextTokenNumber = verb.TokenNumber + 1;
                    if (nextTokenNumber > lastTokenNumber)
                        continue;

                    TreeBankNode nextToken = _root.GetToken(nextTokenNumber);
                    if (nextToken.Category == TreeBankEngine.SyntacticCategory.VerbPastParticiple)
                        passives.Add(verb);
                }

                return passives;
            }
        }

        /// <summary>
        /// Gets whether or not this node subsumes a passive verb construction
        /// </summary>
        public bool IsPassive
        {
            get { return PassiveVerbs.Count > 0; }
        }

        /// <summary>
        /// Gets whether or not this node is anaphoric
        /// </summary>
        public bool IsAnaphoric
        {
            get
            {
                TreeBankNode[] leaves = Leaves;

                /* check if surface text is anaphoric...check leaf length first for efficiency, since no anaphoric expressions contain 
                 * more than two words */
                if (leaves.Length > 0 && leaves.Length <= 2 && _anaphoricWords.Contains(SurfaceText.ToLower()))
                    return true;
                // if only one leaf is subsumed and at least one subsumed node is an anaphoric category, the argument is anaphoric
                else if (leaves.Length == 1)
                    foreach (TreeBankNode node in AllNodes)
                        if (_anaphoricCategories.Contains(node.Category) && !_nonAnaphoricWords.Contains(node.SurfaceText.ToLower()))
                            return true;

                return false;
            }
        }

        /// <summary>
        /// Gets all nodes at or below this node
        /// </summary>
        public TreeBankNode[] AllNodes
        {
            get
            {
                TreeBankNode[] nodes = new TreeBankNode[_totalNodesInTree];

                // add all child nodes first
                int nodeNum = 0;
                foreach (TreeBankNode child in _children)
                    foreach (TreeBankNode node in child.AllNodes)
                        nodes[nodeNum++] = node;

                // add current node
                nodes[nodeNum++] = this;

                if (nodeNum != _totalNodesInTree)
                    throw new Exception("Node number mismatch");

                return nodes;
            }
        }

        /// <summary>
        /// Gets or sets the list of syntactic categories this node's category is ambiguous with
        /// </summary>
        public List<TreeBankEngine.SyntacticCategory> AmbiguousWithCategories
        {
            get { return _ambiguousWithCategories; }
        }

        /// <summary>
        /// Gets the co-index referent of this node
        /// </summary>
        public TreeBankNode CoIndexReferent
        {
            get
            {
                if (_category != TreeBankEngine.SyntacticCategory.CoIndexed && _category != TreeBankEngine.SyntacticCategory.PseudoAttachment)
                    throw new Exception("Node is not coindexed");

                return _coIndexReferent;
            }
            set
            {
                if (_category != TreeBankEngine.SyntacticCategory.CoIndexed && _category != TreeBankEngine.SyntacticCategory.PseudoAttachment)
                    throw new Exception("Node is not coindexed");

                _coIndexReferent = value;
            }
        }

        /// <summary>
        /// Gets or sets the zero-based co-index for this node
        /// </summary>
        public int CoIndexId
        {
            get { return _coIndexID; }
            set
            {
                if (_coIndexID >= 0)
                    throw new Exception("Coindex id has already been set");
                else if (value < 0)
                    throw new Exception("Invalid coindex");

                _coIndexID = value;
            }
        }

        /// <summary>
        /// Gets or sets the syntactic category for this constituent
        /// </summary>
        public TreeBankEngine.SyntacticCategory Category
        {
            get { return _category; }
            set
            {
                // we reset NullElement to something more meaningful after parsing
                if (_category != TreeBankEngine.SyntacticCategory.Unknown && _category != TreeBankEngine.SyntacticCategory.NullElement)
                    throw new Exception("Category has already been set");

                _category = value;
            }
        }

        /// <summary>
        /// Gets mnemonic for this node's syntactic category
        /// </summary>
        public string CategoryMnemonic
        {
            get { return TreeBankEngine.GetMnemonicFor(_category); }
        }

        /// <summary>
        /// Gets syntactic category number
        /// </summary>
        public int CategoryNumber
        {
            get { return (int)_category; }
        }

        /// <summary>
        /// Gets the number of children nodes below this node
        /// </summary>
        public int ChildCount
        {
            get { return _children.Count; }
        }

        /// <summary>
        /// Gets whether or not this node is a leaf node. A node is a leaf if it has no children of any kind (null or otherwise). Compare to IsToken.
        /// </summary>
        public bool IsLeaf
        {
            get { return _children.Count == 0; }
        }

        /// <summary>
        /// Gets the collection of leaf nodes stored at the root of this node. This is a constant-time alternative
        /// to calling Leaves, which is linear in the number of leaves subsumed by a node; however, this will return
        /// all leaves in the tree as opposed to just those subsumed by this node.
        /// </summary>
        public TreeBankNodeList RootLeaves
        {
            get { return _root._leafNodes; }
        }

        /// <summary>
        /// Gets the leaf nodes at or below this node
        /// </summary>
        public TreeBankNode[] Leaves
        {
            get
            {
                // create leaf collection
                int numLeaves = _lastLeafNumber - _firstLeafNumber + 1;
                TreeBankNode[] leaves = new TreeBankNode[numLeaves];

                // insert leaves
                TreeBankNodeList allLeaves = RootLeaves;
                for (int i = _firstLeafNumber; i <= _lastLeafNumber; ++i)
                    leaves[i - _firstLeafNumber] = allLeaves[i];

                return leaves;
            }
        }

        /// <summary>
        /// Gets first leaf subsumed by this node
        /// </summary>
        public TreeBankNode FirstLeaf
        {
            get { return RootLeaves[_firstLeafNumber]; }
        }

        /// <summary>
        /// Gets the last leaf subsumed by this node
        /// </summary>
        public TreeBankNode LastLeaf
        {
            get { return RootLeaves[_lastLeafNumber]; }
        }

        /// <summary>
        /// Gets whether or not this node is a token node. A token node is a non-null leaf node.
        /// </summary>
        public bool IsToken
        {
            get { return IsLeaf && !IsNullElement; }
        }

        /// <summary>
        /// Gets token nodes subsumed by this node
        /// </summary>
        public TreeBankNode[] Tokens
        {
            get
            {
                // get number of tokens
                int numTokens = _lastLeafNumber - _firstLeafNumber + 1;

                // subtract null leaf numbers from number of tokens
                if (_root._nullLeafNumbers != null)
                    foreach (int nullLeafNumber in _root._nullLeafNumbers)
                        if (nullLeafNumber >= _firstLeafNumber && nullLeafNumber <= _lastLeafNumber)
                            --numTokens;

                TreeBankNode[] tokens = new TreeBankNode[numTokens];

                if (numTokens == 0)
                    return tokens;

                // add tokens from the root's list of all tokens
                TreeBankNodeList allLeaves = RootLeaves;
                int numAdded = 0;
                for (int i = _firstLeafNumber; i <= _lastLeafNumber; ++i)
                {
                    TreeBankNode leaf = allLeaves[i];
                    if (leaf.IsToken)
                        tokens[numAdded++] = leaf;
                }

                // sanity check
                if (numAdded != tokens.Length)
                    throw new Exception("Token count mismatch");

                return tokens;
            }
        }

        /// <summary>
        /// Gets the first token subsumed by this node
        /// </summary>
        public TreeBankNode FirstToken
        {
            get
            {
                TreeBankNode[] tokens = Tokens;
                if (tokens.Length == 0)
                    return null;

                return tokens[0];
            }
        }

        /// <summary>
        /// Gets the last token subsumed by this node
        /// </summary>
        public TreeBankNode LastToken
        {
            get
            {
                TreeBankNode[] tokens = Tokens;
                if (tokens.Length == 0)
                    return null;

                return tokens[tokens.Length - 1];
            }
        }

        /// <summary>
        /// Gets or sets the surface text. For inner nodes, the surface text is the concatenation of the surface text
        /// held by all children nodes. For leaf nodes, the surface text is a token from the sentence or the empty string
        /// in the case of null nodes (e.g., traces).
        /// </summary>
        public string SurfaceText
        {
            get
            {
                // return text for leaves
                if (IsLeaf)
                {
                    if (_surfaceText == null)
                        throw new Exception("Invalid surface text");

                    return _surfaceText;
                }

                StringBuilder text = new StringBuilder();
                bool addSpacePrefix = false;
                foreach (TreeBankNode child in _children)
                {
                    string childText = child.SurfaceText;

                    if (childText != "")
                    {
                        // add non-null child text
                        text.Append((addSpacePrefix ? " " : "") + childText);

                        // add space prefix to following child text
                        addSpacePrefix = true;
                    }
                }

                return text.ToString();
            }
            set
            {
                // disallow whitespace in leaf nodes
                if (_whitespaceRE.Match(value).Success)
                    throw new Exception("Cannot set surface text to a string containing whitespace");

                // only allow setting for leaf nodes
                if (IsLeaf)
                    _surfaceText = value;
                else
                    throw new Exception("Cannot set surface text for inner nodes");
            }
        }

        /// <summary>
        /// Attempts to reconstruct the untokenized text using a variety of heuristics for removing
        /// spaces around punctuation (e.g., "$ 100 dollars" becomes "$100 dollars", etc.). This is
        /// approximate, and you should not rely on it for anything that is crucial.
        /// </summary>
        public string UntokenizedText
        {
            get { return SurfaceText.Replace(_untokenizationReplacements, true); }
        }

        /// <summary>
        /// Gets or sets the grammatical functions of this node
        /// </summary>
        public List<TreeBankEngine.GrammaticalFunction> Functions
        {
            get { return _functions; }
        }

        /// <summary>
        /// Gets the parent node
        /// </summary>
        public TreeBankNode Parent
        {
            get { return _parent; }
        }

        /// <summary>
        /// Gets the depth of this node (i.e., the number of ancestor nodes above the current node)
        /// </summary>
        public int Depth
        {
            get
            {
                if (IsRoot)
                    return 0;

                return _parent.Depth + 1;
            }
        }

        /// <summary>
        /// Gets the height of this node above its first leaf node
        /// </summary>
        public int Height
        {
            get
            {
                // get height of this node above first leaf node
                int height = FirstLeaf.Depth - Depth;
                if (height < 0)
                    throw new Exception("Invalid height");

                return height;
            }
        }

        /// <summary>
        /// Gets the location in terminal:height notation, where terminal is equal to FirstLeaf.LeafNumber
        /// </summary>
        public string Location
        {
            get { return FirstLeaf.LeafNumber + ":" + Height; }
        }

        /// <summary>
        /// Gets the fully-specified location of this node. Includes MRG file and sentence number, in addition to what is returned by Location.
        /// Format:  wsj_xxxx:sent:terminal:height
        /// </summary>
        public string FullLocation
        {
            get { return Path.GetFileNameWithoutExtension(MrgFile) + ":" + SentenceNumber + ":" + Location; }
        }

        /// <summary>
        /// Gets or sets the root of the tree containing this node
        /// </summary>
        public TreeBankNode Root
        {
            get
            {
                if (_root == null)
                    throw new Exception("Invalid root node");

                return _root;
            }
            set
            {
                if (_root != null)
                    throw new Exception("Root node has already been set");
                else if (!value.IsAncestorOf(this))
                    throw new Exception("Root node must be an ancestor of the current node");
                else
                    _root = value;
            }
        }

        /// <summary>
        /// Gets whether or not this is a root node
        /// </summary>
        public bool IsRoot
        {
            get { return _parent == null; }
        }

        /// <summary>
        /// Gets zero-based index of this node with respect to its parent and its left siblings
        /// </summary>
        public int ChildIndex
        {
            get
            {
                if (IsRoot)
                    return -1;

                int index = _parent._children.IndexOf(this);
                if (index < 0)
                    throw new Exception("Invalid child index");

                return index;
            }
        }

        /// <summary>
        /// Gets list of left sibling nodes of the current node
        /// </summary>
        public TreeBankNode[] LeftSiblings
        {
            get
            {
                // get child index of current node (-1 for root nodes)
                int currentIndex = ChildIndex;
                if (currentIndex == -1)
                    return new TreeBankNode[0];

                // get siblings
                TreeBankNode[] leftSiblings = new TreeBankNode[currentIndex];
                for (int siblingIndex = 0; siblingIndex < currentIndex; ++siblingIndex)
                    leftSiblings[siblingIndex] = _parent.GetChild(siblingIndex);

                return leftSiblings;
            }
        }

        /// <summary>
        /// Gets list of right sibling nodes of the current node
        /// </summary>
        public TreeBankNode[] RightSiblings
        {
            get
            {
                // get child index of current node (-1 for root nodes)
                int currentIndex = ChildIndex;
                if (currentIndex == -1)
                    return new TreeBankNode[0];

                // get number of right sibs
                int firstRightSiblingIndex = currentIndex + 1;
                int lastRightSiblingIndex = _parent.ChildCount - 1;
                int numRightSiblings = lastRightSiblingIndex - firstRightSiblingIndex + 1;

                // get right sibs
                TreeBankNode[] rightSiblings = new TreeBankNode[numRightSiblings];
                for (int siblingIndex = firstRightSiblingIndex; siblingIndex <= lastRightSiblingIndex; ++siblingIndex)
                    rightSiblings[siblingIndex - firstRightSiblingIndex] = _parent.GetChild(siblingIndex);

                return rightSiblings;
            }
        }

        /// <summary>
        /// Gets all siblings of the current node
        /// </summary>
        public TreeBankNode[] Siblings
        {
            get
            {
                TreeBankNode[] leftSibs = LeftSiblings;
                TreeBankNode[] rightSibs = RightSiblings;
                TreeBankNode[] siblings = new TreeBankNode[leftSibs.Length + rightSibs.Length];

                int sibNum = 0;
                foreach (TreeBankNode leftSib in leftSibs)
                    siblings[sibNum++] = leftSib;
                foreach (TreeBankNode rightSib in rightSibs)
                    siblings[sibNum++] = rightSib;

                if (sibNum != siblings.Length)
                    throw new Exception("Sibling count mismatch");

                return siblings;
            }
        }

        /// <summary>
        /// Gets whether or not this is a null element. A node is null if its syntactic category is 
        /// in {UnderstoodSubject, ZeroVariant, CoIndexed, PseudoAttachment} or if all of its children
        /// are null elements.
        /// </summary>
        public bool IsNullElement
        {
            get
            {
                // null if this node is null
                bool isNull = _category == TreeBankEngine.SyntacticCategory.UnderstoodSubject ||
                              _category == TreeBankEngine.SyntacticCategory.ZeroVariant ||
                              _category == TreeBankEngine.SyntacticCategory.CoIndexed ||
                              _category == TreeBankEngine.SyntacticCategory.PseudoAttachment ||
                              _category == TreeBankEngine.SyntacticCategory.NullElement;

                if (isNull)
                    return true;

                // if no children, this node is not null
                if (IsLeaf)
                    return false;

                // if all children are null, this node is null
                bool allChildrenNull = true;
                foreach (TreeBankNode child in _children)
                {
                    allChildrenNull = allChildrenNull && child.IsNullElement;
                    if (!allChildrenNull)
                        break;
                }

                return allChildrenNull;
            }
        }

        /// <summary>
        /// Gets or sets the zero-based leaf number of this node. This differs from TokenNumber because of the presence
        /// of null nodes (e.g., trace), which are counted towards this value but not towards TokenNumber.
        /// </summary>
        public int LeafNumber
        {
            get
            {
                if (!IsLeaf)
                    throw new Exception("LeafNumber is not valid for non-leaf nodes");
                else if (_leafNumber < 0)
                    throw new Exception("Invalid leaf number");

                return _leafNumber;
            }
            set
            {
                if (!IsLeaf)
                    throw new Exception("LeafNumber is not valid for non-leaf nodes");
                else if (_leafNumber >= 0)
                    throw new Exception("Leaf number has already been set");
                else if (value < 0)
                    throw new Exception("Invalid leaf number");

                _leafNumber = value;
            }
        }

        /// <summary>
        /// Gets or sets the zero-based token number of this node. This differs from LeafNumber because of the presence 
        /// of null nodes (e.g., trace), which are not counted towards this value but are counted towards LeafNumber.
        /// </summary>
        public int TokenNumber
        {
            get
            {
                if (!IsToken)
                    throw new Exception("TokenNumber is not valid for non-token nodes");
                else if (_tokenNumber < 0)
                    throw new Exception("Invalid token number");

                return _tokenNumber;
            }
            set
            {
                if (!IsToken)
                    throw new Exception("TokenNumber is not valid for non-token nodes");
                else if (_tokenNumber >= 0)
                    throw new Exception("Token number has already been set");
                else if (value < 0)
                    throw new Exception("Invalid token number");

                _tokenNumber = value;
            }
        }

        /// <summary>
        /// Gets or sets the zero-based token start character. This number indicates the start position within the 
        /// sentence, ignoring whitespace. This is useful for tasks that use alternate tokenizations of the same
        /// TreeBank data, as this value will be constant across all tokenizations.
        /// </summary>
        public int TokenStartCharacter
        {
            get
            {
                if (!IsToken)
                    throw new Exception("TokenStartCharacter is not valid for non-token nodes");
                else if (_tokenStartCharacter < 0)
                    throw new Exception("Invalid token number");

                return _tokenStartCharacter;
            }
            set
            {
                if (!IsToken)
                    throw new Exception("TokenStartCharacter is not valid for non-token nodes");
                else if (_tokenStartCharacter >= 0)
                    throw new Exception("Token number has already been set");
                else if (value < 0)
                    throw new Exception("Invalid token number");

                _tokenStartCharacter = value;
            }
        }

        /// <summary>
        /// Gets the zero-based token end character. This number indicates the end position within the 
        /// sentence, ignoring whitespace. This is useful for tasks that use alternate tokenizations of the same
        /// TreeBank data, as this value will be constant across all tokenizations.
        /// </summary>
        public int TokenEndCharacter
        {
            get
            {
                if (!IsToken)
                    throw new Exception("TokenEndCharacter is not valid for non-token nodes");

                return TokenStartCharacter + _surfaceText.Replace(" ", "").Length - 1;
            }
        }

        /// <summary>
        /// Gets the head child of this node. Differs from HeadToken in that this property returns the
        /// first head child instead of traversing the tree to a head token. The head child of this node 
        /// is determined in one of two ways:  (1) by the head finding rules from Collins' dissertation (not very good), 
        /// or (2) by assigning them explicitly via the set method of this property. If the head is not assigned
        /// explicitly, it will be searched for using method (1). Subsequent gets of this property will
        /// return the previously set or searched-for head.
        /// </summary>
        public TreeBankNode Head
        {
            get
            {
                // check if we've assigned the head before, either explicitly or through the head search method
                if (_head != null)
                    return _head;

                // find the head and save it for next time
                _head = TreeBankEngine.FindHead(this, null);

                return _head;
            }
            set
            {
                if (_head != null)
                    throw new Exception("Head has already been set");
                else if (value == null)
                    throw new Exception("Invalid head value");

                _head = value;
            }
        }

        /// <summary>
        /// Gets the head token of this node. Differs from Head in that this property traverses
        /// the tree via head nodes until it reaches a head token, which is returned.
        /// </summary>
        public TreeBankNode HeadToken
        {
            get
            {
                // start searching for head nodes at the current node
                TreeBankNode currHead = this;
                Set<TreeBankNode> excludeNodes = new Set<TreeBankNode>();
                while (true)
                {
                    /* find the next head node - use the preset head node or search for the next head, but only use
                     * this head if it isn't in the exclusion list */
                    TreeBankNode nextHead = null;
                    if (currHead.Head != null && !excludeNodes.Contains(currHead.Head))
                        nextHead = currHead.Head;
                    // if we weren't successful, find a head under the current node that satisfies the exclusion list
                    else
                        nextHead = TreeBankEngine.FindHead(currHead, excludeNodes);

                    // if we couldn't find the next head...
                    if (nextHead == null)
                    {
                        // ...if we're at the start of the search, no head exists...
                        if (currHead == this)
                            return null;
                        // ...otherwise, back up the tree and search again, excluding the current head from the search
                        else
                        {
                            excludeNodes.Add(currHead);
                            currHead = currHead.Parent;
                        }
                    }
                    // if we found the next head and it has no children, we found the head token
                    else if (nextHead.ChildCount == 0)
                        return nextHead;
                    // keep looking
                    else
                        currHead = nextHead;
                }
            }
        }

        /// <summary>
        /// Gets the semantic head token. This is the token that semantically defines the current node. For example, the 
        /// head of a PP is typically not semantically informative, whereas the head of the PP's object can be. This 
        /// function is at best an approximation.
        /// </summary>
        public TreeBankNode SemanticHeadToken
        {
            get
            {
                // get standard head token for starters
                TreeBankNode semanticHeadToken = HeadToken;

                // if we've got a parent node, consider changing the head
                if (semanticHeadToken != null && semanticHeadToken.Parent != null)
                {
                    // if the current node is non-semantic, use the semantic head of the first null-element node that is found
                    SiblingSide direction;
                    if (_semanticHeadCategoryReplacement.TryGetValue(semanticHeadToken.Category, out direction))
                    {
                        // get sibling nodes...
                        TreeBankNode[] siblings = null;
                        if (direction == SiblingSide.Right)
                            siblings = semanticHeadToken.RightSiblings;
                        // ...reversing for left siblings so as to search outwards from the head token
                        else if (direction == SiblingSide.Left)
                        {
                            siblings = semanticHeadToken.LeftSiblings;
                            siblings.Reverse();
                        }
                        else
                            throw new Exception("Unrecognized direction");

                        // grab the first non-null sibling node and use its semantic head, if non-null
                        foreach (TreeBankNode sibling in siblings)
                            if (!sibling.IsNullElement)
                            {
                                TreeBankNode siblingSemanticHeadToken = sibling.SemanticHeadToken;
                                if (siblingSemanticHeadToken != null)
                                {
                                    semanticHeadToken = siblingSemanticHeadToken;
                                    break;
                                }
                            }
                    }
                }

                return semanticHeadToken;
            }
        }

        /// <summary>
        /// Gets whether or not this node is a noun
        /// </summary>
        public bool IsNoun
        {
            get { return TreeBankEngine.NounPartsOfSpeech.Contains(_category); }
        }

        /// <summary>
        /// Gets the determiner for NPs. Throws exception if called on a non-NP node.
        /// </summary>
        public TreeBankNode NounPhraseDeterminer
        {
            get
            {
                if (_category != TreeBankEngine.SyntacticCategory.NounPhrase)
                    throw new Exception("Can only be called on NPs");

                if (_children.Count >= 2)
                {
                    TreeBankNode detNode = _children[0];
                    if (detNode.Category == TreeBankEngine.SyntacticCategory.Determiner ||
                        detNode.Category == TreeBankEngine.SyntacticCategory.PreDeterminer ||
                        detNode.Category == TreeBankEngine.SyntacticCategory.WhDeterminer)
                        return detNode;
                }

                return null;
            }
        }

        /// <summary>
        /// Gets whether or not the current node is a proper noun
        /// </summary>
        public bool IsProperNoun
        {
            get { return _category == TreeBankEngine.SyntacticCategory.NounProperPlural || _category == TreeBankEngine.SyntacticCategory.NounProperSingular; }
        }

        /// <summary>
        /// Gets whether or not this node is a verb
        /// </summary>
        public bool IsVerb
        {
            get { return TreeBankEngine.VerbPartsOfSpeech.Contains(_category); }
        }

        /// <summary>
        /// Gets wether or not this node is an adverb
        /// </summary>
        public bool IsAdverb
        {
            get { return TreeBankEngine.AdverbPartsOfSpeech.Contains(_category); }
        }

        /// <summary>
        /// Gets whether or not this node is an adjective
        /// </summary>
        public bool IsAdjective
        {
            get { return TreeBankEngine.AdjectivePartsOfSpeech.Contains(_category); }
        }

        /// <summary>
        /// Gets whether or not this node is punctuation
        /// </summary>
        public bool IsPunctuation
        {
            get { return TreeBankEngine.PunctuationCategories.Contains(_category); }
        }

        /// <summary>
        /// Gets whether or not this node is a head node
        /// </summary>
        public bool IsHead
        {
            get
            {
                if (_parent == null)
                    return false;

                return _parent.Head == this;
            }
        }

        /// <summary>
        /// Gets the highest ancestor of the current node that exists along a non-branching path
        /// </summary>
        public TreeBankNode HighestNonbranchingAncestor
        {
            get
            {
                // roots are their own HNA
                if (IsRoot)
                    return this;

                // if the parent of the current node has multiple children, the current node is the HNA
                if (_parent.ChildCount > 1)
                    return this;

                // otherwise, the HNA is the parent's HNA
                return _parent.HighestNonbranchingAncestor;
            }
        }

        /// <summary>
        /// Gets the lowest descendant of the current node that exists along a non-branching path
        /// </summary>
        public TreeBankNode LowestNonbranchingDescendant
        {
            get
            {
                // leaves are their own LND
                if (IsLeaf)
                    return this;

                // if the current node has multiple children, the current node is the LND
                if (_children.Count > 1)
                    return this;

                // otherwise, the LND is the LND of the sole child node
                return _children[0].LowestNonbranchingDescendant;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parent">Parent node</param>
        public TreeBankNode(TreeBankNode parent)
        {
            _parent = parent;
            _head = null;
            _functions = new List<TreeBankEngine.GrammaticalFunction>();
            _ambiguousWithCategories = new List<TreeBankEngine.SyntacticCategory>();
            _children = new TreeBankNodeList();
            _category = TreeBankEngine.SyntacticCategory.Unknown;
            _coIndexID = -1;
            _coIndexReferent = null;
            _mrgFile = null;
            _sentenceNumber = -1;
            _leafNumber = -1;
            _firstLeafNumber = -1;
            _lastLeafNumber = -1;
            _leafNodes = null;
            _surfaceText = null;
            _totalNodesInTree = 1;
            _tokenNumber = -1;
            _tokenStartCharacter = -1;

            // if we don't have a parent node, the current node is its own root
            if (_parent == null)
                _root = this;
            // if we have a parent node, the current node's root is the parent's root
            else
                _root = parent.Root;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="treeBankNode">TreeBankNode to construct the current one from</param>
        /// <param name="parent">Parent of constructed node</param>
        /// <param name="childConstructor">Constructor to use for creating child nodes. If null, TreeBankNode() will be used.</param>
        protected TreeBankNode(TreeBankNode treeBankNode,
                               TreeBankNode parent,
                               TreeBankNodeConstructor childConstructor)
            : this(parent)
        {
            if (treeBankNode == null)
                throw new Exception("Cannot construct a TreeBankNode from a null TreeBankNode");

            // if we're creating a root node, set the MRG file and sentence number
            if (IsRoot && treeBankNode.IsTreeBanked)
            {
                MrgFile = treeBankNode.MrgFile;
                SentenceNumber = treeBankNode.SentenceNumber;
            }

            Functions.AddRange(treeBankNode.Functions);
            Category = treeBankNode.Category;
            AmbiguousWithCategories.AddRange(treeBankNode.AmbiguousWithCategories);
            FirstLeafNumber = treeBankNode.FirstLeafNumber;
            LastLeafNumber = treeBankNode.LastLeafNumber;

            // set leaf text
            if (treeBankNode.IsLeaf)
            {
                SurfaceText = treeBankNode.SurfaceText;
                Root.AddLeaf(this);
                LeafNumber = treeBankNode.LeafNumber;

                if (treeBankNode.IsNullElement)
                    Root.AddNullLeafNumber(LeafNumber);
            }

            // set token properties
            if (treeBankNode.IsToken)
            {
                TokenNumber = treeBankNode.TokenNumber;
                TokenStartCharacter = treeBankNode.TokenStartCharacter;
            }

            // recursively construct children nodes
            foreach (TreeBankNode child in treeBankNode._children)
            {
                TreeBankNode node = null;

                // if the caller supplied a child constructor, use it; otherwise, use the TreeBank constructor
                if (childConstructor == null)
                    node = new TreeBankNode(child, this, childConstructor);
                else
                    node = childConstructor(child, this);

                AddChildNode(node);
            }

            /* set head node - for leaves, this is the leaf itself; for inner nodes that have a head node (all non-null-element
             * nodes), use the head node */
            if (IsLeaf)
                Head = this;
            else if (treeBankNode.Head != null)
                Head = _children[treeBankNode.Head.ChildIndex];
        }

        /// <summary>
        /// Adds a child node to the current tree. Access to this function is quite restricted due to the fact that the function
        /// does not automatically update critical fields in this class, e.g., leaf indexes, leaf nodes, etc. The caller must keep
        /// track of these things and update them accordingly. A naive call to this function can seriously screw up the
        /// data structure, so I take the easy way out and disallow such calls.
        /// </summary>
        /// <param name="childNode">Child node to add</param>
        protected internal void AddChildNode(TreeBankNode childNode)
        {
            _children.Add(childNode);

            _totalNodesInTree += childNode.TotalNodesInTree;
        }

        /// <summary>
        /// Gets a child node by its index
        /// </summary>
        /// <param name="index">Index of child node to get</param>
        /// <returns>Child node</returns>
        public TreeBankNode GetChild(int index)
        {
            return _children[index];
        }

        /// <summary>
        /// Gets a node subsumed by this one (only valid for root nodes)
        /// </summary>
        /// <param name="aboveTerminal">Leaf terminal above which to get node</param>
        /// <param name="height">0-based height of node above terminal</param>
        /// <returns>TreeBankNode</returns>
        public TreeBankNode GetNode(int aboveTerminal, int height)
        {
            if (!IsRoot)
                throw new Exception("Only valid for root nodes");
            else if (height < 0)
                throw new Exception("Invalid height");

            // get terminal node
            TreeBankNode terminal = _leafNodes[aboveTerminal];
            if (terminal.LeafNumber != aboveTerminal)
                throw new Exception("Leaf number mismatch");

            // get node above terminal
            while (height > 0)
            {
                terminal = terminal.Parent;
                --height;
            }
            
            // make sure we got a valid node
            if (terminal == null)
                throw new Exception("Failed to get node");

            return terminal;
        }

        /// <summary>
        /// Gets a node in the tree using the "terminal:height" notation, where terminal is the zero-based terminal and height is the
        /// zero-based height of node above terminal to get. Only valid for root nodes.
        /// </summary>
        /// <param name="location">Location of node to get, in "terminal:height" notation</param>
        /// <returns>TreeBankNode</returns>
        public TreeBankNode GetNode(string location)
        {
            // get terminal number and height
            string[] locParams = location.Split(':');
            int terminalNumber = int.Parse(locParams[0]);
            int height = int.Parse(locParams[1]);

            return GetNode(terminalNumber, height);
        }

        /// <summary>
        /// Gets nodes at or below this one with a specified function
        /// </summary>
        /// <param name="function">Function to search for</param>
        /// <returns>List of nodes</returns>
        public TreeBankNodeList GetNodesByFunction(TreeBankEngine.GrammaticalFunction function)
        {
            TreeBankNodeList nodes = new TreeBankNodeList();

            if (_functions.Contains(function))
                nodes.Add(this);

            foreach (TreeBankNode child in _children)
                nodes.AddRange(child.GetNodesByFunction(function));

            return nodes;
        }

        /// <summary>
        /// Gets list of nodes subsumed by this node of a particular syntactic category
        /// </summary>
        /// <param name="category">Category to retrieve</param>
        /// <returns>List of nodes of category type</returns>
        public TreeBankNodeList GetNodesBySyntacticCategory(TreeBankEngine.SyntacticCategory category)
        {
            return GetNodesBySyntacticCategory(new Set<TreeBankEngine.SyntacticCategory>(new TreeBankEngine.SyntacticCategory[] { category }));
        }

        /// <summary>
        /// Gets list of nodes subsumed by this node that are of one of the given categories
        /// </summary>
        /// <param name="categories">Categories to retrieve</param>
        /// <returns>List of nodes</returns>
        public TreeBankNodeList GetNodesBySyntacticCategory(Set<TreeBankEngine.SyntacticCategory> categories)
        {
            TreeBankNodeList nodes = new TreeBankNodeList();
            GetNodesBySyntacticCategory(categories, ref nodes);

            return nodes;
        }

        /// <summary>
        /// Gets list of nodes subsumed by this node that are of one of the given categories. This is a helper function to the public
        /// GetNodesBySyntacticCategory. The goal is to pass only a single list, which is added to by each child. This avoids allocation
        /// of a new list by each node.
        /// </summary>
        /// <param name="categories">Categories to retrieve</param>
        /// <param name="nodes">Current list of nodes</param>
        /// <returns>List of nodes</returns>
        private void GetNodesBySyntacticCategory(Set<TreeBankEngine.SyntacticCategory> categories, ref TreeBankNodeList nodes)
        {
            // add the current node if its category matches
            if (categories.Contains(_category))
                nodes.Add(this);

            // process child nodes
            foreach (TreeBankNode child in _children)
                child.GetNodesBySyntacticCategory(categories, ref nodes);
        }

        /// <summary>
        /// Gets all verb nodes in the tree
        /// </summary>
        /// <returns>List of verbs</returns>
        public TreeBankNodeList GetVerbs()
        {
            return GetNodesBySyntacticCategory(TreeBankEngine.VerbPartsOfSpeech);
        }

        /// <summary>
        /// Gets a co-indexed node by its index
        /// </summary>
        /// <param name="index">Index of node to get</param>
        /// <returns>TreeBankNode</returns>
        public TreeBankNode GetCoIndexedNode(int index)
        {
            if (index < 0)
                throw new Exception("Invalid index");

            foreach (TreeBankNode n in AllNodes)
                if (n.CoIndexId == index)
                    return n;

            return null;
        }

        /// <summary>
        /// Gets the surface text of this parse tree (calls TreeBankNode.SurfaceText)
        /// </summary>
        /// <returns>Surface text of this parse tree</returns>
        public override string ToString()
        {
            return SurfaceText;
        }

        /// <summary>
        /// Gets the lowest common ancestor of this node and another
        /// </summary>
        /// <param name="node">Other node</param>
        /// <returns>Lowest common ancestor</returns>
        public TreeBankNode GetLowestCommonAncestor(TreeBankNode node)
        {
            // start with two end points
            TreeBankNode node1 = this;
            TreeBankNode node2 = node;

            // we should always find a LCA
            while (true)
            {
                if (node1.IsAncestorOf(node2))
                    return node1;

                if (node2.IsAncestorOf(node1))
                    return node2;

                // move up in the tree
                node1 = node1.Parent;
                node2 = node2.Parent;

                if (node1 == null || node2 == null)
                    throw new NullReferenceException("Nodes are not from the same tree. There should always be a LCA.");
            }
        }

        /// <summary>
        /// Checks whether this node is an ancestor of another node
        /// </summary>
        /// <param name="node">Proposed descendant</param>
        /// <returns>True if this node is an ancestor of the given node, false otherwise</returns>
        public bool IsAncestorOf(TreeBankNode node)
        {
            if (!InSameSentenceAs(node, false))
                throw new Exception("Nodes must be in the same sentence in order to check ancestor relationship");

            // start at given node and check for equality with the current one while proceeding up the ancestor links
            TreeBankNode currentAncestor = node;
            do
            {
                if (currentAncestor == this)
                    return true;
            }
            while ((currentAncestor = currentAncestor.Parent) != null);

            // not an ancestor of given node
            return false;
        }

        /// <summary>
        /// Gets whether or not this node is a descendant of another node
        /// </summary>
        /// <param name="node">Proposed ancestor</param>
        /// <returns>True if this node is a descendant of the given node, false otherwise</returns>
        public bool IsDescendantOf(TreeBankNode node)
        {
            return node.IsAncestorOf(this);
        }

        /// <summary>
        /// Gets whether or not this node is a direct relative (i.e., an ancestor or descendant) of another node
        /// </summary>
        /// <param name="node">Relative node to check</param>
        /// <returns>True if the current node is a direct relative, false otherwise</returns>
        public bool IsDirectRelativeOf(TreeBankNode node)
        {
            return IsAncestorOf(node) || IsDescendantOf(node);
        }

        /// <summary>
        /// Distance from another node through the parse tree
        /// </summary>
        /// <param name="other">Node to measure distance from</param>
        /// <param name="allowDirectionChange">Whether or not to allow direction changes through the parse tree</param>
        /// <returns>Distance from this node to another node in the tree</returns>
        public int ParseTreeDistanceFrom(TreeBankNode other, bool allowDirectionChange)
        {
            int dist = -1;

            // check if one node is an ancestor of the other
            if (IsAncestorOf(other) || other.IsAncestorOf(this))
            {
                // get ancestor and descendant
                TreeBankNode ancestor = IsAncestorOf(other) ? this : other;
                TreeBankNode descendant = IsAncestorOf(other) ? other : this;

                // compute distance
                dist = 0;
                while (descendant != ancestor)
                {
                    descendant = descendant.Parent;
                    ++dist;
                }
            }
            // otherwise, if we're allowing direction changes, find distance through LCA
            else if (allowDirectionChange)
            {
                TreeBankNode lca = GetLowestCommonAncestor(other);
                dist = lca.ParseTreeDistanceFrom(this, false) + lca.ParseTreeDistanceFrom(other, false);
            }

            if (dist < 0)
                throw new Exception("Should have found a non-negative distance");

            return dist;
        }

        /// <summary>
        /// Gets signed token distance from another node. Throws exception if nodes are not from the same 
        /// sentence (i.e., the files and sentence numbers are not the same).
        /// </summary>
        /// <param name="other">Node to get distance to from the current one</param>
        /// <returns>Positive/negative number if the current token comes after/before the other one, or zero 
        /// if the nodes overlap.</returns>
        public int TokenDistanceFrom(TreeBankNode other)
        {
            // we don't care about the object references, just the token positions
            if (!InSameSentenceAs(other, true))
                throw new Exception("Can't compute token distance unless nodes are in the same sentence");

            // check if current node comes after other node (positive distance)
            int otherLastTokenNumber = other.LastToken.TokenNumber;
            int currentFirstTokenNumber = FirstToken.TokenNumber;
            if (otherLastTokenNumber < currentFirstTokenNumber)
                return currentFirstTokenNumber - otherLastTokenNumber;
            else
            {
                // check if current node comes before other node (negative distance)
                int currentLastTokenNumber = LastToken.TokenNumber;
                int otherFirstTokenNumber = other.FirstToken.TokenNumber;
                if (currentLastTokenNumber < otherFirstTokenNumber)
                    return currentLastTokenNumber - otherFirstTokenNumber;
                // nodes overlap
                else
                    return 0;
            }
        }

        /// <summary>
        /// Similar to the SurfaceText property, but provides options for including the syntactic category of the leaf nodes
        /// </summary>
        /// <param name="includeSyntacticCategory">Whether or not to include the syntactic category of the leaves</param>
        /// <param name="useCategoryMnemonics">Whether or not to use mnemonic categories</param>
        /// <returns>Surface text</returns>
        public string GetSurfaceText(bool includeSyntacticCategory, bool useCategoryMnemonics)
        {
            StringBuilder text = new StringBuilder();

            // append each leaf node
            bool prependSpace = false;
            foreach (TreeBankNode leaf in Leaves)
                if (!leaf.IsNullElement)
                {
                    text.Append((prependSpace ? " " : "") +
                                leaf.SurfaceText +
                                (includeSyntacticCategory ? "/" + (useCategoryMnemonics ? TreeBankEngine.GetMnemonicFor(leaf.Category)
                                                                                        : leaf.Category.ToString())
                                                          : ""));
                    prependSpace = true;
                }

            return text.ToString();
        }

        /// <summary>
        /// Gets bracketed text for this parse tree
        /// </summary>
        /// <param name="useCategoryMnemonics">Whether or not to use mnemonic syntactic category names instead
        /// of the more verbose enumeration names (e.g., "NP" instead of "NounPhrase"). Mnemonic names correspond
        /// to the syntactic labels used in the TreeBank parse tree files.</param>
        /// <param name="includeHeadIndexes">Whether or not to include head indexes in the output</param>
        /// <returns>Bracketed text for this parse tree</returns>
        public string GetBracketedText(bool useCategoryMnemonics, bool includeHeadIndexes)
        {
            // null elements have no bracketing text
            if (IsNullElement)
                return "";

            StringBuilder text = new StringBuilder();

            // get indentation prefix
            string indent = "";
            int indentLevel = Depth;
            for (int i = 0; i < indentLevel; ++i)
                indent += "  ";

            // add category label
            text.Append(indent + "(" + (useCategoryMnemonics ?
                                        TreeBankEngine.GetMnemonicFor(Category) :
                                        Category.ToString()));

            // add head index - leaves are their own heads and have a head index of zero. other nodes have have children heads
            if (includeHeadIndexes)
                text.Append(" H:" + (IsLeaf ? "0" : Head.ChildIndex.ToString()));

            // if this is a leaf node, put leaf text on same line as category
            if (IsLeaf)
                text.Append(" " + _surfaceText + ")");
            // otherwise, start each child node on its own line
            else
            {
                // add bracketed text for each child
                text.Append(Environment.NewLine);
                foreach (TreeBankNode child in _children)
                    text.Append(child.GetBracketedText(useCategoryMnemonics, includeHeadIndexes));
                text.Append(indent + ")");
            }

            text.Append(Environment.NewLine);

            return text.ToString();
        }

        /// <summary>
        /// Gets a leaf by its leaf number (only valid for root nodes)
        /// </summary>
        /// <param name="leafNumber">Number of leaf to get</param>
        /// <returns>Leaf node</returns>
        public TreeBankNode GetLeaf(int leafNumber)
        {
            if (!IsRoot)
                throw new Exception("Only valid for root nodes");

            TreeBankNode leaf = _leafNodes[leafNumber];
            if (leaf.LeafNumber != leafNumber)
                throw new Exception("Leaf number mismatch");

            return leaf;
        }

        /// <summary>
        /// Gets a token by its token number (only valid for root nodes)
        /// </summary>
        /// <param name="tokenNumber">Number of token to get</param>
        /// <returns>Token node</returns>
        public TreeBankNode GetToken(int tokenNumber)
        {
            if (!IsRoot)
                throw new Exception("Only valid for root nodes");

            TreeBankNode token = Tokens[tokenNumber];
            if (token.TokenNumber != tokenNumber)
                throw new Exception("Token number mismatch");

            return token;
        }

        /// <summary>
        /// Tries to get a token by its token number (only valid for root nodes)
        /// </summary>
        /// <param name="tokenNumber">Number of token to get</param>
        /// <param name="token">Reference to token, if found</param>
        /// <returns>True if token was found, false otherwise</returns>
        public bool TryGetToken(int tokenNumber, out TreeBankNode token)
        {
            if (!IsRoot)
                throw new Exception("Only valid for root nodes");
            else if (tokenNumber < 0)
                throw new Exception("Invalid token number");

            token = null;

            // make sure the token number is in range
            TreeBankNode[] tokens = Tokens;
            if (tokenNumber >= tokens.Length)
                return false;

            token = tokens[tokenNumber];
            if (token.TokenNumber != tokenNumber)
                throw new Exception("Token number mismatch");

            return true;
        }

        /// <summary>
        /// Gets the lowest common ancestor of two tokens (only valid for root nodes)
        /// </summary>
        /// <param name="token1Num">Number of first token</param>
        /// <param name="token2Num">Number of second token</param>
        /// <returns>Lowest common ancestor of the two tokens</returns>
        public TreeBankNode GetLowestCommonAncestorOfTokens(int token1Num, int token2Num)
        {
            TreeBankNode token1 = GetToken(token1Num);
            TreeBankNode token2 = GetToken(token2Num);

            return token1.GetLowestCommonAncestor(token2);
        }

        /// <summary>
        /// Gets descriptor for this node's syntactic category
        /// </summary>
        /// <param name="descriptor">Type of descriptor to get</param>
        /// <returns>Descriptor</returns>
        public string GetCategoryDescriptor(TreeBankEngine.SyntacticCategoryDescriptor descriptor)
        {
            string desc = null;

            if (descriptor == TreeBankEngine.SyntacticCategoryDescriptor.Full)
                desc = Category.ToString();
            else if (descriptor == TreeBankEngine.SyntacticCategoryDescriptor.Mnemonic)
                desc = CategoryMnemonic;
            else if (descriptor == TreeBankEngine.SyntacticCategoryDescriptor.Integer)
                desc = CategoryNumber.ToString();
            else
                throw new Exception("Invalid descriptor type");

            return desc;
        }

        /// <summary>
        /// Gets parse tree path from this node to another node in the tree
        /// </summary>
        /// <param name="destinationNode">Node to get path to</param>
        /// <param name="descriptor">Descriptor type to use in parse path</param>
        /// <returns>String description of path</returns>
        public string GetParseTreePath(TreeBankNode destinationNode, TreeBankEngine.SyntacticCategoryDescriptor descriptor)
        {
            // get LCA, through which parse path goes
            TreeBankNode lca = GetLowestCommonAncestor(destinationNode);

            // get path up to (and not including) lca from the current node
            string upPath = "";
            TreeBankNode fromNode = this;
            while (fromNode != lca)
            {
                string cat = fromNode.GetCategoryDescriptor(descriptor);
                upPath += cat + ">";

                fromNode = fromNode.Parent;
            }

            // get path down from (and not including) lca to destination
            string downPath = "";
            while (destinationNode != lca)
            {
                string cat = destinationNode.GetCategoryDescriptor(descriptor);
                downPath = @"<" + cat + downPath;

                destinationNode = destinationNode.Parent;
            }

            // create full path
            string lcaCat = lca.GetCategoryDescriptor(descriptor);
            string path = upPath + lcaCat + downPath;

            return path;
        }

        /// <summary>
        /// Gets the context-free grammar rule that has this node as the left-hand side
        /// </summary>
        /// <param name="descriptor">Syntactic category descriptor type to use</param>
        /// <returns>CFG rule</returns>
        public string GetCfgRule(TreeBankEngine.SyntacticCategoryDescriptor descriptor)
        {
            // LHS is this nodes category
            string rule = GetCategoryDescriptor(descriptor) + "->";

            // RHS is the list of children's categories
            bool prependComma = false;
            foreach (TreeBankNode child in _children)
            {
                rule += (prependComma ? "," : "") + child.GetCategoryDescriptor(descriptor);
                prependComma = true;
            }

            return rule;
        }

        /// <summary>
        /// Gets the minial subsumer of the given span (only valid for root nodes). Returns null if such a node doesn't exist.
        /// </summary>
        /// <param name="firstToken">First token in span</param>
        /// <param name="lastToken">Last token in span</param>
        /// <returns>Minimal subsumer of given span, or null if no such node exists</returns>
        public TreeBankNode GetMinimallySubsumingNode(TreeBankNode firstToken, TreeBankNode lastToken)
        {
            if (!firstToken.InSameSentenceAs(lastToken, false))
                throw new Exception("Tokens must be from the same sentence in order to get MSN");

            return GetMinimallySubsumingNode(firstToken.TokenNumber, lastToken.TokenNumber);
        }

        /// <summary>
        /// Gets minimally subsuming node of the given start and end characters. Start and end characters
        /// must land precisely on the start and end of a token, respectively. Character positions only 
        /// count printable characters - they DO NOT count white space! Returns null if a MSN doesn't exist.
        /// </summary>
        /// <param name="startCharacter">Start character position</param>
        /// <param name="endCharacter">End character position</param>
        /// <param name="throwExceptionOnBadAlignment">Whether or not to throw an exception when character positions don't align perfectly.
        /// If this is false and alignment is not perfect, null will be returned.</param>
        /// <param name="unescapeBraces">Whether or not to unescape brace characters. TreeBank uses special strings (e.g., -LRB-) to denote
        /// braces, which might affect character counting. If the caller is expecting braces to be counted as a single character, this should
        /// be true. If the caller is expecting -LRB- to count for 5 characters, this should be false.</param>
        /// <returns>TreeBank node denoting the minimal subsumer of the characters, or null if none exists</returns>
        public TreeBankNode GetMinimallySubsumingNode(int startCharacter, int endCharacter, bool throwExceptionOnBadAlignment, bool unescapeBraces)
        {
            Set<TreeBankEngine.SyntacticCategory> bracketCategories = new Set<TreeBankEngine.SyntacticCategory>(new TreeBankEngine.SyntacticCategory[]{
                TreeBankEngine.SyntacticCategory.RightCurlyBracket, TreeBankEngine.SyntacticCategory.RightRoundBracket, TreeBankEngine.SyntacticCategory.RightSquareBracket,
                TreeBankEngine.SyntacticCategory.LeftCurlyBracket, TreeBankEngine.SyntacticCategory.LeftRoundBracket, TreeBankEngine.SyntacticCategory.LeftSquareBracket});

            int currCharacter = 0;
            TreeBankNode startToken = null;
            TreeBankNode endToken = null;
            foreach (TreeBankNode token in Tokens)
            {
                if (currCharacter == startCharacter)
                    startToken = token;

                // get token text, which has no spaces in it...unescape brackets if needed
                string tokenText = token.SurfaceText;
                if (unescapeBraces)
                    tokenText = TreeBankEngine.UnescapeBrackets(tokenText);

                currCharacter += tokenText.Length - 1;

                if (currCharacter == endCharacter)
                    endToken = token;

                // we can stop checking if we found the start and end characters
                if (startToken != null && endToken != null)
                    break;

                // increment to start of next word
                ++currCharacter;
            }

            // make sure we got a start and end token
            if (startToken == null || endToken == null)
                if (throwExceptionOnBadAlignment)
                    throw new NullReferenceException("Invalid start or end character");
                else
                    return null;

            return GetMinimallySubsumingNode(startToken, endToken);
        }

        /// <summary>
        /// Gets the minial subsumer of the given span (only valid for root nodes). Returns null if such a node doesn't exist.
        /// </summary>
        /// <param name="firstTokenNumber">First token in span</param>
        /// <param name="lastTokenNumber">Last token in span</param>
        /// <returns>Minimal subsumer of given span, or null if no such node exists</returns>
        public TreeBankNode GetMinimallySubsumingNode(int firstTokenNumber, int lastTokenNumber)
        {
            if (!IsRoot)
                throw new Exception("Only valid for root nodes");

            TreeBankNode lcaOfTokens = GetLowestCommonAncestorOfTokens(firstTokenNumber, lastTokenNumber);
            if (lcaOfTokens == null)
                throw new Exception("No LCA of tokens was found");

            // return null if span is not minimal
            if (lcaOfTokens.FirstToken.TokenNumber != firstTokenNumber || lcaOfTokens.LastToken.TokenNumber != lastTokenNumber)
                return null;

            return lcaOfTokens;
        }

        /// <summary>
        /// Gets whether or not the current node covers the same tokens as another node. Nodes may be from different sentences 
        /// or documents - this function will return false in all such cases.
        /// </summary>
        /// <param name="node">Node to check the current node's token span against</param>
        /// <returns>True if the spans are equal, false otherwise</returns>
        public bool CoversSameTokensAs(TreeBankNode node)
        {
            if (IsNullElement || node.IsNullElement)
                throw new Exception("Null-element nodes cannot be used in CoversSameTokenSpanAs");

            // make sure nodes are from the same sentence in the same document...allow object-distinct root references
            if (!InSameSentenceAs(node, true))
                return false;

            return FirstToken.TokenNumber == node.FirstToken.TokenNumber && LastToken.TokenNumber == node.LastToken.TokenNumber;
        }

        /// <summary>
        /// Checks whether or not the current node is in the same sentence as another node. 
        /// sentence numbers are the same. 
        /// </summary>
        /// <param name="node">Node to check the current one against</param>
        /// <param name="allowObjectDistinctRoot">Whether or not to allow root nodes to be object-distinct. If this is true, the
        /// function will return true as long as the MRG file names and sentence numbers are the same. NOTE:  "file names" not 
        /// "entire paths". This allows the use of TreeBanked documents that have, for example, been parsed by different parsers 
        /// and are contained in different directory structures. As long as the file names match, we assume the documents are the 
        /// same. If this parameter is false, the current and given nodes' root nodes must have the same object reference.</param>
        /// <returns>True if nodes are in the same sentence, false otherwise</returns>
        public bool InSameSentenceAs(TreeBankNode node, bool allowObjectDistinctRoot)
        {
            // if we're allowing the roots to refer to different objects, just check the MRG file and sentence
            if (allowObjectDistinctRoot)
            {
                // if entire MRG paths aren't equal...
                if (MrgFile != node.MrgFile)
                {
                    // ...check just the file names
                    string fileName1 = Path.GetFileName(MrgFile).Trim();
                    string fileName2 = Path.GetFileName(node.MrgFile).Trim();

                    // if the file names aren't equal either, return false
                    if (fileName1 != fileName2)
                        return false;
                }

                return SentenceNumber == node.SentenceNumber;
            }
            // otherwise, just check the object references
            else
                return _root == node.Root;
        }

        /// <summary>
        /// Adds a leaf node to this node (only valid for root nodes, as they're the ones keeping track of the leaves)
        /// </summary>
        /// <param name="leafNode">Leaf node to add</param>
        public void AddLeaf(TreeBankNode leafNode)
        {
            if (!IsRoot || !leafNode.IsLeaf)
                throw new Exception("Invalid add leaf call");

            if (_leafNodes == null)
                _leafNodes = new TreeBankNodeList();

            _leafNodes.Add(leafNode);
        }

        /// <summary>
        /// Adds a null leaf number to this node (only valid for root nodes, as they're the ones keeping track of the leaves)
        /// </summary>
        /// <param name="leafNumber">Null leaf number to add</param>
        public void AddNullLeafNumber(int leafNumber)
        {
            if (!IsRoot)
                throw new Exception("Not called on root");

            if (leafNumber < 0)
                throw new Exception("Invalid leaf number");

            if (_nullLeafNumbers == null)
                _nullLeafNumbers = new List<int>();

            _nullLeafNumbers.Add(leafNumber);
        }
        
        /// <summary>
        /// Gets the token overlap as measured by Dice's coefficient. Returns zero if the nodes are from different sentences.
        /// </summary>
        /// <param name="node">Node to measure overlap with</param>
        /// <returns>Token overlap as measured by Dice's coefficient</returns>
        public float GetTokenOverlapDiceCoefficientWith(TreeBankNode node)
        {
            // make sure the nodes are from the same sentence
            if (!InSameSentenceAs(node, true))
                return 0;

            // get token numbers under the current node
            Set<int> tokens1 = new Set<int>(Tokens.Select(token => token.TokenNumber).ToArray());

            // get token numbers under the given node
            Set<int> tokens2 = new Set<int>(node.Tokens.Select(token => token.TokenNumber).ToArray());

            // dice:  (2 * size of intersection) / (number of all tokens)
            return (2 * tokens1.Intersect(tokens2).Count()) / (float)(tokens1.Count + tokens2.Count);
        }

        /// <summary>
        /// Sets the surface text of this node to a value that may contain whitespace. The SurfaceText property does not 
        /// allow whitespace to be conservative in its handling of the Penn TreeBank; however, there are cases when a leaf 
        /// node should be allowed to have whitespace in it, e.g., when abstracting a tree. This method is only valid for 
        /// leaf nodes.
        /// </summary>
        /// <param name="text">Text to use</param>
        public void SetSurfaceTextWithWhitespace(string text)
        {
            if (text == null)
                throw new Exception("Invalid text");

            if (IsLeaf)
                _surfaceText = text;
            else
                throw new Exception("Current node must be a leaf");
        }

        /// <summary>
        /// Tests all TreeBank functionality of node
        /// </summary>
        public virtual void Test()
        {
            if (Root == null)
                throw new Exception();

            if (!IsRoot)
            {
                if (Root != Parent.Root)
                    throw new Exception();
            }
            else
                if (Root != this)
                    throw new Exception();

            string text = SurfaceText;

            if (!File.Exists(MrgFile))
                throw new Exception();

            if (SentenceNumber < 0)
                throw new Exception();

            if (!IsNullElement && Head == null)
                throw new Exception();

            if (!IsNullElement && HeadToken == null)
                throw new Exception();

            if (!IsNullElement && SemanticHeadToken == null)
                throw new Exception();

            if (IsLeaf)
                if (LeafNumber != Root.GetLeaf(LeafNumber).LeafNumber)
                    throw new Exception();

            if (IsToken)
            {
                if (TokenNumber != Root.GetToken(TokenNumber).TokenNumber)
                    throw new Exception("Token number mismatch");

                // get start character indexes for each token in tree containing the head
                Dictionary<int, int> tokenStartCharacter = new Dictionary<int, int>();
                Dictionary<int, int> tokenEndCharacter = new Dictionary<int, int>();
                int currChar = 0;
                foreach (TreeBankNode token in Root.Tokens)
                {
                    tokenStartCharacter.Add(token.TokenNumber, currChar);

                    currChar += token.SurfaceText.Replace(" ", "").Length;

                    tokenEndCharacter.Add(token.TokenNumber, currChar - 1);
                }

                if (TokenStartCharacter != tokenStartCharacter[TokenNumber] ||
                    TokenEndCharacter != tokenEndCharacter[TokenNumber])
                    throw new Exception();
            }

            // check leaf nodes
            int lastLeafNum = -1;
            foreach (TreeBankNode leaf in Leaves)
            {
                if (lastLeafNum == -1)
                {
                    lastLeafNum = leaf.LeafNumber;
                    continue;
                }

                if (leaf.LeafNumber != lastLeafNum + 1)
                    throw new Exception();

                ++lastLeafNum;

                if (leaf.LeafNumber != Root.GetLeaf(leaf.LeafNumber).LeafNumber)
                    throw new Exception();
            }

            if (FirstLeafNumber != Leaves[0].LeafNumber ||
                LastLeafNumber != Leaves[Leaves.Length - 1].LeafNumber)
                throw new Exception();

            if (!IsNullElement)
                if (!CoversSameTokensAs(this))
                    throw new Exception();

            TreeBankNode testNode = null;
            TreeBankNode[] testArray = null;
            TreeBankNodeList testList = null;

            int i = ChildIndex;
            i = Depth;
            testArray = Ancestors;
            testArray = Descendants;

            if (Descendants.Length != TotalNodesInTree)
                throw new Exception();

            testArray = DirectRelatives;
            testNode = FirstLeaf;
            i = FirstLeafNumber;
            testNode = FirstToken;
            GetBracketedText(true, true);
            GetCategoryDescriptor(TreeBankEngine.SyntacticCategoryDescriptor.Integer);
            GetCfgRule(TreeBankEngine.SyntacticCategoryDescriptor.Integer);
            testList = RootLeaves;
            testArray = LeftSiblings;
            testList = PassiveVerbs;
            testArray = RightSiblings;
            testList = RootLeaves;
            testArray = Siblings;
            testArray = Tokens;

            if (TotalNodesInTree != AllNodes.Length)
                throw new Exception();

            if (IsRoot)
            {
                TreeBankNode n2;
                TryGetToken(1000, out n2);
                TryGetToken(5, out n2);
            }

            if (Root.GetNode(Location) != this)
                throw new Exception("Error");

            // test child nodes
            int totalDescendentNodes = 1;
            IEnumerator<TreeBankNode> children = Children;
            while (children.MoveNext())
            {
                TreeBankNode child = children.Current;

                if (child == null)
                    throw new Exception();

                child.Test();

                totalDescendentNodes += child.AllNodes.Length;
            }

            if (totalDescendentNodes != TotalNodesInTree ||
                totalDescendentNodes != Descendants.Length)
                throw new Exception();

            for (i = 0; i < Tokens.Length; ++i)
            {
                if (i > 0 && Tokens[i].TokenDistanceFrom(Tokens[i - 1]) != 1)
                    throw new Exception();

                if (i < Tokens.Length - 1 && Tokens[i].TokenDistanceFrom(Tokens[i + 1]) != -1)
                    throw new Exception();
            }
        }

        #region GraphViz Members
        /// <summary>
        /// Gets the GraphViz node ID for this node
        /// </summary>
        private string GraphVizNodeID
        {
            get
            {
                string id = "";

                // if the node is TreeBanked, use MRG file and sentence number in id
                if (IsTreeBanked)
                    id = Path.GetFileNameWithoutExtension(MrgFile) + ":" + SentenceNumber + ":";

                // always use location
                id = "\"" + id + Location + "\"";

                return id;
            }
        }

        /// <summary>
        /// Gets Dot graph specification for this node and all nodes under it
        /// </summary>
        /// <returns>Dot graph specification</returns>
        public string GetDotGraphSpecification()
        {
            StringBuilder graphSpec = new StringBuilder();

            // special start for root nodes
            if (IsRoot)
                graphSpec.Append("graph g { splines = line\n");

            string nodeSize = "width = 0, height = 0";
            string nodeMargin = "margin = 0.11,0.055";
            string nodeShape = "shape = rectangle";
            string nodeLabel = "label = NULL";
            string nodeFontSize = "fontsize = 12";  // the default font size results in the labels being cut off on Windows machines

            // must check null element before getting mnemonic...
            if (!IsNullElement)
                nodeLabel = "label = \"" + TreeBankEngine.GetMnemonicFor(_category) + "\"";

            graphSpec.Append(GraphVizNodeID + " [" + nodeFontSize + "," + nodeLabel + ", " + nodeSize + ", " + nodeMargin + ", " + nodeShape + "];\n");

            string edgeWeight = "weight = 50";
            string edgeLength = "length = 0.5";

            // add leaf nodes
            if (IsLeaf)
            {
                string leafID = "\"" + GraphVizNodeID.Trim('"') + "_leaf\"";

                string leafLabel = null;

                // if node is null, use asterisk for node text
                if (IsNullElement)
                    leafLabel = "label = \"*\"";
                // otherwise, use surface text
                else
                    leafLabel = "label = \"" + SurfaceText + "\"";

                graphSpec.Append(leafID + " [" + nodeFontSize + "," + leafLabel + ", " + nodeSize + ", " + nodeMargin + ", " + nodeShape + "];\n");
                graphSpec.Append(GraphVizNodeID + " -- " + leafID + " [" + edgeWeight + ", " + edgeLength + "]\n");
            }

            // add edges to child nodes and all child graph specs
            foreach (TreeBankNode child in _children)
            {
                graphSpec.Append(GraphVizNodeID + " -- " + child.GraphVizNodeID + "[" + edgeWeight + ", " + edgeLength + "]\n");
                graphSpec.Append(child.GetDotGraphSpecification());
            }

            if (IsRoot)
                graphSpec.Append("}");

            return graphSpec.ToString();
        }
        #endregion
    }
}
