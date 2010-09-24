using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Runtime.Serialization.Formatters.Binary;
using System.Diagnostics;
using System.Collections.ObjectModel;
using System.Linq;

using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Provides access to TreeBank layer of information.
    /// </summary>
    public class TreeBankEngine
    {
        #region static members
        private static Regex _whitespaceRE;
        private static Dictionary<string, char> _expandedBracketChar;
        private static Dictionary<char, string> _charExpandedBracket;

        #region parse tree node extraction
        /// <summary>
        /// For grabbing head index number:  "H:" followed by numbers, possibly some spaces, and a non-whitespace, non-right paren character
        /// </summary>
        private static Regex _headIndexRE = new Regex(@"(?<marker>H:(?<index>[0-9]+))\s*[^\s]");

        // null element leaf text REs
        private static Regex _understoodNullElementRE = new Regex(@"^(\*U\*)|(\*)$");
        private static Regex _coIndexRE = new Regex(@"^(\*T)?\*-(?<coindex>[0-9]+)$");
        private static Regex _zeroVariantRE = new Regex("^0$");
        private static Regex _pseudoAttachmentRE = new Regex(@"^\*(?<attachment_type>(ICH)|(PPA)|(RNR)|(EXP))\*-(?<coindex>[0-9]+)$");

        /// <summary>
        /// Extracts node from raw parse text
        /// </summary>
        /// <param name="parseText">Parse text</param>
        /// <param name="permitWhitespaceInLeaves">Whether or not to permit whitespace in leaf nodes</param>
        /// <returns>TreeBankNode</returns>
        public static TreeBankNode ExtractNode(string parseText, bool permitWhitespaceInLeaves)
        {
            int leafNum = 0;
            int tokenNum = 0;
            int tokenStartCharacter = 0;
            TreeBankNodeList nullElementNodes = new TreeBankNodeList();
            TreeBankNode root = ExtractNode(new RawParseText(parseText), null, permitWhitespaceInLeaves, ref leafNum, ref tokenNum, ref tokenStartCharacter, ref nullElementNodes);

            // process null element nodes
            foreach (TreeBankNode n in nullElementNodes)
                if (n.Category != SyntacticCategory.NullElement)
                    throw new Exception("Expected null element node");
                else
                {
                    Match m;
                    string text = n.SurfaceText;

                    if (_understoodNullElementRE.Match(text).Success)
                        n.Category = SyntacticCategory.UnderstoodSubject;
                    else if (_zeroVariantRE.Match(text).Success)
                        n.Category = SyntacticCategory.ZeroVariant;
                    else if ((m = _pseudoAttachmentRE.Match(text)).Success)
                    {
                        Group indexGroup = m.Groups["coindex"];
                        if (!indexGroup.Success)
                            throw new Exception("Failed to get index");

                        int index = int.Parse(indexGroup.Value);

                        n.Category = SyntacticCategory.PseudoAttachment;
                        n.CoIndexReferent = n.Root.GetCoIndexedNode(index);
                    }
                    else if ((m = _coIndexRE.Match(text)).Success)
                    {
                        Group indexGroup = m.Groups["coindex"];
                        if (!indexGroup.Success)
                            throw new Exception("Failed to get index");

                        int index = int.Parse(indexGroup.Value);

                        n.Category = SyntacticCategory.CoIndexed;
                        n.CoIndexReferent = n.Root.GetCoIndexedNode(index);
                    }
                    else
                        throw new Exception("Failed to match null element text");

                    // reset surface text for null elements...they have none
                    n.SurfaceText = "";
                }

            return root;
        }

        /// <summary>
        /// Extracts node from raw parse text
        /// </summary>
        /// <param name="rawParse">Parse text</param>
        /// <param name="parent">Parent node of extracted node</param>
        /// <param name="permitWhitespaceInLeaves">Whether or not to permit whitespace in leaf nodes</param>
        /// <param name="currentLeafNumber">Current leaf number of parse</param>
        /// <param name="currentTokenNumber">Current token number of parse</param>
        /// <param name="currentTokenStartCharacter">Current start character of token nodes. This only include non-whitespace
        /// characters. See TokenStartCharacter for more on what this gets used for.</param>
        /// <param name="nullElementNodes">List of null element nodes in tree</param>
        /// <returns>TreeBankNode</returns>
        private static TreeBankNode ExtractNode(RawParseText rawParse,
                                                TreeBankNode parent,
                                                bool permitWhitespaceInLeaves,
                                                ref int currentLeafNumber,
                                                ref int currentTokenNumber,
                                                ref int currentTokenStartCharacter,
                                                ref TreeBankNodeList nullElementNodes)
        {
            if (rawParse == null || rawParse.Length == 0)
                throw new Exception("Invalid parse text");

            // create node
            TreeBankNode currNode = new TreeBankNode(parent);

            // set first leaf number
            currNode.FirstLeafNumber = currentLeafNumber;

            // the next printable character must be an opening parenthesis
            int parenIndex = GetNextNonWhitespaceIndex(rawParse, 0);
            if (rawParse[parenIndex] != '(')
                throw new Exception("Invalid parse");

            // get node label
            int labelStart = GetNextNonWhitespaceIndex(rawParse, parenIndex + 1);
            int labelEnd = GetNextWhitespaceIndex(rawParse, labelStart) - 1;
            string label = rawParse.Substring(labelStart, labelEnd - labelStart + 1);

            // trim off leading and trailing '-' for special labels like -NONE-, -LRB-, -RRB-, etc.
            label = label.Trim('-');

            // split on internal '-'
            string[] labelParts = label.Split('-');

            // get tag...watch out for ambiguous tags (separated by '|')
            string[] tags = labelParts[0].Split('|');
            currNode.Category = GetSyntacticCategory(tags[0]);

            // add ambiguous tags if any
            for (int i = 1; i < tags.Length; ++i)
                currNode.AmbiguousWithCategories.Add(GetSyntacticCategory(tags[i]));

            // get functions and co-index
            for (int i = 1; i < labelParts.Length; ++i)
            {
                string labelPart = labelParts[i];

                // if we can parse the label segment as an integer, it's a co-index
                int id;
                if (int.TryParse(labelPart, out id))
                    currNode.CoIndexId = id;
                // otherwise it's a function
                else
                    currNode.Functions.Add(GetFunction(labelPart));
            }

            // advance to next non-whitespace, which is the start of either a child node or leaf text
            int childStartCharIndex = GetNextNonWhitespaceIndex(rawParse, labelEnd + 1);

            /* Check for head index. This is not present in TreeBank proper, but is used by some syntactic parsers (e.g., Charniak's)
             * to indicate which child is the syntactic head */
            int headIndex = -1;
            Match headIndexMatch;
            if ((headIndexMatch = rawParse.GetMatch(_headIndexRE, childStartCharIndex)).Success)
            {
                Group markerGroup = headIndexMatch.Groups["marker"];
                Group indexGroup = headIndexMatch.Groups["index"];

                if (!markerGroup.Success || !indexGroup.Success)
                    throw new Exception("Failed to match head index");

                headIndex = int.Parse(indexGroup.Value);

                // move to next non-whitespace character after the head index marker
                childStartCharIndex = GetNextNonWhitespaceIndex(rawParse, childStartCharIndex + markerGroup.Length);

                // we should never see this...indicates that we parsed actual leaf text of "H:<number>" as a head index
                if (rawParse[childStartCharIndex] == ')')
                    throw new Exception("Parsed leaf text as head index");
            }

            // get children or leaf node text
            bool hasChildren = false;
            bool isLeaf = false;
            while (true)
            {
                char childStartChar = rawParse[childStartCharIndex];

                // check for inner node
                if (childStartChar == '(')
                {
                    // we should get a leaf or a child node, but not both
                    if (isLeaf)
                        throw new Exception("Unexpected child node");

                    // null elements are always leaves
                    if (currNode.Category == SyntacticCategory.NullElement)
                        throw new Exception("Found non-leaf null element");

                    hasChildren = true;

                    // get end of inner node child text
                    int childEndCharIndex = IndexOfBalancingParen(rawParse, childStartCharIndex);

                    // get inner node child text
                    RawParseText childText = new RawParseText(rawParse.Substring(childStartCharIndex, childEndCharIndex - childStartCharIndex + 1));

                    // extract nodes
                    TreeBankNode childNode = ExtractNode(childText, currNode, permitWhitespaceInLeaves, ref currentLeafNumber, ref currentTokenNumber, ref currentTokenStartCharacter, ref nullElementNodes);
                    currNode.AddChildNode(childNode);

                    // move to next character
                    childStartCharIndex = childEndCharIndex + 1;
                }
                // otherwise, we're at a leaf node
                else
                {
                    // we should get a leaf or a child node, but not both
                    if (hasChildren)
                        throw new Exception("Unexpected leaf text");

                    // shouldn't get pieces of leaf text
                    if (isLeaf)
                        throw new Exception("Multiple pieces of leaf text");

                    isLeaf = true;

                    // start of leaf text is the child start
                    int leafTextStart = childStartCharIndex;

                    // find end of leaf text, which must come immediately before the end parenthesis
                    int endParenIndex = NextParenthesis(rawParse, leafTextStart);
                    int leafTextEnd = endParenIndex - 1;
                    if (rawParse[endParenIndex] != ')')
                        throw new Exception("Invalid leaf node");

                    // set leaf node text...if we are permitting whitespace in leaves, use the appropriate method
                    string leafText = rawParse.Substring(leafTextStart, leafTextEnd - leafTextStart + 1);
                    if (permitWhitespaceInLeaves)
                        currNode.SetSurfaceTextWithWhitespace(leafText);
                    else
                        currNode.SurfaceText = leafText;

                    // set leaf number
                    currNode.LeafNumber = currentLeafNumber++;

                    // if this is a null-element node, add to lists
                    if (currNode.IsNullElement)
                    {
                        currNode.Root.AddNullLeafNumber(currNode.LeafNumber);
                        nullElementNodes.Add(currNode);
                    }
                    // otherwise, we're at a token, so update the corresponding counters
                    else
                    {
                        currNode.TokenNumber = currentTokenNumber++;
                        currNode.TokenStartCharacter = currentTokenStartCharacter;

                        // update current start character...it's possible to have whitespace in surface text (see permitWhitespaceInLeaves)
                        currentTokenStartCharacter += currNode.SurfaceText.Replace(" ", "").Length;
                    }

                    currNode.Root.AddLeaf(currNode);

                    // move to next character
                    childStartCharIndex = endParenIndex + 1;
                }

                // move to next non-whitespace character
                childStartCharIndex = GetNextNonWhitespaceIndex(rawParse, childStartCharIndex);

                // if we didn't find another character or we found the end paren for the current node, we're done
                if (childStartCharIndex == -1 || rawParse[childStartCharIndex] == ')')
                    break;
            }

            currNode.LastLeafNumber = currentLeafNumber - 1;

            // set head index if we had one
            if (headIndex >= 0)
                // leaf nodes have no children, and are their own heads
                if (currNode.IsLeaf)
                {
                    // double-check index
                    if (headIndex != 0)
                        throw new Exception("Leaf head indexes must be zero");
                    else
                        currNode.Head = currNode;
                }
                // inner nodes have children and thus have head nodes
                else
                {
                    currNode.Head = currNode.GetChild(headIndex);

                    // cannot preset to a null-element head node
                    if (currNode.Head.IsNullElement)
                        throw new Exception("Cannot preset to a null-element head node");
                }

            return currNode;
        }

        /// <summary>
        /// Gets the next printable (non-whitespace) character within a piece of text
        /// </summary>
        /// <param name="text">Text to search</param>
        /// <param name="start">Where to start search</param>
        /// <returns>Index of first non-whitespace character within text at or beyond specified start</returns>
        private static int GetNextNonWhitespaceIndex(RawParseText text, int start)
        {
            while (start < text.Length && _whitespaceRE.Match(text[start].ToString()).Success)
                ++start;

            if (start == text.Length)
                start = -1;

            return start;
        }

        /// <summary>
        /// Gets the next whitespace character within a piece of text
        /// </summary>
        /// <param name="text">Text to search</param>
        /// <param name="start">Where to start search</param>
        /// <returns>Index of first whitespace character within text at or beyond specified start</returns>
        private static int GetNextWhitespaceIndex(RawParseText text, int start)
        {
            while (start < text.Length && !_whitespaceRE.Match(text[start].ToString()).Success)
                ++start;

            if (start == text.Length)
                start = -1;

            return start;
        }

        /// <summary>
        /// Gets the next parenthesis within a piece of text
        /// </summary>
        /// <param name="text">Text to search</param>
        /// <param name="start">Where to start search</param>
        /// <returns>Index of first parenthesis within text at or beyond specified start</returns>
        private static int NextParenthesis(RawParseText text, int start)
        {
            while (start < text.Length && text[start] != '(' && text[start] != ')')
                ++start;

            if (start == text.Length)
                throw new Exception("Failed to get next paren");

            return start;
        }

        /// <summary>
        /// Gets the index of the parenthesis that balances the parenthesis at a specified position
        /// </summary>
        /// <param name="text">Text to search</param>
        /// <param name="startParenIndex">Where to start search</param>
        /// <returns>Index of balancing parenthesis</returns>
        public static int IndexOfBalancingParen(RawParseText text, int startParenIndex)
        {
            if (text[startParenIndex] != '(')
                throw new Exception("Invalid start");

            // check paren balance
            int parenBal = 0;
            for (int i = startParenIndex; i < text.Length; ++i)
            {
                char c = text[i];
                parenBal += c == '(' ? 1 :
                            (c == ')' ? -1 : 0);

                if (parenBal == 0)
                    return i;
            }

            throw new Exception("Failed to get balancing paren");
        }
        #endregion

        #region head finding
        private static Dictionary<string, ChildSearch> _headFindingRules;
        private static List<ChildSearch> _npHeadFindingRules;

        /// <summary>
        /// Finds syntactic head using Collins's 1999 dissertation rules for identifying heads
        /// </summary>
        /// <param name="node">Node to find head for</param>
        /// <param name="excludeNodes">Nodes to exclude from the search</param>
        /// <returns>Syntactic head</returns>
        public static TreeBankNode FindHead(TreeBankNode node, Set<TreeBankNode> excludeNodes)
        {
            // use empty set of exclusion nodes if none was passed
            if (excludeNodes == null)
                excludeNodes = new Set<TreeBankNode>();

            // leaves are their own heads
            if (node.IsLeaf)
                return node;
            // null-element nodes have no head
            else if (node.IsNullElement)
                return null;

            // the default head is left-most, non-null-element child that isn't excluded
            TreeBankNode head = null;
            for (int i = 0; i < node.ChildCount; ++i)
            {
                TreeBankNode child = node.GetChild(i);
                if (!excludeNodes.Contains(child) && !child.IsNullElement)
                {
                    head = child;
                    break;
                }
            }

            /* if we don't have any non-null children, none of the child search rules will match and 
             * it doesn't make sense to return a null-element head node, so return a null reference */
            if (head == null)
                return null;

            string cat = GetMnemonicFor(node.Category);

            // NPs are special
            if (cat == "NP")
            {
                // try each NP rule
                foreach (ChildSearch search in _npHeadFindingRules)
                {
                    TreeBankNode result = search.Run(node, excludeNodes);
                    if (result != null)
                    {
                        head = result;
                        break;
                    }
                }
            }
            // use regular head rules
            else if (_headFindingRules.ContainsKey(cat))
            {
                ChildSearch search = _headFindingRules[cat];
                TreeBankNode result = search.Run(node, excludeNodes);
                if (result != null)
                    head = result;
            }

            // check for CC condition
            int index = head.ChildIndex;
            if (index >= 2 && node.GetChild(index - 1).Category == TreeBankEngine.SyntacticCategory.CoordinatingConjunction)
                head = node.GetChild(index - 2);

            return head;
        }
        #endregion

        #region syntactic categories, grammatical functions, and accessors
        private static Dictionary<string, GrammaticalFunction> _mnemonicFunction;
        private static Dictionary<string, SyntacticCategory> _mnemonicCategory;
        private static Dictionary<SyntacticCategory, string> _categoryMnemonic;
        private static Dictionary<int, SyntacticCategory> _numberCategory;
        private static Set<SyntacticCategory> _clauseCategories;
        private static Set<SyntacticCategory> _phraseCategories;
        private static Set<SyntacticCategory> _terminalCategories;

        /// <summary>
        /// Gets set of clause categories. It is possible to modify the returned collection, but you do this at your own peril!
        /// </summary>
        public static Set<SyntacticCategory> ClauseCategories
        {
            get { return TreeBankEngine._clauseCategories; }
        }

        /// <summary>
        /// Gets set of phrase categories. It is possible to modify the returned collection, but you do this at your own peril!
        /// </summary>
        public static Set<SyntacticCategory> PhraseCategories
        {
            get { return TreeBankEngine._phraseCategories; }
        }

        /// <summary>
        /// Gets set of terminal categories. It is possible to modify the returned collection, but you do this at your own peril!
        /// </summary>
        public static Set<SyntacticCategory> TerminalCategories
        {
            get { return TreeBankEngine._terminalCategories; }
        }

        /* See "Bracketing Guidelines for Treebank II Style Penn Treebank Project" for 
         * information about the following enumerations */

        /// <summary>
        /// Grammatical functions
        /// </summary>
        public enum GrammaticalFunction
        {
            Adverbial, NonNpNounPhrase, Dative, LogicalPassiveSubject, SecondaryPredication,
            Put, SurfaceSubject, TopicalFrontedConstituent, Vocative, Benefactive,
            Direction, Extent, Location, Manner, Purpose, Temporal, CloselyRelated,
            TrueCleft, Headline, Title,

            #region functions added for brown corpus
            // see http://www.ldc.upenn.edu/Catalog/docs/LDC99T42/changes.txt
            Imperative, Unfinished, Etcetera,

            // a terrible hack for Brown CL23.mrg sentence 69
            Check, If, Below
            #endregion
        }

        /// <summary>
        /// Syntactic categories
        /// </summary>
        public enum SyntacticCategory
        {
            Unknown,

            // clause level
            SimpleDeclarativeClause, SubConClause, WhQuestionClause, SubAuxInvClause,
            WhQuestionNoWhClause, ReducedRelativeClause,

            // phrase level
            AdjectivePhrase, AdverbPhrase, ConjunctionPhrase, Fragment, Interjection,
            ListMarker, NonConstituent, NounPhrase, NpHead, PrepPhrase, ParentheticalPhrase,
            ParticlePhrase, QuantifierPhrase, UnlikeCoordinatedPhrase, VerbPhrase, WhAdjectivePhrase, WhAdverbPhrase,
            WhNounPhrase, WhPrepPhrase,

            // word level
            ParticleWord, CoordinatingConjunction, CardinalNumber, Determiner, ExistentialThere, ForeignWord, Preposition, Adjective,
            AdjectiveComparative, AdjectiveSuperlative, ListItemMarker, Modal, NounSingular, NounPlural,
            NounProperSingular, NounProperPlural, PreDeterminer, PersonalPronoun,
            PossessivePronoun, Adverb, AdverbComparative, AdverbSuperlative, To, InterjectionWord,
            VerbBase, VerbPast, VerbGerund, VerbPastParticiple, VerbNon3rdSingularPresent,
            Verb3rdSingularPresent, WhDeterminer, WhPronoun, WhPronounPossessive, WhAdverb, Possessive,
            Comma, Colon, SemiColon, StraightDoubleQuote, LeftSingleQuote, LeftDoubleQuote,
            RightSingleQuote, RightDoubleQuote, Pound, Dollar, SentenceFinal,
            LeftRoundBracket, RightRoundBracket, LeftSquareBracket, RightSquareBracket, LeftCurlyBracket,
            RightCurlyBracket, Symbol,

            // word level - used by Charniak's parser...see "A Maximum Entropy-Inspired Parser"
            AuxiliaryVerb,
            AuxiliaryVerbG,

            /// <summary>
            /// General null element tag 
            /// </summary>
            NullElement,

            /***** the following are specific null element tags *****/

            /// <summary>
            /// Used for tracking wh-traces, passive movement, etc.
            /// </summary>
            CoIndexed,

            /// <summary>
            /// Zero-variant of "that" in subordinate clauses
            /// </summary>
            ZeroVariant,

            /// <summary>
            /// Understood subject of infinitive or imperative
            /// </summary>
            UnderstoodSubject,

            /// <summary>
            /// Pseudo-attachment markers
            /// </summary>
            PseudoAttachment,

            #region syntactic categories added for brown corpus
            /* added for brown corpus...see http://www.ldc.upenn.edu/Catalog/docs/LDC99T42/changes.txt for the
             * first two. not sure where neg and adv came from */
            Typo, Edited, Neg, Adv
            #endregion
        };

        #region part of speech lists
        /// <summary>
        /// All noun parts of speech
        /// </summary>
        private static Set<SyntacticCategory> _nounPartsOfSpeech = new Set<SyntacticCategory>(new SyntacticCategory[]{
                                                                           SyntacticCategory.NounPlural, 
                                                                           SyntacticCategory.NounProperPlural,
                                                                           SyntacticCategory.NounProperSingular, 
                                                                           SyntacticCategory.NounSingular});
        /// <summary>
        /// Gets all noun parts of speech
        /// </summary>
        public static Set<SyntacticCategory> NounPartsOfSpeech
        {
            get { return TreeBankEngine._nounPartsOfSpeech; }
        }

        /// <summary>
        /// All verb parts of speech
        /// </summary>
        private static Set<SyntacticCategory> _verbPartsOfSpeech = new Set<SyntacticCategory>(new SyntacticCategory[]{
                                                                           SyntacticCategory.Verb3rdSingularPresent, 
                                                                           SyntacticCategory.VerbBase,
                                                                           SyntacticCategory.VerbGerund, 
                                                                           SyntacticCategory.VerbNon3rdSingularPresent,
                                                                           SyntacticCategory.VerbPast, 
                                                                           SyntacticCategory.VerbPastParticiple,
                                                                           SyntacticCategory.AuxiliaryVerb,
                                                                           SyntacticCategory.AuxiliaryVerbG});
        /// <summary>
        /// Gets all verb parts of speech
        /// </summary>
        public static Set<SyntacticCategory> VerbPartsOfSpeech
        {
            get { return TreeBankEngine._verbPartsOfSpeech; }
        }

        /// <summary>
        /// All adverb parts of speech
        /// </summary>
        private static Set<SyntacticCategory> _adverbPartsOfSpeech = new Set<SyntacticCategory>(new SyntacticCategory[] { 
                                                                             SyntacticCategory.Adverb, 
                                                                             SyntacticCategory.AdverbComparative, 
                                                                             SyntacticCategory.AdverbSuperlative });
        /// <summary>
        /// Gets all adverb parts of speech
        /// </summary>
        public static Set<SyntacticCategory> AdverbPartsOfSpeech
        {
            get { return TreeBankEngine._adverbPartsOfSpeech; }
        }

        /// <summary>
        /// All adjective parts of speech
        /// </summary>
        private static Set<SyntacticCategory> _adjectivePartsOfSpeech = new Set<SyntacticCategory>(new SyntacticCategory[] { 
                                                                                SyntacticCategory.Adjective, 
                                                                                SyntacticCategory.AdjectiveComparative, 
                                                                                SyntacticCategory.AdjectiveSuperlative });
        /// <summary>
        /// Gets all adjective parts of speech
        /// </summary>
        public static Set<SyntacticCategory> AdjectivePartsOfSpeech
        {
            get { return TreeBankEngine._adjectivePartsOfSpeech; }
        }

        /// <summary>
        /// All punctuation parts of speech
        /// </summary>
        private static Set<SyntacticCategory> _punctuationCategories = new Set<SyntacticCategory>(new SyntacticCategory[] {
                    SyntacticCategory.Comma, SyntacticCategory.Colon, SyntacticCategory.SemiColon, 
                    SyntacticCategory.StraightDoubleQuote, SyntacticCategory.LeftSingleQuote, SyntacticCategory.LeftDoubleQuote,
                    SyntacticCategory.RightSingleQuote, SyntacticCategory.RightDoubleQuote, SyntacticCategory.Pound, 
                    SyntacticCategory.Dollar, SyntacticCategory.SentenceFinal, SyntacticCategory.LeftRoundBracket, 
                    SyntacticCategory.RightRoundBracket, SyntacticCategory.LeftSquareBracket, SyntacticCategory.RightSquareBracket, 
                    SyntacticCategory.LeftCurlyBracket, SyntacticCategory.RightCurlyBracket, SyntacticCategory.Symbol});

        /// <summary>
        /// Gets all punctuation parts of speech
        /// </summary>
        public static Set<SyntacticCategory> PunctuationCategories
        {
            get { return TreeBankEngine._punctuationCategories; }
        }
        #endregion

        /// <summary>
        /// Gets the mnemonics for all syntactic categories
        /// </summary>
        public static IEnumerable<string> SyntacticCategoryMnemonics
        {
            get { return _mnemonicCategory.Keys; }
        }

        /// <summary>
        /// Gets all syntactic categories
        /// </summary>
        public static IEnumerable<SyntacticCategory> SyntacticCategories
        {
            get { return _categoryMnemonic.Keys; }
        }
        #endregion

        /// <summary>
        /// Types of syntactic category descriptors
        /// </summary>
        public enum SyntacticCategoryDescriptor
        {
            /// <summary>
            /// Use full syntactic categories (e.g., NounPhrase, VerbPhrase, ...)
            /// </summary>
            Full,

            /// <summary>
            /// Use mnemonics for syntactic categories (e.g., NP, VP, ...)
            /// </summary>
            Mnemonic,

            /// <summary>
            /// Use integers for syntactic categories (e.g., 0, 1, ...)
            /// </summary>
            Integer
        }

        /// <summary>
        /// Trims a tag off of bracketed text. This will peel off the outmost tag instance only. Furthermore, it won't
        /// work for parse text that doesn't represent a tree. Basically, this is a dangerous function that shouldn't
        /// be used unless you understand exactly how it works.
        /// </summary>
        /// <param name="parseText">Parse text containing tag</param>
        /// <param name="tag">Tag to trim</param>
        /// <returns>Same parse text, sans tag</returns>
        public static string TrimTag(string parseText, string tag)
        {
            parseText = parseText.Trim();

            if (!parseText.StartsWith("(" + tag) || !parseText.EndsWith(")"))
                throw new Exception("Invalid Charniak parse text:  missing " + tag + " tag");

            // find index of next constituent
            int index = parseText.IndexOf("(", 1);
            if (index == -1)
                throw new Exception("Invalid parse text");

            parseText = parseText.Substring(index);
            parseText = parseText.Substring(0, parseText.Length - 1);

            // the trimmed final paren might have been the only character on the line...get ride of preceding newline
            return parseText.Trim();
        }

        /// <summary>
        /// Static constructor
        /// </summary>
        static TreeBankEngine()
        {
            _whitespaceRE = new Regex(@"\s");
            _mnemonicCategory = new Dictionary<string, SyntacticCategory>();
            _mnemonicFunction = new Dictionary<string, GrammaticalFunction>();
            _clauseCategories = new Set<SyntacticCategory>();
            _phraseCategories = new Set<SyntacticCategory>();
            _terminalCategories = new Set<SyntacticCategory>();

            #region function tags and syntactic categories added for brown corpus
            // functions
            _mnemonicFunction.Add("IMP", GrammaticalFunction.Imperative);
            _mnemonicFunction.Add("UNF", GrammaticalFunction.Unfinished);
            _mnemonicFunction.Add("ETC", GrammaticalFunction.Etcetera);
            _mnemonicFunction.Add("CHECK", GrammaticalFunction.Check);
            _mnemonicFunction.Add("IF", GrammaticalFunction.If);
            _mnemonicFunction.Add("BELOW", GrammaticalFunction.Below);

            // syntactic categories
            _mnemonicCategory.Add("ADV", SyntacticCategory.Adv);
            _mnemonicCategory.Add("TYPO", SyntacticCategory.Typo);
            _mnemonicCategory.Add("EDITED", SyntacticCategory.Edited);
            _mnemonicCategory.Add("NEG", SyntacticCategory.Neg);
            #endregion

            #region function tags
            _mnemonicFunction.Add("ADV", GrammaticalFunction.Adverbial);
            _mnemonicFunction.Add("NOM", GrammaticalFunction.NonNpNounPhrase);
            _mnemonicFunction.Add("DTV", GrammaticalFunction.Dative);
            _mnemonicFunction.Add("LGS", GrammaticalFunction.LogicalPassiveSubject);
            _mnemonicFunction.Add("PRD", GrammaticalFunction.SecondaryPredication);
            _mnemonicFunction.Add("PUT", GrammaticalFunction.Put);
            _mnemonicFunction.Add("SBJ", GrammaticalFunction.SurfaceSubject);
            _mnemonicFunction.Add("TPC", GrammaticalFunction.TopicalFrontedConstituent);
            _mnemonicFunction.Add("VOC", GrammaticalFunction.Vocative);
            _mnemonicFunction.Add("BNF", GrammaticalFunction.Benefactive);
            _mnemonicFunction.Add("DIR", GrammaticalFunction.Direction);
            _mnemonicFunction.Add("EXT", GrammaticalFunction.Extent);
            _mnemonicFunction.Add("LOC", GrammaticalFunction.Location);
            _mnemonicFunction.Add("MNR", GrammaticalFunction.Manner);
            _mnemonicFunction.Add("PRP", GrammaticalFunction.Purpose);
            _mnemonicFunction.Add("TMP", GrammaticalFunction.Temporal);
            _mnemonicFunction.Add("CLR", GrammaticalFunction.CloselyRelated);
            _mnemonicFunction.Add("CLF", GrammaticalFunction.TrueCleft);
            _mnemonicFunction.Add("HLN", GrammaticalFunction.Headline);
            _mnemonicFunction.Add("TTL", GrammaticalFunction.Title);
            #endregion

            #region syntactic category tags
            // clause level
            _mnemonicCategory.Add("S", SyntacticCategory.SimpleDeclarativeClause);
            _mnemonicCategory.Add("SBAR", SyntacticCategory.SubConClause);
            _mnemonicCategory.Add("SBARQ", SyntacticCategory.WhQuestionClause);
            _mnemonicCategory.Add("SINV", SyntacticCategory.SubAuxInvClause);
            _mnemonicCategory.Add("SQ", SyntacticCategory.WhQuestionNoWhClause);
            _mnemonicCategory.Add("RRC", SyntacticCategory.ReducedRelativeClause);

            _clauseCategories.Add(SyntacticCategory.SimpleDeclarativeClause);
            _clauseCategories.Add(SyntacticCategory.SubConClause);
            _clauseCategories.Add(SyntacticCategory.WhQuestionClause);
            _clauseCategories.Add(SyntacticCategory.SubAuxInvClause);
            _clauseCategories.Add(SyntacticCategory.WhQuestionNoWhClause);
            _clauseCategories.Add(SyntacticCategory.ReducedRelativeClause);

            // phrase level
            _mnemonicCategory.Add("ADJP", SyntacticCategory.AdjectivePhrase);
            _mnemonicCategory.Add("ADVP", SyntacticCategory.AdverbPhrase);
            _mnemonicCategory.Add("CONJP", SyntacticCategory.ConjunctionPhrase);
            _mnemonicCategory.Add("FRAG", SyntacticCategory.Fragment);
            _mnemonicCategory.Add("NP", SyntacticCategory.NounPhrase);
            _mnemonicCategory.Add("NX", SyntacticCategory.NpHead);
            _mnemonicCategory.Add("PP", SyntacticCategory.PrepPhrase);
            _mnemonicCategory.Add("PRN", SyntacticCategory.ParentheticalPhrase);
            _mnemonicCategory.Add("PRT", SyntacticCategory.ParticlePhrase);
            _mnemonicCategory.Add("QP", SyntacticCategory.QuantifierPhrase);
            _mnemonicCategory.Add("UCP", SyntacticCategory.UnlikeCoordinatedPhrase);
            _mnemonicCategory.Add("VP", SyntacticCategory.VerbPhrase);
            _mnemonicCategory.Add("WHADJP", SyntacticCategory.WhAdjectivePhrase);
            _mnemonicCategory.Add("WHADVP", SyntacticCategory.WhAdverbPhrase);
            _mnemonicCategory.Add("WHNP", SyntacticCategory.WhNounPhrase);
            _mnemonicCategory.Add("WHPP", SyntacticCategory.WhPrepPhrase);

            _phraseCategories.Add(SyntacticCategory.AdjectivePhrase);
            _phraseCategories.Add(SyntacticCategory.AdverbPhrase);
            _phraseCategories.Add(SyntacticCategory.ConjunctionPhrase);
            _phraseCategories.Add(SyntacticCategory.Fragment);
            _phraseCategories.Add(SyntacticCategory.NounPhrase);
            _phraseCategories.Add(SyntacticCategory.NpHead);
            _phraseCategories.Add(SyntacticCategory.PrepPhrase);
            _phraseCategories.Add(SyntacticCategory.ParentheticalPhrase);
            _phraseCategories.Add(SyntacticCategory.ParticlePhrase);
            _phraseCategories.Add(SyntacticCategory.QuantifierPhrase);
            _phraseCategories.Add(SyntacticCategory.UnlikeCoordinatedPhrase);
            _phraseCategories.Add(SyntacticCategory.VerbPhrase);
            _phraseCategories.Add(SyntacticCategory.WhAdjectivePhrase);
            _phraseCategories.Add(SyntacticCategory.WhAdverbPhrase);
            _phraseCategories.Add(SyntacticCategory.WhNounPhrase);
            _phraseCategories.Add(SyntacticCategory.WhPrepPhrase);

            // terminal level
            _mnemonicCategory.Add("CC", SyntacticCategory.CoordinatingConjunction);
            _mnemonicCategory.Add("CD", SyntacticCategory.CardinalNumber);
            _mnemonicCategory.Add("DT", SyntacticCategory.Determiner);
            _mnemonicCategory.Add("EX", SyntacticCategory.ExistentialThere);
            _mnemonicCategory.Add("FW", SyntacticCategory.ForeignWord);
            _mnemonicCategory.Add("IN", SyntacticCategory.Preposition);
            _mnemonicCategory.Add("JJ", SyntacticCategory.Adjective);
            _mnemonicCategory.Add("JJR", SyntacticCategory.AdjectiveComparative);
            _mnemonicCategory.Add("JJS", SyntacticCategory.AdjectiveSuperlative);
            _mnemonicCategory.Add("LS", SyntacticCategory.ListItemMarker);
            _mnemonicCategory.Add("MD", SyntacticCategory.Modal);
            _mnemonicCategory.Add("NN", SyntacticCategory.NounSingular);
            _mnemonicCategory.Add("NNS", SyntacticCategory.NounPlural);
            _mnemonicCategory.Add("NNP", SyntacticCategory.NounProperSingular);
            _mnemonicCategory.Add("NNPS", SyntacticCategory.NounProperPlural);
            _mnemonicCategory.Add("PDT", SyntacticCategory.PreDeterminer);
            _mnemonicCategory.Add("POS", SyntacticCategory.Possessive);
            _mnemonicCategory.Add("PRP", SyntacticCategory.PersonalPronoun);
            _mnemonicCategory.Add("PRP$", SyntacticCategory.PossessivePronoun);
            _mnemonicCategory.Add("RB", SyntacticCategory.Adverb);
            _mnemonicCategory.Add("RBR", SyntacticCategory.AdverbComparative);
            _mnemonicCategory.Add("RBS", SyntacticCategory.AdverbSuperlative);
            _mnemonicCategory.Add("RP", SyntacticCategory.ParticleWord);
            _mnemonicCategory.Add("SYM", SyntacticCategory.Symbol);
            _mnemonicCategory.Add("TO", SyntacticCategory.To);
            _mnemonicCategory.Add("UH", SyntacticCategory.InterjectionWord);
            _mnemonicCategory.Add("VB", SyntacticCategory.VerbBase);
            _mnemonicCategory.Add("VBD", SyntacticCategory.VerbPast);
            _mnemonicCategory.Add("VBG", SyntacticCategory.VerbGerund);
            _mnemonicCategory.Add("VBN", SyntacticCategory.VerbPastParticiple);
            _mnemonicCategory.Add("VBP", SyntacticCategory.VerbNon3rdSingularPresent);
            _mnemonicCategory.Add("VBZ", SyntacticCategory.Verb3rdSingularPresent);
            _mnemonicCategory.Add("WDT", SyntacticCategory.WhDeterminer);
            _mnemonicCategory.Add("WP", SyntacticCategory.WhPronoun);
            _mnemonicCategory.Add("WP$", SyntacticCategory.WhPronounPossessive);
            _mnemonicCategory.Add("WRB", SyntacticCategory.WhAdverb);
            _mnemonicCategory.Add("#", SyntacticCategory.Pound);
            _mnemonicCategory.Add("$", SyntacticCategory.Dollar);
            _mnemonicCategory.Add(".", SyntacticCategory.SentenceFinal);
            _mnemonicCategory.Add(",", SyntacticCategory.Comma);
            _mnemonicCategory.Add(":", SyntacticCategory.Colon);
            _mnemonicCategory.Add(";", SyntacticCategory.Colon);
            _mnemonicCategory.Add("LRB", SyntacticCategory.LeftRoundBracket);
            _mnemonicCategory.Add("RRB", SyntacticCategory.RightRoundBracket);
            _mnemonicCategory.Add("LSB", SyntacticCategory.LeftSquareBracket);
            _mnemonicCategory.Add("RSB", SyntacticCategory.RightSquareBracket);
            _mnemonicCategory.Add("LCB", SyntacticCategory.LeftCurlyBracket);
            _mnemonicCategory.Add("RCB", SyntacticCategory.RightCurlyBracket);
            _mnemonicCategory.Add("''", SyntacticCategory.StraightDoubleQuote);
            _mnemonicCategory.Add("`", SyntacticCategory.LeftSingleQuote);
            _mnemonicCategory.Add("``", SyntacticCategory.LeftDoubleQuote);
            _mnemonicCategory.Add("'", SyntacticCategory.RightSingleQuote);
            _mnemonicCategory.Add("\"", SyntacticCategory.RightDoubleQuote);
            _mnemonicCategory.Add("INTJ", SyntacticCategory.Interjection);
            _mnemonicCategory.Add("LST", SyntacticCategory.ListMarker);

            _terminalCategories.Add(SyntacticCategory.CoordinatingConjunction);
            _terminalCategories.Add(SyntacticCategory.CardinalNumber);
            _terminalCategories.Add(SyntacticCategory.Determiner);
            _terminalCategories.Add(SyntacticCategory.ExistentialThere);
            _terminalCategories.Add(SyntacticCategory.ForeignWord);
            _terminalCategories.Add(SyntacticCategory.Preposition);
            _terminalCategories.Add(SyntacticCategory.Adjective);
            _terminalCategories.Add(SyntacticCategory.AdjectiveComparative);
            _terminalCategories.Add(SyntacticCategory.AdjectiveSuperlative);
            _terminalCategories.Add(SyntacticCategory.ListItemMarker);
            _terminalCategories.Add(SyntacticCategory.Modal);
            _terminalCategories.Add(SyntacticCategory.NounSingular);
            _terminalCategories.Add(SyntacticCategory.NounPlural);
            _terminalCategories.Add(SyntacticCategory.NounProperSingular);
            _terminalCategories.Add(SyntacticCategory.NounProperPlural);
            _terminalCategories.Add(SyntacticCategory.PreDeterminer);
            _terminalCategories.Add(SyntacticCategory.Possessive);
            _terminalCategories.Add(SyntacticCategory.PersonalPronoun);
            _terminalCategories.Add(SyntacticCategory.PossessivePronoun);
            _terminalCategories.Add(SyntacticCategory.Adverb);
            _terminalCategories.Add(SyntacticCategory.AdverbComparative);
            _terminalCategories.Add(SyntacticCategory.AdverbSuperlative);
            _terminalCategories.Add(SyntacticCategory.ParticleWord);
            _terminalCategories.Add(SyntacticCategory.Symbol);
            _terminalCategories.Add(SyntacticCategory.To);
            _terminalCategories.Add(SyntacticCategory.InterjectionWord);
            _terminalCategories.Add(SyntacticCategory.VerbBase);
            _terminalCategories.Add(SyntacticCategory.VerbPast);
            _terminalCategories.Add(SyntacticCategory.VerbGerund);
            _terminalCategories.Add(SyntacticCategory.VerbPastParticiple);
            _terminalCategories.Add(SyntacticCategory.VerbNon3rdSingularPresent);
            _terminalCategories.Add(SyntacticCategory.Verb3rdSingularPresent);
            _terminalCategories.Add(SyntacticCategory.WhDeterminer);
            _terminalCategories.Add(SyntacticCategory.WhPronoun);
            _terminalCategories.Add(SyntacticCategory.WhPronounPossessive);
            _terminalCategories.Add(SyntacticCategory.WhAdverb);
            _terminalCategories.Add(SyntacticCategory.Pound);
            _terminalCategories.Add(SyntacticCategory.Dollar);
            _terminalCategories.Add(SyntacticCategory.SentenceFinal);
            _terminalCategories.Add(SyntacticCategory.Comma);
            _terminalCategories.Add(SyntacticCategory.Colon);
            _terminalCategories.Add(SyntacticCategory.LeftRoundBracket);
            _terminalCategories.Add(SyntacticCategory.RightRoundBracket);
            _terminalCategories.Add(SyntacticCategory.LeftSquareBracket);
            _terminalCategories.Add(SyntacticCategory.RightSquareBracket);
            _terminalCategories.Add(SyntacticCategory.LeftCurlyBracket);
            _terminalCategories.Add(SyntacticCategory.RightCurlyBracket);
            _terminalCategories.Add(SyntacticCategory.StraightDoubleQuote);
            _terminalCategories.Add(SyntacticCategory.LeftSingleQuote);
            _terminalCategories.Add(SyntacticCategory.LeftDoubleQuote);
            _terminalCategories.Add(SyntacticCategory.RightSingleQuote);
            _terminalCategories.Add(SyntacticCategory.RightDoubleQuote);
            _terminalCategories.Add(SyntacticCategory.Interjection);
            _terminalCategories.Add(SyntacticCategory.ListMarker);

            // used by Charniak's parser
            _mnemonicCategory.Add("AUX", SyntacticCategory.AuxiliaryVerb);
            _mnemonicCategory.Add("AUXG", SyntacticCategory.AuxiliaryVerbG);

            _terminalCategories.Add(SyntacticCategory.AuxiliaryVerb);
            _terminalCategories.Add(SyntacticCategory.AuxiliaryVerbG);

            // other categories
            _mnemonicCategory.Add("X", SyntacticCategory.Unknown);
            _mnemonicCategory.Add("NAC", SyntacticCategory.NonConstituent);

            // null element categories...these aren't in the the terminal, phrase, clause lists
            _mnemonicCategory.Add("NONE", SyntacticCategory.NullElement);
            _mnemonicCategory.Add("USUB", SyntacticCategory.UnderstoodSubject);
            _mnemonicCategory.Add("ZVAR", SyntacticCategory.ZeroVariant);
            _mnemonicCategory.Add("COID", SyntacticCategory.CoIndexed);
            _mnemonicCategory.Add("PSEU", SyntacticCategory.PseudoAttachment);

            // build category-to-string map
            _categoryMnemonic = new Dictionary<SyntacticCategory, string>();
            foreach (string s in _mnemonicCategory.Keys)
                _categoryMnemonic[_mnemonicCategory[s]] = s;

            // build number-to-category map
            _numberCategory = new Dictionary<int, SyntacticCategory>();
            foreach (SyntacticCategory category in _categoryMnemonic.Keys)
                _numberCategory.Add((int)category, category);
            #endregion

            #region head finding rules
            /* Taken from "Head-driven statistical models for natural language parsing"
             * Michael Collins, PhD thesis, 1999 */

            // add head rules
            _headFindingRules = new Dictionary<string, ChildSearch>();
            _headFindingRules.Add("ADJP", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "NNS", "QP", "NN", "$", "ADVP", "JJ", "VBN", "VBG", "ADJP", "JJR", 
                    "NP", "JJS", "DT", "FW", "RBR", "RBS", "SBAR", "RB"})));
            _headFindingRules.Add("ADVP", new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] {
                    "RB", "RBR", "RBS", "FW", "ADVP", "TO", "CD", "JJR", "JJ", "IN", 
                    "NP", "JJS", "NN" })));
            _headFindingRules.Add("FRAG", new ChildSearch(ChildSearch.SearchDirection.RightToLeft, new List<string>()));
            _headFindingRules.Add("INTJ", new ChildSearch(ChildSearch.SearchDirection.LeftToRight, new List<string>()));
            _headFindingRules.Add("LST", new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] { "LS", ":" })));
            _headFindingRules.Add("NAC", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "NN", "NNS", "NNP", "NNPS", "NP", "NAC", "EX", "$", "CD", "QP", 
                    "PRP", "VBG", "JJ", "JJS", "JJR", "ADJP", "FW" })));
            _headFindingRules.Add("PP", new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] {
                    "IN", "TO", "VBG", "VBN", "RP", "FW" })));
            _headFindingRules.Add("PRN", new ChildSearch(ChildSearch.SearchDirection.LeftToRight, new List<string>()));
            _headFindingRules.Add("PRT", new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] { "RP" })));
            _headFindingRules.Add("QP", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "$", "IN", "NNS", "NN", "JJ", "RB", "DT", "CD", "QP", "JJR", "JJS"})));
            _headFindingRules.Add("RRC", new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] {
                    "VP", "NP", "ADVP", "ADJP", "PP" })));
            _headFindingRules.Add("S", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "TO", "IN", "VP", "S", "SBAR", "ADJP", "UCP", "NP" })));
            _headFindingRules.Add("SBAR", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "WHNP", "WHPP", "WHADVP", "WHADJP", "IN", "DT", "S", "SQ", "SINV",
                    "SBAR", "FRAG" })));
            _headFindingRules.Add("SBARQ", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "SQ", "S", "SINV", "SBARQ", "FRAG" })));
            _headFindingRules.Add("SINV", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "VBZ", "VBD", "VBP", "VB", "MD", "VP", "S", "SINV", "ADJP", "NP" })));
            _headFindingRules.Add("SQ", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "VBZ", "VBD", "VBP", "VB", "MD", "VP", "SQ" })));
            _headFindingRules.Add("UCP", new ChildSearch(ChildSearch.SearchDirection.RightToLeft, new List<string>()));
            _headFindingRules.Add("VP", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "VP", "ADJP", 
                    "NN", "NNS", "NP" })));
            _headFindingRules.Add("WHADJP", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] { "CC", "WRB", "JJ", "ADJP" })));
            _headFindingRules.Add("WHADVP", new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] { "CC", "WRB" })));
            _headFindingRules.Add("WHNP", new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] {
                    "WDT", "WP", "WP$", "WHADJP", "WHPP", "WHNP" })));
            _headFindingRules.Add("WHPP", new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] { "IN", "TO", "FW" })));

            // add NP head rules
            _npHeadFindingRules = new List<ChildSearch>(new ChildSearch[] {
                new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] {
                    "POS" })),

                new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] {
                    "NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR" })),

                new ChildSearch(ChildSearch.SearchDirection.LeftToRight,
                new List<string>(new string[] { "NP" })),

                new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] {
                    "$", "ADJP", "PRN" })),

                new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] { "CD" })),

                new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>(new string[] { "JJ", "JJS", "RB", "QP" })),

                new ChildSearch(ChildSearch.SearchDirection.RightToLeft,
                new List<string>())});

            // sanity check on categories
            foreach (string cat1 in _headFindingRules.Keys)
            {
                GetSyntacticCategory(cat1);

                foreach (string cat2 in _headFindingRules[cat1].SearchList)
                    GetSyntacticCategory(cat2);
            }

            // sanity check on categories
            foreach (ChildSearch search in _npHeadFindingRules)
                foreach (string cat in search.SearchList)
                    GetSyntacticCategory(cat);
            #endregion

            #region bracket characters
            // brackets are escaped in the TreeBank parse tree definitions
            _expandedBracketChar = new Dictionary<string, char>();
            _expandedBracketChar.Add("-LRB-", '(');
            _expandedBracketChar.Add("-RRB-", ')');
            _expandedBracketChar.Add("-LSB-", '[');
            _expandedBracketChar.Add("-RSB-", ']');
            _expandedBracketChar.Add("-LCB-", '{');
            _expandedBracketChar.Add("-RCB-", '}');

            _charExpandedBracket = new Dictionary<char, string>();
            foreach (string expandedBracket in _expandedBracketChar.Keys)
                _charExpandedBracket.Add(_expandedBracketChar[expandedBracket], expandedBracket);
            #endregion

            // the following are publicly exposed and should be read-only
            _terminalCategories.IsReadOnly = _phraseCategories.IsReadOnly = _clauseCategories.IsReadOnly = true;
        }

        /// <summary>
        /// Gets whether or not a category is a clause
        /// </summary>
        /// <param name="category">Category to check</param>
        /// <returns>True if category is a clause, false otherwise</returns>
        public static bool IsClause(SyntacticCategory category)
        {
            return _clauseCategories.Contains(category);
        }

        /// <summary>
        /// Gets whether or not a category is a phrase
        /// </summary>
        /// <param name="category">Category to check</param>
        /// <returns>True if category is a phrase, false otherwise</returns>
        public static bool IsPhrase(SyntacticCategory category)
        {
            return _phraseCategories.Contains(category);
        }

        /// <summary>
        /// Gets whether or not a category is a terminal
        /// </summary>
        /// <param name="category">Category to check</param>
        /// <returns>True if category is a terminal, false otherwise</returns>
        public static bool IsTerminal(SyntacticCategory category)
        {
            return _terminalCategories.Contains(category);
        }

        /// <summary>
        /// Gets the syntactic category from its mnemonic form
        /// </summary>
        /// <param name="s">Mnemonic form of syntactic category</param>
        /// <returns>SyntacticCategory</returns>
        public static SyntacticCategory GetSyntacticCategory(string s)
        {
            // we don't handle gapping right now
            if (s.Contains("="))
                s = s.Substring(0, s.IndexOf('='));

            return _mnemonicCategory[s];
        }

        /// <summary>
        /// Gets the grammatical function given a string
        /// </summary>
        /// <param name="s">String form of grammatical function</param>
        /// <returns>GrammaticalFunction</returns>
        public static GrammaticalFunction GetFunction(string s)
        {
            // we don't handle gapping right now
            if (s.Contains("="))
                s = s.Substring(0, s.IndexOf('='));

            return _mnemonicFunction[s];
        }

        /// <summary>
        /// Gets the mnemonic for a syntactic category (e.g., "NP" for SyntacticCategory.NounPhrase)
        /// </summary>
        /// <param name="category">Gets mnemonic for a SyntacticCategory</param>
        /// <returns>String mnemonic</returns>
        public static string GetMnemonicFor(SyntacticCategory category)
        {
            return _categoryMnemonic[category];
        }

        /// <summary>
        /// Replaces escaped bracket characters (e.g., -LRB-, -RRB- , etc.) with their standard characters
        /// </summary>
        /// <param name="s">String to replace characters in</param>
        /// <returns>String with bracket characters converted to standard characters</returns>
        public static string UnescapeBrackets(string s)
        {
            if (s == null || s.Length == 0)
                throw new Exception("Invalid text");

            string[] parts = s.Split(' ');
            StringBuilder unexpanded = new StringBuilder();
            for (int i = 0; i < parts.Length; ++i)
            {
                string part = parts[i];

                // convert -LRB-, -RRB-, etc. back to their surface forms '(', ')', etc.
                char bracketChar;
                if (_expandedBracketChar.TryGetValue(part, out bracketChar))
                    part = bracketChar.ToString();

                if (i > 0)
                    unexpanded.Append(" ");

                unexpanded.Append(part);
            }

            return unexpanded.ToString();
        }

        /// <summary>
        /// Gets the label for a special character
        /// </summary>
        /// <param name="specialChar">Special character</param>
        /// <returns>Label</returns>
        public static string GetSpecialCharLabel(char specialChar)
        {
            return _charExpandedBracket[specialChar];
        }

        /// <summary>
        /// Gets whether or not a special char is contained
        /// </summary>
        /// <param name="specialChar">Special char to check for</param>
        /// <returns>True if char is contained, false otherwise</returns>
        public static bool ContainsSpecialChar(char specialChar)
        {
            return _charExpandedBracket.ContainsKey(specialChar);
        }

        /// <summary>
        /// Converts a parse tree path's category descriptors to a different type
        /// </summary>
        /// <param name="path">Path to convert</param>
        /// <param name="sourceDescriptor">Source descriptor type</param>
        /// <param name="destinationDescriptor">Destination descriptor type</param>
        /// <returns>Converted parse tree path</returns>
        public static string ConvertParseTreePath(string path, SyntacticCategoryDescriptor sourceDescriptor, SyntacticCategoryDescriptor destinationDescriptor)
        {
            // makes no sense to convert to same descriptor
            if (sourceDescriptor == destinationDescriptor)
                throw new Exception("It makes no sense to convert to the same descriptor type");

            StringBuilder converted = new StringBuilder();
            int previousBranchLoc = -1;
            Regex branchRE = new Regex(">|<");
            while (previousBranchLoc != path.Length)
            {
                // get next branch location
                int nextBrachLoc = -1;
                Match m = branchRE.Match(path, previousBranchLoc + 1);
                if (m.Success)
                    nextBrachLoc = m.Index;
                else
                    nextBrachLoc = path.Length;

                // get descriptor location
                int start = previousBranchLoc + 1;
                int end = nextBrachLoc - 1;
                int length = end - start + 1;

                string sourceStr = path.Substring(start, length);

                // get source category
                SyntacticCategory sourceCat = SyntacticCategory.Unknown;
                if (sourceDescriptor == SyntacticCategoryDescriptor.Full)
                    sourceCat = (SyntacticCategory)Enum.Parse(typeof(SyntacticCategory), sourceStr);
                else if (sourceDescriptor == SyntacticCategoryDescriptor.Mnemonic)
                    sourceCat = _mnemonicCategory[sourceStr];
                else if (sourceDescriptor == SyntacticCategoryDescriptor.Integer)
                    sourceCat = _numberCategory[int.Parse(sourceStr)];
                else
                    throw new Exception("Unrecognized descriptor type");

                // get destination string
                string destStr = null;
                if (destinationDescriptor == SyntacticCategoryDescriptor.Full)
                    destStr = sourceCat.ToString();
                if (destinationDescriptor == SyntacticCategoryDescriptor.Integer)
                    destStr = ((int)sourceCat).ToString();
                else if (destinationDescriptor == SyntacticCategoryDescriptor.Mnemonic)
                    destStr = _categoryMnemonic[sourceCat];
                else
                    throw new Exception("Unrecognized descriptor type");

                converted.Append(destStr);

                // append branch if we found one
                if (m.Success)
                    converted.Append(m.Value);

                previousBranchLoc = nextBrachLoc;
            }

            return converted.ToString();
        }
        #endregion

        /// <summary>
        /// Index from (TreeBank .mrg file and sentence number) to (character start and length of parse tree within TreeBank .mrg file)
        /// </summary>
        private Dictionary<string, Dictionary<int, ParseTextStartLength>> _pathSentenceStartLength;

        /// <summary>
        /// Path to the directory of the TreeBank from which MRG files were drawn
        /// </summary>
        private string _mrgPath;

        /// <summary>
        /// Path to the index directory
        /// </summary>
        private string _indexDirectory;

        /// <summary>
        /// Maps MRG file names (e.g., "wsj_0000.mrg") to their full paths (e.g., "c:\path\to\wsj_0000.mrg")
        /// </summary>
        private Dictionary<string, string> _mrgFileNameFullPath;

        /// <summary>
        /// Matches MRG file names
        /// </summary>
        private Regex _mrgFileNameRE;

        /// <summary>
        /// Gets or sets the regular expression for matching MRG file names. The default value is the one used for
        /// processing Penn TreeBank files in the form "wsj_XXYY.mrg", where XX gives the section number and YY
        /// gives the article number. If you set this value, there must be at least two groups, "section" and 
        /// "number", which capture the section and article number, respectively.
        /// </summary>
        public Regex MrgFileNameRE
        {
            get { return _mrgFileNameRE; }
            set { _mrgFileNameRE = value; }
        }

        /// <summary>
        /// Gets the path to the MRG sentence position index path
        /// </summary>
        private string MrgSentenceStartLengthPositionPath
        {
            get { return Path.Combine(_indexDirectory, "treebank_mrg_sentence_positions"); }
        }

        /// <summary>
        /// Gets the path to the MRG subdirectory
        /// </summary>
        public string MrgPath
        {
            get { return _mrgPath; }
        }

        /// <summary>
        /// Gets the path to the index directory
        /// </summary>
        public string IndexDirectory
        {
            get { return _indexDirectory; }
        }

        /// <summary>
        /// Gets a list of all indexed MRG files
        /// </summary>
        public List<string> IndexedMrgFiles
        {
            get { return new List<string>(_pathSentenceStartLength.Keys); }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Directory to search (recursively) for .mrg files</param>
        /// <param name="indexDirectory">Path to the index directory, where all indexing information is stored</param>
        public TreeBankEngine(string mrgPath, string indexDirectory)
        {
            _mrgPath = mrgPath.ToLower();
            _indexDirectory = indexDirectory;
            _mrgFileNameRE = new Regex(@"wsj_(?<section>[0-9][0-9])(?<number>[0-9][0-9])(\.mrg)?$");

            // build index
            if (!Directory.Exists(_indexDirectory))
                Directory.CreateDirectory(_indexDirectory);

            BuildMrgSentencePositionIndex();

            // build partial-to-full path index
            _mrgFileNameFullPath = new Dictionary<string, string>();
            foreach (string mrgFile in IndexedMrgFiles)
                _mrgFileNameFullPath.Add(Path.GetFileName(mrgFile), mrgFile);
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Directory to search (recursively) for .mrg files</param>
        /// <param name="indexDirectory">Path to the index directory, where all indexing information is stored</param>
        /// <param name="mrgFileNameRE">Regular expression for MRG file name. See MrgFileNameRE property for details.</param>
        public TreeBankEngine(string mrgPath, string indexDirectory, Regex mrgFileNameRE)
            : this(mrgPath, indexDirectory)
        {
            _mrgFileNameRE = mrgFileNameRE;
        }

        #region index building
        /// <summary>
        /// Recursively builds the sentence position index, starting in some parent directory.
        /// </summary>
        private void BuildMrgSentencePositionIndex()
        {
            if (!Directory.Exists(_mrgPath))
                throw new Exception("Invalid MRG path:  " + _mrgPath);

            // check if already built
            if (File.Exists(MrgSentenceStartLengthPositionPath))
            {
                // read sentence position index
                _pathSentenceStartLength = new Dictionary<string, Dictionary<int, ParseTextStartLength>>();
                StreamReader loadFile = new StreamReader(MrgSentenceStartLengthPositionPath);
                string line;
                while ((line = loadFile.ReadLine()) != null)
                {
                    // get MRG file
                    int pipeLoc = line.IndexOf('|');
                    string mrgFile = line.Substring(0, pipeLoc);
                    _pathSentenceStartLength.Add(mrgFile, new Dictionary<int, ParseTextStartLength>());

                    // get sentence start/length pairs, in format:  |sent start length|
                    while (pipeLoc != line.Length - 1)
                    {
                        int nextPipe = line.IndexOf('|', pipeLoc + 1);
                        int firstSpace = line.IndexOf(' ', pipeLoc + 1);
                        int secondSpace = line.IndexOf(' ', firstSpace + 1);

                        int sentNum = int.Parse(line.Substring(pipeLoc + 1, firstSpace - pipeLoc - 1));
                        int start = int.Parse(line.Substring(firstSpace + 1, secondSpace - firstSpace - 1));
                        int length = int.Parse(line.Substring(secondSpace + 1, nextPipe - secondSpace - 1));

                        ParseTextStartLength startLength = new ParseTextStartLength(start, length);

                        _pathSentenceStartLength[mrgFile].Add(sentNum, startLength);

                        pipeLoc = nextPipe;
                    }
                }

                loadFile.Close();
            }
            else
            {
                _pathSentenceStartLength = new Dictionary<string, Dictionary<int, ParseTextStartLength>>();
                BuildMrgSentencePositionIndexRecursive(_mrgPath);

                // save index
                StreamWriter saveFile = new StreamWriter(MrgSentenceStartLengthPositionPath);
                foreach (string mrgFile in _pathSentenceStartLength.Keys)
                {
                    saveFile.Write(mrgFile);

                    // write start and length for each sentence, delimited by pipes
                    foreach (int sentNum in _pathSentenceStartLength[mrgFile].Keys)
                    {
                        ParseTextStartLength startLength = _pathSentenceStartLength[mrgFile][sentNum];
                        saveFile.Write("|" + sentNum + " " + startLength.Start + " " + startLength.Length);
                    }

                    saveFile.WriteLine("|");
                }

                saveFile.Close();
            }
        }

        /// <summary>
        /// Recursively builds sentence position index
        /// </summary>
        /// <param name="directory">Directory to build index from</param>
        private void BuildMrgSentencePositionIndexRecursive(string directory)
        {
            Regex startParseRE = new Regex(@"^\s*\(\s*\(", RegexOptions.Multiline);

            // process each file in directory
            foreach (string path in Directory.GetFiles(directory, "*.mrg"))
            {
                // index sentence positions
                string parseText = File.ReadAllText(path);
                Dictionary<int, ParseTextStartLength> sentenceStartLength = new Dictionary<int, ParseTextStartLength>();
                MatchCollection sentenceStartMatches = startParseRE.Matches(parseText);
                int sentenceByteStart = 0;
                for (int i = 0; i < sentenceStartMatches.Count; ++i)
                {
                    Match sentenceStartMatch = sentenceStartMatches[i];

                    // get textual start and end of sentence
                    int sentenceTextStart = sentenceStartMatch.Index;
                    int sentenceTextEnd = parseText.Length - 1;

                    if (i < sentenceStartMatches.Count - 1)
                        sentenceTextEnd = sentenceStartMatches[i + 1].Index - 1;

                    int sentenceTextLength = sentenceTextEnd - sentenceTextStart + 1;

                    // get byte length
                    int sentenceByteLength = Encoding.UTF8.GetByteCount(parseText.Substring(sentenceTextStart, sentenceTextLength));
                    sentenceStartLength.Add(i, new ParseTextStartLength(sentenceByteStart, sentenceByteLength));

                    // update sentence byte start
                    sentenceByteStart += sentenceByteLength;
                }

                /* Windows ignores casing when handling file paths, so make everything lowercase 
                 * to handle case discrepancies between other resources' TreeBank paths and
                 * the actual files in the TreeBank distribution. */
                _pathSentenceStartLength.Add(path.ToLower(), sentenceStartLength);
            }

            // process subdirectories
            foreach (string subDirectory in Directory.GetDirectories(directory))
                BuildMrgSentencePositionIndexRecursive(subDirectory);
        }
        #endregion

        /// <summary>
        /// Gets full path to MRG file (e.g., "c:\path\to\wsj_0000.mrg") from a partial path (e.g., "wsj_0000.mrg") or a full
        /// path with different directory structure (e.g., "c:\some\other\path\to\wsj_0000.mrg")
        /// </summary>
        /// <param name="mrgFile">Path to (or filename of) MRG file</param>
        /// <returns>Full path (e.g., "c:\path\to\wsj_0000.mrg") within this index</returns>
        public string GetFullMrgPath(string mrgFile)
        {
            string fileName = Path.GetFileName(mrgFile);

            // look up full file path for file name
            string fullPath;
            if (!_mrgFileNameFullPath.TryGetValue(fileName, out fullPath))
                throw new Exception("TreeBank does not contain MRG file \"" + fileName + "\"");

            return fullPath;
        }

        /// <summary>
        /// Gets sorted list of sentence numbers in an indexed MRG file
        /// </summary>
        /// <param name="path">Path to indexed MRG file</param>
        /// <returns>Sorted list of sentence numbers</returns>
        public List<int> GetSentenceNumbers(string path)
        {
            List<int> sentenceNumbers = new List<int>(_pathSentenceStartLength[path].Keys);
            sentenceNumbers.Sort();

            return sentenceNumbers;
        }

        /// <summary>
        /// Gets a parse tree from a TreeBank MRG file
        /// </summary>
        /// <param name="path">MRG file to get parse tree from</param>
        /// <param name="sentenceNumber">Zero-based index of parse tree to get</param>
        /// <returns>Parse tree</returns>
        public TreeBankNode GetParseTree(string path, int sentenceNumber)
        {
            /* Windows ignores casing when handling file paths, so make everything lowercase 
             * to handle case discrepancies between PropBank file paths (in prop.txt) and
             * the actual file names in the TreeBank distribution */
            path = path.ToLower();

            // make sure we have start-length positions for the give filename
            Dictionary<int, ParseTextStartLength> sentenceStartLength;
            if (!_pathSentenceStartLength.TryGetValue(path, out sentenceStartLength))
                throw new Exception("Failed to get sentence index for file \"" + path + "\"");

            // get start and length of parse text within file
            ParseTextStartLength startLength;
            if (!sentenceStartLength.TryGetValue(sentenceNumber, out startLength))
                throw new Exception("Failed to get position of sentence " + sentenceNumber + " within file \"" + path + "\"");

            // read parse tree text
            char[] parseTextChars = new char[startLength.Length];
            StreamReader file = new StreamReader(path);
            file.BaseStream.Position = startLength.Start;
            file.Read(parseTextChars, 0, startLength.Length);
            file.Close();

            // ignore extra () around parse
            string parseText = new string(parseTextChars).Trim();
            parseText = parseText.Substring(1, parseText.Length - 2).Trim();

            // extract parse tree
            TreeBankNode root = ExtractNode(parseText, false);
            root.MrgFile = path;
            root.SentenceNumber = sentenceNumber;

            return root;
        }

        /// <summary>
        /// Gets the parse tree within a MRG file using a Gorn address, where the first Gorn index gives the sentence
        /// number within the MRG file, and the remaining indexes are interpreted in the standard way.
        /// </summary>
        /// <param name="path">MRG file to get tree from</param>
        /// <param name="gornAddress">Gorn address of tree</param>
        /// <returns>TreeBankNode</returns>
        public TreeBankNode GetParseTreeNode(string path, int[] gornAddress)
        {
            TreeBankNode node = GetParseTree(path, gornAddress[0]);

            for (int i = 1; i < gornAddress.Length; ++i)
                node = node.GetChild(gornAddress[i]);

            return node;
        }

        /// <summary>
        /// Gets a node from its full location, as given by TreeBankNode.FullLocation
        /// </summary>
        /// <param name="fullLocation">Full location, as given by TreeBankNode.FullLocation</param>
        /// <returns></returns>
        public TreeBankNode GetNodeFromFullLocation(string fullLocation)
        {
            int firstColon = fullLocation.IndexOf(':');
            int secondColon = fullLocation.IndexOf(':', firstColon + 1);

            // get tree
            string mrgFile = GetFullMrgPath(fullLocation.Substring(0, firstColon) + ".mrg");
            int sentNum = int.Parse(fullLocation.Substring(firstColon + 1, secondColon - firstColon - 1));

            TreeBankNode tree = GetParseTree(mrgFile, sentNum);

            // return desired node
            return tree.GetNode(fullLocation.Substring(secondColon + 1));
        }

        /// <summary>
        /// Gets TreeBank section for a file
        /// </summary>
        /// <param name="fileName">File to get section of</param>
        /// <returns>Section number the given file comes from</returns>
        public int GetSectionNumber(string fileName)
        {
            // make sure we've only got the file name
            fileName = Path.GetFileName(fileName);

            // match file name
            Match m = _mrgFileNameRE.Match(fileName);
            if (!m.Success)
                throw new Exception("Failed to match file for section");

            // get section group
            Group sectionGroup = m.Groups["section"];
            if (!sectionGroup.Success)
                throw new Exception("Failed to get section");

            // get section number
            int section = int.Parse(sectionGroup.Value);

            return section;
        }

        /// <summary>
        /// Gets article number for a file
        /// </summary>
        /// <param name="fileName">File to get article number for</param>
        /// <returns>Article number</returns>
        public int GetArticleNumber(string fileName)
        {
            // make sure we've only got the file name
            fileName = Path.GetFileName(fileName);

            // match file name
            Match m = _mrgFileNameRE.Match(fileName);
            if (!m.Success)
                throw new Exception("Failed to match file for number");

            // get number group
            Group numberGroup = m.Groups["number"];
            if (!numberGroup.Success)
                throw new Exception("Failed to get number");

            // get article number
            int number = int.Parse(numberGroup.Value);

            return number;
        }

        /// <summary>
        /// Checks whether or not a TreeBank query is valid. It makes sure the MRG file is actually indexed, but only
        /// checks to make sure the sentence and leaf/token numbers aren't negative. Throws exception if anything is wrong.
        /// </summary>
        /// <param name="path">MRG file to check</param>
        /// <param name="sentence">Sentence number to check</param>
        /// <param name="leafTokenNumber">Leaf/token number to check</param>
        protected void EnsureValidQuery(string path, int sentence, int leafTokenNumber)
        {
            if (!_pathSentenceStartLength.ContainsKey(path))
                throw new FileNotFoundException("Invalid MRG file:  " + path);

            if (sentence < 0)
                throw new ArgumentOutOfRangeException("Invalid sentence number:  " + sentence);

            if (leafTokenNumber < 0)
                throw new ArgumentOutOfRangeException("Invalid leaf/token number:  " + leafTokenNumber);
        }
    }
}
