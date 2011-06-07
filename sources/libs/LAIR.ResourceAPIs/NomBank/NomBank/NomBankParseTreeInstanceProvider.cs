using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Returns training nodes from all parse trees in TreeBank. This returns no duplicates, so each instance is
    /// a distinct parse tree node. Provides the option to label support verbs and predicating nominals in the tree.
    /// </summary>
    public class NomBankParseTreeInstanceProvider : TreeBankParseTreeInstanceProvider
    {
        private bool _labelPredicates;
        private bool _labelSupportVerbs;
        private NomBankEngine _goldParsedNomBankEngine;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="labelPredicates">Whether or not to label predicates</param>
        /// <param name="labelSupportVerbs">Whether or not to label support verbs</param>
        /// <param name="autoParsedNomBankEngine">Automatically parsed version of NomBank, from which to draw parse tree structure</param>
        /// <param name="goldParsedNomBankEngine">Gold-parsed version of NomBank, from which to draw predicate and support verb labels. This is 
        /// necessary because auto-parsed versions of NomBank don't include annotations for instances whose arguments cannot all be minimally
        /// subsumed.</param>
        /// <param name="instanceFilter">Instance filter to apply</param>
        /// <param name="sections">TreeBank sections to draw instances from (null for all sections)</param>
        public NomBankParseTreeInstanceProvider(bool labelPredicates,
                                                bool labelSupportVerbs,
                                                NomBankEngine autoParsedNomBankEngine,
                                                NomBankEngine goldParsedNomBankEngine,
                                                InstanceFilterDelegate instanceFilter,
                                                Set<int> sections)
            : base(autoParsedNomBankEngine, instanceFilter, sections)
        {
            _labelPredicates = labelPredicates;
            _labelSupportVerbs = labelSupportVerbs;
            _goldParsedNomBankEngine = goldParsedNomBankEngine;
        }

        /// <summary>
        /// Gets training instance parse tree as a NomBank node, marking predicates and support verbs
        /// as determined from constructor.
        /// </summary>
        /// <param name="mrgFile">MRG file of tree</param>
        /// <param name="sentenceNumber">Sentence number of tree</param>
        /// <returns>Training instance parse tree as a NomBank node</returns>
        protected override TreeBankNode GetTrainingInstanceParseTree(string mrgFile, int sentenceNumber)
        {
            // get parse tree as usual
            TreeBankNode parseTree = base.GetTrainingInstanceParseTree(mrgFile, sentenceNumber);

            // turn parse tree into NomBank tree
            NomBankNode nomBankParseTree = new NomBankNode(parseTree);

            // label predicate and support verb nodes
            if (_labelPredicates || _labelSupportVerbs)
            {
                /* get MRG file in gold-parsed NomBank engine...we must use the gold NomBank engine for marking stuff because
                 * the automatic version won't know about all of the markables due to syntactic parse errors that prevent some
                 * propositions from being included in the auto-parse propositions file */
                string goldMrgFile = _goldParsedNomBankEngine.GetFullMrgPath(mrgFile);

                foreach (NomBankNode token in nomBankParseTree.Tokens)
                {
                    int tokenNumber = token.TokenNumber;

                    if (_labelPredicates)
                        if (_goldParsedNomBankEngine.TokenIsMarkable(goldMrgFile, sentenceNumber, tokenNumber))
                            token.AddLabel(new NomBankNodeLabel(NomBankNodeLabel.NodeType.Predicate, 1), true);

                    if (_labelSupportVerbs)
                        if (_goldParsedNomBankEngine.TokenIsSupportVerb(goldMrgFile, sentenceNumber, tokenNumber))
                            token.AddLabel(new NomBankNodeLabel(NomBankNodeLabel.NodeType.Support, 1), true);
                }
            }

            return nomBankParseTree;
        }
    }
}
