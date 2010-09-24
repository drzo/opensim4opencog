using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using LAIR.Collections.Generic;
using LAIR.MachineLearning;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Returns training nodes from all parse trees in TreeBank. This returns no duplicates, so each instance is
    /// a distinct parse tree node.
    /// </summary>
    public class TreeBankParseTreeInstanceProvider : TreeBankInstanceProvider
    {
        private List<int>.Enumerator _sentEnum;
        private IEnumerator<TreeBankNode> _nodeEnum;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="treeBankEngine">TreeBank engine to draw parse tree nodes from</param>
        /// <param name="instanceFilter">Instance filter to apply to nodes</param>
        /// <param name="sections">Sections to draw nodes from</param>
        public TreeBankParseTreeInstanceProvider(TreeBankEngine treeBankEngine, InstanceFilterDelegate instanceFilter, Set<int> sections)
            : base(treeBankEngine, instanceFilter, sections)
        {
        }

        /// <summary>
        /// Starts the training node sequence at the beginning
        /// </summary>
        public override void Start()
        {
            base.Start();

            // reset enumerators to empty list
            _nodeEnum = new List<TreeBankNode>().GetEnumerator();
            _sentEnum = new List<int>().GetEnumerator();
        }

        /// <summary>
        /// Gets the next training node
        /// </summary>
        /// <returns>Training node</returns>
        public override ClassifiableEntity GetNextInstance()
        {
            // try to move to next node in current sentence
            while (!_nodeEnum.MoveNext())
            {
                // try to move to next sentence in current MRG file
                while (!_sentEnum.MoveNext())
                {
                    // try to move to next MRG file...if there are none, we're done
                    if (!MoveToNextValidMrgFile())
                        return null;

                    // start at first sentence of next MRG file
                    _sentEnum = TreeBankEngine.GetSentenceNumbers(CurrentMrgFile).GetEnumerator();
                }

                // filter all nodes in the tree, keeping the good ones
                TreeBankNode root = GetTrainingInstanceParseTree(CurrentMrgFile, _sentEnum.Current);
                List<TreeBankNode> filteredNodes = new List<TreeBankNode>();
                foreach (TreeBankNode n in root.AllNodes)
                    if (Filter(n))
                        filteredNodes.Add(n);

                _nodeEnum = filteredNodes.GetEnumerator();
            }

            return _nodeEnum.Current;
        }

        /// <summary>
        /// Gets parse tree containing training instance nodes
        /// </summary>
        /// <param name="mrgFile">MRG file containing tree</param>
        /// <param name="sentenceNumber">Sentence number of tree</param>
        /// <returns>Training parse tree</returns>
        protected virtual TreeBankNode GetTrainingInstanceParseTree(string mrgFile, int sentenceNumber)
        {
            return TreeBankEngine.GetParseTree(mrgFile, sentenceNumber);
        }

        #region members not implemented
        /// <summary>
        /// Not implemented
        /// </summary>
        /// <returns>Instance</returns>
        public override ClassifiableEntity GetPreviousInstance()
        {
            throw new NotImplementedException();
        }
        
        /// <summary>
        /// Not implemented
        /// </summary>
        public override int Count
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Not implemented
        /// </summary>
        public override int CurrentInstanceNumber
        {
            get
            {
                throw new NotImplementedException();
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        /// <summary>
        /// Not implemented
        /// </summary>
        public override int Remaining
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Not implemented
        /// </summary>
        public override ClassifiableEntity CurrentInstance
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Not implemented
        /// </summary>
        public override bool HasAnotherInstance
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Not implemented
        /// </summary>
        public override bool HasPreviousInstance
        {
            get { throw new NotImplementedException(); }
        }
        #endregion
    }
}
