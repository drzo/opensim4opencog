using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using LAIR.MachineLearning;
using LAIR.Collections.Generic;
using LAIR.ResourceAPIs.PennBank.TreeBank;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Returns training nodes from all NomBank predicate trees, where a single predicate tree is defined as
    /// an annotated instance of a NomBank noun. Since a single sentence can have multiple predicate trees (one for
    /// each predicating noun in the sentence) this returns "duplicate" parse tree nodes in the sense that
    /// the same parse nodes are returned for each predicating nominal in the sentence, even though they're
    /// labeled differently.
    /// </summary>
    public class NomBankPredicateTreeInstanceProvider : TreeBankInstanceProvider
    {
        private IEnumerator<string> _nouns;
        private List<NounInfo>.Enumerator _nounInfo;
        private List<NomBankNode>.Enumerator _nodes;
        private List<NomBankNode> _filteredNodes;

        /// <summary>
        /// Sets the nominalizations that training instances should be provided for (pass null for all nouns)
        /// </summary>
        public Set<string> Nouns
        {
            set
            {
                if (value != null)
                {
                    if (value.Count == 0)
                        throw new Exception("It makes no sense to train over zero nouns");

                    _nouns = value.GetEnumerator();
                }
                // if value is null, use all nouns
                else
                    _nouns = (TreeBankEngine as NomBankEngine).AllNouns.GetEnumerator();
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="nouns">Nouns to get predicates trees for (null for all)</param>
        /// <param name="nomBankEngine">NomBank engine to draw training predicate trees from</param>
        /// <param name="instanceFilter">Instance filter to apply</param>
        /// <param name="sections">TreeBank sections to draw instances from</param>
        public NomBankPredicateTreeInstanceProvider(Set<string> nouns,
                                                    NomBankEngine nomBankEngine,
                                                    InstanceFilterDelegate instanceFilter,
                                                    Set<int> sections)
            : base(nomBankEngine, instanceFilter, sections)
        {
            Nouns = nouns;
            _filteredNodes = new List<NomBankNode>();
        }

        /// <summary>
        /// Starts training instance iterator
        /// </summary>
        public override void Start()
        {
            // reset noun enumerator to beginning
            _nouns.Reset();

            // reset other enumerators
            _nounInfo = new List<NounInfo>().GetEnumerator();
            _nodes = new List<NomBankNode>().GetEnumerator();
        }

        /// <summary>
        /// Gets next training instance for models build over NomBank
        /// </summary>
        /// <returns>Next training instance</returns>
        public override ClassifiableEntity GetNextInstance()
        {
            // try to move to next node
            while (!_nodes.MoveNext())
            {
                NomBankEngine nomBankEngine = TreeBankEngine as NomBankEngine;

                // try to move to next NounInfo
                while (!MoveToNextValidNounInfo(ref _nounInfo))
                {
                    // try to move to next noun...if there are none we're done
                    if (!_nouns.MoveNext())
                        return null;

                    // start before first NounInfo for current noun
                    _nounInfo = nomBankEngine.GetNounInfo(_nouns.Current).GetEnumerator();
                }

                // filter all nodes in the tree, keeping the ones that pass
                NomBankNode root = nomBankEngine.GetNomBankTree(_nounInfo.Current);
                _filteredNodes.Clear();  // reuse node collection for better memory usage
                foreach (NomBankNode n in root.AllNodes)
                    if (Filter(n))
                        _filteredNodes.Add(n);

                _nodes = _filteredNodes.GetEnumerator();
            }

            return _nodes.Current;
        }

        /// <summary>
        /// Moves a NounInfo enumerator to the next valid entry
        /// </summary>
        /// <param name="nounInfoEnum">NounInfo enumerator to move</param>
        /// <returns>True if valid NounInfo was found, false otherwise</returns>
        private bool MoveToNextValidNounInfo(ref List<NounInfo>.Enumerator nounInfoEnum)
        {
            // move to next NounInfo for the current noun...if we're out, quit looking
            if (!nounInfoEnum.MoveNext())
                return false;

            // move to the next NounInfo that satisfies the TreeBank section constraint
            if (Sections != null)
                while (!Sections.Contains(TreeBankEngine.GetSectionNumber(nounInfoEnum.Current.File)))
                    // if we're out of NounInfo, return
                    if (!nounInfoEnum.MoveNext())
                        return false;

            return true;
        }

        #region members not implemented
        /// <summary>
        /// Not implemented
        /// </summary>
        /// <returns></returns>
        public override ClassifiableEntity GetPreviousInstance()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Not implemented
        /// </summary>
        public override int CurrentInstanceNumber
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
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
