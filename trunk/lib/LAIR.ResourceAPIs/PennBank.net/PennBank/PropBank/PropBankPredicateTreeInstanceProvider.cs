using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using LAIR.MachineLearning;
using LAIR.Collections.Generic;
using LAIR.ResourceAPIs.PennBank.TreeBank;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Returns training nodes from all PropBank predicate trees, where a single predicate tree is defined as
    /// an annotated instance of a PropBank verb. Since a single sentence can have multiple predicate trees (one for
    /// each predicating verb in the sentence) this returns "duplicate" parse tree nodes in the sense that
    /// the same parse nodes are returned for each predicating verb in the sentence, even though they're
    /// labeled differently.
    /// </summary>
    public class PropBankPredicateTreeInstanceProvider : TreeBankInstanceProvider
    {
        private IEnumerator<string> _verbEnum;
        private List<VerbInfo>.Enumerator _verbInfoEnum;
        private List<PropBankNode>.Enumerator _nodeEnum;

        /// <summary>
        /// Sets the verbs that training instances should be provided for (pass null for all verbs)
        /// </summary>
        public Set<string> Verbs
        {
            set
            {
                if (value != null)
                {
                    if (value.Count == 0)
                        throw new Exception("It makes no sense to train over zero verbs");

                    _verbEnum = value.GetEnumerator();
                }
                // if value is null, use all verbs
                else
                    _verbEnum = (TreeBankEngine as PropBankEngine).AllVerbs.GetEnumerator();
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="verbs">Verbs to get predicates trees for (null for all)</param>
        /// <param name="propBankEngine">PropBank engine to draw training predicate trees from</param>
        /// <param name="instanceFilter">Instance filter to apply</param>
        /// <param name="sections">TreeBank sections to draw instances from</param>
        public PropBankPredicateTreeInstanceProvider(Set<string> verbs,
                                                     PropBankEngine propBankEngine,
                                                     InstanceFilterDelegate instanceFilter,
                                                     Set<int> sections)
            : base(propBankEngine, instanceFilter, sections)
        {
            Verbs = verbs;
        }

        /// <summary>
        /// Starts training instance iterator
        /// </summary>
        public override void Start()
        {
            // reset verb enumerator to beginning
            _verbEnum.Reset();

            // reset other enumerators
            _verbInfoEnum = new List<VerbInfo>().GetEnumerator();
            _nodeEnum = new List<PropBankNode>().GetEnumerator();
        }

        /// <summary>
        /// Gets next training instance for models build over PropBank
        /// </summary>
        /// <returns>Next training instance</returns>
        public override ClassifiableEntity GetNextInstance()
        {
            // try to move to next node
            while (!_nodeEnum.MoveNext())
            {
                PropBankEngine propBankEngine = TreeBankEngine as PropBankEngine;

                // try to move to next VerbInfo
                while (!MoveToNextValidVerbInfo(ref _verbInfoEnum))
                {
                    // try to move to next verb...if there are none we're done
                    if (!_verbEnum.MoveNext())
                        return null;

                    // start before first VerbInfo for current verb
                    _verbInfoEnum = propBankEngine.GetVerbInfo(_verbEnum.Current).GetEnumerator();
                }

                // filter all nodes in the tree, keeping the good ones
                PropBankNode root = propBankEngine.GetPropBankTree(_verbInfoEnum.Current);
                List<PropBankNode> filteredNodes = new List<PropBankNode>();
                foreach (PropBankNode n in root.AllNodes)
                    if (Filter(n))
                        filteredNodes.Add(n);

                _nodeEnum = filteredNodes.GetEnumerator();
            }

            return _nodeEnum.Current;
        }

        /// <summary>
        /// Moves a VerbInfo enumerator to the next valid entry
        /// </summary>
        /// <param name="verbInfoEnum">VerbInfo enumerator to move</param>
        /// <returns>True if valid VerbInfo was found, false otherwise</returns>
        private bool MoveToNextValidVerbInfo(ref List<VerbInfo>.Enumerator verbInfoEnum)
        {
            // move to next VerbInfo for the current verb...if we're out, quit looking
            if (!verbInfoEnum.MoveNext())
                return false;

            // move to the next VerbInfo that satisfies the TreeBank section constraint
            if (Sections != null)
                while (!Sections.Contains(TreeBankEngine.GetSectionNumber(verbInfoEnum.Current.File)))
                    // if we're out of VerbInfo, return
                    if (!verbInfoEnum.MoveNext())
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
