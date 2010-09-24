using System;
using System.Collections.Generic;
using System.Text;
using LAIR.ResourceAPIs.PennBank.PropBank;
using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Represents a collection of labeled NomBank nodes
    /// </summary>
    public class NomBankLabeledNodeCollection : LabeledNodeCollection
    {
        private NomBankNodeLabel _label;
        private List<NomBankNodeLabel.HyphenationIndex> _appliedIndexes;

        /// <summary>
        /// Gets or sets the label used in this collection
        /// </summary>
        public NomBankNodeLabel Label
        {
            get { return _label; }
            set { _label = value; }
        }

        /// <summary>
        /// Gets the list of indexes applied to nodes in this collection
        /// </summary>
        public List<NomBankNodeLabel.HyphenationIndex> AppliedIndexes
        {
            get { return _appliedIndexes; }
            set { _appliedIndexes = value; }
        }

        /// <summary>
        /// Gets location label for this collection
        /// </summary>
        public override string NodeLocations
        {
            get
            {
                string locations = base.NodeLocations +                                     // node locations
                                   "-" + NomBankNodeLabel.GetNodeTypeString(_label.Type) +  // label type
                                   (_label.Feature != NomBankNodeLabel.NodeFeature.None ? "-" + NomBankNodeLabel.GetNodeFeatureString(_label.Feature) : "");  // feature

                // add hyphen indexes
                foreach (NomBankNodeLabel.HyphenationIndex hyphenIndex in _label.HyphenIndexes)
                    locations += "-" + hyphenIndex;

                return locations;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="label">Label for collection</param>
        public NomBankLabeledNodeCollection(NomBankNodeLabel label)
        {
            _label = label;
            _appliedIndexes = new List<NomBankNodeLabel.HyphenationIndex>();
        }

        /// <summary>
        /// Adds a single node to this collection
        /// </summary>
        /// <param name="singleNode">Single node to add</param>
        public override void AddSingleNode(TreeBankNode singleNode)
        {
            ApplyType(singleNode as NomBankNode);

            base.AddSingleNode(singleNode);
        }

        /// <summary>
        /// Adds a split node to this collection
        /// </summary>
        /// <param name="splitNode">Split node to add</param>
        public override void AddSplitNode(List<TreeBankNode> splitNode)
        {
            foreach (NomBankNode node in splitNode)
                ApplyType(node);

            base.AddSplitNode(splitNode);
        }

        /// <summary>
        /// Applies the current type to a node, or does nothing if the node already has the type
        /// </summary>
        /// <param name="n">Node to apply type to</param>
        private void ApplyType(NomBankNode n)
        {
            // don't reapply type
            if (n.HasLabel(_label))
                return;

            // add label without hyphen indexes, and don't bother synching with root collection (this collection will be added to the root)
            NomBankNodeLabel label = _label.Copy();
            label.HyphenIndexes.Clear();
            n.AddLabel(label, false);

            // only apply hyphen indexes to text with a hyphen or slash
            string nodeText = n.SurfaceText;
            char[] hyphenSlash = new char[] { '-', '/' };
            if (nodeText.IndexOfAny(hyphenSlash) == -1)
                return;

            // try to apply each hyphen index
            foreach (NomBankNodeLabel.HyphenationIndex hyphenIndex in _label.HyphenIndexes)
            {
                int numParts = nodeText.Split(hyphenSlash, StringSplitOptions.RemoveEmptyEntries).Length;

                // get numeric hyphen index
                int index = int.Parse(hyphenIndex.ToString().Substring(1));

                // add index to node's label if applicable
                if (index < numParts)
                {
                    if (_appliedIndexes.Contains(hyphenIndex))
                        throw new Exception("Hyphen index applied more than once");

                    label.AddHyphenIndex(hyphenIndex);
                    _appliedIndexes.Add(hyphenIndex);
                }
            }
        }

        /// <summary>
        /// Gets node locations
        /// </summary>
        /// <returns>Node locations</returns>
        public override string ToString()
        {
            return NodeLocations;
        }

        /// <summary>
        /// Gets a copy of this collection
        /// </summary>
        /// <returns>Copy of this collection</returns>
        public override LabeledNodeCollection Copy()
        {
            // create copy
            NomBankLabeledNodeCollection copy = new NomBankLabeledNodeCollection(_label.Copy());

            // add single nodes
            foreach (TreeBankNode singleNode in SingleNodes)
                copy.AddSingleNode(singleNode);

            // add split nodes
            foreach (List<TreeBankNode> splitNode in SplitNodes)
                copy.AddSplitNode(splitNode);

            return copy;
        }
    }
}
