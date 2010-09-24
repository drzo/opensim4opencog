using System;
using System.Collections.Generic;
using System.Text;
using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Represents a collection of labeled PropBank nodes
    /// </summary>
    public class PropBankLabeledNodeCollection : LabeledNodeCollection
    {
        private PropBankNodeLabel _label;

        /// <summary>
        /// Gets the label on this node list
        /// </summary>
        public PropBankNodeLabel Label
        {
            get { return _label; }
        }

        /// <summary>
        /// Gets node locations for this list of nodes
        /// </summary>
        public override string NodeLocations
        {
            get
            {
                string locations = base.NodeLocations +                                      // node locations
                                   "-" + PropBankNodeLabel.GetNodeTypeString(_label.Type) +  // label type
                                   (_label.Feature != PropBankNodeLabel.NodeFeature.None ? "-" + PropBankNodeLabel.GetNodeFeatureString(_label.Feature) : "");  // feature

                return locations;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="label">Label for this collection</param>
        public PropBankLabeledNodeCollection(PropBankNodeLabel label)
        {
            _label = label;
        }

        /// <summary>
        /// Adds a single node to this list
        /// </summary>
        /// <param name="singleNode">Single node to add</param>
        public override void AddSingleNode(TreeBankNode singleNode)
        {
            // apply label to node
            PropBankNode propBankNode = singleNode as PropBankNode;
            propBankNode.SetLabel(_label, false);

            base.AddSingleNode(propBankNode);
        }

        /// <summary>
        /// Adds a split node to this list
        /// </summary>
        /// <param name="splitNode">Nodes that make up split node</param>
        public override void AddSplitNode(List<TreeBankNode> splitNode)
        {
            // re-type all nodes in split node
            foreach (PropBankNode node in splitNode)
                node.SetLabel(_label, false);

            base.AddSplitNode(splitNode);
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
            PropBankLabeledNodeCollection copy = new PropBankLabeledNodeCollection(new PropBankNodeLabel(_label.Type, _label.Feature, _label.Confidence));

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
