using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Inductive
{
    using Aima.Core.Learning.Framework;

    public class DecisionTree 
    {
        private String attributeName;

        // each node modelled as a hash of attribute_value/decisiontree
        private Dictionary<String, DecisionTree> nodes;

        protected DecisionTree() 
        {

        }

        public DecisionTree(String attributeName)
        {
            this.attributeName = attributeName;
            nodes = new Dictionary<String, DecisionTree>();

        }

        public virtual void AddLeaf(string attributeValue, string decision) 
        {
            //TODO: verify that this interdependency is ok
            nodes[attributeValue] = new ConstantDecisonTree(decision);
        }

        public virtual void AddNode(string attributeValue, DecisionTree tree) 
        {
            nodes[attributeValue] = tree;
        }

        public virtual object Predict(Example e) 
        {
            var attrValue = e.GetAttributeValueAsString(attributeName);
            if (nodes.ContainsKey(attrValue)) 
            {
                return nodes[attrValue].Predict(e);
            } 
            else 
            {
                throw new ApplicationException("no node exists for attribute value " + attrValue);
            }
        }

        public static DecisionTree GetStumpFor(DataSet ds, string attributeName, string attributeValue, string returnValueIfMatched,
                List<string> unmatchedValues, string returnValueIfUnmatched) 
        {
            var dt = new DecisionTree(attributeName);
            dt.AddLeaf(attributeValue, returnValueIfMatched);
            foreach (string unmatchedValue in unmatchedValues) 
            {
                dt.AddLeaf(unmatchedValue, returnValueIfUnmatched);
            }
            return dt;
        }

        public static List<DecisionTree> GetStumpsFor(DataSet ds,
                String returnValueIfMatched, String returnValueIfUnmatched) 
        {
            var attributes = ds.GetNonTargetAttributes();
            return (from attribute in attributes
                    let values = ds.GetPossibleAttributeValues(attribute)
                    from value in values
                    let unmatchedValues = Util.Util.RemoveFrom(ds.GetPossibleAttributeValues(attribute), value)
                    select GetStumpFor(ds, attribute, value, returnValueIfMatched, unmatchedValues, returnValueIfUnmatched)).ToList();
        }

        public string GetAttributeName() 
        {
            return attributeName;
        }

        public override string ToString() 
        {
            return ToString(1, new StringBuilder());
        }

        public virtual string ToString(int depth, StringBuilder buf)
        {
            if (this.attributeName != null)
            {
                buf.Append(Util.Util.Ntimes("\t", depth));
                buf.Append(Util.Util.Ntimes("***", 1));
                buf.Append(this.attributeName + " \n");
                foreach (var attributeValue in this.nodes.Keys)
                {
                    buf.Append(Util.Util.Ntimes("\t", depth + 1));
                    buf.Append("+" + attributeValue);
                    buf.Append("\n");
                    var child = this.nodes[attributeValue];
                    buf.Append(child.ToString(depth + 1, new StringBuilder()));
                }
            }

            return buf.ToString();
        }
    }

}
