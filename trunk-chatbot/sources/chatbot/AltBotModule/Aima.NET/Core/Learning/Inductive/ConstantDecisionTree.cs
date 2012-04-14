using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Inductive
{
    using Aima.Core.Learning.Framework;

    public class ConstantDecisonTree : DecisionTree 
    {
        // represents leaf nodes like "Yes" or "No"
        private String value;

        public ConstantDecisonTree(String value) 
        {
            this.value = value;
        }

        public override void AddLeaf(string attributeValue, string decision) 
        {
            throw new ApplicationException("cannot add Leaf to ConstantDecisonTree");
        }

        public override void AddNode(string attributeValue, DecisionTree tree) {
            throw new ApplicationException("cannot add Node to ConstantDecisonTree");
        }

        public override object Predict(Example e) 
        {
            return value;
        }

        public override string ToString() 
        {
            return String.Format("DECISION -> {0}", value);
        }

        public override string ToString(int depth, StringBuilder buf) 
        {
            buf.Append(Util.Util.Ntimes("\t", depth + 1));
            buf.Append(String.Format("DECISION -> {0}\n", this.value));
            return buf.ToString();
        }
    }
}
