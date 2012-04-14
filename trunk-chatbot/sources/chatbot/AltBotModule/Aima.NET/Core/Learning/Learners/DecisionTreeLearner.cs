using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Learners
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Learning.Inductive;

    public class DecisionTreeLearner : ILearner 
    {
        private DecisionTree tree;

        private string defaultValue;

        public DecisionTreeLearner() {
            this.defaultValue = "Unable To Classify";

        }

        // used when you have to test a non induced tree (eg: for testing)
        public DecisionTreeLearner(DecisionTree tree, string defaultValue) {
            this.tree = tree;
            this.defaultValue = defaultValue;
        }

        public virtual void Train(DataSet ds) {
            var attributes = ds.GetNonTargetAttributes();
            this.tree = this.DecisionTreeLearning(ds, attributes,
                    new ConstantDecisonTree(defaultValue));
        }

        public string Predict(Example e) {
            return (string) tree.Predict(e);
        }

        public int[] Test(DataSet ds) {
            int[] results = new int[] { 0, 0 };

            foreach (Example e in ds.examples) 
            {
                if (e.TargetValue().Equals(tree.Predict(e))) {
                    results[0] = results[0] + 1;
                } else {
                    results[1] = results[1] + 1;
                }
            }
            return results;
        }

        public DecisionTree GetDecisionTree() {
            return tree;
        }

        //
        // PRIVATE METHODS
        //

        private DecisionTree DecisionTreeLearning(DataSet ds,
                IList<string> attributeNames, ConstantDecisonTree defaultTree) {
            if (ds.Count == 0) {
                return defaultTree;
            }
            if (this.AllExamplesHaveSameClassification(ds)) {
                return new ConstantDecisonTree(ds.GetExample(0).TargetValue());
            }
            if (attributeNames.Count == 0) {
                return this.MajorityValue(ds);
            }
            string chosenAttribute = this.ChooseAttribute(ds, attributeNames);

            var tree = new DecisionTree(chosenAttribute);
            ConstantDecisonTree m = this.MajorityValue(ds);

            IList<string> values = ds.GetPossibleAttributeValues(chosenAttribute);
            foreach (string v in values) {
                DataSet filtered = ds.MatchingDataSet(chosenAttribute, v);
                IList<string> newAttribs = Util.Util.RemoveFrom(attributeNames,
                        chosenAttribute);
                DecisionTree subTree = this.DecisionTreeLearning(filtered, newAttribs, m);
                tree.AddNode(v, subTree);

            }

            return tree;
        }

        private ConstantDecisonTree MajorityValue(DataSet ds) {
            ILearner learner = new MajorityLearner();
            learner.Train(ds);
            return new ConstantDecisonTree(learner.Predict(ds.GetExample(0)));
        }

        private string ChooseAttribute(DataSet ds, IList<string> attributeNames) {
            double greatestGain = 0.0;
            string attributeWithGreatestGain = attributeNames[0];
            foreach (string attr in attributeNames) 
            {
                double gain = ds.CalculateGainFor(attr);
                if (gain > greatestGain) {
                    greatestGain = gain;
                    attributeWithGreatestGain = attr;
                }
            }

            return attributeWithGreatestGain;
        }

        private bool AllExamplesHaveSameClassification(DataSet ds) {
            string classification = ds.GetExample(0).TargetValue();
            return ds.All(e => e.TargetValue().Equals(classification));
        }
    }

}
