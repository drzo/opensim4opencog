using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Learners
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Learning.Inductive;

    public class DecisionListLearner : ILearner 
    {
        public static readonly string Failure = "Failure";

        private DecisionList decisionList;

        private string positive, negative;

        private DLTestFactory testFactory;

        public DecisionListLearner(string positive, string negative,
                DLTestFactory testFactory) {
            this.positive = positive;
            this.negative = negative;
            this.testFactory = testFactory;
        }

        public void Train(DataSet ds) {
            this.decisionList = this.DecisionListLearning(ds);
        }

        public string Predict(Example e) {
            if (decisionList == null) {
                throw new ApplicationException(
                        "learner has not been trained with dataset yet!");
            }
            return decisionList.Predict(e);
        }

        public int[] Test(DataSet ds) {
            int[] results = new int[] { 0, 0 };

            foreach (Example e in ds.examples) 
            {
                if (e.TargetValue().Equals(decisionList.Predict(e))) {
                    results[0] = results[0] + 1;
                } else {
                    results[1] = results[1] + 1;
                }
            }
            return results;
        }

        public DecisionList GetDecisionList() {
            return decisionList;
        }

        private DecisionList DecisionListLearning(DataSet ds) {
            if (ds.Count == 0) 
            {
                return new DecisionList(positive, negative);
            }
            var possibleTests = testFactory.CreateDLTestsWithAttributeCount(ds, 1);
            DLTest test = this.GetValidTest(possibleTests, ds);
            if (test == null) {
                return new DecisionList(null, Failure);
            }
            // at this point there is a test that classifies some subset of examples
            // with the same target value
            DataSet matched = test.MatchedExamples(ds);
            var list = new DecisionList(positive, negative);
            list.Add(test, matched.GetExample(0).TargetValue());
            return list.MergeWith(this.DecisionListLearning(test.UnmatchedExamples(ds)));
        }

        private DLTest GetValidTest(IList<DLTest> possibleTests, DataSet ds)
        {
            return (from test in possibleTests
                    let matched = test.MatchedExamples(ds)
                    where matched.Count != 0
                    where this.AllExamplesHaveSameTargetValue(matched)
                    select test).FirstOrDefault();
        }

        private bool AllExamplesHaveSameTargetValue(DataSet matched) 
        {
            // assumes at least i example in dataset
            string targetValue = matched.GetExample(0).TargetValue();
            return matched.examples.All(e => e.TargetValue().Equals(targetValue));
        }
    }
}
