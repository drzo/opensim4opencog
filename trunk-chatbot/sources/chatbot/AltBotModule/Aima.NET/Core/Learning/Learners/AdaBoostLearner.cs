using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Learners
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Util.Datastructure;

    public class AdaBoostLearner : ILearner 
    {

        private IList<ILearner> learners;

        private DataSet dataSet;

        private double[] exampleWeights;

        private Dictionary<ILearner, double> learnerWeights;

        public AdaBoostLearner(IList<ILearner> learners, DataSet ds) {
            this.learners = learners;
            this.dataSet = ds;

            this.InitializeExampleWeights(ds.examples.Count);
            this.InitializeHypothesisWeights(learners.Count);
        }

        public void Train(DataSet ds) {
            this.InitializeExampleWeights(ds.examples.Count);

            foreach (ILearner learner in learners) 
            {
                learner.Train(ds);

                double error = this.CalculateError(ds, learner);
                if (error < 0.0001) {
                    break;
                }

                this.AdjustExampleWeights(ds, learner, error);

                double newHypothesisWeight = learnerWeights[learner] * Math.Log((1.0 - error) / error);
                learnerWeights[learner] = newHypothesisWeight;
            }
        }

        public string Predict(Example e) {
            return this.WeightedMajority(e);
        }

        public int[] Test(DataSet ds)
        {
            int[] results = new int[] { 0, 0 };

            foreach (Example e in ds.examples)
            {
                if (e.TargetValue().Equals(this.Predict(e)))
                {
                    results[0] = results[0] + 1;
                }
                else
                {
                    results[1] = results[1] + 1;
                }
            }
            return results;
        }

        private string WeightedMajority(Example e) {
            IList<string> targetValues = dataSet.GetPossibleAttributeValues(dataSet
                    .GetTargetAttributeName());

            Table<string, ILearner, double> table = this.CreateTargetValueLearnerTable(
                    targetValues, e);
            return this.GetTargetValueWithTheMaximumVotes(targetValues, table);
        }

        private Table<string, ILearner, double> CreateTargetValueLearnerTable(
                IList<string> targetValues, Example e) {
            // create a table with target-attribute values as rows and learners as
            // columns and cells containing the weighted votes of each ILearner for a
            // target value
            // Learner1 Learner2 Laerner3 .......
            // Yes 0.83 0.5 0
            // No 0 0 0.6

            Table<string, ILearner, double> table = new Table<string, ILearner, double>(
                    targetValues, learners);
            // initialize table
            foreach (ILearner l in learners) {
                foreach (string s in targetValues) {
                    table.Set(s, l, 0.0);
                }
            }
            foreach (ILearner learner in learners) {
                string predictedValue = learner.Predict(e);
                foreach (string v in targetValues) {
                    if (predictedValue.Equals(v)) {
                        table.Set(v, learner, table.Get(v, learner) + learnerWeights[learner] * 1);
                    }
                }
            }
            return table;
        }

        private string GetTargetValueWithTheMaximumVotes(IList<string> targetValues,
                Table<string, ILearner, double> table) {
            string targetValueWithMaxScore = targetValues[0];
            double score = this.ScoreOfValue(targetValueWithMaxScore, table, learners);
            foreach (string value in targetValues) {
                double scoreOfValue = ScoreOfValue(value, table, learners);
                if (scoreOfValue > score) 
                {
                    targetValueWithMaxScore = value;
                    score = scoreOfValue;
                }
            }
            return targetValueWithMaxScore;
        }

        private void InitializeExampleWeights(int size) {
            if (size == 0) {
                throw new ApplicationException(
                        "cannot initialize Ensemble learning with Empty Dataset");
            }
            double value = 1.0 / (1.0 * size);
            exampleWeights = new double[size];
            for (int i = 0; i < size; i++) {
                exampleWeights[i] = value;
            }
        }

        private void InitializeHypothesisWeights(int size) {
            if (size == 0) {
                throw new ApplicationException(
                        "cannot initialize Ensemble learning with Zero Learners");
            }

            learnerWeights = new Dictionary<ILearner, double>();
            foreach (ILearner le in learners) {
                learnerWeights[le] = 1.0;
            }
        }

        private double CalculateError(DataSet ds, ILearner l) {
            double error = 0.0;
            for (int i = 0; i < ds.examples.Count; i++) {
                Example e = ds.GetExample(i);
                if (!(l.Predict(e).Equals(e.TargetValue()))) {
                    error = error + exampleWeights[i];
                }
            }
            return error;
        }

        private void AdjustExampleWeights(DataSet ds, ILearner l, double error) {
            double epsilon = error / (1.0 - error);
            for (int j = 0; j < ds.examples.Count; j++) {
                Example e = ds.GetExample(j);
                if ((l.Predict(e).Equals(e.TargetValue()))) {
                    exampleWeights[j] = exampleWeights[j] * epsilon;
                }
            }
            exampleWeights = Util.Util.Normalize(exampleWeights);
        }

        private double ScoreOfValue(string targetValue,
                Table<string, ILearner, double> table, IList<ILearner> learners) {
            double score = 0.0;
            foreach (ILearner l in learners) {
                score += table.Get(targetValue, l);
            }
            return score;
        }
    }

}
