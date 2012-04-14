using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Learners
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Learning.Knowledge;
    using Aima.Core.Logic.FOL.Inference;
    using Aima.Core.Logic.FOL.KB;

    public class CurrentBestLearner : ILearner 
    {
        private string trueGoalValue;
        private FOLDataSetDomain folDSDomain;
        private FOLKnowledgeBase kb;
        private Hypothesis currentBestHypothesis;

        public CurrentBestLearner(string trueGoalValue) 
        {
            this.trueGoalValue = trueGoalValue;
        }

        public void Train(DataSet ds) 
        {

            folDSDomain = new FOLDataSetDomain(ds.Specification, trueGoalValue);
            IList<FOLExample> folExamples = new List<FOLExample>();
            int egNo = 1;
            foreach (Example e in ds.examples) 
            {
                folExamples.Add(new FOLExample(folDSDomain, e, egNo));
                egNo++;
            }

            // Setup a KB to be used for learning
            kb = new FOLKnowledgeBase(folDSDomain, new FOLOTTERLikeTheoremProver(1000, false));

            var cbl = new CurrentBestLearning(folDSDomain, kb);

            currentBestHypothesis = cbl.GetCurrentBestLearningHypothesis(folExamples);
        }

        public string Predict(Example e) 
        {
            string prediction = "~" + e.TargetValue();
            if (null != currentBestHypothesis) {
                var etp = new FOLExample(folDSDomain, e, 0);
                kb.Clear();
                kb.tell(etp.GetDescription());
                kb.tell(currentBestHypothesis.GetHypothesis());
                IInferenceResult ir = kb.Ask(etp.GetClassification());
                if (ir.IsTrue()) {
                    if (trueGoalValue.Equals(e.TargetValue())) {
                        prediction = e.TargetValue();
                    }
                } else if (ir.IsPossiblyFalse() || ir.IsUnknownDueToTimeout()) {
                    if (!trueGoalValue.Equals(e.TargetValue())) {
                        prediction = e.TargetValue();
                    }
                }
            }

            return prediction;
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
    }
}
