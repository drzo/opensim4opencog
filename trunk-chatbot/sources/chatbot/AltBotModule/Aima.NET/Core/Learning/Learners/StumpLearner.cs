using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Learners
{
    using Aima.Core.Learning.Framework;
    using Aima.Core.Learning.Inductive;

    public class StumpLearner : DecisionTreeLearner 
    {

        public StumpLearner(DecisionTree sl, string unableToClassify) : base(sl, unableToClassify)
        {
        }

        public override void Train(DataSet ds) 
        {
            // System.out.println("Stump learner training");
            // do nothing the stump is not inferred from the dataset
        }
    }
}
