using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    public class HMMAgent
    {
        private HiddenMarkovModel hmm;

        public RandomVariable Belief { get; private set; }

        public HMMAgent(HiddenMarkovModel hmm)
        {
            this.hmm = hmm;
            this.Belief = hmm.Prior().Duplicate();
        }

        public void Act(String action)
        {
            this.Belief = hmm.Predict(this.Belief, action);
        }

        public void WaitWithoutActing()
        {
            this.Act(HmmConstants.DoNothing);
        }

        public void Perceive(String perception)
        {
            this.Belief = hmm.PerceptionUpdate(this.Belief, perception);
        }
    }

}
