using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    public class MDPPerception<TState> {

        private TState state;

        private double reward;

        public MDPPerception(TState state, double reward) {
            this.state = state;
            this.reward = reward;
        }

        public double GetReward() {
            return reward;
        }

        public TState GetState() {
            return state;
        }

        public override string ToString() {
            return string.Format("[ {0} , {1} ] ", state, reward);
        }
    }

}
