using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    public class MDPRewardFunction<TState> {
        Dictionary<TState, Double> stateToReward;

        public MDPRewardFunction() {
            stateToReward = new Dictionary<TState, double>();
        }

        public double GetRewardFor(TState state) {
            return stateToReward[state];
        }

        public void SetReward(TState state, double reward) {
            stateToReward[state] = reward;
        }

        public override string ToString() {
            return stateToReward.ToString();
        }

        public MDPUtilityFunction<TState> AsUtilityFunction() {
            var uf = new MDPUtilityFunction<TState>();
            foreach (TState state in stateToReward.Keys) {
                uf.SetUtility(state, this.GetRewardFor(state));
            }
            return uf;
        }
    }
}
