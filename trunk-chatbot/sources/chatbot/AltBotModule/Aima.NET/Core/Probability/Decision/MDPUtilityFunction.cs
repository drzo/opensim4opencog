using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    using System.Diagnostics;

    public class MDPUtilityFunction<TState> {
        private Dictionary<TState, double> hash;

        public MDPUtilityFunction() {
            hash = new Dictionary<TState, double>();
        }

        public double GetUtility(TState state)
        {

            double d = 0;
            if (hash.ContainsKey(state))
            {
                d = hash[state];
            }
            else
            {
                Debug.Print("no value for " + state);
            }
            return d;
        }

        public void SetUtility(TState state, double utility) {
            hash[state] = utility;
        }

        public MDPUtilityFunction<TState> Copy() 
        {
            var other = new MDPUtilityFunction<TState>();
            foreach (TState state in hash.Keys) {
                other.SetUtility(state, hash[state]);
            }
            return other;
        }

        public override string ToString() {
            return hash.ToString();
        }

        public bool HasUtilityFor(TState state) {

            return hash.Keys.Contains(state);
        }
    }

}
