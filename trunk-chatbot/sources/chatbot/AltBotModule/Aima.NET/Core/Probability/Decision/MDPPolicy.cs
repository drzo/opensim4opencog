using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Decision
{
    public class MDPPolicy<TState, TAction> {
        Dictionary<TState, TAction> stateToAction;

        public MDPPolicy() {
            stateToAction = new Dictionary<TState, TAction>();
        }

        public TAction GetAction(TState state) {
            return stateToAction[state];
        }

        public void SetAction(TState state, TAction action) {
            stateToAction[state] = action;
        }

        public override string ToString() {
            return stateToAction.ToString();
        }

        //TODO: What is the proper replacement for java's Set? Is it IEnumerable as this appears to be used for Keys collection anyway
        public IEnumerable<TState> states() {

            return stateToAction.Keys;
        }
    }
}
