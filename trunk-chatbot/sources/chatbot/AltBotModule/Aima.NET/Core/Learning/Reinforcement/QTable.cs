using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Learning.Reinforcement
{
    using Aima.Core.Probability.Decision;
    using Aima.Core.Util.Datastructure;

    public class QTable<TState, TAction>
        where TAction : class
    {

        Dictionary<Pair<TState, TAction>, double> table;

        private IList<TAction> allPossibleActions;

        public QTable(IList<TAction> allPossibleActions) {
            this.table = new Dictionary<Pair<TState, TAction>, double>();
            this.allPossibleActions = allPossibleActions;
        }

        public double GetQValue(TState state, TAction action) {
            var stateActionPair = new Pair<TState, TAction>(
                    state, action);
            if (!(table.Keys.Contains(stateActionPair))) {
                return 0.0;
            }
            return table[stateActionPair];
        }

        public Pair<TAction, double> MaxDiff(TState startState,
                TAction action, TState endState) {
            double maxDiff = 0.0;
            TAction maxAction = null;
            // randomly choose an action so that it doesn't return the same action
            // every time if all Q(a,s) are zero
            maxAction = Util.Util.SelectRandomlyFromList(allPossibleActions);
            maxDiff = this.GetQValue(endState, maxAction)
                    - this.GetQValue(startState, action);

            foreach (TAction anAction in allPossibleActions) 
            {
                double diff = this.GetQValue(endState, anAction)
                        - this.GetQValue(startState, action);
                if (diff > maxDiff) {
                    maxAction = anAction;
                    maxDiff = diff;
                }
            }

            return new Pair<TAction, double>(maxAction, maxDiff);
        }

        public void SetQValue(TState state, TAction action, double d) {
            var stateActionPair = new Pair<TState, TAction>(state, action);
            table[stateActionPair] = d;
        }

        public TAction UpDateQ(TState startState, TAction action,
                TState endState, double alpha, double reward, double phi) {
            double oldQValue = this.GetQValue(startState, action);
            Pair<TAction, double> actionAndMaxDiffValue = this.MaxDiff(startState,
                    action, endState);
            double addedValue = alpha
                    * (reward + (phi * actionAndMaxDiffValue.GetSecond()));
            this.SetQValue(startState, action, oldQValue + addedValue);
            return actionAndMaxDiffValue.GetFirst();
        }

        public void Normalize() {
            double maxValue = this.FindMaximumValue();
            if (maxValue != 0.0) {
                foreach (Pair<TState, TAction> key in table.Keys) 
                {
                    double presentValue = table[key];
                    table[key] = presentValue / maxValue;
                }
            }
        }

        public MDPPolicy<TState, TAction> GetPolicy() {
            var policy = new MDPPolicy<TState, TAction>();
            var startingStatesRecorded = this.GetAllStartingStates();

            foreach (TState state in startingStatesRecorded) {
                TAction action = this.GetRecordedActionWithMaximumQValue(state);
                policy.SetAction(state, action);
            }
            return policy;
        }

        public override string ToString() {
            return table.ToString();
        }

        private double FindMaximumValue() {
            if (table.Keys.Count > 0) 
            {
                double maxValue = table[table.Keys.First()];
                foreach (Pair<TState, TAction> key in table.Keys) {
                    double v = table[key];
                    if (v > maxValue) {
                        maxValue = v;
                    }
                }
                return maxValue;

            } else {
                return 0.0;
            }
        }

        private TAction GetRecordedActionWithMaximumQValue(TState state) {
            double maxValue = double.NegativeInfinity;
            TAction action = null;
            foreach (Pair<TState, TAction> stateActionPair in table.Keys) {
                if (stateActionPair.GetFirst().Equals(state)) {
                    TAction ac = stateActionPair.GetSecond();
                    double value = table[stateActionPair];
                    if (value > maxValue) {
                        maxValue = value;
                        action = ac;
                    }
                }
            }
            return action;
        }

        private IList<TState> GetAllStartingStates() {
            IList<TState> states = new List<TState>();
            foreach (Pair<TState, TAction> stateActionPair in table.Keys) {
                TState state = stateActionPair.GetFirst();
                if (!(states).Contains(state)) {
                    states.Add(state);
                }
            }
            return states;
        }
    }
}
